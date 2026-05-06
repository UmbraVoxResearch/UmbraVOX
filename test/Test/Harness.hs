-- SPDX-License-Identifier: Apache-2.0
-- | Multi-client integration test harness.
--
-- Provides helpers to create connected client pairs, perform handshakes,
-- exchange encrypted messages, and assert that no plaintext appears on
-- the wire.
module Test.Harness
    ( TransportBackend(..)
    , TestClient(..)
    , createClientPair
    , createClientPairWith
    , createNamedClientPair
    , createNamedClientPairWith
    , handshakeClients
    , clientSend
    , clientRecv
    , closeClient
    , waitForHistory
    , getTrafficLog
    , assertNoPlaintextInTraffic
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')

import UmbraVox.Chat.Session (ChatSession, sendChatMessage, recvChatMessage)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey)
import UmbraVox.Network.Transport.Intercept
    (TrafficEntry(..), wrapWithIntercept)
import UmbraVox.Network.Transport.Loopback (newLoopbackPair)
import UmbraVox.Network.Transport (connect, listen)
import UmbraVox.Network.TransportClass (AnyTransport(..), anySend, anyRecv, anyClose)
import UmbraVox.Protocol.Encoding (putWord32BE, getWord32BE)
import UmbraVox.TUI.Handshake (handshakeInitiator, handshakeResponder, genIdentity)

-- | Transport choices for integration scenarios.
data TransportBackend
    = LoopbackBackend
    | TCPBackend Int

-- | A test client with identity, session state, message history, and transport.
data TestClient = TestClient
    { tcIdentity  :: !IdentityKey                -- ^ PQXDH identity key
    , tcSession   :: !(IORef (Maybe ChatSession)) -- ^ Current ratchet session
    , tcSessionLock :: !(MVar ())                -- ^ Serializes ratchet updates
    , tcHistory   :: !(IORef [ByteString])        -- ^ Received plaintext history
    , tcTransport :: !AnyTransport                -- ^ Wrapped transport handle
    , tcName      :: !String                      -- ^ Logical name (e.g. "alice")
    }

-- | Create a connected pair of test clients over a loopback transport
-- with interception logging.
createClientPair :: IORef [TrafficEntry] -> IO (TestClient, TestClient)
createClientPair logRef =
    createNamedClientPairWith logRef LoopbackBackend "alice" Nothing "bob" Nothing

-- | Create a connected pair with an explicit backend.
createClientPairWith :: IORef [TrafficEntry] -> TransportBackend -> IO (TestClient, TestClient)
createClientPairWith logRef backend =
    createNamedClientPairWith logRef backend "alice" Nothing "bob" Nothing

-- | Create a connected pair of test clients with explicit endpoint names.
-- Identities may be supplied to model one logical client across multiple peers.
createNamedClientPair
    :: IORef [TrafficEntry]
    -> String
    -> Maybe IdentityKey
    -> String
    -> Maybe IdentityKey
    -> IO (TestClient, TestClient)
createNamedClientPair logRef leftName leftIdentity rightName rightIdentity =
    createNamedClientPairWith logRef LoopbackBackend leftName leftIdentity rightName rightIdentity

-- | Create a connected pair with explicit transport backend.
createNamedClientPairWith
    :: IORef [TrafficEntry]
    -> TransportBackend
    -> String
    -> Maybe IdentityKey
    -> String
    -> Maybe IdentityKey
    -> IO (TestClient, TestClient)
createNamedClientPairWith logRef backend leftName leftIdentity rightName rightIdentity = do
    (baseA, baseB) <- createTransportPair backend
    -- Shared monotonic counter for traffic entries
    counterRef <- newIORef (0 :: Int)

    -- Wrap both sides with interception
    interceptA <- wrapWithIntercept logRef counterRef leftName rightName
                      baseA
    interceptB <- wrapWithIntercept logRef counterRef rightName leftName
                      baseB

    -- Generate identity keys
    leftId  <- maybe genIdentity pure leftIdentity
    rightId <- maybe genIdentity pure rightIdentity

    -- Create session and history refs
    aliceSess <- newIORef Nothing
    bobSess   <- newIORef Nothing
    aliceLock <- newMVar ()
    bobLock   <- newMVar ()
    aliceHist <- newIORef []
    bobHist   <- newIORef []

    let alice = TestClient
            { tcIdentity  = leftId
            , tcSession   = aliceSess
            , tcSessionLock = aliceLock
            , tcHistory   = aliceHist
            , tcTransport = AnyTransport interceptA
            , tcName      = leftName
            }
        bob = TestClient
            { tcIdentity  = rightId
            , tcSession   = bobSess
            , tcSessionLock = bobLock
            , tcHistory   = bobHist
            , tcTransport = AnyTransport interceptB
            , tcName      = rightName
            }

    pure (alice, bob)

createTransportPair :: TransportBackend -> IO (AnyTransport, AnyTransport)
createTransportPair LoopbackBackend = do
    (loopA, loopB) <- newLoopbackPair "e2e"
    pure (AnyTransport loopA, AnyTransport loopB)
createTransportPair (TCPBackend port) = do
    serverVar <- newEmptyMVar
    _ <- forkIO $ do
        server <- listen port
        putMVar serverVar (AnyTransport server)
    threadDelay 50000
    client <- connect "127.0.0.1" port
    server <- takeMVar serverVar
    pure (AnyTransport client, server)

-- | Perform a PQXDH handshake between two clients.
-- Bob (responder) sends his bundle first, Alice (initiator) receives it.
handshakeClients :: TestClient -> TestClient -> IO ()
handshakeClients alice bob = do
    -- Bob runs the responder in a separate thread
    readyVar <- newEmptyMVar :: IO (MVar ())
    resultVar <- newEmptyMVar :: IO (MVar ChatSession)
    _ <- forkIO $ do
        putMVar readyVar ()
        sess <- handshakeResponder (tcTransport bob) (tcIdentity bob) (\_ -> pure True)
        putMVar resultVar sess

    -- Wait until the responder thread has started before initiating.
    takeMVar readyVar

    -- Alice runs the initiator on the main thread
    aliceSess <- handshakeInitiator (tcTransport alice) (tcIdentity alice)
    writeIORef (tcSession alice) (Just aliceSess)

    -- Wait for Bob's session
    bobSess <- takeMVar resultVar
    writeIORef (tcSession bob) (Just bobSess)

-- | Send an encrypted message from a client.
-- Encrypts via the Double Ratchet, then length-prefixes and sends.
clientSend :: TestClient -> ByteString -> IO ()
clientSend client msg = do
    withMVar (tcSessionLock client) $ \_ -> do
        mSess <- readIORef (tcSession client)
        case mSess of
            Nothing   -> fail $ tcName client ++ ": no active session"
            Just sess -> do
                (sess', wireBytes) <- sendChatMessage sess msg
                writeIORef (tcSession client) (Just sess')
                -- Length-prefix the wire bytes and send
                let lenPrefix = putWord32BE (fromIntegral (BS.length wireBytes))
                anySend (tcTransport client) (lenPrefix <> wireBytes)

-- | Receive and decrypt a message for a client.
-- Reads the length prefix, then the payload, decrypts, appends to history.
clientRecv :: TestClient -> IO (Maybe ByteString)
clientRecv client = do
    -- Read transport bytes outside the lock so receiving does not block local sends.
    lenBs <- anyRecv (tcTransport client) 4
    let len = fromIntegral (getWord32BE lenBs) :: Int
    wireBytes <- anyRecv (tcTransport client) len
    withMVar (tcSessionLock client) $ \_ -> do
        mSess <- readIORef (tcSession client)
        case mSess of
            Nothing   -> fail $ tcName client ++ ": no active session"
            Just sess -> do
                result <- recvChatMessage sess wireBytes
                case result of
                    Nothing -> pure Nothing
                    Just (sess', plaintext) -> do
                        writeIORef (tcSession client) (Just sess')
                        modifyIORef' (tcHistory client) (plaintext :)
                        pure (Just plaintext)

closeClient :: TestClient -> IO ()
closeClient = anyClose . tcTransport

-- | Wait up to @timeout@ milliseconds for @expectedCount@ messages in history.
-- Polls every 10ms. Returns 'True' if the expected count is reached.
waitForHistory :: TestClient -> Int -> Int -> IO Bool
waitForHistory client expectedCount timeout
    | timeout <= 0 = do
        hist <- readIORef (tcHistory client)
        pure (length hist >= expectedCount)
    | otherwise = do
        hist <- readIORef (tcHistory client)
        if length hist >= expectedCount
            then pure True
            else do
                threadDelay 10000  -- 10ms
                waitForHistory client expectedCount (timeout - 10)

-- | Retrieve the current traffic log (most recent entry first).
getTrafficLog :: IORef [TrafficEntry] -> IO [TrafficEntry]
getTrafficLog = readIORef

-- | Assert that a given plaintext bytestring does NOT appear as a
-- substring in any traffic entry's raw bytes. Returns 'True' if the
-- assertion holds (no plaintext found on wire).
assertNoPlaintextInTraffic :: IORef [TrafficEntry] -> ByteString -> IO Bool
assertNoPlaintextInTraffic logRef plaintext = do
    entries <- readIORef logRef
    let leaks = filter (\e -> plaintext `BS.isInfixOf` teRawBytes e) entries
    case leaks of
        [] -> do
            putStrLn $ "  PASS: no plaintext leak for " ++ show (BS.length plaintext) ++ " bytes"
            pure True
        _  -> do
            putStrLn $ "  FAIL: plaintext found in " ++ show (length leaks) ++ " traffic entries"
            pure False
