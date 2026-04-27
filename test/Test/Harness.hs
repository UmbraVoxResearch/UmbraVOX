-- | Multi-client integration test harness.
--
-- Provides helpers to create connected client pairs, perform handshakes,
-- exchange encrypted messages, and assert that no plaintext appears on
-- the wire.
module Test.Harness
    ( TestClient(..)
    , createClientPair
    , handshakeClients
    , clientSend
    , clientRecv
    , waitForHistory
    , getTrafficLog
    , assertNoPlaintextInTraffic
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')

import UmbraVox.Chat.Session (ChatSession, sendChatMessage, recvChatMessage)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey)
import UmbraVox.Network.Transport.Intercept
    (InterceptTransport, TrafficEntry(..), wrapWithIntercept)
import UmbraVox.Network.Transport.Loopback (newLoopbackPair)
import UmbraVox.Network.TransportClass (AnyTransport(..), anySend, anyRecv)
import UmbraVox.Protocol.Encoding (putWord32BE, getWord32BE)
import UmbraVox.TUI.Handshake (handshakeInitiator, handshakeResponder, genIdentity)

-- | A test client with identity, session state, message history, and transport.
data TestClient = TestClient
    { tcIdentity  :: !IdentityKey                -- ^ PQXDH identity key
    , tcSession   :: !(IORef (Maybe ChatSession)) -- ^ Current ratchet session
    , tcHistory   :: !(IORef [ByteString])        -- ^ Received plaintext history
    , tcTransport :: !AnyTransport                -- ^ Wrapped transport handle
    , tcName      :: !String                      -- ^ Logical name (e.g. "alice")
    }

-- | Create a connected pair of test clients over a loopback transport
-- with interception logging.
createClientPair :: IORef [TrafficEntry] -> IO (TestClient, TestClient)
createClientPair logRef = do
    -- Create loopback pair
    (loopA, loopB) <- newLoopbackPair "e2e"

    -- Shared monotonic counter for traffic entries
    counterRef <- newIORef (0 :: Int)

    -- Wrap both sides with interception
    interceptA <- wrapWithIntercept logRef counterRef "alice" "bob"
                      (AnyTransport loopA)
    interceptB <- wrapWithIntercept logRef counterRef "bob" "alice"
                      (AnyTransport loopB)

    -- Generate identity keys
    aliceId <- genIdentity
    bobId   <- genIdentity

    -- Create session and history refs
    aliceSess <- newIORef Nothing
    bobSess   <- newIORef Nothing
    aliceHist <- newIORef []
    bobHist   <- newIORef []

    let alice = TestClient
            { tcIdentity  = aliceId
            , tcSession   = aliceSess
            , tcHistory   = aliceHist
            , tcTransport = AnyTransport interceptA
            , tcName      = "alice"
            }
        bob = TestClient
            { tcIdentity  = bobId
            , tcSession   = bobSess
            , tcHistory   = bobHist
            , tcTransport = AnyTransport interceptB
            , tcName      = "bob"
            }

    pure (alice, bob)

-- | Perform a PQXDH handshake between two clients.
-- Bob (responder) sends his bundle first, Alice (initiator) receives it.
handshakeClients :: TestClient -> TestClient -> IO ()
handshakeClients alice bob = do
    -- Bob runs the responder in a separate thread
    resultVar <- newEmptyMVar :: IO (MVar ChatSession)
    _ <- forkIO $ do
        sess <- handshakeResponder (tcTransport bob) (tcIdentity bob)
        putMVar resultVar sess

    -- Small delay to ensure Bob's thread is ready to send
    threadDelay 10000  -- 10ms

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
    mSess <- readIORef (tcSession client)
    case mSess of
        Nothing   -> fail $ tcName client ++ ": no active session"
        Just sess -> do
            -- Read 4-byte length prefix
            lenBs <- anyRecv (tcTransport client) 4
            let len = fromIntegral (getWord32BE lenBs) :: Int
            -- Read the encrypted payload
            wireBytes <- anyRecv (tcTransport client) len
            -- Decrypt
            result <- recvChatMessage sess wireBytes
            case result of
                Nothing -> pure Nothing
                Just (sess', plaintext) -> do
                    writeIORef (tcSession client) (Just sess')
                    modifyIORef' (tcHistory client) (plaintext :)
                    pure (Just plaintext)

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
