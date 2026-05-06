-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ExistentialQuantification #-}
-- | Fault-injection hardening scenarios over the shared harness.
module Test.Hardening.Fault (runTests) where

import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Timeout (timeout)
import Data.Word (Word8)

import Test.Harness
    ( TestClient(..), createClientPair, handshakeClients, clientSend, clientRecv )
import Test.Util (assertEq)
import UmbraVox.Network.TransportClass
    ( AnyTransport(..), TransportHandle(..), anySend, anyRecv, anyClose, anyInfo )

runTests :: IO Bool
runTests = do
    putStrLn "[Hardening/Fault] Running fault-injection tests..."
    results <- sequence
        [ testDuplicateFrameReplayRejected
        , testCorruptedFrameRejected
        , testReorderedPairStillDecrypts
        , testDisconnectAfterSendFailsGracefully
        ]
    let passed = length (filter id results)
        total = length results
    putStrLn $ "[Hardening/Fault] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

data FaultMode
    = DuplicateOnce
    | CorruptOnce
    | ReversePair
    | DisconnectAfterSend

data FaultState = FaultState
    { fsTriggered :: !Bool
    , fsBuffered  :: !(Maybe BS.ByteString)
    }

data FaultyTransport = FaultyTransport
    { ftMode  :: !FaultMode
    , ftState :: !(IORef FaultState)
    , ftBase  :: !AnyTransport
    }

instance TransportHandle FaultyTransport where
    thSend ft payload = do
        st <- readIORef (ftState ft)
        case ftMode ft of
            DuplicateOnce
                | not (fsTriggered st) -> do
                    anySend (ftBase ft) payload
                    anySend (ftBase ft) payload
                    writeIORef (ftState ft) st { fsTriggered = True }
                | otherwise -> anySend (ftBase ft) payload
            CorruptOnce
                | not (fsTriggered st) -> do
                    anySend (ftBase ft) (corruptByte payload)
                    writeIORef (ftState ft) st { fsTriggered = True }
                | otherwise -> anySend (ftBase ft) payload
            ReversePair ->
                case fsBuffered st of
                    Nothing ->
                        writeIORef (ftState ft) st { fsBuffered = Just payload }
                    Just prev -> do
                        anySend (ftBase ft) payload
                        anySend (ftBase ft) prev
                        writeIORef (ftState ft) st { fsTriggered = True, fsBuffered = Nothing }
            DisconnectAfterSend
                | not (fsTriggered st) -> do
                    anySend (ftBase ft) payload
                    anyClose (ftBase ft)
                    writeIORef (ftState ft) st { fsTriggered = True }
                | otherwise -> pure ()
    thRecv = anyRecv . ftBase
    thClose = anyClose . ftBase
    thInfo ft = "fault:" ++ anyInfo (ftBase ft)

runTestsWithFault :: FaultMode -> (TestClient -> TestClient -> IO Bool) -> IO Bool
runTestsWithFault mode action = do
    logRef <- newIORef []
    (alice0, bob) <- createClientPair logRef
    handshakeClients alice0 bob
    alice <- injectFault mode alice0
    action alice bob

injectFault :: FaultMode -> TestClient -> IO TestClient
injectFault mode client = do
    stateRef <- newIORef (FaultState False Nothing)
    let faulty = FaultyTransport mode stateRef (tcTransport client)
    pure client { tcTransport = AnyTransport faulty }

testDuplicateFrameReplayRejected :: IO Bool
testDuplicateFrameReplayRejected =
    runTestsWithFault DuplicateOnce $ \alice bob -> do
        clientSend alice (BC.pack "dup-me")
        first <- recvWithin bob
        second <- recvWithin bob
        let outcomes = [first, second]
            successes = length (filter (== Just (BC.pack "dup-me")) outcomes)
            failures = length (filter (== Nothing) outcomes)
        ok1 <- assertEq "duplicate: exactly one delivery succeeds" 1 successes
        ok2 <- assertEq "duplicate: exactly one replay rejected" 1 failures
        pure (ok1 && ok2)

testCorruptedFrameRejected :: IO Bool
testCorruptedFrameRejected =
    runTestsWithFault CorruptOnce $ \alice bob -> do
        clientSend alice (BC.pack "corrupt-me")
        result <- recvWithin bob
        assertEq "corrupt: frame rejected" Nothing result

testReorderedPairStillDecrypts :: IO Bool
testReorderedPairStillDecrypts =
    runTestsWithFault ReversePair $ \alice bob -> do
        clientSend alice (BC.pack "first")
        clientSend alice (BC.pack "second")
        msg1 <- recvWithin bob
        msg2 <- recvWithin bob
        ok1 <- assertEq "reorder: first recv is second message" (Just (BC.pack "second")) msg1
        ok2 <- assertEq "reorder: delayed first message still decrypts" (Just (BC.pack "first")) msg2
        pure (ok1 && ok2)

testDisconnectAfterSendFailsGracefully :: IO Bool
testDisconnectAfterSendFailsGracefully =
    runTestsWithFault DisconnectAfterSend $ \alice bob -> do
        clientSend alice (BC.pack "bye")
        result <- recvWithin bob
        assertEq "disconnect: first frame still delivered" (Just (BC.pack "bye")) result

corruptByte :: BS.ByteString -> BS.ByteString
corruptByte bs
    | BS.null bs = bs
    | otherwise = BS.cons (BS.head bs `xorByte` 0xff) (BS.tail bs)

xorByte :: Word8 -> Word8 -> Word8
xorByte = xor

recvWithin :: TestClient -> IO (Maybe BS.ByteString)
recvWithin client = do
    result <- timeout (2 * 1000000) (clientRecv client)
    pure (maybe Nothing id result)
