-- SPDX-License-Identifier: Apache-2.0
-- | Chat session test suite.
--
-- Tests initChatSession, sendChatMessage/recvChatMessage round-trip,
-- and multiple sequential messages.
module Test.Chat.Session (runTests) where

import Test.Util
import qualified Data.ByteString as BS
import Data.Word (Word64)
import UmbraVox.Chat.Session
    ( ChatSession(..), initChatSession, sendChatMessage, recvChatMessage )
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Signal.DoubleRatchet (RatchetState, ratchetInitBob)

-- | Wall-clock stub for tests (no real time needed).
testWallClock :: Word64
testWallClock = 1000

runTests :: IO Bool
runTests = do
    putStrLn "[ChatSession] Running chat session tests..."
    results <- sequence
        [ testSendRecvRoundTrip
        , testMultipleMessages
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[ChatSession] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Helper: create an Alice/Bob chat session pair from deterministic secrets.
mkSessionPair :: PRNG -> IO (ChatSession, BobState)
mkSessionPair g0 = do
    let (sharedSecret, g1) = nextBytes 32 g0
        (aliceDHSec,   g2) = nextBytes 32 g1
        (bobDHSec,     _)  = nextBytes 32 g2
        -- x25519 returns Maybe ByteString; the basepoint is non-degenerate so
        -- the result is always Just for non-zero secrets.
        Just bobDHPub = x25519 bobDHSec x25519Basepoint
    mAlice <- initChatSession sharedSecret aliceDHSec bobDHPub
    let alice = case mAlice of
                    Just s  -> s
                    Nothing -> error "mkSessionPair: initChatSession returned Nothing"
    let bobRatchet = ratchetInitBob sharedSecret bobDHSec
    pure (alice, BobState bobRatchet)

-- | Wrapper to hold Bob's ratchet state for use with recvChatMessage.
newtype BobState = BobState { bsRatchet :: RatchetState }

-- | Dummy 32-byte sender identity hash for tests.
testSenderId :: BS.ByteString
testSenderId = BS.replicate 32 0xAA

-- | Send a message from Alice and receive it as Bob.
testSendRecvRoundTrip :: IO Bool
testSendRecvRoundTrip = do
    let g = mkPRNG 100
    (alice, bob) <- mkSessionPair g
    let msg = strToBS "Hello Bob!"
    sendResult <- sendChatMessage alice testSenderId msg testWallClock
    case sendResult of
        Left _ -> putStrLn "  FAIL: send/recv round-trip (send error)" >> pure False
        Right (_alice', wire) -> do
            -- Bob decrypts using a ChatSession wrapping his ratchet state
            let bobSession = ChatSession { csRatchet = bsRatchet bob, csRouteTokens = Nothing }
            recvResult <- recvChatMessage bobSession Nothing wire
            case recvResult of
                Left _  -> putStrLn "  FAIL: send/recv round-trip (recv error)" >> pure False
                Right Nothing -> do
                    putStrLn "  FAIL: send/recv round-trip (decryption failed)"
                    pure False
                Right (Just (_, sid, pt)) -> do
                    ok1 <- assertEq "send/recv round-trip (plaintext)" msg pt
                    ok2 <- assertEq "send/recv round-trip (senderId)" testSenderId sid
                    pure (ok1 && ok2)

-- | Multiple messages in sequence.
testMultipleMessages :: IO Bool
testMultipleMessages = do
    let g = mkPRNG 200
    (alice0, bob) <- mkSessionPair g
    let bobSession0 = ChatSession { csRatchet = bsRatchet bob, csRouteTokens = Nothing }
        msg1 = strToBS "first"
        msg2 = strToBS "second message here"
        msg3 = strToBS "third"
    -- Message 1
    send1 <- sendChatMessage alice0 testSenderId msg1 testWallClock
    case send1 of
        Left _ -> putStrLn "  FAIL: multi-message 1 (send error)" >> pure False
        Right (alice1, wire1) -> do
            r1 <- recvChatMessage bobSession0 Nothing wire1
            case r1 of
                Left _  -> putStrLn "  FAIL: multi-message 1 (recv error)" >> pure False
                Right Nothing -> do
                    putStrLn "  FAIL: multi-message 1 (decryption failed)"
                    pure False
                Right (Just (bobSession1, _, pt1)) -> do
                    ok1 <- assertEq "multi-message 1" msg1 pt1
                    -- Message 2
                    send2 <- sendChatMessage alice1 testSenderId msg2 testWallClock
                    case send2 of
                        Left _ -> putStrLn "  FAIL: multi-message 2 (send error)" >> pure False
                        Right (alice2, wire2) -> do
                            r2 <- recvChatMessage bobSession1 Nothing wire2
                            case r2 of
                                Left _  -> putStrLn "  FAIL: multi-message 2 (recv error)" >> pure False
                                Right Nothing -> do
                                    putStrLn "  FAIL: multi-message 2 (decryption failed)"
                                    pure False
                                Right (Just (bobSession2, _, pt2)) -> do
                                    ok2 <- assertEq "multi-message 2" msg2 pt2
                                    -- Message 3
                                    send3 <- sendChatMessage alice2 testSenderId msg3 testWallClock
                                    case send3 of
                                        Left _ -> putStrLn "  FAIL: multi-message 3 (send error)" >> pure False
                                        Right (_, wire3) -> do
                                            r3 <- recvChatMessage bobSession2 Nothing wire3
                                            case r3 of
                                                Left _  -> putStrLn "  FAIL: multi-message 3 (recv error)" >> pure False
                                                Right Nothing -> do
                                                    putStrLn "  FAIL: multi-message 3 (decryption failed)"
                                                    pure False
                                                Right (Just (_, _, pt3)) -> do
                                                    ok3 <- assertEq "multi-message 3" msg3 pt3
                                                    pure (ok1 && ok2 && ok3)
