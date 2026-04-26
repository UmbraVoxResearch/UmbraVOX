-- | Chat session test suite.
--
-- Tests initChatSession, sendChatMessage/recvChatMessage round-trip,
-- and multiple sequential messages.
module Test.Chat.Session (runTests) where

import Test.Util
import UmbraVox.Chat.Session
    ( ChatSession(..), initChatSession, sendChatMessage, recvChatMessage )
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Signal.DoubleRatchet (RatchetState, ratchetInitBob)

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
        bobDHPub = x25519 bobDHSec x25519Basepoint
    alice <- initChatSession sharedSecret aliceDHSec bobDHPub
    let bobRatchet = ratchetInitBob sharedSecret bobDHSec
    pure (alice, BobState bobRatchet)

-- | Wrapper to hold Bob's ratchet state for use with recvChatMessage.
newtype BobState = BobState { bsRatchet :: RatchetState }

-- | Send a message from Alice and receive it as Bob.
testSendRecvRoundTrip :: IO Bool
testSendRecvRoundTrip = do
    let g = mkPRNG 100
    (alice, bob) <- mkSessionPair g
    let msg = strToBS "Hello Bob!"
    (_alice', wire) <- sendChatMessage alice msg
    -- Bob decrypts using a ChatSession wrapping his ratchet state
    let bobSession = ChatSession { csRatchet = bsRatchet bob }
    result <- recvChatMessage bobSession wire
    case result of
        Nothing -> do
            putStrLn "  FAIL: send/recv round-trip (decryption failed)"
            pure False
        Just (_, pt) ->
            assertEq "send/recv round-trip" msg pt

-- | Multiple messages in sequence.
testMultipleMessages :: IO Bool
testMultipleMessages = do
    let g = mkPRNG 200
    (alice0, bob) <- mkSessionPair g
    let bobSession0 = ChatSession { csRatchet = bsRatchet bob }
        msg1 = strToBS "first"
        msg2 = strToBS "second message here"
        msg3 = strToBS "third"
    -- Message 1
    (alice1, wire1) <- sendChatMessage alice0 msg1
    r1 <- recvChatMessage bobSession0 wire1
    case r1 of
        Nothing -> do
            putStrLn "  FAIL: multi-message 1 (decryption failed)"
            pure False
        Just (bobSession1, pt1) -> do
            ok1 <- assertEq "multi-message 1" msg1 pt1
            -- Message 2
            (alice2, wire2) <- sendChatMessage alice1 msg2
            r2 <- recvChatMessage bobSession1 wire2
            case r2 of
                Nothing -> do
                    putStrLn "  FAIL: multi-message 2 (decryption failed)"
                    pure False
                Just (bobSession2, pt2) -> do
                    ok2 <- assertEq "multi-message 2" msg2 pt2
                    -- Message 3
                    (_, wire3) <- sendChatMessage alice2 msg3
                    r3 <- recvChatMessage bobSession2 wire3
                    case r3 of
                        Nothing -> do
                            putStrLn "  FAIL: multi-message 3 (decryption failed)"
                            pure False
                        Just (_, pt3) -> do
                            ok3 <- assertEq "multi-message 3" msg3 pt3
                            pure (ok1 && ok2 && ok3)
