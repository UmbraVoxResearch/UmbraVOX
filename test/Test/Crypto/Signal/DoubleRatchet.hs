-- SPDX-License-Identifier: Apache-2.0
-- | Double Ratchet test suite: init + encrypt/decrypt, bidirectional,
-- out-of-order, property tests, and tamper rejection.
module Test.Crypto.Signal.DoubleRatchet (runTests) where

import qualified Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Signal.DoubleRatchet

runTests :: IO Bool
runTests = do
    putStrLn "[Double Ratchet] Running basic encrypt/decrypt tests..."
    basicResults <- sequence
        [ testInitAndSingleMessage
        , testMultipleMessagesOneDirection
        ]
    putStrLn "[Double Ratchet] Running bidirectional tests..."
    bidiResults <- sequence
        [ testBidirectional
        ]
    putStrLn "[Double Ratchet] Running out-of-order delivery tests..."
    oooResults <- sequence
        [ testOutOfOrder
        ]
    putStrLn "[Double Ratchet] Running tamper rejection tests..."
    tamperResults <- sequence
        [ testTamperedCiphertext
        ]
    putStrLn "[Double Ratchet] Running property tests..."
    propResults <- sequence
        [ checkPropertyIO "random bidirectional (50 iterations)" 50 propBidirectional
        ]
    let results = basicResults ++ bidiResults ++ oooResults ++ tamperResults ++ propResults
        passed  = length (filter id results)
        total   = length results
    putStrLn $ "[Double Ratchet] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Test key material
------------------------------------------------------------------------

-- Deterministic "X3DH shared secret" for testing
sharedSecret :: ByteString
sharedSecret = BS.pack [0x01 .. 0x20]  -- 32 bytes

-- Bob's SPK secret key
bobSPKSecret :: ByteString
bobSPKSecret = BS.pack [0x21 .. 0x40]  -- 32 bytes

-- Alice's first DH secret
aliceDHSecret :: ByteString
aliceDHSecret = BS.pack [0x41 .. 0x60]  -- 32 bytes

-- Derived public keys
-- x25519 now returns Maybe; basepoint mult of a non-zero secret is always Just.
bobSPKPublic :: ByteString
bobSPKPublic = case x25519 bobSPKSecret x25519Basepoint of
    Just pub -> pub
    Nothing  -> error "bobSPKPublic: impossible all-zero from known test secret"

------------------------------------------------------------------------
-- Test 1: Init Alice + Init Bob, single message A->B
------------------------------------------------------------------------

testInitAndSingleMessage :: IO Bool
testInitAndSingleMessage = do
    mAlice <- ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
    case mAlice of
        Nothing    -> putStrLn "  FAIL: ratchetInitAlice returned Nothing" >> pure False
        Just alice -> do
            bob <- ratchetInitBob sharedSecret bobSPKSecret
            let msg = strToBS "Hello, Bob!"
            encResult <- ratchetEncrypt alice msg
            case encResult of
                Left err -> putStrLn ("  FAIL: ratchetEncrypt: " ++ show err) >> pure False
                Right (_alice', header, ct, tag) -> do
                    decResult <- ratchetDecrypt bob header ct tag
                    case decResult of
                        Left err        -> putStrLn ("  FAIL: ratchetDecrypt: " ++ show err) >> pure False
                        Right Nothing   -> putStrLn "  FAIL: Init + single message A->B (decryption failed)" >> pure False
                        Right (Just (_, pt)) -> assertEq "Init + single message A->B" msg pt

------------------------------------------------------------------------
-- Test 2: Multiple messages in same direction (A->B, A->B, A->B)
------------------------------------------------------------------------

testMultipleMessagesOneDirection :: IO Bool
testMultipleMessagesOneDirection = do
    mAlice <- ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
    case mAlice of
        Nothing    -> putStrLn "  FAIL: ratchetInitAlice returned Nothing" >> pure False
        Just alice0 -> do
            bob0 <- ratchetInitBob sharedSecret bobSPKSecret
            let msg1 = strToBS "Message 1"
                msg2 = strToBS "Message 2"
                msg3 = strToBS "Message 3"
            r1 <- ratchetEncrypt alice0 msg1
            case r1 of
                Left err -> putStrLn ("  FAIL: enc msg1: " ++ show err) >> pure False
                Right (alice1, h1, ct1, tag1) -> do
                    r2 <- ratchetEncrypt alice1 msg2
                    case r2 of
                        Left err -> putStrLn ("  FAIL: enc msg2: " ++ show err) >> pure False
                        Right (alice2, h2, ct2, tag2) -> do
                            r3 <- ratchetEncrypt alice2 msg3
                            case r3 of
                                Left err -> putStrLn ("  FAIL: enc msg3: " ++ show err) >> pure False
                                Right (_alice3, h3, ct3, tag3) -> do
                                    dr1 <- ratchetDecrypt bob0 h1 ct1 tag1
                                    case dr1 of
                                        Left e -> putStrLn ("  FAIL: dec msg1: " ++ show e) >> pure False
                                        Right Nothing -> putStrLn "  FAIL: Multi A->B msg1 decrypt" >> pure False
                                        Right (Just (bob1, pt1)) -> do
                                            ok1 <- assertEq "Multi A->B msg1" msg1 pt1
                                            dr2 <- ratchetDecrypt bob1 h2 ct2 tag2
                                            case dr2 of
                                                Left e -> putStrLn ("  FAIL: dec msg2: " ++ show e) >> pure False
                                                Right Nothing -> putStrLn "  FAIL: Multi A->B msg2 decrypt" >> pure False
                                                Right (Just (bob2, pt2)) -> do
                                                    ok2 <- assertEq "Multi A->B msg2" msg2 pt2
                                                    dr3 <- ratchetDecrypt bob2 h3 ct3 tag3
                                                    case dr3 of
                                                        Left e -> putStrLn ("  FAIL: dec msg3: " ++ show e) >> pure False
                                                        Right Nothing -> putStrLn "  FAIL: Multi A->B msg3 decrypt" >> pure False
                                                        Right (Just (_, pt3)) -> do
                                                            ok3 <- assertEq "Multi A->B msg3" msg3 pt3
                                                            pure (ok1 && ok2 && ok3)

------------------------------------------------------------------------
-- Test 3: Bidirectional (A->B then B->A, triggers DH ratchet)
------------------------------------------------------------------------

testBidirectional :: IO Bool
testBidirectional = do
    mAlice <- ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
    case mAlice of
        Nothing    -> putStrLn "  FAIL: ratchetInitAlice returned Nothing" >> pure False
        Just alice0 -> do
            bob0 <- ratchetInitBob sharedSecret bobSPKSecret
            let msgAB  = strToBS "Hello from Alice"
            encAB <- ratchetEncrypt alice0 msgAB
            case encAB of
                Left err -> putStrLn ("  FAIL: Bidi enc A->B: " ++ show err) >> pure False
                Right (alice1, hAB, ctAB, tagAB) -> do
                    resultAB <- ratchetDecrypt bob0 hAB ctAB tagAB
                    case resultAB of
                        Left err -> putStrLn ("  FAIL: Bidi dec A->B: " ++ show err) >> pure False
                        Right Nothing -> putStrLn "  FAIL: Bidi A->B decrypt" >> pure False
                        Right (Just (bob1, ptAB)) -> do
                            ok1 <- assertEq "Bidi A->B" msgAB ptAB
                            let msgBA = strToBS "Hello from Bob"
                            encBA <- ratchetEncrypt bob1 msgBA
                            case encBA of
                                Left err -> putStrLn ("  FAIL: Bidi enc B->A: " ++ show err) >> pure False
                                Right (_bob2, hBA, ctBA, tagBA) -> do
                                    resultBA <- ratchetDecrypt alice1 hBA ctBA tagBA
                                    case resultBA of
                                        Left err -> putStrLn ("  FAIL: Bidi dec B->A: " ++ show err) >> pure False
                                        Right Nothing -> putStrLn "  FAIL: Bidi B->A decrypt" >> pure False
                                        Right (Just (_, ptBA)) -> do
                                            ok2 <- assertEq "Bidi B->A" msgBA ptBA
                                            pure (ok1 && ok2)

------------------------------------------------------------------------
-- Test 4: Out-of-order delivery (send 3, deliver in reverse)
------------------------------------------------------------------------

testOutOfOrder :: IO Bool
testOutOfOrder = do
    mAlice <- ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
    case mAlice of
        Nothing    -> putStrLn "  FAIL: ratchetInitAlice returned Nothing" >> pure False
        Just alice0 -> do
            bob0 <- ratchetInitBob sharedSecret bobSPKSecret
            let msg1 = strToBS "First"
                msg2 = strToBS "Second"
                msg3 = strToBS "Third"
            e1 <- ratchetEncrypt alice0 msg1
            case e1 of
                Left err -> putStrLn ("  FAIL: OOO enc1: " ++ show err) >> pure False
                Right (alice1, h1, ct1, tag1) -> do
                    e2 <- ratchetEncrypt alice1 msg2
                    case e2 of
                        Left err -> putStrLn ("  FAIL: OOO enc2: " ++ show err) >> pure False
                        Right (alice2, h2, ct2, tag2) -> do
                            e3 <- ratchetEncrypt alice2 msg3
                            case e3 of
                                Left err -> putStrLn ("  FAIL: OOO enc3: " ++ show err) >> pure False
                                Right (_alice3, h3, ct3, tag3) -> do
                                    -- Deliver in reverse order: msg3, msg2, msg1
                                    r3 <- ratchetDecrypt bob0 h3 ct3 tag3
                                    case r3 of
                                        Left e -> putStrLn ("  FAIL: OOO dec3: " ++ show e) >> pure False
                                        Right Nothing -> putStrLn "  FAIL: OOO msg3 decrypt" >> pure False
                                        Right (Just (bob1, pt3)) -> do
                                            ok3 <- assertEq "OOO msg3 (delivered first)" msg3 pt3
                                            r2 <- ratchetDecrypt bob1 h2 ct2 tag2
                                            case r2 of
                                                Left e -> putStrLn ("  FAIL: OOO dec2: " ++ show e) >> pure False
                                                Right Nothing -> putStrLn "  FAIL: OOO msg2 decrypt" >> pure False
                                                Right (Just (bob2, pt2)) -> do
                                                    ok2 <- assertEq "OOO msg2 (delivered second)" msg2 pt2
                                                    r1 <- ratchetDecrypt bob2 h1 ct1 tag1
                                                    case r1 of
                                                        Left e -> putStrLn ("  FAIL: OOO dec1: " ++ show e) >> pure False
                                                        Right Nothing -> putStrLn "  FAIL: OOO msg1 decrypt" >> pure False
                                                        Right (Just (_, pt1)) -> do
                                                            ok1 <- assertEq "OOO msg1 (delivered third)" msg1 pt1
                                                            pure (ok3 && ok2 && ok1)

------------------------------------------------------------------------
-- Test 5: Property test — random keys, 5 messages each direction
------------------------------------------------------------------------

propBidirectional :: PRNG -> IO Bool
propBidirectional g0 = do
    let (sk, g1) = nextBytes 32 g0
        (bobSk, g2) = nextBytes 32 g1
        (aliceSk, g3) = nextBytes 32 g2
        mBobPub = x25519 bobSk x25519Basepoint
    case mBobPub of
        Nothing -> pure True  -- degenerate key; skip
        Just bobPub ->
            do mAlice <- ratchetInitAlice sk bobPub aliceSk
               case mAlice of
                Nothing    -> pure False  -- should not happen for random keys
                Just alice0 -> do
                    bob0 <- ratchetInitBob sk bobSk
                    -- Send 5 messages A->B
                    let (msgs, g4) = genMessages 5 g3
                    result <- sendAndRecv alice0 bob0 msgs
                    case result of
                        Nothing -> pure False
                        Just (alice1, bob1) -> do
                            -- Send 5 messages B->A
                            let (msgs2, _) = genMessages 5 g4
                            result2 <- sendAndRecv bob1 alice1 msgs2
                            case result2 of
                                Nothing -> pure False
                                Just _  -> pure True
  where
    genMessages :: Int -> PRNG -> ([ByteString], PRNG)
    genMessages 0 g = ([], g)
    genMessages n g =
        let (msg, g') = nextBytesRange 1 128 g
            (rest, g'') = genMessages (n - 1) g'
        in (msg : rest, g'')

    sendAndRecv :: RatchetState -> RatchetState -> [ByteString]
                -> IO (Maybe (RatchetState, RatchetState))
    sendAndRecv sender receiver [] = pure (Just (sender, receiver))
    sendAndRecv sender receiver (msg:msgs) = do
        encResult <- ratchetEncrypt sender msg
        case encResult of
            Left _  -> pure Nothing
            Right (sender', hdr, ct, tag) -> do
                decResult <- ratchetDecrypt receiver hdr ct tag
                case decResult of
                    Left _                   -> pure Nothing
                    Right Nothing            -> pure Nothing
                    Right (Just (receiver', pt))
                        | pt == msg -> sendAndRecv sender' receiver' msgs
                        | otherwise -> pure Nothing

------------------------------------------------------------------------
-- Test 6: Reject tampered ciphertext
------------------------------------------------------------------------

testTamperedCiphertext :: IO Bool
testTamperedCiphertext = do
    mAlice <- ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
    case mAlice of
        Nothing    -> putStrLn "  FAIL: ratchetInitAlice returned Nothing" >> pure False
        Just alice -> do
            bob <- ratchetInitBob sharedSecret bobSPKSecret
            let msg = strToBS "Secret message"
            encResult <- ratchetEncrypt alice msg
            case encResult of
                Left err -> putStrLn ("  FAIL: tamper enc: " ++ show err) >> pure False
                Right (_alice', header, ct, tag) -> do
                    let tamperedCt = flipByte 0 ct
                    decResult <- ratchetDecrypt bob header tamperedCt tag
                    case decResult of
                        Left err     -> putStrLn ("  FAIL: tamper dec error: " ++ show err) >> pure False
                        Right Nothing -> assertEq "Tampered ciphertext rejected" True True
                        Right (Just _) -> putStrLn "  FAIL: Tampered ciphertext was accepted!" >> pure False

flipByte :: Int -> ByteString -> ByteString
flipByte i bs
    | i >= BS.length bs = bs
    | otherwise = BS.concat
        [ BS.take i bs
        , BS.singleton (Data.Bits.xor (BS.index bs i) 0xff)
        , BS.drop (i + 1) bs
        ]
