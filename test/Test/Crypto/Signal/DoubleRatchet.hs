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
bobSPKPublic :: ByteString
bobSPKPublic = x25519 bobSPKSecret x25519Basepoint

------------------------------------------------------------------------
-- Test 1: Init Alice + Init Bob, single message A->B
------------------------------------------------------------------------

testInitAndSingleMessage :: IO Bool
testInitAndSingleMessage = do
    let alice = ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
        bob   = ratchetInitBob sharedSecret bobSPKSecret
        msg   = strToBS "Hello, Bob!"
    (_alice', header, ct, tag) <- ratchetEncrypt alice msg
    result <- ratchetDecrypt bob header ct tag
    case result of
        Just (_, pt) -> assertEq "Init + single message A->B" msg pt
        Nothing      -> putStrLn "  FAIL: Init + single message A->B (decryption failed)" >> pure False

------------------------------------------------------------------------
-- Test 2: Multiple messages in same direction (A->B, A->B, A->B)
------------------------------------------------------------------------

testMultipleMessagesOneDirection :: IO Bool
testMultipleMessagesOneDirection = do
    let alice0 = ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
        bob0   = ratchetInitBob sharedSecret bobSPKSecret

        msg1 = strToBS "Message 1"
    (alice1, h1, ct1, tag1) <- ratchetEncrypt alice0 msg1

    let msg2 = strToBS "Message 2"
    (alice2, h2, ct2, tag2) <- ratchetEncrypt alice1 msg2

    let msg3 = strToBS "Message 3"
    (_alice3, h3, ct3, tag3) <- ratchetEncrypt alice2 msg3

    r1 <- ratchetDecrypt bob0 h1 ct1 tag1
    case r1 of
        Just (bob1, pt1) -> do
            ok1 <- assertEq "Multi A->B msg1" msg1 pt1
            r2 <- ratchetDecrypt bob1 h2 ct2 tag2
            case r2 of
                Just (bob2, pt2) -> do
                    ok2 <- assertEq "Multi A->B msg2" msg2 pt2
                    r3 <- ratchetDecrypt bob2 h3 ct3 tag3
                    case r3 of
                        Just (_, pt3) -> do
                            ok3 <- assertEq "Multi A->B msg3" msg3 pt3
                            pure (ok1 && ok2 && ok3)
                        Nothing -> putStrLn "  FAIL: Multi A->B msg3 decrypt" >> pure False
                Nothing -> putStrLn "  FAIL: Multi A->B msg2 decrypt" >> pure False
        Nothing -> putStrLn "  FAIL: Multi A->B msg1 decrypt" >> pure False

------------------------------------------------------------------------
-- Test 3: Bidirectional (A->B then B->A, triggers DH ratchet)
------------------------------------------------------------------------

testBidirectional :: IO Bool
testBidirectional = do
    let alice0 = ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
        bob0   = ratchetInitBob sharedSecret bobSPKSecret

        -- Alice sends to Bob
        msgAB = strToBS "Hello from Alice"
    (alice1, hAB, ctAB, tagAB) <- ratchetEncrypt alice0 msgAB

    resultAB <- ratchetDecrypt bob0 hAB ctAB tagAB
    case resultAB of
        Just (bob1, ptAB) -> do
            ok1 <- assertEq "Bidi A->B" msgAB ptAB
            -- Bob sends to Alice (this triggers DH ratchet on Bob)
            let msgBA = strToBS "Hello from Bob"
            (_bob2, hBA, ctBA, tagBA) <- ratchetEncrypt bob1 msgBA
            resultBA <- ratchetDecrypt alice1 hBA ctBA tagBA
            case resultBA of
                Just (_, ptBA) -> do
                    ok2 <- assertEq "Bidi B->A" msgBA ptBA
                    pure (ok1 && ok2)
                Nothing -> putStrLn "  FAIL: Bidi B->A decrypt" >> pure False
        Nothing -> putStrLn "  FAIL: Bidi A->B decrypt" >> pure False

------------------------------------------------------------------------
-- Test 4: Out-of-order delivery (send 3, deliver in reverse)
------------------------------------------------------------------------

testOutOfOrder :: IO Bool
testOutOfOrder = do
    let alice0 = ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
        bob0   = ratchetInitBob sharedSecret bobSPKSecret

        msg1 = strToBS "First"
    (alice1, h1, ct1, tag1) <- ratchetEncrypt alice0 msg1

    let msg2 = strToBS "Second"
    (alice2, h2, ct2, tag2) <- ratchetEncrypt alice1 msg2

    let msg3 = strToBS "Third"
    (_alice3, h3, ct3, tag3) <- ratchetEncrypt alice2 msg3

    -- Deliver in reverse order: msg3, msg2, msg1
    r3 <- ratchetDecrypt bob0 h3 ct3 tag3
    case r3 of
        Just (bob1, pt3) -> do
            ok3 <- assertEq "OOO msg3 (delivered first)" msg3 pt3
            r2 <- ratchetDecrypt bob1 h2 ct2 tag2
            case r2 of
                Just (bob2, pt2) -> do
                    ok2 <- assertEq "OOO msg2 (delivered second)" msg2 pt2
                    r1 <- ratchetDecrypt bob2 h1 ct1 tag1
                    case r1 of
                        Just (_, pt1) -> do
                            ok1 <- assertEq "OOO msg1 (delivered third)" msg1 pt1
                            pure (ok3 && ok2 && ok1)
                        Nothing -> putStrLn "  FAIL: OOO msg1 decrypt" >> pure False
                Nothing -> putStrLn "  FAIL: OOO msg2 decrypt" >> pure False
        Nothing -> putStrLn "  FAIL: OOO msg3 decrypt" >> pure False

------------------------------------------------------------------------
-- Test 5: Property test — random keys, 5 messages each direction
------------------------------------------------------------------------

propBidirectional :: PRNG -> IO Bool
propBidirectional g0 = do
    let (sk, g1) = nextBytes 32 g0
        (bobSk, g2) = nextBytes 32 g1
        (aliceSk, g3) = nextBytes 32 g2

        bobPub = x25519 bobSk x25519Basepoint

        alice0 = ratchetInitAlice sk bobPub aliceSk
        bob0   = ratchetInitBob sk bobSk

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
        (sender', hdr, ct, tag) <- ratchetEncrypt sender msg
        result <- ratchetDecrypt receiver hdr ct tag
        case result of
            Just (receiver', pt)
                | pt == msg -> sendAndRecv sender' receiver' msgs
                | otherwise -> pure Nothing
            Nothing -> pure Nothing

------------------------------------------------------------------------
-- Test 6: Reject tampered ciphertext
------------------------------------------------------------------------

testTamperedCiphertext :: IO Bool
testTamperedCiphertext = do
    let alice = ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
        bob   = ratchetInitBob sharedSecret bobSPKSecret
        msg   = strToBS "Secret message"
    (_alice', header, ct, tag) <- ratchetEncrypt alice msg
    let -- Flip first bit of ciphertext
        tamperedCt = flipByte 0 ct
    result <- ratchetDecrypt bob header tamperedCt tag
    case result of
        Nothing -> assertEq "Tampered ciphertext rejected" True True
        Just _  -> putStrLn "  FAIL: Tampered ciphertext was accepted!" >> pure False

flipByte :: Int -> ByteString -> ByteString
flipByte i bs
    | i >= BS.length bs = bs
    | otherwise = BS.concat
        [ BS.take i bs
        , BS.singleton (Data.Bits.xor (BS.index bs i) 0xff)
        , BS.drop (i + 1) bs
        ]
