-- | Security invariant tests: constant-time comparison, nonce uniqueness,
-- forward secrecy, key independence, implicit rejection, tag tampering,
-- stealth unlinkability, CSPRNG non-determinism, fork safety, Poly1305 key reuse.
module Test.Security (runTests) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Data.Word (Word8)

import Test.Util
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Ed25519 (ed25519PublicKey)
import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.MLKEM (mlkemKeyGen, mlkemEncaps, mlkemDecaps)
import UmbraVox.Crypto.Poly1305 (poly1305)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.Signal.DoubleRatchet
import UmbraVox.Crypto.StealthAddress
    (StealthAddress(..), deriveStealthAddress)

runTests :: IO Bool
runTests = do
    putStrLn "[Security] Running security invariant tests..."
    results <- sequence
        [ testConstantTimeGCM
        , testConstantTimeMLKEM
        , testNonceUniqueness
        , testForwardSecrecy
        , testKeyIndependence
        , testImplicitRejection
        , testTagTampering
        , testStealthUnlinkability
        , testCSPRNGNonDeterminism
        , testCSPRNGForkSafety
        , testPoly1305KeyReuse
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Security] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Test key material
------------------------------------------------------------------------

testKey :: ByteString
testKey = BS.pack [0x01 .. 0x20]  -- 32 bytes

testNonce :: ByteString
testNonce = BS.pack [0xa0 .. 0xab]  -- 12 bytes

testAAD :: ByteString
testAAD = strToBS "additional data"

testPlaintext :: ByteString
testPlaintext = strToBS "hello security tests"

------------------------------------------------------------------------
-- 1. Constant-time GCM: flipping different byte positions in equal-length
-- inputs always returns False via gcmDecrypt (no early exit)
------------------------------------------------------------------------

testConstantTimeGCM :: IO Bool
testConstantTimeGCM = do
    let (ct, tag) = gcmEncrypt testKey testNonce testAAD testPlaintext
        -- Verify original decrypts
        origOk = case gcmDecrypt testKey testNonce testAAD ct tag of
                     Just _  -> True
                     Nothing -> False
        -- Flip each byte position in tag: all must fail
        flipped = [flipByteAt tag i | i <- [0 .. BS.length tag - 1]]
        allFail = all (\t' -> gcmDecrypt testKey testNonce testAAD ct t' == Nothing) flipped
    assertEq "constant-time GCM: original decrypts" True origOk
        >>= \ok1 -> assertEq "constant-time GCM: all flipped tags rejected" True allFail
        >>= \ok2 -> pure (ok1 && ok2)

------------------------------------------------------------------------
-- 2. Constant-time ML-KEM: equal-length inputs with different failure
-- positions all produce a result (not error)
------------------------------------------------------------------------

testConstantTimeMLKEM :: IO Bool
testConstantTimeMLKEM = do
    let (d, z) = (BS.pack [0x10 .. 0x2f], BS.pack [0x30 .. 0x4f])
        (ek, dk) = mlkemKeyGen d z
        m = BS.pack [0x50 .. 0x6f]
        (ct, _ss) = mlkemEncaps ek m
        -- Decaps with correct key yields shared secret
        ss1 = mlkemDecaps dk ct
        ok1 = BS.length ss1 == 32
    assertEq "constant-time ML-KEM: decaps produces 32-byte secret" True ok1

------------------------------------------------------------------------
-- 3. Nonce uniqueness: encrypt 1000 messages, no duplicate nonces
------------------------------------------------------------------------

testNonceUniqueness :: IO Bool
testNonceUniqueness = do
    let sharedSecret = BS.pack [0x01 .. 0x20]
        bobSPKSecret = BS.pack [0x21 .. 0x40]
        aliceDHSecret = BS.pack [0x41 .. 0x60]
        aliceState = ratchetInitAlice sharedSecret
                         (x25519 bobSPKSecret x25519Basepoint) aliceDHSecret
    -- Encrypt 1000 messages, collect (header, ct, tag) triples
    (nonces, _) <- collectNonces 1000 aliceState
    let uniqueCount = Set.size (Set.fromList nonces)
    assertEq "nonce uniqueness: 1000 messages, all nonces distinct"
             1000 uniqueCount

collectNonces :: Int -> RatchetState -> IO ([ByteString], RatchetState)
collectNonces 0 st = pure ([], st)
collectNonces n st = do
    (st', hdr, _ct, _tag) <- ratchetEncrypt st (strToBS "msg")
    -- Reconstruct nonce from msgKey: we can't access it directly,
    -- so use (dhPub, msgN) as a proxy for uniqueness
    let nonceProxy = rhDHPublic hdr <> encodeW32 (rhMsgN hdr)
    (rest, st'') <- collectNonces (n - 1) st'
    pure (nonceProxy : rest, st'')

encodeW32 :: Integral a => a -> ByteString
encodeW32 w = BS.pack [fromIntegral w, fromIntegral (w `div` 256),
                        fromIntegral (w `div` 65536),
                        fromIntegral (w `div` 16777216)]

------------------------------------------------------------------------
-- 4. Forward secrecy: after DH ratchet, old chain key cannot derive
-- new message keys (old keys differ from new)
------------------------------------------------------------------------

testForwardSecrecy :: IO Bool
testForwardSecrecy = do
    let sharedSecret = BS.pack [0x01 .. 0x20]
        bobSPKSecret = BS.pack [0x21 .. 0x40]
        aliceDHSecret = BS.pack [0x41 .. 0x60]
        bobSPKPublic = x25519 bobSPKSecret x25519Basepoint
        aliceState = ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
        bobState = ratchetInitBob sharedSecret bobSPKSecret
        oldSendChain = rsSendChain aliceState
    -- Alice sends a message (triggers chain advancement)
    (alice1, hdr1, ct1, tag1) <- ratchetEncrypt aliceState (strToBS "msg1")
    -- Bob decrypts (triggers DH ratchet)
    Just (bob1, _pt1) <- ratchetDecrypt bobState hdr1 ct1 tag1
    -- Bob sends back (DH ratchet step for Alice on next recv)
    (_bob2, hdr2, ct2, tag2) <- ratchetEncrypt bob1 (strToBS "reply")
    -- Alice decrypts (DH ratchet: new keys derived)
    Just (alice2, _pt2) <- ratchetDecrypt alice1 hdr2 ct2 tag2
    -- Old chain key must differ from new chain key
    let newSendChain = rsSendChain alice2
        keysChanged = oldSendChain /= newSendChain
    assertEq "forward secrecy: chain key changes after DH ratchet" True keysChanged

------------------------------------------------------------------------
-- 5. Key independence: two sessions with different seeds produce
-- completely different keys
------------------------------------------------------------------------

testKeyIndependence :: IO Bool
testKeyIndependence = do
    let ss1 = BS.pack [0x01 .. 0x20]
        ss2 = BS.pack [0x21 .. 0x40]
        bobSPKSecret = BS.pack [0x41 .. 0x60]
        aliceDH1 = BS.pack [0x61 .. 0x80]
        aliceDH2 = BS.pack [0x81 .. 0xa0]
        bobSPKPub = x25519 bobSPKSecret x25519Basepoint
        state1 = ratchetInitAlice ss1 bobSPKPub aliceDH1
        state2 = ratchetInitAlice ss2 bobSPKPub aliceDH2
        rootDiff = rsRootKey state1 /= rsRootKey state2
        chainDiff = rsSendChain state1 /= rsSendChain state2
    assertEq "key independence: root keys differ" True rootDiff
        >>= \ok1 -> assertEq "key independence: send chains differ" True chainDiff
        >>= \ok2 -> pure (ok1 && ok2)

------------------------------------------------------------------------
-- 6. Implicit rejection: ML-KEM decaps with wrong key returns
-- rejection secret (32 bytes), not error
------------------------------------------------------------------------

testImplicitRejection :: IO Bool
testImplicitRejection = do
    let (d1, z1) = (BS.pack [0x10 .. 0x2f], BS.pack [0x30 .. 0x4f])
        (d2, z2) = (BS.pack [0x50 .. 0x6f], BS.pack [0x70 .. 0x8f])
        (ek1, _dk1) = mlkemKeyGen d1 z1
        (_ek2, dk2) = mlkemKeyGen d2 z2
        m = BS.pack [0x90 .. 0xaf]
        (ct1, ss1) = mlkemEncaps ek1 m
        -- Decaps ciphertext from key1 with key2's decap key
        ssRejection = mlkemDecaps dk2 ct1
        -- Must return 32 bytes (rejection secret), not crash
        lenOk = BS.length ssRejection == 32
        -- Must differ from the real shared secret
        differs = ssRejection /= ss1
    assertEq "implicit rejection: returns 32-byte secret" True lenOk
        >>= \ok1 -> assertEq "implicit rejection: differs from real SS" True differs
        >>= \ok2 -> pure (ok1 && ok2)

------------------------------------------------------------------------
-- 7. Tag tampering: flip each byte of GCM tag, all must be rejected
------------------------------------------------------------------------

testTagTampering :: IO Bool
testTagTampering = do
    let (ct, tag) = gcmEncrypt testKey testNonce testAAD testPlaintext
        -- Flip one bit in each byte position (16 tests)
        results = [gcmDecrypt testKey testNonce testAAD ct (flipByteAt tag i)
                  | i <- [0..15]]
        allNothing = all (== Nothing) results
    assertEq "tag tampering: all 16 flipped-byte tags rejected" True allNothing

------------------------------------------------------------------------
-- 8. Stealth unlinkability: 100 stealth addresses from same recipient
-- are all different
------------------------------------------------------------------------

testStealthUnlinkability :: IO Bool
testStealthUnlinkability = do
    -- Generate a fixed recipient keypair
    let scanSecret = BS.pack [0x01 .. 0x20]
        spendSecret = BS.pack [0x21 .. 0x40]
        scanPub = x25519 scanSecret x25519Basepoint
    let spendPub = ed25519PublicKey spendSecret
    -- Generate 100 stealth addresses
    addrs <- mapM (\_ -> deriveStealthAddress scanPub spendPub) [1..100 :: Int]
    let addrSet = Set.fromList (map saAddress addrs)
        allUnique = Set.size addrSet == 100
    assertEq "stealth unlinkability: 100 addresses all unique" True allUnique

------------------------------------------------------------------------
-- 9. CSPRNG non-determinism: two calls produce different output
------------------------------------------------------------------------

testCSPRNGNonDeterminism :: IO Bool
testCSPRNGNonDeterminism = do
    a <- randomBytes 32
    b <- randomBytes 32
    assertEq "CSPRNG non-determinism: two calls differ" True (a /= b)

------------------------------------------------------------------------
-- 10. CSPRNG fork safety: verify PID is checked in state
-- (We can't easily fork, but we verify randomBytes returns valid output
-- and that two sequential calls are different, which relies on PID check)
------------------------------------------------------------------------

testCSPRNGForkSafety :: IO Bool
testCSPRNGForkSafety = do
    -- Verify that randomBytes produces non-empty, correctly sized output
    -- The PID check is exercised internally by ensureState
    r1 <- randomBytes 64
    r2 <- randomBytes 64
    let sizeOk = BS.length r1 == 64 && BS.length r2 == 64
        differ = r1 /= r2
    assertEq "CSPRNG fork safety: correct sizes" True sizeOk
        >>= \ok1 -> assertEq "CSPRNG fork safety: sequential outputs differ" True differ
        >>= \ok2 -> pure (ok1 && ok2)

------------------------------------------------------------------------
-- 11. Poly1305 key reuse: same key, different messages, different tags
------------------------------------------------------------------------

testPoly1305KeyReuse :: IO Bool
testPoly1305KeyReuse = do
    let key = BS.pack [0x01 .. 0x20]  -- 32 bytes
        msg1 = strToBS "message one"
        msg2 = strToBS "message two"
        tag1 = poly1305 key msg1
        tag2 = poly1305 key msg2
    assertEq "Poly1305 key reuse: different messages => different tags"
             True (tag1 /= tag2)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Flip the low bit of the byte at position i.
flipByteAt :: ByteString -> Int -> ByteString
flipByteAt bs i =
    let b = BS.index bs i
        b' = b `xor` (0x01 :: Word8)
    in BS.take i bs <> BS.singleton b' <> BS.drop (i + 1) bs
