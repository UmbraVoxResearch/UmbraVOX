-- SPDX-License-Identifier: Apache-2.0
-- | Comprehensive fuzz testing for all crypto primitives.
--
-- Each fuzz target runs 1000 iterations with deterministic PRNG inputs,
-- checking for crashes, invariant violations, and round-trip failures.
module Test.Fuzz (runTests) where

import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8)

import Test.Util

import UmbraVox.Crypto.SHA256 (sha256)
import UmbraVox.Crypto.SHA512 (sha512)
import UmbraVox.Crypto.Keccak (sha3_256, shake128)
import UmbraVox.Crypto.AES (aesEncrypt, aesDecrypt)
import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Ed25519 (ed25519Sign, ed25519Verify, ed25519PublicKey)
import UmbraVox.Crypto.MLKEM (mlkemKeyGen, mlkemEncaps, mlkemDecaps, MLKEMEncapKey(..))
import UmbraVox.Crypto.HMAC (hmacSHA256, hmacSHA512)
import UmbraVox.Crypto.HKDF (hkdf)
import UmbraVox.Crypto.Poly1305 (poly1305)
import UmbraVox.Crypto.SecureBytes (toByteString)
import UmbraVox.Crypto.Signal.DoubleRatchet
    (ratchetInitAlice, ratchetInitBob, ratchetEncrypt, ratchetDecrypt)
import UmbraVox.Crypto.Signal.X3DH
    (generateKeyPair, generateIdentityKey, signPreKey,
     KeyPair(..), IdentityKey(..))
import UmbraVox.Crypto.Signal.PQXDH
    (PQPreKeyBundle(..), PQXDHResult(..),
     pqxdhInitiate, pqxdhRespond)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

iterations :: Int
iterations = 1000

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

runTests :: IO Bool
runTests = do
    putStrLn "[Fuzz] Running fuzz tests (1000 iterations each)..."
    results <- sequence
        [ fuzzSHA256
        , fuzzSHA512
        , fuzzSHA3_256
        , fuzzSHAKE128
        , fuzzAES256
        , fuzzAESGCM
        , fuzzX25519
        , fuzzEd25519
        , fuzzMLKEM
        , fuzzHMAC
        , fuzzHKDF
        , fuzzPoly1305
        , fuzzDoubleRatchet
        , fuzzPQXDH
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Fuzz] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- 1. SHA-256 fuzz: random input 0-10000 bytes -> output always 32 bytes
------------------------------------------------------------------------

fuzzSHA256 :: IO Bool
fuzzSHA256 = checkProperty "SHA-256: output always 32 bytes" iterations prop
  where
    prop g =
        let (input, _) = nextBytesRange 0 10000 g
            out = sha256 input
        in BS.length out == 32

------------------------------------------------------------------------
-- 2. SHA-512 fuzz: random input 0-10000 bytes -> output always 64 bytes
------------------------------------------------------------------------

fuzzSHA512 :: IO Bool
fuzzSHA512 = checkProperty "SHA-512: output always 64 bytes" iterations prop
  where
    prop g =
        let (input, _) = nextBytesRange 0 10000 g
            out = sha512 input
        in BS.length out == 64

------------------------------------------------------------------------
-- 3. SHA3-256 fuzz: random input -> always 32 bytes
------------------------------------------------------------------------

fuzzSHA3_256 :: IO Bool
fuzzSHA3_256 = checkProperty "SHA3-256: output always 32 bytes" iterations prop
  where
    prop g =
        let (input, _) = nextBytesRange 0 10000 g
            out = sha3_256 input
        in BS.length out == 32

------------------------------------------------------------------------
-- 4. SHAKE-128 fuzz: random input + random output length 1-1000
------------------------------------------------------------------------

fuzzSHAKE128 :: IO Bool
fuzzSHAKE128 = checkProperty "SHAKE-128: output matches requested length" iterations prop
  where
    prop g =
        let (input, g1) = nextBytesRange 0 5000 g
            (w, _) = nextWord32 g1
            outLen = 1 + fromIntegral (w `mod` 1000)
            out = shake128 input outLen
        in BS.length out == outLen

------------------------------------------------------------------------
-- 5. AES-256 fuzz: encrypt then decrypt = original
------------------------------------------------------------------------

fuzzAES256 :: IO Bool
fuzzAES256 = checkProperty "AES-256: encrypt/decrypt round-trip" iterations prop
  where
    prop g =
        let (key, g1)   = nextBytes 32 g
            (block, _)  = nextBytes 16 g1
            ciphertext  = aesEncrypt key block
            plaintext   = aesDecrypt key ciphertext
        in plaintext == block && BS.length ciphertext == 16

------------------------------------------------------------------------
-- 6. AES-GCM fuzz: round-trip + tag size + tamper rejection
------------------------------------------------------------------------

fuzzAESGCM :: IO Bool
fuzzAESGCM = checkProperty "AES-GCM: round-trip, tag=16, tamper rejected" iterations prop
  where
    prop g =
        let (key, g1)   = nextBytes 32 g
            (nonce, g2) = nextBytes 12 g1
            (aad, g3)   = nextBytesRange 0 200 g2
            (pt, _)     = nextBytesRange 0 500 g3
            (ct, tag)   = gcmEncrypt key nonce aad pt
            decrypted   = gcmDecrypt key nonce aad ct tag
            -- Tamper: flip first byte of tag
            badTag      = flipFirstByte tag
            tampered    = gcmDecrypt key nonce aad ct badTag
        in BS.length tag == 16
           && decrypted == Just pt
           && tampered == Nothing

------------------------------------------------------------------------
-- 7. X25519 fuzz: random scalar + basepoint -> always 32-byte output
------------------------------------------------------------------------

fuzzX25519 :: IO Bool
fuzzX25519 = checkProperty "X25519: output is Nothing or 32 bytes" iterations prop
  where
    prop g =
        let (scalar, g1) = nextBytes 32 g
            (point, _)   = nextBytes 32 g1
            -- x25519 now returns Maybe ByteString; Nothing means all-zero DH
            okLen Nothing   = True
            okLen (Just bs) = BS.length bs == 32
        in okLen (x25519 scalar x25519Basepoint) && okLen (x25519 scalar point)

------------------------------------------------------------------------
-- 8. Ed25519 fuzz: sign then verify; flip bit -> verify fails
------------------------------------------------------------------------

fuzzEd25519 :: IO Bool
fuzzEd25519 = checkProperty "Ed25519: sign/verify + bit-flip rejection" iterations prop
  where
    prop g =
        let (secret, g1) = nextBytes 32 g
            (msg, _)     = nextBytesRange 0 1000 g1
            pubKey       = ed25519PublicKey secret
            sig          = ed25519Sign secret msg
            valid        = ed25519Verify pubKey msg sig
            -- Flip a bit in the message
            badMsg       = flipFirstByte msg
            invalid      = ed25519Verify pubKey badMsg sig
        in valid && (BS.null msg || not invalid)

------------------------------------------------------------------------
-- 9. ML-KEM fuzz: keygen + encaps + decaps round-trip
------------------------------------------------------------------------

fuzzMLKEM :: IO Bool
fuzzMLKEM = checkProperty "ML-KEM: encaps/decaps round-trip" iterations prop
  where
    prop g =
        let (d, g1)      = nextBytes 32 g
            (z, g2)      = nextBytes 32 g1
            (m, _)       = nextBytes 32 g2
            (ek, dk)     = mlkemKeyGen d z
            (ct, ssEnc)  = mlkemEncaps ek m
            ssDec        = mlkemDecaps dk ct
        in ssEnc == ssDec && BS.length ssEnc == 32

------------------------------------------------------------------------
-- 10. HMAC fuzz: output always 32 (SHA256) or 64 (SHA512) bytes
------------------------------------------------------------------------

fuzzHMAC :: IO Bool
fuzzHMAC = checkProperty "HMAC: SHA256=32 bytes, SHA512=64 bytes" iterations prop
  where
    prop g =
        let (key, g1) = nextBytesRange 0 200 g
            (msg, _)  = nextBytesRange 0 1000 g1
            h256      = hmacSHA256 key msg
            h512      = hmacSHA512 key msg
        in BS.length h256 == 32 && BS.length h512 == 64

------------------------------------------------------------------------
-- 11. HKDF fuzz: random salt+ikm+info, length 1-255 -> correct length
------------------------------------------------------------------------

fuzzHKDF :: IO Bool
fuzzHKDF = checkProperty "HKDF: output matches requested length" iterations prop
  where
    prop g =
        let (salt, g1) = nextBytesRange 0 100 g
            (ikm, g2)  = nextBytesRange 1 200 g1
            (info, g3) = nextBytesRange 0 100 g2
            (w, _)     = nextWord32 g3
            outLen     = 1 + fromIntegral (w `mod` 255)
            out        = hkdf salt ikm info outLen
        in BS.length out == outLen

------------------------------------------------------------------------
-- 12. Poly1305 fuzz: random 32-byte key + msg -> output always 16 bytes
------------------------------------------------------------------------

fuzzPoly1305 :: IO Bool
fuzzPoly1305 = checkProperty "Poly1305: output always 16 bytes" iterations prop
  where
    prop g =
        let (key, g1) = nextBytes 32 g
            (msg, _)  = nextBytesRange 0 5000 g1
            out       = poly1305 key msg
        in BS.length out == 16

------------------------------------------------------------------------
-- 13. Double Ratchet fuzz: init + encrypt N messages + decrypt all
------------------------------------------------------------------------

fuzzDoubleRatchet :: IO Bool
fuzzDoubleRatchet = checkPropertyIO "Double Ratchet: encrypt/decrypt round-trip" iterations prop
  where
    prop g = do
        let (sharedSecret, g1) = nextBytes 32 g
            (bobSPKSecret, g2) = nextBytes 32 g1
            (aliceDHSecret, g3) = nextBytes 32 g2
            mBobSPKPub = x25519 bobSPKSecret x25519Basepoint
            -- Generate 3 random messages
            (msg1, g4) = nextBytesRange 1 200 g3
            (msg2, g5) = nextBytesRange 1 200 g4
            (msg3, _)  = nextBytesRange 1 200 g5
        case mBobSPKPub of
            Nothing -> pure True  -- degenerate key; skip
            Just bobSPKPublic -> do
                mAlice <- ratchetInitAlice sharedSecret bobSPKPublic aliceDHSecret
                case mAlice of
                    Nothing -> pure False  -- should not happen for random keys
                    Just alice0 -> do
                        bob0 <- ratchetInitBob sharedSecret bobSPKSecret
                        -- Encrypt and decrypt 3 messages
                        e1 <- ratchetEncrypt alice0 msg1
                        case e1 of
                            Left _ -> pure False
                            Right (alice1, h1, ct1, tag1) -> do
                                r1 <- ratchetDecrypt bob0 h1 ct1 tag1
                                case r1 of
                                    Left _ -> pure False
                                    Right Nothing -> pure False
                                    Right (Just (bob1, pt1)) -> do
                                        e2 <- ratchetEncrypt alice1 msg2
                                        case e2 of
                                            Left _ -> pure False
                                            Right (alice2, h2, ct2, tag2) -> do
                                                r2 <- ratchetDecrypt bob1 h2 ct2 tag2
                                                case r2 of
                                                    Left _ -> pure False
                                                    Right Nothing -> pure False
                                                    Right (Just (bob2, pt2)) -> do
                                                        e3 <- ratchetEncrypt alice2 msg3
                                                        case e3 of
                                                            Left _ -> pure False
                                                            Right (_alice3, h3, ct3, tag3) -> do
                                                                r3 <- ratchetDecrypt bob2 h3 ct3 tag3
                                                                case r3 of
                                                                    Left _ -> pure False
                                                                    Right Nothing -> pure False
                                                                    Right (Just (_, pt3)) ->
                                                                        pure (pt1 == msg1 && pt2 == msg2 && pt3 == msg3)

------------------------------------------------------------------------
-- 14. PQXDH fuzz: random keys -> initiate + respond -> same secret
------------------------------------------------------------------------

fuzzPQXDH :: IO Bool
fuzzPQXDH = checkPropertyIO "PQXDH: initiate/respond derive same secret" iterations prop
  where
    prop g = do
        let -- Generate deterministic key material
            (aliceEdSec, g1)  = nextBytes 32 g
            (aliceXSec, g2)   = nextBytes 32 g1
            (bobEdSec, g3)    = nextBytes 32 g2
            (bobXSec, g4)     = nextBytes 32 g3
            (spkSecret, g5)   = nextBytes 32 g4
            (opkSecret, g6)   = nextBytes 32 g5
            (mlkemD, g7)      = nextBytes 32 g6
            (mlkemZ, g8)      = nextBytes 32 g7
            (ekSecret, g9)    = nextBytes 32 g8
            (mlkemRand, _)    = nextBytes 32 g9
        aliceIK <- generateIdentityKey aliceEdSec aliceXSec
        bobIK   <- generateIdentityKey bobEdSec bobXSec
        spk     <- generateKeyPair spkSecret
        spkSig  <- signPreKey bobIK (kpPublic spk)
        opk     <- generateKeyPair opkSecret
        let (ek, dk)          = mlkemKeyGen mlkemD mlkemZ
            MLKEMEncapKey ekBytes = ek
        bobEdSec' <- toByteString (ikEd25519Secret bobIK)
        spkSec'   <- toByteString (kpSecret spk)
        opkSec'   <- toByteString (kpSecret opk)
        let pqSig   = ed25519Sign bobEdSec' ekBytes
            bundle = PQPreKeyBundle
                { pqpkbIdentityKey     = ikX25519Public bobIK
                , pqpkbSignedPreKey    = kpPublic spk
                , pqpkbSPKSignature    = spkSig
                , pqpkbIdentityEd25519 = ikEd25519Public bobIK
                , pqpkbOneTimePreKey   = Just (kpPublic opk)
                , pqpkbPQPreKey        = ek
                , pqpkbPQKeySignature  = pqSig
                }
        mResult <- pqxdhInitiate aliceIK bundle ekSecret mlkemRand
        case mResult of
            Nothing -> pure False  -- SPK signature should verify
            Just result -> do
                let aliceSecret = pqxdhSharedSecret result
                mBobSecret <- pqxdhRespond
                        bobIK
                        spkSec'
                        (Just opkSec')
                        dk
                        (ikX25519Public aliceIK)
                        (pqxdhEphemeralKey result)
                        (pqxdhPQCiphertext result)
                case mBobSecret of
                    Nothing        -> pure False  -- DH all-zero should not occur with random keys
                    Just bobSecret -> pure (aliceSecret == bobSecret
                                      && BS.length aliceSecret == 32)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Flip the first byte of a ByteString (XOR with 0xFF).
-- Returns unchanged if empty.
flipFirstByte :: ByteString -> ByteString
flipFirstByte bs
    | BS.null bs = bs
    | otherwise  =
        let b = BS.head bs
            flipped = b `xorW8` 0xFF
        in BS.cons flipped (BS.tail bs)

xorW8 :: Word8 -> Word8 -> Word8
xorW8 = xor
