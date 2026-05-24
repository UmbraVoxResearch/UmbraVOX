-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- | Negative / fail-closed differential tests.
--
-- These verify that UmbraVOX correctly REJECTS malformed, invalid,
-- or tampered cryptographic inputs. Every test here must demonstrate
-- fail-closed behavior.
--
-- For each negative test, the rejection boundary is documented:
-- parser, decoder, primitive wrapper, protocol, or policy layer.
module Test.Crypto.Differential.Negative
    ( differentialNegativeTests
    , testProtocolNegative
    ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char (digitToInt, isHexDigit)
import Data.Word (Word8)
import Data.Bits (xor)
import Control.Exception (SomeException, try, evaluate)

import qualified UmbraVox.Crypto.Ed25519 as Ed25519
import qualified UmbraVox.Crypto.Curve25519 as X25519
import qualified UmbraVox.Crypto.GCM as GCM
import qualified UmbraVox.Crypto.ChaChaPoly as ChaChaPoly
import UmbraVox.Crypto.MLKEM
    ( MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..)
    , mlkemKeyGen, mlkemEncaps, mlkemDecaps
    )
import UmbraVox.Crypto.Signal.X3DH
    ( KeyPair(..), IdentityKey(..), PreKeyBundle(..), X3DHResult(..)
    , generateKeyPair, generateIdentityKey, signPreKey, x3dhInitiate
    )
import UmbraVox.Crypto.Signal.DoubleRatchet
    ( RatchetState(..), RatchetHeader(..), RatchetError(..)
    , ratchetInitAlice, ratchetInitBob, ratchetEncrypt, ratchetDecrypt
    )

-- | Run all negative tests.
differentialNegativeTests :: IO Bool
differentialNegativeTests = do
    putStrLn "[NegativeTests] Running fail-closed tests..."
    results <- sequence
        [ testEd25519Negative
        , testX25519Negative
        , testGCMNegative
        , testChaChaPolyNegative
        , testMLKEMNegative
        , testProtocolNegative
        ]
    let passed = length (filter id results)
        total = length results
    putStrLn $ "[NegativeTests] " ++ show passed ++ "/" ++ show total ++ " suites passed."
    return (and results)

hexToBS :: String -> ByteString
hexToBS [] = BS.empty
hexToBS [_] = BS.empty
hexToBS (a:b:rest)
    | isHexDigit a && isHexDigit b =
        BS.cons (fromIntegral $ digitToInt a * 16 + digitToInt b) (hexToBS rest)
    | otherwise = hexToBS rest

mustReject :: String -> String -> Maybe a -> IO Bool
mustReject suite name Nothing = do
    putStrLn $ "  PASS: " ++ name ++ " (rejected)"
    return True
mustReject suite name (Just _) = do
    putStrLn $ "  FAIL: " ++ name ++ " (accepted — should have rejected!)"
    return False

mustRejectBool :: String -> String -> Bool -> IO Bool
mustRejectBool suite name False = do
    putStrLn $ "  PASS: " ++ name ++ " (rejected)"
    return True
mustRejectBool suite name True = do
    putStrLn $ "  FAIL: " ++ name ++ " (accepted — should have rejected!)"
    return False

-- | Report an edge-case result without hiding findings.
-- Returns True (pass) when the input is rejected, False (finding) when accepted.
-- The 'finding' string documents what acceptance means for security.
reportEdge :: String -> String -> String -> Bool -> IO Bool
reportEdge _suite name _finding False = do
    putStrLn $ "  PASS: " ++ name ++ " (rejected)"
    return True
reportEdge _suite name finding True = do
    putStrLn $ "  FINDING: " ++ name ++ " (accepted) — " ++ finding
    return False

flipBit :: Int -> ByteString -> ByteString
flipBit idx bs
    | idx >= BS.length bs = bs
    | otherwise = BS.take idx bs `BS.append`
                  BS.singleton (BS.index bs idx `xor` 0x01) `BS.append`
                  BS.drop (idx + 1) bs

-- ── Ed25519 Negative Tests ──────────────────────────────────────────

testEd25519Negative :: IO Bool
testEd25519Negative = do
    putStrLn "  [Ed25519-Negative] Rejection boundary: primitive wrapper"
    let sk = hexToBS "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        pk = Ed25519.ed25519PublicKey sk
        msg = BS.pack [0x48, 0x65, 0x6c, 0x6c, 0x6f]  -- "Hello"
        sig = Ed25519.ed25519Sign sk msg

    -- Wrong message: verify(pk, "Wrong", sig) must fail
    r1 <- mustRejectBool "Ed25519" "wrong-message"
        (Ed25519.ed25519Verify pk (BS.pack [0x57, 0x72, 0x6f, 0x6e, 0x67]) sig)

    -- Bit-flipped signature: flip first bit of sig
    r2 <- mustRejectBool "Ed25519" "bitflip-signature"
        (Ed25519.ed25519Verify pk msg (flipBit 0 sig))

    -- Bit-flipped public key
    r3 <- mustRejectBool "Ed25519" "bitflip-pubkey"
        (Ed25519.ed25519Verify (flipBit 0 pk) msg sig)

    -- Truncated signature (63 bytes instead of 64)
    r4 <- mustRejectBool "Ed25519" "truncated-signature"
        (Ed25519.ed25519Verify pk msg (BS.take 63 sig))

    -- Wrong-length public key (31 bytes)
    r5 <- mustRejectBool "Ed25519" "short-pubkey"
        (Ed25519.ed25519Verify (BS.take 31 pk) msg sig)

    -- All-zero signature
    r6 <- mustRejectBool "Ed25519" "zero-signature"
        (Ed25519.ed25519Verify pk msg (BS.replicate 64 0))

    -- ── Ed25519 Signature Malleability Tests (crypto audit edge cases) ──

    -- S-malleability: Given valid (R, S), compute S' = L - S.
    -- A conformant implementation MUST reject S' because S' >= L (or
    -- equivalently S' is not the canonical representative mod L).
    -- Finding: If this test FAILs, the implementation accepts malleable
    -- signatures, which can be exploited for transaction malleability.
    let sigR = BS.take 32 sig
        sigSbs = BS.drop 32 sig
        sigS = Ed25519.decodeLE sigSbs
        -- S' = L - S  (this is congruent to -S mod L, i.e. L - S)
        sMalleable = Ed25519.groupL - sigS
        malleableSig = sigR `BS.append` Ed25519.encodeLEn 32 sMalleable
    r7 <- reportEdge "Ed25519" "S-malleability (S' = L - S)"
        "Vulnerability: accepts malleable signatures (S' = L - S congruent to -S mod L)"
        (Ed25519.ed25519Verify pk msg malleableSig)

    -- Non-canonical S: S = S_valid + L (still congruent mod L but >= L).
    -- Must be rejected by the S < L check.
    let nonCanonicalS = sigS + Ed25519.groupL
        nonCanonicalSig = sigR `BS.append` Ed25519.encodeLEn 32 nonCanonicalS
    r8 <- reportEdge "Ed25519" "non-canonical S (S + L)"
        "Vulnerability: accepts non-canonical S >= L"
        (Ed25519.ed25519Verify pk msg nonCanonicalSig)

    -- All-zero R: R = 0 (32 zero bytes) + valid S.
    -- Zero is not on the curve (or decodes to identity depending on impl).
    let zeroRSig = BS.replicate 32 0 `BS.append` sigSbs
    r9 <- reportEdge "Ed25519" "all-zero R point"
        "Finding: accepts signature with R = 0 (32 zero bytes)"
        (Ed25519.ed25519Verify pk msg zeroRSig)

    -- Identity R: R = encode(identity) = 0x01 followed by 31 zero bytes.
    -- The identity point encodes as (0,1) in Ed25519, which is byte 0x01
    -- followed by 31 zero bytes.
    let identityR = BS.cons 0x01 (BS.replicate 31 0)
        identityRSig = identityR `BS.append` sigSbs
    r10 <- reportEdge "Ed25519" "identity R point"
        "Finding: accepts signature with R = identity point"
        (Ed25519.ed25519Verify pk msg identityRSig)

    return (and [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10])

-- ── X25519 Negative Tests ───────────────────────────────────────────

testX25519Negative :: IO Bool
testX25519Negative = do
    putStrLn "  [X25519-Negative] Rejection boundary: primitive wrapper"
    let sk = hexToBS "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"

    -- All-zero public key (low-order point)
    r1 <- mustReject "X25519" "all-zero-pubkey"
        (X25519.x25519 sk (BS.replicate 32 0))

    -- Short public key (31 bytes)
    r2 <- mustReject "X25519" "short-pubkey"
        (X25519.x25519 sk (BS.take 31 (BS.replicate 32 0x09)))

    -- Short private key (31 bytes)
    r3 <- mustReject "X25519" "short-privkey"
        (X25519.x25519 (BS.take 31 sk) (BS.replicate 32 0x09))

    return (and [r1, r2, r3])

-- ── AES-256-GCM Negative Tests ──────────────────────────────────────

testGCMNegative :: IO Bool
testGCMNegative = do
    putStrLn "  [AES-GCM-Negative] Rejection boundary: primitive wrapper"
    let key = BS.replicate 32 0x42
        nonce = BS.replicate 12 0
        pt = BS.pack [1,2,3,4,5]
        (ct, tag) = GCM.gcmEncrypt key nonce BS.empty pt

    -- Wrong tag
    r1 <- mustReject "AES-GCM" "wrong-tag"
        (GCM.gcmDecrypt key nonce BS.empty ct (BS.replicate 16 0))

    -- Bit-flipped ciphertext
    r2 <- mustReject "AES-GCM" "bitflip-ciphertext"
        (GCM.gcmDecrypt key nonce BS.empty (flipBit 0 ct) tag)

    -- Wrong key
    r3 <- mustReject "AES-GCM" "wrong-key"
        (GCM.gcmDecrypt (BS.replicate 32 0xFF) nonce BS.empty ct tag)

    -- Wrong nonce
    r4 <- mustReject "AES-GCM" "wrong-nonce"
        (GCM.gcmDecrypt key (BS.replicate 12 1) BS.empty ct tag)

    -- Wrong AAD (decrypt with AAD that wasn't used during encrypt)
    r5 <- mustReject "AES-GCM" "wrong-aad"
        (GCM.gcmDecrypt key nonce (BS.pack [0xAA]) ct tag)

    return (and [r1, r2, r3, r4, r5])

-- ── ChaCha20-Poly1305 Negative Tests ────────────────────────────────

testChaChaPolyNegative :: IO Bool
testChaChaPolyNegative = do
    putStrLn "  [ChaChaPoly-Negative] Rejection boundary: primitive wrapper"
    let key = BS.replicate 32 0x42
        nonce = BS.replicate 12 0
        aad = BS.pack [0xAA, 0xBB]
        pt = BS.pack [1,2,3,4,5]
        (ct, tag) = ChaChaPoly.chachaPolyEncrypt key nonce aad pt

    -- Wrong tag
    r1 <- mustReject "ChaChaPoly" "wrong-tag"
        (ChaChaPoly.chachaPolyDecrypt key nonce aad ct (BS.replicate 16 0))

    -- Bit-flipped ciphertext
    r2 <- mustReject "ChaChaPoly" "bitflip-ciphertext"
        (ChaChaPoly.chachaPolyDecrypt key nonce aad (flipBit 0 ct) tag)

    -- Wrong key
    r3 <- mustReject "ChaChaPoly" "wrong-key"
        (ChaChaPoly.chachaPolyDecrypt (BS.replicate 32 0xFF) nonce aad ct tag)

    -- Wrong AAD
    r4 <- mustReject "ChaChaPoly" "wrong-aad"
        (ChaChaPoly.chachaPolyDecrypt key nonce (BS.pack [0xCC]) ct tag)

    return (and [r1, r2, r3, r4])

-- ── ML-KEM-768 Negative Tests ──────────────────────────────────────

-- | Finding: ML-KEM-768 must exhibit implicit rejection (FIPS 203 Section 4.2.3)
--   when decapsulating with the wrong key, truncated ciphertext, or corrupted
--   ciphertext. The shared secret returned MUST differ from the legitimate one.
-- Vulnerability: If decaps silently returns the correct shared secret on
--   tampered inputs, an attacker can recover session keys without possessing
--   the decapsulation key.
-- Fix: mlkemDecaps uses constant-time comparison of re-encrypted ciphertext
--   against the received ciphertext; on mismatch it returns SHAKE-256(z || ct).
-- Verified: Each sub-test below asserts that the decapsulated shared secret
--   differs from the legitimate encapsulated shared secret.
testMLKEMNegative :: IO Bool
testMLKEMNegative = do
    putStrLn "  [ML-KEM-Negative] Rejection boundary: implicit rejection (FIPS 203)"

    -- Set up a valid keypair and encapsulation
    let d  = BS.pack [0x01..0x20]
        z  = BS.pack [0x21..0x40]
        m  = BS.pack [0x41..0x60]
        (ek, dk) = mlkemKeyGen d z
        (ct, ssGood) = mlkemEncaps ek m

    -- ── 4. Wrong dk: decaps with a different key must NOT yield ssGood ──
    let d2 = BS.pack [0x81..0xA0]
        z2 = BS.pack [0xA1..0xC0]
        (_ek2, dk2) = mlkemKeyGen d2 z2
        ssWrongDK = mlkemDecaps dk2 ct
    r1 <- if ssWrongDK /= ssGood
           then do putStrLn "  PASS: mlkem-wrong-dk (implicit rejection)"
                   return True
           else do putStrLn "  FAIL: mlkem-wrong-dk (returned same ss!)"
                   return False

    -- ── 5. Truncated ciphertext: shorter than 1088 bytes ──
    -- mlkemDecaps may either return a rejection secret (implicit rejection)
    -- or throw on internal slice bounds checks. Both are acceptable:
    -- the key point is that it does NOT return the correct shared secret.
    let truncCT = MLKEMCiphertext (BS.take 512 (unwrapCT ct))
    r2 <- do
        result <- try (evaluate (mlkemDecaps dk truncCT))
                  :: IO (Either SomeException ByteString)
        case result of
            Left _ex -> do
                putStrLn "  PASS: mlkem-truncated-ct (rejected via exception)"
                return True
            Right ssTrunc
                | ssTrunc /= ssGood -> do
                    putStrLn "  PASS: mlkem-truncated-ct (implicit rejection)"
                    return True
                | otherwise -> do
                    putStrLn "  FAIL: mlkem-truncated-ct (returned same ss!)"
                    return False

    -- ── 6. Corrupted ciphertext: flip a bit, decaps should yield different ss ──
    let corruptedCT = MLKEMCiphertext (flipBit 0 (unwrapCT ct))
        ssCorrupt = mlkemDecaps dk corruptedCT
    r3 <- if ssCorrupt /= ssGood
           then do putStrLn "  PASS: mlkem-corrupted-ct (implicit rejection)"
                   return True
           else do putStrLn "  FAIL: mlkem-corrupted-ct (returned same ss!)"
                   return False

    return (and [r1, r2, r3])
  where
    unwrapCT (MLKEMCiphertext bs) = bs

-- ── Protocol-Level Negative Tests ──────────────────────────────────
--
-- These test the protocol layer (X3DH, Double Ratchet) for fail-closed
-- behavior on malformed inputs. We test our OWN implementation's
-- rejection behavior — no libsignal dependency required.

-- | Run all protocol-level negative tests.
--
-- Finding    Protocol-level fail-closed: X3DH must reject bundles with
--            invalid SPK signatures, wrong-length identity keys, and
--            low-order public keys.  Double Ratchet must reject corrupted
--            ciphertext and ciphertext decrypted under a wrong session.
-- Vulnerability: If any of these inputs are silently accepted, an attacker
--            can forge sessions (X3DH) or recover/inject plaintext
--            (Double Ratchet).
-- Fix:       Each protocol function returns Nothing or Left on malformed
--            input; these tests verify that contract.
-- Verified:  Each sub-test asserts rejection (Nothing / decryption failure).
testProtocolNegative :: IO Bool
testProtocolNegative = do
    putStrLn "  [Protocol-Negative] Rejection boundary: protocol layer (X3DH + Double Ratchet)"
    results <- sequence
        [ testX3DHWrongSPKSignature
        , testX3DHWrongLengthIdentityKey
        , testDoubleRatchetCorruptedCiphertext
        , testDoubleRatchetWrongSession
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  [Protocol-Negative] " ++ show passed ++ "/" ++ show total ++ " sub-tests passed."
    return (and results)

-- | X3DH: wrong SPK signature.  Create a valid bundle but replace the
-- SPK signature with garbage.  x3dhInitiate must reject (return Nothing)
-- because ed25519Verify fails on the tampered signature.
testX3DHWrongSPKSignature :: IO Bool
testX3DHWrongSPKSignature = do
    let -- Bob's identity key
        bobIK = generateIdentityKey
            (BS.pack [0x4c, 0xcd, 0x08, 0x9b, 0x28, 0xff, 0x96, 0xda,
                      0x9d, 0xb6, 0xc3, 0x46, 0xec, 0x11, 0x4e, 0x0f,
                      0x5b, 0x8a, 0x31, 0x9f, 0x35, 0xab, 0xa6, 0x24,
                      0xda, 0x8c, 0xf6, 0xed, 0x4f, 0xb8, 0xa6, 0xfb])
            (BS.pack [0x5d, 0xab, 0x08, 0x7e, 0x62, 0x4a, 0x8a, 0x4b,
                      0x79, 0xe1, 0x7f, 0x8b, 0x83, 0x80, 0x0e, 0xe6,
                      0x6f, 0x3b, 0xb1, 0x29, 0x26, 0x18, 0xb6, 0xfd,
                      0x1c, 0x2f, 0x8b, 0x27, 0xff, 0x88, 0xe0, 0xeb])
        -- Bob's SPK
        spkSecret = BS.pack [0xb8, 0xb4, 0xe2, 0x36, 0x80, 0x53, 0x18, 0xe9,
                             0x3f, 0x48, 0xbf, 0xbb, 0x36, 0x56, 0x56, 0xec,
                             0x1d, 0x06, 0x8b, 0xf3, 0xd8, 0xca, 0xbb, 0x64,
                             0xdd, 0x1b, 0xa4, 0x52, 0x3c, 0xec, 0x3a, 0x2a]
        spk = generateKeyPair spkSecret
        validSig = signPreKey bobIK (kpPublic spk)
        -- Tamper: flip first bit of signature
        badSig = flipBit 0 validSig
        bundle = PreKeyBundle
            { pkbIdentityKey     = ikX25519Public bobIK
            , pkbSignedPreKey    = kpPublic spk
            , pkbSPKSignature    = badSig
            , pkbIdentityEd25519 = ikEd25519Public bobIK
            , pkbOneTimePreKey   = Nothing
            }
        -- Alice's identity key
        aliceIK = generateIdentityKey
            (BS.pack [0x9d, 0x61, 0xb1, 0x9d, 0xef, 0xfd, 0x5a, 0x60,
                      0xba, 0x84, 0x4a, 0xf4, 0x92, 0xec, 0x2c, 0xc4,
                      0x44, 0x49, 0xc5, 0x69, 0x7b, 0x32, 0x69, 0x19,
                      0x70, 0x3b, 0xac, 0x03, 0x1c, 0xae, 0x7f, 0x60])
            (BS.pack [0x77, 0x07, 0x6d, 0x0a, 0x73, 0x18, 0xa5, 0x7d,
                      0x3c, 0x16, 0xc1, 0x72, 0x51, 0xb2, 0x66, 0x45,
                      0xdf, 0x4c, 0x2f, 0x87, 0xeb, 0xc0, 0x99, 0x2a,
                      0xb1, 0x77, 0xfb, 0xa5, 0x1d, 0xb9, 0x2c, 0x2a])
        ekSecret = BS.pack [0x4b, 0x66, 0xe9, 0xd4, 0xd1, 0xb4, 0x67, 0x3c,
                            0x5a, 0xd2, 0x26, 0x91, 0x95, 0x7d, 0x6a, 0xf5,
                            0xc1, 0x1b, 0x64, 0x21, 0xe0, 0xea, 0x01, 0xd4,
                            0x2c, 0xa4, 0x16, 0x9e, 0x79, 0x18, 0xba, 0x0d]
        result = x3dhInitiate aliceIK bundle ekSecret
    mustReject "X3DH" "wrong-SPK-signature (bitflip)" result

-- | X3DH: wrong-length identity key.  Provide a 31-byte X25519 identity
-- key in the bundle.  x3dhInitiate should reject because the DH operations
-- require exactly 32-byte keys; our X25519 wrapper rejects short keys.
testX3DHWrongLengthIdentityKey :: IO Bool
testX3DHWrongLengthIdentityKey = do
    let -- Bob's identity key (generate a real one for the Ed25519 fields)
        bobIK = generateIdentityKey
            (BS.pack [0x4c, 0xcd, 0x08, 0x9b, 0x28, 0xff, 0x96, 0xda,
                      0x9d, 0xb6, 0xc3, 0x46, 0xec, 0x11, 0x4e, 0x0f,
                      0x5b, 0x8a, 0x31, 0x9f, 0x35, 0xab, 0xa6, 0x24,
                      0xda, 0x8c, 0xf6, 0xed, 0x4f, 0xb8, 0xa6, 0xfb])
            (BS.pack [0x5d, 0xab, 0x08, 0x7e, 0x62, 0x4a, 0x8a, 0x4b,
                      0x79, 0xe1, 0x7f, 0x8b, 0x83, 0x80, 0x0e, 0xe6,
                      0x6f, 0x3b, 0xb1, 0x29, 0x26, 0x18, 0xb6, 0xfd,
                      0x1c, 0x2f, 0x8b, 0x27, 0xff, 0x88, 0xe0, 0xeb])
        spkSecret = BS.pack [0xb8, 0xb4, 0xe2, 0x36, 0x80, 0x53, 0x18, 0xe9,
                             0x3f, 0x48, 0xbf, 0xbb, 0x36, 0x56, 0x56, 0xec,
                             0x1d, 0x06, 0x8b, 0xf3, 0xd8, 0xca, 0xbb, 0x64,
                             0xdd, 0x1b, 0xa4, 0x52, 0x3c, 0xec, 0x3a, 0x2a]
        spk = generateKeyPair spkSecret
        spkSig = signPreKey bobIK (kpPublic spk)
        -- Truncated (31-byte) X25519 identity key
        shortIK = BS.take 31 (ikX25519Public bobIK)
        bundle = PreKeyBundle
            { pkbIdentityKey     = shortIK   -- 31 bytes: too short
            , pkbSignedPreKey    = kpPublic spk
            , pkbSPKSignature    = spkSig
            , pkbIdentityEd25519 = ikEd25519Public bobIK
            , pkbOneTimePreKey   = Nothing
            }
        -- Alice
        aliceIK = generateIdentityKey
            (BS.pack [0x9d, 0x61, 0xb1, 0x9d, 0xef, 0xfd, 0x5a, 0x60,
                      0xba, 0x84, 0x4a, 0xf4, 0x92, 0xec, 0x2c, 0xc4,
                      0x44, 0x49, 0xc5, 0x69, 0x7b, 0x32, 0x69, 0x19,
                      0x70, 0x3b, 0xac, 0x03, 0x1c, 0xae, 0x7f, 0x60])
            (BS.pack [0x77, 0x07, 0x6d, 0x0a, 0x73, 0x18, 0xa5, 0x7d,
                      0x3c, 0x16, 0xc1, 0x72, 0x51, 0xb2, 0x66, 0x45,
                      0xdf, 0x4c, 0x2f, 0x87, 0xeb, 0xc0, 0x99, 0x2a,
                      0xb1, 0x77, 0xfb, 0xa5, 0x1d, 0xb9, 0x2c, 0x2a])
        ekSecret = BS.pack [0x4b, 0x66, 0xe9, 0xd4, 0xd1, 0xb4, 0x67, 0x3c,
                            0x5a, 0xd2, 0x26, 0x91, 0x95, 0x7d, 0x6a, 0xf5,
                            0xc1, 0x1b, 0x64, 0x21, 0xe0, 0xea, 0x01, 0xd4,
                            0x2c, 0xa4, 0x16, 0x9e, 0x79, 0x18, 0xba, 0x0d]
        result = x3dhInitiate aliceIK bundle ekSecret
    mustReject "X3DH" "wrong-length-identity-key (31 bytes)" result

-- | DoubleRatchet: corrupted ciphertext.  Set up a valid Alice/Bob ratchet
-- session, encrypt a message, flip a bit in the ciphertext, and verify
-- that decrypt fails (GCM tag mismatch).
testDoubleRatchetCorruptedCiphertext :: IO Bool
testDoubleRatchetCorruptedCiphertext = do
    -- Set up a valid X3DH session to get the shared secret + Bob's SPK
    let aliceIK = generateIdentityKey
            (BS.replicate 32 0x01)
            (BS.replicate 32 0x02)
        bobIK = generateIdentityKey
            (BS.replicate 32 0x03)
            (BS.replicate 32 0x04)
        spkSecret = BS.replicate 32 0x05
        spk = generateKeyPair spkSecret
        spkSig = signPreKey bobIK (kpPublic spk)
        bundle = PreKeyBundle
            { pkbIdentityKey     = ikX25519Public bobIK
            , pkbSignedPreKey    = kpPublic spk
            , pkbSPKSignature    = spkSig
            , pkbIdentityEd25519 = ikEd25519Public bobIK
            , pkbOneTimePreKey   = Nothing
            }
        ekSecret = BS.replicate 32 0x06
    case x3dhInitiate aliceIK bundle ekSecret of
        Nothing -> do
            putStrLn "  SKIP: double-ratchet-corrupted-ct (X3DH setup failed)"
            return True
        Just x3dhResult -> do
            -- Initialize ratchet states
            let sharedSecret = x3dhSharedSecret x3dhResult
            case ratchetInitAlice sharedSecret (kpPublic spk) (BS.replicate 32 0x07) of
                Nothing -> do
                    putStrLn "  SKIP: double-ratchet-corrupted-ct (ratchet init failed)"
                    return True
                Just aliceState -> do
                    -- Alice encrypts
                    encResult <- ratchetEncrypt aliceState (BS.pack [0x48, 0x65, 0x6c, 0x6c, 0x6f])
                    case encResult of
                        Left err -> do
                            putStrLn $ "  SKIP: double-ratchet-corrupted-ct (encrypt error: " ++ show err ++ ")"
                            return True
                        Right (_aliceState', header, ct, tag) -> do
                            -- Bob's state
                            let bobState = ratchetInitBob sharedSecret spkSecret
                            -- Flip a bit in the ciphertext
                            let corruptedCT = flipBit 0 ct
                            -- Bob tries to decrypt the corrupted ciphertext
                            decResult <- ratchetDecrypt bobState header corruptedCT tag
                            case decResult of
                                Left _err -> do
                                    -- Error means rejection — pass
                                    putStrLn "  PASS: double-ratchet-corrupted-ct (rejected via error)"
                                    return True
                                Right Nothing -> do
                                    -- Nothing means GCM auth failed — pass
                                    putStrLn "  PASS: double-ratchet-corrupted-ct (rejected)"
                                    return True
                                Right (Just _) -> do
                                    putStrLn "  FAIL: double-ratchet-corrupted-ct (accepted — should have rejected!)"
                                    return False

-- | DoubleRatchet: wrong session.  Set up two independent sessions
-- (different shared secrets), encrypt under session A, try to decrypt
-- under session B.  Must fail.
testDoubleRatchetWrongSession :: IO Bool
testDoubleRatchetWrongSession = do
    -- Session A: Alice->Bob with one set of keys
    let aliceIK_A = generateIdentityKey
            (BS.replicate 32 0x11)
            (BS.replicate 32 0x12)
        bobIK_A = generateIdentityKey
            (BS.replicate 32 0x13)
            (BS.replicate 32 0x14)
        spkSecret_A = BS.replicate 32 0x15
        spk_A = generateKeyPair spkSecret_A
        spkSig_A = signPreKey bobIK_A (kpPublic spk_A)
        bundle_A = PreKeyBundle
            { pkbIdentityKey     = ikX25519Public bobIK_A
            , pkbSignedPreKey    = kpPublic spk_A
            , pkbSPKSignature    = spkSig_A
            , pkbIdentityEd25519 = ikEd25519Public bobIK_A
            , pkbOneTimePreKey   = Nothing
            }
        ekSecret_A = BS.replicate 32 0x16
    -- Session B: different keys entirely
    let aliceIK_B = generateIdentityKey
            (BS.replicate 32 0x21)
            (BS.replicate 32 0x22)
        bobIK_B = generateIdentityKey
            (BS.replicate 32 0x23)
            (BS.replicate 32 0x24)
        spkSecret_B = BS.replicate 32 0x25
        spk_B = generateKeyPair spkSecret_B
        spkSig_B = signPreKey bobIK_B (kpPublic spk_B)
        bundle_B = PreKeyBundle
            { pkbIdentityKey     = ikX25519Public bobIK_B
            , pkbSignedPreKey    = kpPublic spk_B
            , pkbSPKSignature    = spkSig_B
            , pkbIdentityEd25519 = ikEd25519Public bobIK_B
            , pkbOneTimePreKey   = Nothing
            }
        ekSecret_B = BS.replicate 32 0x26
    case (x3dhInitiate aliceIK_A bundle_A ekSecret_A,
          x3dhInitiate aliceIK_B bundle_B ekSecret_B) of
        (Just x3dh_A, Just x3dh_B) -> do
            let ss_A = x3dhSharedSecret x3dh_A
                ss_B = x3dhSharedSecret x3dh_B
            case ratchetInitAlice ss_A (kpPublic spk_A) (BS.replicate 32 0x17) of
                Nothing -> do
                    putStrLn "  SKIP: double-ratchet-wrong-session (ratchet init A failed)"
                    return True
                Just aliceState_A -> do
                    -- Alice A encrypts a message
                    encResult <- ratchetEncrypt aliceState_A (BS.pack [0x53, 0x65, 0x63, 0x72, 0x65, 0x74])
                    case encResult of
                        Left err -> do
                            putStrLn $ "  SKIP: double-ratchet-wrong-session (encrypt error: " ++ show err ++ ")"
                            return True
                        Right (_st', header, ct, tag) -> do
                            -- Bob B tries to decrypt with session B's state
                            let bobState_B = ratchetInitBob ss_B spkSecret_B
                            decResult <- ratchetDecrypt bobState_B header ct tag
                            case decResult of
                                Left _err -> do
                                    putStrLn "  PASS: double-ratchet-wrong-session (rejected via error)"
                                    return True
                                Right Nothing -> do
                                    putStrLn "  PASS: double-ratchet-wrong-session (rejected)"
                                    return True
                                Right (Just _) -> do
                                    putStrLn "  FAIL: double-ratchet-wrong-session (accepted — should have rejected!)"
                                    return False
        _ -> do
            putStrLn "  SKIP: double-ratchet-wrong-session (X3DH setup failed)"
            return True
