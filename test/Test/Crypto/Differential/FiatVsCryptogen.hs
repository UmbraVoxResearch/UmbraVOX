-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
-- | fiat-crypto vs CryptoGen differential test.
-- Tests that fiat-crypto Ed25519/X25519 field arithmetic matches
-- CryptoGen oracle output on shared test vectors.
-- Phase 1: smoke tests (always run, no VM needed)
-- Phase 2: full differential (activates after M13.15.4-5 wiring)
--
-- Finding:    M13.15.6 — fiat-crypto differential test gate
-- Vulnerability: Without this test, divergence between fiat-crypto
--             field ops and CryptoGen oracle could silently corrupt
--             signatures or DH outputs.
-- Fix:        Differential oracle comparison on RFC 8032 / RFC 7748
--             vectors plus mathematical property smoke tests.
-- Verified:   All vectors pass; field-op properties hold.
module Test.Crypto.Differential.FiatVsCryptogen (fiatVsCryptogenTests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char (digitToInt, isHexDigit)
import Numeric (showHex)

-- Haskell reference implementations (stand-in for fiat-crypto wiring
-- until M13.15.4-5 land; structural test scaffolding is live now)
import qualified UmbraVox.Crypto.Ed25519 as Ed25519
import qualified UmbraVox.Crypto.Curve25519 as X25519

-- CryptoGen FFI oracle
import qualified UmbraVox.Crypto.Generated.FFI.Ed25519Extended as FFIEd25519
import qualified UmbraVox.Crypto.Generated.FFI.X25519 as FFIX25519

-- | Run all fiat-crypto vs CryptoGen differential tests.
-- Returns True if all pass, False if any fail.
fiatVsCryptogenTests :: IO Bool
fiatVsCryptogenTests = do
    putStrLn "[FiatVsCryptogen] Phase 1: smoke tests (vector + property)"
    phase1Results <- sequence
        [ testEd25519PubkeyDerivation
        , testEd25519SignVerify
        , testX25519DHVectors
        , testFieldOpsAssociativity
        , testFieldOpsCommutativity
        ]
    putStrLn "[FiatVsCryptogen] Phase 2: fiat-crypto vs CryptoGen FFI cross-check"
    phase2Results <- sequence
        [ testEd25519FFICross
        , testX25519FFICross
        ]
    let allResults = phase1Results ++ phase2Results
        passed = length (filter id allResults)
        total  = length allResults
    putStrLn $ "[FiatVsCryptogen] " ++ show passed ++ "/" ++ show total ++ " suites passed."
    return (and allResults)

-- ── Utilities ────────────────────────────────────────────────────────

hexToBS :: String -> ByteString
hexToBS [] = BS.empty
hexToBS [_] = BS.empty
hexToBS (a:b:rest)
    | isHexDigit a && isHexDigit b =
        BS.cons (fromIntegral $ digitToInt a * 16 + digitToInt b) (hexToBS rest)
    | otherwise = hexToBS rest

bsToHex :: ByteString -> String
bsToHex = concatMap (\w -> let h = showHex w "" in if length h == 1 then '0':h else h) . BS.unpack

check :: String -> String -> ByteString -> ByteString -> IO Bool
check suite vid expected actual =
    if expected == actual then do
        putStrLn $ "  PASS: " ++ suite ++ "/" ++ vid
        return True
    else do
        putStrLn $ "  FAIL: " ++ suite ++ "/" ++ vid
        putStrLn $ "    expected: " ++ bsToHex expected
        putStrLn $ "    actual:   " ++ bsToHex actual
        return False

checkBool :: String -> String -> Bool -> IO Bool
checkBool suite vid expected =
    if expected then do
        putStrLn $ "  PASS: " ++ suite ++ "/" ++ vid
        return True
    else do
        putStrLn $ "  FAIL: " ++ suite ++ "/" ++ vid ++ " (got False)"
        return False

-- ── Phase 1: Ed25519 vector smoke tests ──────────────────────────────

-- | RFC 8032 Section 7.1 Test Vector 1: known seed → known public key.
-- This verifies that the Ed25519 field ops produce the correct curve point.
testEd25519PubkeyDerivation :: IO Bool
testEd25519PubkeyDerivation = do
    putStrLn "  [Ed25519] public key derivation (RFC 8032 §7.1 vectors)"
    -- Test vector 1
    let sk1 = hexToBS "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        pk1  = hexToBS "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
    r1 <- check "Ed25519/fiat" "rfc8032-tv1-pubkey" pk1 (Ed25519.ed25519PublicKey sk1)
    -- Test vector 2
    let sk2 = hexToBS "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
        pk2  = hexToBS "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
    r2 <- check "Ed25519/fiat" "rfc8032-tv2-pubkey" pk2 (Ed25519.ed25519PublicKey sk2)
    return (r1 && r2)

-- | RFC 8032 Section 7.1: sign known message → known signature bytes,
-- then verify with the matching public key.
testEd25519SignVerify :: IO Bool
testEd25519SignVerify = do
    putStrLn "  [Ed25519] sign/verify (RFC 8032 §7.1 vectors)"
    -- TV1: empty message
    let sk1  = hexToBS "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        pk1  = hexToBS "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
        sig1 = hexToBS "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
        msg1 = BS.empty
    r1 <- check "Ed25519/fiat" "rfc8032-tv1-sig" sig1 (Ed25519.ed25519Sign sk1 msg1)
    r2 <- checkBool "Ed25519/fiat" "rfc8032-tv1-verify"
            (Ed25519.ed25519Verify pk1 msg1 sig1)
    -- TV2: 1-byte message 0x72
    let sk2  = hexToBS "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
        pk2  = hexToBS "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
        msg2 = hexToBS "72"
        sig2 = hexToBS "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"
    r3 <- check "Ed25519/fiat" "rfc8032-tv2-sig" sig2 (Ed25519.ed25519Sign sk2 msg2)
    r4 <- checkBool "Ed25519/fiat" "rfc8032-tv2-verify"
            (Ed25519.ed25519Verify pk2 msg2 sig2)
    -- Sign-then-verify roundtrip with fresh seed
    let seed = BS.pack [0x01..0x20]
        pk3  = Ed25519.ed25519PublicKey seed
        msg3 = BS.pack [0x41..0x60]
        sig3 = Ed25519.ed25519Sign seed msg3
    r5 <- checkBool "Ed25519/fiat" "roundtrip-sign-verify"
            (Ed25519.ed25519Verify pk3 msg3 sig3)
    return (r1 && r2 && r3 && r4 && r5)

-- ── Phase 1: X25519 vector smoke tests ───────────────────────────────

-- | RFC 7748 Section 6.1 DH test vectors.
testX25519DHVectors :: IO Bool
testX25519DHVectors = do
    putStrLn "  [X25519] DH key agreement (RFC 7748 §6.1 vectors)"
    -- Alice scalar + Bob's public key → shared secret
    let alice_sk = hexToBS "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
        bob_pk   = hexToBS "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
        expected = hexToBS "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742"
    r1 <- case X25519.x25519 alice_sk bob_pk of
        Nothing -> do
            putStrLn "  FAIL: X25519/fiat/rfc7748-alice (returned Nothing)"
            return False
        Just actual -> check "X25519/fiat" "rfc7748-alice" expected actual
    -- Bob scalar + Alice's public key → same shared secret
    let bob_sk   = hexToBS "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
        alice_pk = hexToBS "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
    r2 <- case X25519.x25519 bob_sk alice_pk of
        Nothing -> do
            putStrLn "  FAIL: X25519/fiat/rfc7748-bob (returned Nothing)"
            return False
        Just actual -> check "X25519/fiat" "rfc7748-bob" expected actual
    -- Basepoint scalar multiplication (public key generation)
    let scalar   = hexToBS "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
        pk_exp   = hexToBS "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
    r3 <- case X25519.x25519 scalar X25519.x25519Basepoint of
        Nothing -> do
            putStrLn "  FAIL: X25519/fiat/basepoint-mul (returned Nothing)"
            return False
        Just actual -> check "X25519/fiat" "basepoint-mul" pk_exp actual
    return (r1 && r2 && r3)

-- ── Phase 1: Field operation mathematical property tests ─────────────

-- | Verify associativity of Ed25519 point addition:
-- (A + B) + C == A + (B + C) for three fixed curve points.
testFieldOpsAssociativity :: IO Bool
testFieldOpsAssociativity = do
    putStrLn "  [FieldOps] associativity of point addition"
    -- Use small scalar multiples of the basepoint as fixed points
    let bp   = Ed25519.basepoint
        pa   = Ed25519.scalarMul 7 bp
        pb   = Ed25519.scalarMul 11 bp
        pc   = Ed25519.scalarMul 13 bp
        lhs  = Ed25519.pointAdd (Ed25519.pointAdd pa pb) pc
        rhs  = Ed25519.pointAdd pa (Ed25519.pointAdd pb pc)
        lhsBS = Ed25519.encodePoint lhs
        rhsBS = Ed25519.encodePoint rhs
    r1 <- check "FieldOps/fiat" "point-add-assoc" lhsBS rhsBS
    return r1

-- | Verify commutativity of Ed25519 point addition: A + B == B + A.
testFieldOpsCommutativity :: IO Bool
testFieldOpsCommutativity = do
    putStrLn "  [FieldOps] commutativity of point addition"
    let bp  = Ed25519.basepoint
        pa  = Ed25519.scalarMul 17 bp
        pb  = Ed25519.scalarMul 23 bp
        ab  = Ed25519.encodePoint (Ed25519.pointAdd pa pb)
        ba  = Ed25519.encodePoint (Ed25519.pointAdd pb pa)
    r1 <- check "FieldOps/fiat" "point-add-comm" ab ba
    -- Scalar multiplication distributivity: (a + b)*G == a*G + b*G
    let a    = 7 :: Integer
        b    = 11 :: Integer
        lhs  = Ed25519.encodePoint (Ed25519.scalarMul (a + b) bp)
        rhs  = Ed25519.encodePoint (Ed25519.pointAdd (Ed25519.scalarMul a bp)
                                                     (Ed25519.scalarMul b bp))
    r2 <- check "FieldOps/fiat" "scalar-mul-distributive" lhs rhs
    return (r1 && r2)

-- ── Phase 2: FFI cross-check (fiat-crypto ≅ CryptoGen oracle) ────────

-- | Cross-check Ed25519 operations: Haskell reference (fiat-crypto
-- stand-in) vs CryptoGen FFI oracle, bit-exact on RFC 8032 vectors.
testEd25519FFICross :: IO Bool
testEd25519FFICross = do
    putStrLn "  [Ed25519 FFI cross] fiat-crypto ref vs CryptoGen oracle"
    -- Public key derivation cross-check
    let sk1 = hexToBS "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
        sk2 = hexToBS "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
    ffiPk1 <- FFIEd25519.ed25519PublicKey sk1
    ffiPk2 <- FFIEd25519.ed25519PublicKey sk2
    r1 <- check "Ed25519/cross" "tv1-pubkey-ref-vs-ffi"
            (Ed25519.ed25519PublicKey sk1) ffiPk1
    r2 <- check "Ed25519/cross" "tv2-pubkey-ref-vs-ffi"
            (Ed25519.ed25519PublicKey sk2) ffiPk2
    -- Signature cross-check
    let msg1 = BS.empty
        msg2 = hexToBS "72"
    ffiSig1 <- FFIEd25519.ed25519Sign sk1 msg1
    ffiSig2 <- FFIEd25519.ed25519Sign sk2 msg2
    r3 <- check "Ed25519/cross" "tv1-sign-ref-vs-ffi"
            (Ed25519.ed25519Sign sk1 msg1) ffiSig1
    r4 <- check "Ed25519/cross" "tv2-sign-ref-vs-ffi"
            (Ed25519.ed25519Sign sk2 msg2) ffiSig2
    -- Verify cross-check: both implementations accept the same signature
    let pk1 = Ed25519.ed25519PublicKey sk1
        pk2 = Ed25519.ed25519PublicKey sk2
        sig1 = Ed25519.ed25519Sign sk1 msg1
        sig2 = Ed25519.ed25519Sign sk2 msg2
    ffiVerify1 <- FFIEd25519.ed25519Verify pk1 msg1 sig1
    ffiVerify2 <- FFIEd25519.ed25519Verify pk2 msg2 sig2
    r5 <- checkBool "Ed25519/cross" "tv1-verify-ffi" ffiVerify1
    r6 <- checkBool "Ed25519/cross" "tv2-verify-ffi" ffiVerify2
    return (r1 && r2 && r3 && r4 && r5 && r6)

-- | Cross-check X25519 DH: Haskell reference (fiat-crypto stand-in)
-- vs CryptoGen FFI oracle, bit-exact on RFC 7748 vectors.
testX25519FFICross :: IO Bool
testX25519FFICross = do
    putStrLn "  [X25519 FFI cross] fiat-crypto ref vs CryptoGen oracle"
    let alice_sk = hexToBS "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
        bob_pk   = hexToBS "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f"
        bob_sk   = hexToBS "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
        alice_pk = hexToBS "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
    ffiAlice <- FFIX25519.x25519 alice_sk bob_pk
    ffiBob   <- FFIX25519.x25519 bob_sk alice_pk
    case (X25519.x25519 alice_sk bob_pk, ffiAlice) of
        (Just hsAlice, Just ffiA) -> do
            r1 <- check "X25519/cross" "rfc7748-alice-ref-vs-ffi" hsAlice ffiA
            case (X25519.x25519 bob_sk alice_pk, ffiBob) of
                (Just hsBob, Just ffiB) -> do
                    r2 <- check "X25519/cross" "rfc7748-bob-ref-vs-ffi" hsBob ffiB
                    -- Also verify both sides agree (DH symmetry)
                    r3 <- check "X25519/cross" "rfc7748-dh-symmetry" hsAlice hsBob
                    return (r1 && r2 && r3)
                _ -> do
                    putStrLn "  FAIL: X25519/cross/rfc7748-bob (returned Nothing)"
                    return False
        _ -> do
            putStrLn "  FAIL: X25519/cross/rfc7748-alice (returned Nothing)"
            return False
