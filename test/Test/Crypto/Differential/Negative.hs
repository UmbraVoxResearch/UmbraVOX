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
    ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char (digitToInt, isHexDigit)
import Data.Word (Word8)
import Data.Bits (xor)

import qualified UmbraVox.Crypto.Ed25519 as Ed25519
import qualified UmbraVox.Crypto.Curve25519 as X25519
import qualified UmbraVox.Crypto.GCM as GCM
import qualified UmbraVox.Crypto.ChaChaPoly as ChaChaPoly

-- | Run all negative tests.
differentialNegativeTests :: IO Bool
differentialNegativeTests = do
    putStrLn "[NegativeTests] Running fail-closed tests..."
    results <- sequence
        [ testEd25519Negative
        , testX25519Negative
        , testGCMNegative
        , testChaChaPolyNegative
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

    return (and [r1, r2, r3, r4, r5, r6])

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
