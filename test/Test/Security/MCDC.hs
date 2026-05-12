-- SPDX-License-Identifier: Apache-2.0
-- | DO-178C DAL A — MC/DC test suite for compound guards in crypto modules.
--
-- Each test targets a specific row in doc/MCDC-TABLES.md that was not covered
-- by pre-existing tests.  The structure mirrors the table numbering: a comment
-- identifies the module, guard location, table row, and the condition being
-- independently varied.
--
-- Test IDs follow the gap table in doc/MCDC-TABLES.md:
--   GE = gcmEncrypt, GD = gcmDecrypt, CE = chachaPolyEncrypt,
--   CD = chachaPolyDecrypt, AE = aesEncrypt, AD = aesDecrypt,
--   EX = decryptExport, DP = Ed25519.decodePoint, KS = KeyStore blob length,
--   KK = Keccak sponge rate, DW = Noise decryptWithKey.
module Test.Security.MCDC (runTests) where

import Control.Exception (evaluate, try, SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util (assertEq)
import UmbraVox.Crypto.AES (aesEncrypt, aesDecrypt)
import UmbraVox.Crypto.ChaChaPoly (chachaPolyEncrypt, chachaPolyDecrypt)
import UmbraVox.Crypto.Ed25519 (decodePoint)
import UmbraVox.Crypto.Export (decryptExport)
import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.Keccak (sha3_256, shake128)
import UmbraVox.Network.Noise.Handshake (decryptWithKey)

------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------

runTests :: IO Bool
runTests = do
    putStrLn "[Security/MCDC] Running DO-178C DAL A MC/DC gap-fill tests..."
    results <- sequence
        [ -- GCM gcmEncrypt guards (table 1 in MCDC-TABLES.md)
          testGE1_keyWrongLen
        , testGE2_nonceWrongLen

          -- GCM gcmDecrypt guards (table 2)
        , testGD1_keyWrongLen
        , testGD2_nonceWrongLen
        , testGD3_tagWrongLen

          -- ChaChaPoly chachaPolyEncrypt guards (table 3)
        , testCE1_keyWrongLen
        , testCE2_nonceWrongLen

          -- ChaChaPoly chachaPolyDecrypt guards (table 4)
        , testCD1_keyWrongLen
        , testCD2_nonceWrongLen
        , testCD3_tagWrongLen

          -- AES aesEncrypt guards (table 5)
        , testAE1_keyWrongLen
        , testAE2_ptWrongLen

          -- AES aesDecrypt guards (table 6)
        , testAD1_keyWrongLen
        , testAD2_ctWrongLen

          -- Export decryptExport guard (table 13)
        , testEX1_blobTooShort

          -- Ed25519 decodePoint guard (table 11)
        , testDP1_wrongLen

          -- Keccak sponge rate guards (table 8) — reached via exported wrappers
        , testKK_sha3_256_valid
        , testKK_shake128_valid

          -- Noise decryptWithKey guard (table 14)
        , testDW1_cipherMacTooShort
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Security/MCDC] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Assert that a pure thunk throws when forced.  The thunk must evaluate
-- to WHNF, which is sufficient to trigger an 'error' call at the top level.
-- Uses seq-via-IO: passes the value through an IO action that forces it.
assertThrowsPure :: String -> IO () -> IO Bool
assertThrowsPure name forceAction = do
    result <- (try forceAction :: IO (Either SomeException ()))
    case result of
        Left _  -> putStrLn ("  PASS: " ++ name) >> pure True
        Right _ -> putStrLn ("  FAIL: " ++ name ++ " (expected exception, got none)")
                   >> pure False

-- | Valid 32-byte key and 12-byte nonce used as baselines.
validKey32 :: ByteString
validKey32 = BS.replicate 32 0xaa

validNonce12 :: ByteString
validNonce12 = BS.replicate 12 0xbb

validPT :: ByteString
validPT = BS.replicate 16 0xcc

validBlock :: ByteString
validBlock = BS.replicate 16 0xdd

validTag16 :: ByteString
validTag16 = BS.replicate 16 0x00

------------------------------------------------------------------------
-- GCM gcmEncrypt guards
------------------------------------------------------------------------

-- GE1: key_wrong = T, nonce_wrong = F  (table 1, row 2)
-- GCM.hs:161 — BS.length key /= 32 = error
testGE1_keyWrongLen :: IO Bool
testGE1_keyWrongLen =
    assertThrowsPure "GE1: gcmEncrypt wrong key length triggers error" $
        evaluate (fst (gcmEncrypt (BS.replicate 16 0) validNonce12 BS.empty validPT))
        >> pure ()

-- GE2: key_wrong = F, nonce_wrong = T  (table 1, row 3)
-- GCM.hs:162 — BS.length nonce /= 12 = error
testGE2_nonceWrongLen :: IO Bool
testGE2_nonceWrongLen =
    assertThrowsPure "GE2: gcmEncrypt wrong nonce length triggers error" $
        evaluate (fst (gcmEncrypt validKey32 (BS.replicate 8 0) BS.empty validPT))
        >> pure ()

------------------------------------------------------------------------
-- GCM gcmDecrypt guards
------------------------------------------------------------------------

-- GD1: key_wrong = T  (table 2, row 2)
-- GCM.hs:183 — BS.length key /= 32 = error
testGD1_keyWrongLen :: IO Bool
testGD1_keyWrongLen =
    assertThrowsPure "GD1: gcmDecrypt wrong key length triggers error" $
        evaluate (gcmDecrypt (BS.replicate 16 0) validNonce12 BS.empty validPT validTag16)
        >> pure ()

-- GD2: nonce_wrong = T  (table 2, row 3)
-- GCM.hs:184 — BS.length nonce /= 12 = error
testGD2_nonceWrongLen :: IO Bool
testGD2_nonceWrongLen =
    assertThrowsPure "GD2: gcmDecrypt wrong nonce length triggers error" $
        evaluate (gcmDecrypt validKey32 (BS.replicate 8 0) BS.empty validPT validTag16)
        >> pure ()

-- GD3: tag_wrong = T  (table 2, row 4)
-- GCM.hs:185 — BS.length tag /= 16 = Nothing
testGD3_tagWrongLen :: IO Bool
testGD3_tagWrongLen = do
    let shortTag = BS.replicate 8 0x00
    let result   = gcmDecrypt validKey32 validNonce12 BS.empty validPT shortTag
    assertEq "GD3: gcmDecrypt tag len /= 16 returns Nothing"
        Nothing result

------------------------------------------------------------------------
-- ChaChaPoly chachaPolyEncrypt guards
------------------------------------------------------------------------

-- CE1: key_wrong = T  (table 3, row 2)
-- ChaChaPoly.hs:70 — BS.length key /= 32 = error
testCE1_keyWrongLen :: IO Bool
testCE1_keyWrongLen =
    assertThrowsPure "CE1: chachaPolyEncrypt wrong key length triggers error" $
        evaluate (fst (chachaPolyEncrypt (BS.replicate 16 0) validNonce12 BS.empty validPT))
        >> pure ()

-- CE2: nonce_wrong = T  (table 3, row 3)
-- ChaChaPoly.hs:71 — BS.length nonce /= 12 = error
testCE2_nonceWrongLen :: IO Bool
testCE2_nonceWrongLen =
    assertThrowsPure "CE2: chachaPolyEncrypt wrong nonce length triggers error" $
        evaluate (fst (chachaPolyEncrypt validKey32 (BS.replicate 8 0) BS.empty validPT))
        >> pure ()

------------------------------------------------------------------------
-- ChaChaPoly chachaPolyDecrypt guards
------------------------------------------------------------------------

-- CD1: key_wrong = T  (table 4, row 2)
-- ChaChaPoly.hs:101 — BS.length key /= 32 = error
testCD1_keyWrongLen :: IO Bool
testCD1_keyWrongLen =
    assertThrowsPure "CD1: chachaPolyDecrypt wrong key length triggers error" $
        evaluate (chachaPolyDecrypt (BS.replicate 16 0) validNonce12 BS.empty validPT validTag16)
        >> pure ()

-- CD2: nonce_wrong = T  (table 4, row 3)
-- ChaChaPoly.hs:102 — BS.length nonce /= 12 = error
testCD2_nonceWrongLen :: IO Bool
testCD2_nonceWrongLen =
    assertThrowsPure "CD2: chachaPolyDecrypt wrong nonce length triggers error" $
        evaluate (chachaPolyDecrypt validKey32 (BS.replicate 8 0) BS.empty validPT validTag16)
        >> pure ()

-- CD3: tag_wrong = T  (table 4, row 4)
-- ChaChaPoly.hs:103 — BS.length tag /= 16 = Nothing
testCD3_tagWrongLen :: IO Bool
testCD3_tagWrongLen = do
    let shortTag = BS.replicate 8 0x00
    let result   = chachaPolyDecrypt validKey32 validNonce12 BS.empty validPT shortTag
    assertEq "CD3: chachaPolyDecrypt tag len /= 16 returns Nothing"
        Nothing result

------------------------------------------------------------------------
-- AES aesEncrypt guards
------------------------------------------------------------------------

-- AE1: key_wrong = T  (table 5, row 2)
-- AES.hs:292 — BS.length key /= 32 = error
testAE1_keyWrongLen :: IO Bool
testAE1_keyWrongLen =
    assertThrowsPure "AE1: aesEncrypt wrong key length triggers error" $
        evaluate (aesEncrypt (BS.replicate 16 0) validBlock) >> pure ()

-- AE2: pt_wrong = T  (table 5, row 3)
-- AES.hs:293 — BS.length plaintext /= 16 = error
testAE2_ptWrongLen :: IO Bool
testAE2_ptWrongLen =
    assertThrowsPure "AE2: aesEncrypt wrong plaintext length triggers error" $
        evaluate (aesEncrypt validKey32 (BS.replicate 8 0)) >> pure ()

------------------------------------------------------------------------
-- AES aesDecrypt guards
------------------------------------------------------------------------

-- AD1: key_wrong = T  (table 6, row 2)
-- AES.hs:303 — BS.length key /= 32 = error
testAD1_keyWrongLen :: IO Bool
testAD1_keyWrongLen =
    assertThrowsPure "AD1: aesDecrypt wrong key length triggers error" $
        evaluate (aesDecrypt (BS.replicate 16 0) validBlock) >> pure ()

-- AD2: ct_wrong = T  (table 6, row 3)
-- AES.hs:304 — BS.length ciphertext /= 16 = error
testAD2_ctWrongLen :: IO Bool
testAD2_ctWrongLen =
    assertThrowsPure "AD2: aesDecrypt wrong ciphertext length triggers error" $
        evaluate (aesDecrypt validKey32 (BS.replicate 8 0)) >> pure ()

------------------------------------------------------------------------
-- Export decryptExport guard
------------------------------------------------------------------------

-- EX1: blob_too_short = T  (table 13, row 2)
-- Export.hs:102 — BS.length blob < headerLen + tagLen (60) = Nothing
testEX1_blobTooShort :: IO Bool
testEX1_blobTooShort = do
    -- headerLen = 44 (32 salt + 12 nonce), tagLen = 16 → minimum = 60
    let shortBlob = BS.replicate 40 0xde
    let result    = decryptExport BS.empty shortBlob
    assertEq "EX1: decryptExport short blob returns Nothing"
        Nothing result

------------------------------------------------------------------------
-- Ed25519 decodePoint guard
------------------------------------------------------------------------

-- DP1: len_wrong = T  (table 11, row 2)
-- Ed25519.hs:242 — BS.length bs /= 32 = Nothing
testDP1_wrongLen :: IO Bool
testDP1_wrongLen = do
    let shortBS = BS.replicate 16 0x00
    assertEq "DP1: decodePoint wrong length returns Nothing"
        Nothing (decodePoint shortBS)

------------------------------------------------------------------------
-- Keccak sponge rate guards (table 8)
-- sponge is not exported; we verify that the exported wrappers
-- (sha3_256, shake128) exercise the valid-rate path (row 1 of each
-- guard), confirming baseline coverage.  The invalid-rate paths
-- (rows B, 2, 3) are dead code from the perspective of exported
-- callers — all exported functions embed fixed valid rates — and
-- are documented as structurally unreachable in MCDC-TABLES.md.
------------------------------------------------------------------------

-- KK_sha3_256_valid: rate = 136, valid — confirms guard row 1 baseline.
testKK_sha3_256_valid :: IO Bool
testKK_sha3_256_valid = do
    let digest = sha3_256 BS.empty
    assertEq "KK: sha3_256 empty produces 32 bytes (valid rate path)"
        32 (BS.length digest)

-- KK_shake128_valid: rate = 168, valid — confirms guard row 1 baseline.
testKK_shake128_valid :: IO Bool
testKK_shake128_valid = do
    let out = shake128 BS.empty 64
    assertEq "KK: shake128 empty produces 64 bytes (valid rate path)"
        64 (BS.length out)

------------------------------------------------------------------------
-- Noise Handshake decryptWithKey guard
------------------------------------------------------------------------

-- DW1: too_short = T  (table 14, row 2)
-- Handshake.hs:329 — BS.length cipherMac < hsHmacLen (32) = Nothing
testDW1_cipherMacTooShort :: IO Bool
testDW1_cipherMacTooShort = do
    let k          = BS.replicate 32 0x01
        h          = BS.replicate 32 0x02
        shortInput = BS.replicate 16 0x03  -- < 32 bytes (hsHmacLen)
    assertEq "DW1: decryptWithKey cipherMac < 32 bytes returns Nothing"
        Nothing (decryptWithKey k h shortInput)
