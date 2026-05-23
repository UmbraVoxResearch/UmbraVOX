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
--
-- M16.2-4 gap-fill additions:
--   GA = GCM auth-tag decision paths, EV = Ed25519 verify decision paths,
--   CS = Curve25519 cswap/x25519 paths, WF = WireFormat decode paths.
module Test.Security.MCDC (runTests) where

import Control.Exception (evaluate, try, SomeException)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util (assertEq)
import UmbraVox.Crypto.AES (aesEncrypt, aesDecrypt)
import UmbraVox.Crypto.ChaChaPoly (chachaPolyEncrypt, chachaPolyDecrypt)
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Ed25519 (ed25519Sign, ed25519Verify, ed25519PublicKey,
                                 decodePoint, groupL, encodeLEn)
import UmbraVox.Crypto.Export (decryptExport)
import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt, gcmEncryptSafe, gcmDecryptSafe)
import UmbraVox.Crypto.Keccak (sha3_256, shake128)
import UmbraVox.Network.Noise.Handshake (decryptWithKey)
import UmbraVox.Protocol.WireFormat (Envelope(..), decodeEnvelope, wrapEnvelope,
                                      encodeEnvelope, decodeEnvelopeAEAD,
                                      encodeEnvelopeAEAD, deriveEnvelopeKey)

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

          -- M16.2-4: GCM auth-tag decision coverage
        , testGA1_gcmAuthTagValid
        , testGA2_gcmAuthTagInvalid
        , testGA3_gcmEncryptSafeValid
        , testGA4_gcmDecryptSafeTagMismatch

          -- M16.2-4: Ed25519 verify decision paths
        , testEV1_validSignature
        , testEV2_sigWrongLen
        , testEV3_pubkeyWrongLen
        , testEV4_smallOrderPubkey
        , testEV5_smallOrderR
        , testEV6_sGEGroupL
        , testEV7_invalidPubkeyDecode
        , testEV8_invalidRDecode

          -- M16.2-4: X25519/cswap decision paths
        , testCS1_x25519ValidDH
        , testCS2_x25519WrongScalarLen
        , testCS3_x25519WrongPointLen
        , testCS4_x25519LowOrderPoint

          -- M16.2-4: WireFormat decode decision paths
        , testWF1_decodeTooShort
        , testWF2_decodeWrongVersion
        , testWF3_decodeHmacMismatch
        , testWF4_decodeRoundTrip
        , testWF5_decodeAEADTooShort
        , testWF6_decodeAEADWrongVersion
        , testWF7_decodeAEADRoundTrip
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

------------------------------------------------------------------------
-- M16.2-4: GCM authentication tag decision coverage
------------------------------------------------------------------------
-- These tests exercise the constantEq tag-verification branch inside
-- gcmDecrypt (GCM.hs:221) and the safe variants, covering:
--   - Tag matches (Just plaintext) — constantEq returns True
--   - Tag mismatches (Nothing) — constantEq returns False

-- GA1: valid encrypt + decrypt roundtrip — auth tag matches
testGA1_gcmAuthTagValid :: IO Bool
testGA1_gcmAuthTagValid = do
    let (ct, tag) = gcmEncrypt validKey32 validNonce12 BS.empty validPT
        result    = gcmDecrypt validKey32 validNonce12 BS.empty ct tag
    assertEq "GA1: gcmDecrypt valid tag returns Just plaintext"
        (Just validPT) result

-- GA2: decrypt with corrupted tag — auth tag mismatch
testGA2_gcmAuthTagInvalid :: IO Bool
testGA2_gcmAuthTagInvalid = do
    let (ct, tag) = gcmEncrypt validKey32 validNonce12 BS.empty validPT
        badTag    = BS.map (+ 1) tag  -- corrupt every byte
        result    = gcmDecrypt validKey32 validNonce12 BS.empty ct badTag
    assertEq "GA2: gcmDecrypt corrupted tag returns Nothing"
        Nothing result

-- GA3: gcmEncryptSafe valid inputs — Right path
testGA3_gcmEncryptSafeValid :: IO Bool
testGA3_gcmEncryptSafeValid = do
    let result = gcmEncryptSafe validKey32 validNonce12 BS.empty validPT
    case result of
        Right (ct, tag) -> assertEq "GA3: gcmEncryptSafe valid returns Right with 16-byte tag"
                               True (BS.length tag == 16 && BS.length ct == BS.length validPT)
        Left msg        -> putStrLn ("  FAIL: GA3: " ++ msg) >> pure False

-- GA4: gcmDecryptSafe tag mismatch — Right Nothing path
testGA4_gcmDecryptSafeTagMismatch :: IO Bool
testGA4_gcmDecryptSafeTagMismatch = do
    let (ct, tag) = gcmEncrypt validKey32 validNonce12 BS.empty validPT
        badTag    = BS.map (+ 1) tag
        result    = gcmDecryptSafe validKey32 validNonce12 BS.empty ct badTag
    assertEq "GA4: gcmDecryptSafe tag mismatch returns Right Nothing"
        (Right Nothing) result

------------------------------------------------------------------------
-- M16.2-4: Ed25519 verify decision path coverage
------------------------------------------------------------------------
-- ed25519Verify (Ed25519.hs:346-369) has 8 decision branches:
--   1. sig length /= 64 -> False
--   2. pubkey length /= 32 -> False
--   3. isSmallOrder pubkey -> False
--   4. isSmallOrder R -> False
--   5. decodePoint pubkey fails -> False
--   6. s >= groupL -> False
--   7. decodePoint R fails -> False
--   8. verification equation matches -> True
-- Each test independently varies one condition.

-- EV1: all conditions satisfied — valid signature
testEV1_validSignature :: IO Bool
testEV1_validSignature = do
    let sk  = BS.replicate 32 0x42
        pk  = ed25519PublicKey sk
        msg = BS.pack [0x48, 0x65, 0x6c, 0x6c, 0x6f]  -- "Hello"
        sig = ed25519Sign sk msg
    assertEq "EV1: ed25519Verify valid signature returns True"
        True (ed25519Verify pk msg sig)

-- EV2: sig wrong length (branch 1: BS.length sig /= 64)
testEV2_sigWrongLen :: IO Bool
testEV2_sigWrongLen = do
    let sk  = BS.replicate 32 0x42
        pk  = ed25519PublicKey sk
    assertEq "EV2: ed25519Verify sig len 63 returns False"
        False (ed25519Verify pk BS.empty (BS.replicate 63 0))

-- EV3: pubkey wrong length (branch 2: BS.length pubKeyBS /= 32)
testEV3_pubkeyWrongLen :: IO Bool
testEV3_pubkeyWrongLen =
    assertEq "EV3: ed25519Verify pubkey len 31 returns False"
        False (ed25519Verify (BS.replicate 31 0) BS.empty (BS.replicate 64 0))

-- EV4: small-order pubkey (branch 3: isSmallOrder pubKeyBS)
testEV4_smallOrderPubkey :: IO Bool
testEV4_smallOrderPubkey = do
    -- Identity point (0,1)
    let identityPK = BS.pack (1 : replicate 31 0)
    assertEq "EV4: ed25519Verify small-order pubkey returns False"
        False (ed25519Verify identityPK BS.empty (BS.replicate 64 0))

-- EV5: small-order R in signature (branch 4: isSmallOrder rBS)
testEV5_smallOrderR :: IO Bool
testEV5_smallOrderR = do
    let sk  = BS.replicate 32 0x42
        pk  = ed25519PublicKey sk
        -- R = identity point (0,1), S = 0
        identityR = BS.pack (1 : replicate 31 0)
        sig = identityR `BS.append` BS.replicate 32 0
    assertEq "EV5: ed25519Verify small-order R returns False"
        False (ed25519Verify pk BS.empty sig)

-- EV6: s >= groupL (branch 6: s >= groupL)
testEV6_sGEGroupL :: IO Bool
testEV6_sGEGroupL = do
    let sk  = BS.replicate 32 0x42
        pk  = ed25519PublicKey sk
        -- R = valid non-small-order point (the pubkey itself)
        -- S = groupL (>= groupL)
        bigS = encodeLEn 32 groupL
        sig  = pk `BS.append` bigS  -- R=pk (valid point), S=groupL
    assertEq "EV6: ed25519Verify s >= groupL returns False"
        False (ed25519Verify pk BS.empty sig)

-- EV7: decodePoint pubkey fails (branch 5: y >= p)
testEV7_invalidPubkeyDecode :: IO Bool
testEV7_invalidPubkeyDecode = do
    -- y = all 0xFF -> y > p, decodePoint returns Nothing
    let badPK = BS.replicate 32 0xFF
        sig   = BS.replicate 64 0
    -- Not small-order, correct length, but decodePoint will fail
    assertEq "EV7: ed25519Verify invalid pubkey decode returns False"
        False (ed25519Verify badPK BS.empty sig)

-- EV8: decodePoint R fails (branch 7: decodePoint rBS = Nothing)
testEV8_invalidRDecode :: IO Bool
testEV8_invalidRDecode = do
    let sk  = BS.replicate 32 0x42
        pk  = ed25519PublicKey sk
        -- R = all 0xFF (y >= p, decodePoint fails), S = 0 (valid, < groupL)
        badR = BS.replicate 32 0xFF
        sig  = badR `BS.append` BS.replicate 32 0
    assertEq "EV8: ed25519Verify invalid R decode returns False"
        False (ed25519Verify pk BS.empty sig)

------------------------------------------------------------------------
-- M16.2-4: X25519 / cswap decision path coverage
------------------------------------------------------------------------
-- x25519 (Curve25519.hs:141-152) has 4 decision branches:
--   1. scalar length /= 32 -> Nothing
--   2. uCoord length /= 32 -> Nothing
--   3. result == all-zero -> Nothing  (low-order point)
--   4. otherwise -> Just result

-- CS1: valid DH computation — Just result
testCS1_x25519ValidDH :: IO Bool
testCS1_x25519ValidDH = do
    let sk = BS.pack (0x77 : replicate 31 0)
    case x25519 sk x25519Basepoint of
        Just pk -> assertEq "CS1: x25519 valid DH produces 32-byte result"
                       32 (BS.length pk)
        Nothing -> putStrLn "  FAIL: CS1: x25519 returned Nothing for valid input"
                   >> pure False

-- CS2: wrong scalar length — Nothing
testCS2_x25519WrongScalarLen :: IO Bool
testCS2_x25519WrongScalarLen =
    assertEq "CS2: x25519 scalar len 16 returns Nothing"
        Nothing (x25519 (BS.replicate 16 0x42) x25519Basepoint)

-- CS3: wrong point length — Nothing
testCS3_x25519WrongPointLen :: IO Bool
testCS3_x25519WrongPointLen =
    assertEq "CS3: x25519 point len 16 returns Nothing"
        Nothing (x25519 (BS.replicate 32 0x42) (BS.replicate 16 0))

-- CS4: low-order point produces all-zero output — Nothing
testCS4_x25519LowOrderPoint :: IO Bool
testCS4_x25519LowOrderPoint = do
    -- The all-zero point is low-order; x25519 should return Nothing
    let zeroPoint = BS.replicate 32 0
    assertEq "CS4: x25519 low-order (zero) point returns Nothing"
        Nothing (x25519 (BS.replicate 32 0x42) zeroPoint)

------------------------------------------------------------------------
-- M16.2-4: WireFormat decode decision path coverage
------------------------------------------------------------------------
-- decodeEnvelope (WireFormat.hs:121-149) has 4 decision branches:
--   1. too short (< headerSize + hmacSize = 77) -> Nothing
--   2. version /= 2 -> Nothing
--   3. HMAC mismatch -> Nothing
--   4. valid -> Just Envelope
--
-- decodeEnvelopeAEAD (WireFormat.hs:299-322) has 5 branches:
--   1. too short (< 2) -> Nothing
--   2. version /= 2 -> Nothing
--   3. type == 2 -> delegates to decodeEnvelope (HMAC)
--   4. seqNum >= 2^31 -> Nothing
--   5. AEAD decrypt and parse -> Just Envelope / Nothing

-- WF1: too short for HMAC envelope
testWF1_decodeTooShort :: IO Bool
testWF1_decodeTooShort =
    assertEq "WF1: decodeEnvelope too-short input returns Nothing"
        Nothing (decodeEnvelope (BS.replicate 32 0) (BS.replicate 10 0))

-- WF2: wrong version byte
testWF2_decodeWrongVersion :: IO Bool
testWF2_decodeWrongVersion = do
    let key = BS.replicate 32 0xAA
        env = wrapEnvelope 1 0 (BS.replicate 32 0) 0 0 (BS.pack [0x41])
        wire = encodeEnvelope key env
        -- Overwrite version byte (first byte) from 2 to 1
        badWire = BS.cons 1 (BS.drop 1 wire)
    assertEq "WF2: decodeEnvelope wrong version returns Nothing"
        Nothing (decodeEnvelope key badWire)

-- WF3: HMAC mismatch (corrupted payload)
testWF3_decodeHmacMismatch :: IO Bool
testWF3_decodeHmacMismatch = do
    let key = BS.replicate 32 0xAA
        env = wrapEnvelope 1 0 (BS.replicate 32 0) 0 0 (BS.pack [0x41])
        wire = encodeEnvelope key env
        -- Flip a byte in the payload area (byte 45 is in the payload)
        badWire = flipByteAt 45 wire
    assertEq "WF3: decodeEnvelope HMAC mismatch returns Nothing"
        Nothing (decodeEnvelope key badWire)

-- WF4: valid encode/decode roundtrip
testWF4_decodeRoundTrip :: IO Bool
testWF4_decodeRoundTrip = do
    let key     = BS.replicate 32 0xBB
        payload = BS.pack [0x48, 0x65, 0x6c, 0x6c, 0x6f]
        env     = wrapEnvelope 1 42 (BS.replicate 32 0xCC) 0xDD 0x1234 payload
        wire    = encodeEnvelope key env
    case decodeEnvelope key wire of
        Nothing   -> putStrLn "  FAIL: WF4: decodeEnvelope returned Nothing" >> pure False
        Just env' -> assertEq "WF4: decodeEnvelope roundtrip preserves payload"
                         payload (envPayload env')

-- WF5: AEAD too short
testWF5_decodeAEADTooShort :: IO Bool
testWF5_decodeAEADTooShort =
    assertEq "WF5: decodeEnvelopeAEAD too-short returns Nothing"
        Nothing (decodeEnvelopeAEAD (BS.replicate 32 0) 0 (BS.singleton 2))

-- WF6: AEAD wrong version
testWF6_decodeAEADWrongVersion :: IO Bool
testWF6_decodeAEADWrongVersion = do
    let key = deriveEnvelopeKey (BS.replicate 32 0xDD)
        env = wrapEnvelope 1 0 (BS.replicate 32 0) 0 0 (BS.pack [0x41])
        wire = encodeEnvelopeAEAD key 0 env
        badWire = BS.cons 1 (BS.drop 1 wire)
    assertEq "WF6: decodeEnvelopeAEAD wrong version returns Nothing"
        Nothing (decodeEnvelopeAEAD key 0 badWire)

-- WF7: AEAD valid encode/decode roundtrip
testWF7_decodeAEADRoundTrip :: IO Bool
testWF7_decodeAEADRoundTrip = do
    let transportKey = BS.replicate 32 0xEE
        key     = deriveEnvelopeKey transportKey
        payload = BS.pack [0x48, 0x69]
        env     = wrapEnvelope 1 100 (BS.replicate 32 0xFF) 0xAA 0x5678 payload
        wire    = encodeEnvelopeAEAD key 100 env
    case decodeEnvelopeAEAD key 100 wire of
        Nothing   -> putStrLn "  FAIL: WF7: decodeEnvelopeAEAD returned Nothing" >> pure False
        Just env' -> assertEq "WF7: decodeEnvelopeAEAD roundtrip preserves payload"
                         payload (envPayload env')

------------------------------------------------------------------------
-- Additional helpers
------------------------------------------------------------------------

-- | Flip a single byte at position @i@ in a ByteString.
flipByteAt :: Int -> ByteString -> ByteString
flipByteAt i bs
    | i < 0 || i >= BS.length bs = bs
    | otherwise =
        let (before, rest) = BS.splitAt i bs
            byte = BS.index rest 0
            after = BS.drop 1 rest
        in before `BS.append` BS.singleton (xor byte 0xFF) `BS.append` after
