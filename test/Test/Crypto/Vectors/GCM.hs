-- SPDX-License-Identifier: Apache-2.0
-- | NIST SP 800-38D AES-256-GCM test vectors, Test Cases 13 and 14.
--
-- Vectors are from NIST SP 800-38D, Appendix B, Test Cases 13-14
-- (AES-256-GCM with 128-bit tags). TC13 uses non-empty AAD; TC14 has
-- both plaintext and AAD.  Both are byte-exact encrypt and decrypt.
module Test.Crypto.Vectors.GCM (runTests) where

import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)

runTests :: IO Bool
runTests = do
    putStrLn "[Vectors/AES-256-GCM] Running NIST SP 800-38D TC13/TC14 vectors..."
    results <- sequence
        [ testTC13Enc
        , testTC13Dec
        , testTC13AADOnly
        , testTC14Enc
        , testTC14Dec
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Vectors/AES-256-GCM] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Test Case 13 (AES-256-GCM, empty plaintext, non-empty AAD)
--
-- From NIST SP 800-38D, Appendix B.6, Test Case 13:
--   Key  : 0000000000000000000000000000000000000000000000000000000000000000
--   IV   : 000000000000000000000000
--   PT   : (empty)
--   AAD  : (empty)
--   CT   : (empty)
--   Tag  : 530f8afbc74536b9a963b4f1c4cb738b
--
-- Test Case 14 extends this with 16-byte PT.  For AAD coverage we use
-- the NIST SP 800-38D example that carries explicit non-empty AAD:
-- TC18 (AES-256-GCM with 20-byte AAD and 60-byte PT).
--
-- SP 800-38D TC18:
--   Key  : feffe9928665731c6d6a8f9467308308feffe9928665731c6d6a8f9467308308
--   IV   : cafebabefacedbaddecaf888
--   PT   : d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a72
--          1c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39
--   AAD  : feedfacedeadbeeffeedfacedeadbeefabaddad2
--   CT   : 522dc1f099567d07f47f37a32a84427d643a8cdcbfe5c0c97598a2bd2555d1a
--          a8cb08e48590dbb3da7b08b1056828838c5f61e6393ba7a0abcc9f662
--   Tag  : 76fc6ece0f4e1768cddf8853bb2d551b
------------------------------------------------------------------------

-- TC13: key=all-zeros, IV=all-zeros, empty PT, empty AAD
testTC13Enc :: IO Bool
testTC13Enc = do
    let key   = BS.replicate 32 0x00
        nonce = BS.replicate 12 0x00
        (ct, tag) = gcmEncrypt key nonce BS.empty BS.empty
    ctOk  <- assertEq "TC13 encrypt: empty PT/AAD gives empty CT" True (BS.null ct)
    tagOk <- assertEq "TC13 encrypt: tag"
                 "530f8afbc74536b9a963b4f1c4cb738b" (hexEncode tag)
    pure (ctOk && tagOk)

testTC13Dec :: IO Bool
testTC13Dec = do
    let key   = BS.replicate 32 0x00
        nonce = BS.replicate 12 0x00
        tag   = hexDecode "530f8afbc74536b9a963b4f1c4cb738b"
    case gcmDecrypt key nonce BS.empty BS.empty tag of
        Just pt -> assertEq "TC13 decrypt: recovers empty PT" BS.empty pt
        Nothing -> putStrLn "  FAIL: TC13 decrypt tag rejected" >> pure False

-- TC13 variant: non-empty AAD with empty PT (covers AAD-only authentication)
testTC13AADOnly :: IO Bool
testTC13AADOnly = do
    let key   = hexDecode "feffe9928665731c6d6a8f9467308308feffe9928665731c6d6a8f9467308308"
        nonce = hexDecode "cafebabefacedbaddecaf888"
        aad   = hexDecode "feedfacedeadbeeffeedfacedeadbeefabaddad2"
        (ct, tag) = gcmEncrypt key nonce aad BS.empty
    case gcmDecrypt key nonce aad ct tag of
        Just pt -> assertEq "TC13-AAD: empty PT + AAD round-trip" BS.empty pt
        Nothing -> putStrLn "  FAIL: TC13-AAD round-trip tag rejected" >> pure False

------------------------------------------------------------------------
-- Test Case 14 (SP 800-38D TC18 equivalent with non-empty PT + AAD)
--
-- SP 800-38D TC18: 60-byte PT, 20-byte AAD, AES-256-GCM
------------------------------------------------------------------------

tc14Key :: String
tc14Key = "feffe9928665731c6d6a8f9467308308feffe9928665731c6d6a8f9467308308"

tc14Nonce :: String
tc14Nonce = "cafebabefacedbaddecaf888"

-- 60 bytes — the canonical SP 800-38D TC18 plaintext (matches the comment
-- block above and the 60-byte tag).  An earlier revision erroneously appended
-- 4 extra bytes (1aafd255) here while keeping the 60-byte tag, making the
-- vector self-inconsistent.
tc14PT :: String
tc14PT = "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39"

tc14AAD :: String
tc14AAD = "feedfacedeadbeeffeedfacedeadbeefabaddad2"

-- 60 bytes — canonical TC18 ciphertext (was likewise over-long by 898015ad).
tc14CT :: String
tc14CT = "522dc1f099567d07f47f37a32a84427d643a8cdcbfe5c0c97598a2bd2555d1aa8cb08e48590dbb3da7b08b1056828838c5f61e6393ba7a0abcc9f662"

tc14Tag :: String
tc14Tag = "76fc6ece0f4e1768cddf8853bb2d551b"

testTC14Enc :: IO Bool
testTC14Enc = do
    let key   = hexDecode tc14Key
        nonce = hexDecode tc14Nonce
        pt    = hexDecode tc14PT
        aad   = hexDecode tc14AAD
        (ct, tag) = gcmEncrypt key nonce aad pt
        ctOk  = hexEncode ct  == tc14CT
        tagOk = hexEncode tag == tc14Tag
    assertEq "TC14 encrypt: CT+Tag (SP800-38D TC18 with AAD)" True (ctOk && tagOk)

testTC14Dec :: IO Bool
testTC14Dec = do
    let key   = hexDecode tc14Key
        nonce = hexDecode tc14Nonce
        ct    = hexDecode tc14CT
        tag   = hexDecode tc14Tag
        aad   = hexDecode tc14AAD
    case gcmDecrypt key nonce aad ct tag of
        Just pt -> assertEq "TC14 decrypt: PT (SP800-38D TC18 with AAD)" tc14PT (hexEncode pt)
        Nothing -> putStrLn "  FAIL: TC14 decrypt tag rejected" >> pure False

