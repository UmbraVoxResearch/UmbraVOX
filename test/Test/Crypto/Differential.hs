-- SPDX-License-Identifier: Apache-2.0
-- | Differential tests: C implementations vs Haskell oracle.
--
-- For each crypto primitive, runs the same input through both the
-- Haskell reference implementation and the generated C/FFI path,
-- verifying bit-exact output match.
--
-- Currently the FFI wrappers delegate to the Haskell reference
-- implementation, so these tests pass trivially.  When the C
-- implementations become real, these tests will catch any divergence.
module Test.Crypto.Differential (runTests) where

import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)

import Test.Util

-- Haskell reference implementations (the oracle)
import qualified UmbraVox.Crypto.SHA256 as SHA256
import qualified UmbraVox.Crypto.SHA512 as SHA512
import qualified UmbraVox.Crypto.HMAC as HMAC
import qualified UmbraVox.Crypto.HKDF as HKDF
import qualified UmbraVox.Crypto.Random as ChaCha20
import qualified UmbraVox.Crypto.Poly1305 as Poly1305
import qualified UmbraVox.Crypto.AES as AES
import qualified UmbraVox.Crypto.Keccak as Keccak
import qualified UmbraVox.Crypto.Curve25519 as X25519
import qualified UmbraVox.Crypto.GCM as GCM

-- FFI wrappers (will call C when real implementations land)
import qualified UmbraVox.Crypto.Generated.FFI.SHA256 as FFISHA256
import qualified UmbraVox.Crypto.Generated.FFI.SHA512 as FFISHA512
import qualified UmbraVox.Crypto.Generated.FFI.HMAC as FFIHMAC
import qualified UmbraVox.Crypto.Generated.FFI.HKDF as FFIHKDF
import qualified UmbraVox.Crypto.Generated.FFI.ChaCha20 as FFIChaCha20
import qualified UmbraVox.Crypto.Generated.FFI.Poly1305 as FFIPoly1305
import qualified UmbraVox.Crypto.Generated.FFI.AES256 as FFIAES256
import qualified UmbraVox.Crypto.Generated.FFI.Keccak as FFIKeccak
import qualified UmbraVox.Crypto.Generated.FFI.X25519 as FFIX25519
import qualified UmbraVox.Crypto.Generated.FFI.GCM as FFIGCM

runTests :: IO Bool
runTests = do
    putStrLn "[DIFFERENTIAL] Running C vs Haskell differential tests..."
    results <- sequence
        [ testSHA256Differential
        , testSHA512Differential
        , testHMACDifferential
        , testHKDFDifferential
        , testChaCha20Differential
        , testPoly1305Differential
        , testAES256Differential
        , testGCMDifferential
        , testKeccakDifferential
        , testX25519Differential
        , testPropertyFuzz
        , testEdgeCases
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[DIFFERENTIAL] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- SHA-256 differential: NIST CAVP vectors
------------------------------------------------------------------------

testSHA256Differential :: IO Bool
testSHA256Differential = do
    let vectors :: [(String, ByteString)]
        vectors =
            [ ("abc",  strToBS "abc")
            , ("empty", BS.empty)
            , ("single-a", BS.singleton 0x61)
            , ("55-a", BS.replicate 55 0x61)
            , ("56-a", BS.replicate 56 0x61)
            , ("64-a", BS.replicate 64 0x61)
            , ("two-block", strToBS "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
            ]
    results <- forM vectors $ \(name, input) -> do
        let hsResult = SHA256.sha256 input
        ffiResult <- FFISHA256.sha256 input
        let match = hsResult == ffiResult
        if match
            then pure True
            else do
                putStrLn $ "  DIVERGENCE SHA-256 on " ++ name
                    ++ ": haskell=" ++ hexEncode hsResult
                    ++ " ffi=" ++ hexEncode ffiResult
                pure False
    assertEq "SHA-256 differential (NIST vectors)" True (and results)

------------------------------------------------------------------------
-- SHA-512 differential: NIST CAVP vectors
------------------------------------------------------------------------

testSHA512Differential :: IO Bool
testSHA512Differential = do
    let vectors :: [(String, ByteString)]
        vectors =
            [ ("abc", strToBS "abc")
            , ("empty", BS.empty)
            , ("single-a", BS.singleton 0x61)
            , ("two-block", strToBS "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")
            ]
    results <- forM vectors $ \(name, input) -> do
        let hsResult = SHA512.sha512 input
        ffiResult <- FFISHA512.sha512 input
        let match = hsResult == ffiResult
        if match
            then pure True
            else do
                putStrLn $ "  DIVERGENCE SHA-512 on " ++ name
                    ++ ": haskell=" ++ hexEncode hsResult
                    ++ " ffi=" ++ hexEncode ffiResult
                pure False
    assertEq "SHA-512 differential (NIST vectors)" True (and results)

------------------------------------------------------------------------
-- HMAC differential: RFC 4231 vectors
------------------------------------------------------------------------

testHMACDifferential :: IO Bool
testHMACDifferential = do
    -- RFC 4231 Test Case 1: key=0x0b*20, data="Hi There"
    let key1 = BS.replicate 20 0x0b
        msg1 = strToBS "Hi There"
    -- RFC 4231 Test Case 2: key="Jefe", data="what do ya want for nothing?"
    let key2 = strToBS "Jefe"
        msg2 = strToBS "what do ya want for nothing?"
    -- RFC 4231 Test Case 3: key=0xaa*20, data=0xdd*50
    let key3 = BS.replicate 20 0xaa
        msg3 = BS.replicate 50 0xdd

    results256 <- forM [(key1,msg1,"tc1"),(key2,msg2,"tc2"),(key3,msg3,"tc3")] $
        \(key, msg, name) -> do
            let hsResult = HMAC.hmacSHA256 key msg
            ffiResult <- FFIHMAC.hmacSHA256 key msg
            let match = hsResult == ffiResult
            if match
                then pure True
                else do
                    putStrLn $ "  DIVERGENCE HMAC-SHA256 on " ++ name
                    pure False

    results512 <- forM [(key1,msg1,"tc1"),(key2,msg2,"tc2"),(key3,msg3,"tc3")] $
        \(key, msg, name) -> do
            let hsResult = HMAC.hmacSHA512 key msg
            ffiResult <- FFIHMAC.hmacSHA512 key msg
            let match = hsResult == ffiResult
            if match
                then pure True
                else do
                    putStrLn $ "  DIVERGENCE HMAC-SHA512 on " ++ name
                    pure False

    assertEq "HMAC differential (RFC 4231 vectors)" True (and (results256 ++ results512))

------------------------------------------------------------------------
-- HKDF differential: RFC 5869 vectors
------------------------------------------------------------------------

testHKDFDifferential :: IO Bool
testHKDFDifferential = do
    -- RFC 5869 Test Case 1
    let ikm1  = BS.replicate 22 0x0b
        salt1 = BS.pack [0x00..0x0c]
        info1 = BS.pack [0xf0..0xf9]
        len1  = 42

    -- RFC 5869 Test Case 2 (longer inputs)
    let ikm2  = BS.pack [0x00..0x4f]
        salt2 = BS.pack [0x60..0xaf]
        info2 = BS.pack [0xb0..0xff]
        len2  = 82

    results <- forM [(ikm1,salt1,info1,len1,"tc1"),(ikm2,salt2,info2,len2,"tc2")] $
        \(ikm, salt, info, len, name) -> do
            -- RFC 5869 TC1/TC2 use HMAC-SHA-256; compare like-for-like
            let hsResult = HKDF.hkdfSHA256 salt ikm info len
            ffiResult <- FFIHKDF.hkdf salt ikm info len
            let match = hsResult == ffiResult
            if match
                then pure True
                else do
                    putStrLn $ "  DIVERGENCE HKDF on " ++ name
                    pure False

    assertEq "HKDF differential (RFC 5869 vectors)" True (and results)

------------------------------------------------------------------------
-- ChaCha20 differential: RFC 8439 vectors
------------------------------------------------------------------------

testChaCha20Differential :: IO Bool
testChaCha20Differential = do
    -- RFC 8439 Section 2.4.2 test vector
    let key   = hexDecode "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        nonce = hexDecode "000000000000004a00000000"
        counter = 1 :: Word32
        plaintext = strToBS "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."

    let hsResult = ChaCha20.chacha20Encrypt key nonce counter plaintext
    ffiResult <- FFIChaCha20.chacha20Encrypt key nonce counter plaintext
    assertEq "ChaCha20 differential (RFC 8439 encrypt)" hsResult ffiResult

------------------------------------------------------------------------
-- Poly1305 differential: RFC 8439 vectors
------------------------------------------------------------------------

testPoly1305Differential :: IO Bool
testPoly1305Differential = do
    -- RFC 8439 Section 2.5.2 test vector
    let key = hexDecode "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"
        msg = strToBS "Cryptographic Forum Research Group"

    let hsResult = Poly1305.poly1305 key msg
    ffiResult <- FFIPoly1305.poly1305 key msg
    assertEq "Poly1305 differential (RFC 8439)" hsResult ffiResult

------------------------------------------------------------------------
-- AES-256 differential: FIPS 197 vectors
------------------------------------------------------------------------

testAES256Differential :: IO Bool
testAES256Differential = do
    -- FIPS 197 Appendix C.3: AES-256 test vector
    let key   = hexDecode "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
        block = hexDecode "00112233445566778899aabbccddeeff"

    -- Encrypt
    let hsEnc = AES.aesEncrypt key block
    ffiEnc <- FFIAES256.aesEncrypt key block
    r1 <- assertEq "AES-256 differential encrypt (FIPS 197)" hsEnc ffiEnc

    -- Decrypt
    let hsDec = AES.aesDecrypt key hsEnc
    ffiDec <- FFIAES256.aesDecrypt key hsEnc
    r2 <- assertEq "AES-256 differential decrypt (FIPS 197)" hsDec ffiDec

    pure (r1 && r2)

------------------------------------------------------------------------
-- AES-256-GCM differential: Haskell oracle vs HACL* EverCrypt
-- Skipped gracefully on platforms without AES-NI (EverCrypt requirement).
------------------------------------------------------------------------

testGCMDifferential :: IO Bool
testGCMDifferential = do
    let key   = BS.replicate 32 0x00
        nonce = BS.replicate 12 0x00
    probeResult <- try (FFIGCM.gcmEncrypt key nonce BS.empty BS.empty)
                   :: IO (Either SomeException (ByteString, ByteString))
    case probeResult of
        Left ex -> do
            putStrLn $ "  SKIP AES-256-GCM differential (EverCrypt unavailable: " ++ show ex ++ ")"
            pure True
        Right _ -> do
            let vectors =
                    [ ("empty-pt-no-aad",   BS.empty,              BS.empty)
                    , ("16-byte-pt",        BS.replicate 16 0x42,  BS.empty)
                    , ("32-byte-pt",        BS.replicate 32 0xab,  BS.empty)
                    , ("with-aad",          BS.replicate 8  0x01,  BS.replicate 4 0xcd)
                    ]
            results <- forM vectors $ \(name, pt, aad) -> do
                let (hsCt, hsTag) = GCM.gcmEncrypt key nonce aad pt
                (ffiCt, ffiTag) <- FFIGCM.gcmEncrypt key nonce aad pt
                if hsCt /= ffiCt || hsTag /= ffiTag
                    then do
                        putStrLn $ "  DIVERGENCE GCM encrypt on " ++ name
                                ++ ": ct_match=" ++ show (hsCt == ffiCt)
                                ++ " tag_match=" ++ show (hsTag == ffiTag)
                        pure False
                    else do
                        let hsDecResult  = GCM.gcmDecrypt key nonce aad hsCt hsTag
                        ffiDecResult <- FFIGCM.gcmDecrypt key nonce aad ffiCt ffiTag
                        if hsDecResult == ffiDecResult
                            then pure True
                            else do
                                putStrLn $ "  DIVERGENCE GCM decrypt on " ++ name
                                pure False
            assertEq "AES-256-GCM differential (Haskell oracle vs EverCrypt)" True (and results)

------------------------------------------------------------------------
-- Keccak/SHA-3 differential: NIST SHA-3 vectors
------------------------------------------------------------------------

testKeccakDifferential :: IO Bool
testKeccakDifferential = do
    let vectors :: [(String, ByteString)]
        vectors =
            [ ("empty", BS.empty)
            , ("abc", strToBS "abc")
            , ("448-bit", strToBS "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
            ]

    results256 <- forM vectors $ \(name, input) -> do
        let hsResult = Keccak.sha3_256 input
        ffiResult <- FFIKeccak.sha3_256 input
        let match = hsResult == ffiResult
        if match
            then pure True
            else do
                putStrLn $ "  DIVERGENCE SHA3-256 on " ++ name
                pure False

    results512 <- forM vectors $ \(name, input) -> do
        let hsResult = Keccak.sha3_512 input
        ffiResult <- FFIKeccak.sha3_512 input
        let match = hsResult == ffiResult
        if match
            then pure True
            else do
                putStrLn $ "  DIVERGENCE SHA3-512 on " ++ name
                pure False

    assertEq "Keccak differential (SHA3-256/512)" True (and (results256 ++ results512))

------------------------------------------------------------------------
-- X25519 differential: RFC 7748 vectors
------------------------------------------------------------------------

testX25519Differential :: IO Bool
testX25519Differential = do
    -- RFC 7748 Section 6.1 test vector
    let scalar = hexDecode "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
        uCoord = hexDecode "e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c"

    let hsResult = X25519.x25519 scalar uCoord
    ffiResult <- FFIX25519.x25519 scalar uCoord
    assertEq "X25519 differential (RFC 7748)" hsResult ffiResult

------------------------------------------------------------------------
-- Property-based fuzzing: random inputs through SHA-256
------------------------------------------------------------------------

testPropertyFuzz :: IO Bool
testPropertyFuzz =
    checkPropertyIO "SHA-256 differential fuzz (1000 random inputs)" 1000 $ \g -> do
        let (len32, g') = nextWord32 g
            len = fromIntegral (len32 `mod` 256)
            (input, _) = nextBytes len g'
            hsResult = SHA256.sha256 input
        ffiResult <- FFISHA256.sha256 input
        pure (hsResult == ffiResult)

------------------------------------------------------------------------
-- Edge case / boundary differential tests
------------------------------------------------------------------------

testEdgeCases :: IO Bool
testEdgeCases = do
    putStrLn "  Edge case differential tests:"
    results <- sequence
        [ edgeCase "SHA-256 empty"
            (SHA256.sha256 BS.empty)
            (FFISHA256.sha256 BS.empty)
        , edgeCase "SHA-256 single byte"
            (SHA256.sha256 (BS.singleton 0x42))
            (FFISHA256.sha256 (BS.singleton 0x42))
        , edgeCase "SHA-256 55 bytes (pad boundary)"
            (SHA256.sha256 (BS.replicate 55 0x61))
            (FFISHA256.sha256 (BS.replicate 55 0x61))
        , edgeCase "SHA-256 56 bytes (pad boundary)"
            (SHA256.sha256 (BS.replicate 56 0x61))
            (FFISHA256.sha256 (BS.replicate 56 0x61))
        , edgeCase "SHA-256 64 bytes (block boundary)"
            (SHA256.sha256 (BS.replicate 64 0x61))
            (FFISHA256.sha256 (BS.replicate 64 0x61))
        , edgeCase "SHA-256 128 bytes (two blocks)"
            (SHA256.sha256 (BS.replicate 128 0x61))
            (FFISHA256.sha256 (BS.replicate 128 0x61))
        , edgeCase "SHA-512 empty"
            (SHA512.sha512 BS.empty)
            (FFISHA512.sha512 BS.empty)
        , edgeCase "HMAC-SHA256 empty key+msg"
            (HMAC.hmacSHA256 BS.empty BS.empty)
            (FFIHMAC.hmacSHA256 BS.empty BS.empty)
        , edgeCase "AES-256 roundtrip"
            (AES.aesDecrypt aesKey (AES.aesEncrypt aesKey aesBlock))
            (FFIAES256.aesDecrypt aesKey (AES.aesEncrypt aesKey aesBlock))
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  Edge cases: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)
  where
    aesKey   = BS.pack [0..31]
    aesBlock = BS.pack [0..15]

edgeCase :: String -> ByteString -> IO ByteString -> IO Bool
edgeCase label hsResult ffiAction = do
    ffiResult <- ffiAction
    let ok = hsResult == ffiResult
    putStrLn $ (if ok then "    PASS: " else "    FAIL: ") ++ label
    pure ok
