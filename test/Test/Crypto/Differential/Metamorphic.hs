{-# LANGUAGE OverloadedStrings #-}
-- | Metamorphic property tests — self-consistency checks that hold
-- without any external oracle.
--
-- Every property here is a relationship that must be true by
-- construction: roundtrips, commutativity, determinism, avalanche.
-- No reference implementation or test vector is consulted.
module Test.Crypto.Differential.Metamorphic
    ( metamorphicTests
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)
import Data.Bits (xor)
import Data.Char (digitToInt, isHexDigit)

import qualified UmbraVox.Crypto.SHA256 as SHA256
import qualified UmbraVox.Crypto.HMAC as HMAC
import qualified UmbraVox.Crypto.HKDF as HKDF
import qualified UmbraVox.Crypto.Curve25519 as X25519
import qualified UmbraVox.Crypto.Ed25519 as Ed25519
import qualified UmbraVox.Crypto.GCM as GCM
import qualified UmbraVox.Crypto.ChaChaPoly as ChaChaPoly

-- | Run all metamorphic property tests.
-- Returns True if all pass, False if any fail.
metamorphicTests :: IO Bool
metamorphicTests = do
    putStrLn "[Metamorphic] Running self-consistency property tests..."
    results <- sequence
        [ testAEADRoundtrip
        , testEd25519SignThenVerify
        , testEd25519WrongKeyRejection
        , testX25519Commutativity
        , testHMACDeterminism
        , testHKDFSaltSensitivity
        , testSHA256Determinism
        , testSHA256Avalanche
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Metamorphic] " ++ show passed ++ "/" ++ show total ++ " property suites passed."
    return (and results)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

hexToBS :: String -> ByteString
hexToBS [] = BS.empty
hexToBS [_] = BS.empty
hexToBS (a:b:rest)
    | isHexDigit a && isHexDigit b =
        BS.cons (fromIntegral $ digitToInt a * 16 + digitToInt b) (hexToBS rest)
    | otherwise = hexToBS rest

flipBit :: Int -> ByteString -> ByteString
flipBit idx bs
    | idx >= BS.length bs = bs
    | otherwise = BS.take idx bs `BS.append`
                  BS.singleton (BS.index bs idx `xor` 0x01) `BS.append`
                  BS.drop (idx + 1) bs

passFail :: String -> Bool -> IO Bool
passFail label True  = putStrLn ("  PASS: " ++ label) >> return True
passFail label False = putStrLn ("  FAIL: " ++ label) >> return False

------------------------------------------------------------------------
-- 1. AEAD roundtrip: decrypt(encrypt(m)) == m
------------------------------------------------------------------------

-- Five deterministic plaintexts of varying length
aeadPlaintexts :: [(String, ByteString)]
aeadPlaintexts =
    [ ("empty",      BS.empty)
    , ("short",      C8.pack "hello")
    , ("block-size", BS.replicate 16 0xAB)
    , ("unaligned",  BS.pack [0x01..0x1F])
    , ("longer",     BS.replicate 137 0x42)
    ]

testAEADRoundtrip :: IO Bool
testAEADRoundtrip = do
    putStrLn "  [AEAD-roundtrip] decrypt(key, nonce, aad, encrypt(key, nonce, aad, m)) == m"
    let key   = hexToBS "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20"
        nonce = hexToBS "000102030405060708090a0b"
        aad   = C8.pack "metamorphic-aead-test"
    gcmResults <- mapM (aeadRoundtripGCM key nonce aad) aeadPlaintexts
    ccpResults <- mapM (aeadRoundtripCCP key nonce aad) aeadPlaintexts
    return (and gcmResults && and ccpResults)

aeadRoundtripGCM :: ByteString -> ByteString -> ByteString
                 -> (String, ByteString) -> IO Bool
aeadRoundtripGCM key nonce aad (label, pt) = do
    let (ct, tag) = GCM.gcmEncrypt key nonce aad pt
    case GCM.gcmDecrypt key nonce aad ct tag of
        Nothing  -> passFail ("AES-GCM/" ++ label) False
        Just dec -> passFail ("AES-GCM/" ++ label) (dec == pt)

aeadRoundtripCCP :: ByteString -> ByteString -> ByteString
                 -> (String, ByteString) -> IO Bool
aeadRoundtripCCP key nonce aad (label, pt) = do
    let (ct, tag) = ChaChaPoly.chachaPolyEncrypt key nonce aad pt
    case ChaChaPoly.chachaPolyDecrypt key nonce aad ct tag of
        Nothing  -> passFail ("ChaCha20-Poly1305/" ++ label) False
        Just dec -> passFail ("ChaCha20-Poly1305/" ++ label) (dec == pt)

------------------------------------------------------------------------
-- 2. Ed25519 sign-then-verify: verify(pk, m, sign(sk, m)) == True
------------------------------------------------------------------------

ed25519SK :: ByteString
ed25519SK = hexToBS "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"

ed25519PK :: ByteString
ed25519PK = Ed25519.ed25519PublicKey ed25519SK

signVerifyMessages :: [(String, ByteString)]
signVerifyMessages =
    [ ("empty-msg",  BS.empty)
    , ("short-msg",  C8.pack "The quick brown fox")
    , ("binary-msg", BS.pack [0x00, 0xFF, 0x80, 0x7F, 0x01, 0xFE])
    ]

testEd25519SignThenVerify :: IO Bool
testEd25519SignThenVerify = do
    putStrLn "  [Ed25519-sign-verify] verify(pk, m, sign(sk, m)) == True"
    results <- mapM signAndVerify signVerifyMessages
    return (and results)
  where
    signAndVerify (label, msg) = do
        let sig = Ed25519.ed25519Sign ed25519SK msg
            ok  = Ed25519.ed25519Verify ed25519PK msg sig
        passFail ("Ed25519-sign-verify/" ++ label) ok

------------------------------------------------------------------------
-- 3. Ed25519 wrong-key rejection: verify(pk2, m, sign(sk1, m)) == False
------------------------------------------------------------------------

ed25519SK2 :: ByteString
ed25519SK2 = hexToBS "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"

ed25519PK2 :: ByteString
ed25519PK2 = Ed25519.ed25519PublicKey ed25519SK2

testEd25519WrongKeyRejection :: IO Bool
testEd25519WrongKeyRejection = do
    putStrLn "  [Ed25519-wrong-key] verify(pk2, m, sign(sk1, m)) == False"
    -- Sanity: pk1 != pk2
    let keysDistinct = ed25519PK /= ed25519PK2
    r0 <- passFail "Ed25519-wrong-key/keys-distinct" keysDistinct
    let msg = C8.pack "cross-key test message"
        sig = Ed25519.ed25519Sign ed25519SK msg
        rejected = not (Ed25519.ed25519Verify ed25519PK2 msg sig)
    r1 <- passFail "Ed25519-wrong-key/rejected" rejected
    return (r0 && r1)

------------------------------------------------------------------------
-- 4. X25519 commutativity: x25519(a, x25519(b, G)) == x25519(b, x25519(a, G))
------------------------------------------------------------------------

testX25519Commutativity :: IO Bool
testX25519Commutativity = do
    putStrLn "  [X25519-commutativity] x25519(a, x25519(b, G)) == x25519(b, x25519(a, G))"
    results <- mapM checkCommutativity x25519KeyPairs
    return (and results)
  where
    x25519KeyPairs :: [(String, ByteString, ByteString)]
    x25519KeyPairs =
        [ ( "pair1"
          , hexToBS "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
          , hexToBS "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
          )
        , ( "pair2"
          , hexToBS "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
          , hexToBS "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
          )
        ]

    checkCommutativity (label, skA, skB) = do
        let g = X25519.x25519Basepoint
        case (X25519.x25519 skA g, X25519.x25519 skB g) of
            (Just pubA, Just pubB) ->
                case (X25519.x25519 skA pubB, X25519.x25519 skB pubA) of
                    (Just sharedAB, Just sharedBA) ->
                        passFail ("X25519-commutativity/" ++ label)
                                 (sharedAB == sharedBA)
                    _ -> passFail ("X25519-commutativity/" ++ label ++ " (DH failed)") False
            _ -> passFail ("X25519-commutativity/" ++ label ++ " (keygen failed)") False

------------------------------------------------------------------------
-- 5. HMAC determinism: hmac(k, m) == hmac(k, m)
------------------------------------------------------------------------

testHMACDeterminism :: IO Bool
testHMACDeterminism = do
    putStrLn "  [HMAC-determinism] hmac(k, m) == hmac(k, m)"
    let key = hexToBS "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
        msg = C8.pack "determinism check"
        h1  = HMAC.hmacSHA256 key msg
        h2  = HMAC.hmacSHA256 key msg
    passFail "HMAC-determinism" (h1 == h2)

------------------------------------------------------------------------
-- 6. HKDF output changes when salt changes
------------------------------------------------------------------------

testHKDFSaltSensitivity :: IO Bool
testHKDFSaltSensitivity = do
    putStrLn "  [HKDF-salt-sensitivity] hkdf(salt1, ikm, info, len) != hkdf(salt2, ikm, info, len)"
    let ikm   = hexToBS "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
        salt1 = hexToBS "000102030405060708090a0b0c"
        salt2 = hexToBS "ff0102030405060708090a0b0c"
        info  = hexToBS "f0f1f2f3f4f5f6f7f8f9"
        len   = 32
        out1  = HKDF.hkdfSHA256 salt1 ikm info len
        out2  = HKDF.hkdfSHA256 salt2 ikm info len
    passFail "HKDF-salt-sensitivity" (out1 /= out2)

------------------------------------------------------------------------
-- 7. SHA-256 determinism: sha256(m) == sha256(m)
------------------------------------------------------------------------

testSHA256Determinism :: IO Bool
testSHA256Determinism = do
    putStrLn "  [SHA-256-determinism] sha256(m) == sha256(m)"
    let msg = C8.pack "deterministic hashing"
        h1  = SHA256.sha256 msg
        h2  = SHA256.sha256 msg
    passFail "SHA-256-determinism" (h1 == h2)

------------------------------------------------------------------------
-- 8. SHA-256 avalanche: sha256(m) != sha256(m') where m' differs by 1 bit
------------------------------------------------------------------------

testSHA256Avalanche :: IO Bool
testSHA256Avalanche = do
    putStrLn "  [SHA-256-avalanche] sha256(m) != sha256(flipBit(m))"
    let msg  = C8.pack "avalanche effect test"
        msg' = flipBit 0 msg
        h1   = SHA256.sha256 msg
        h2   = SHA256.sha256 msg'
    r1 <- passFail "SHA-256-avalanche/inputs-differ" (msg /= msg')
    r2 <- passFail "SHA-256-avalanche/hashes-differ" (h1 /= h2)
    return (r1 && r2)
