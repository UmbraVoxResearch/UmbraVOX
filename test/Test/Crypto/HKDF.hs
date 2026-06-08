-- SPDX-License-Identifier: Apache-2.0
-- | HKDF test suite: RFC 5869 KAT vectors + edge cases + property/fuzz tests.
module Test.Crypto.HKDF (runTests) where

import Control.Exception (IOException, catch, evaluate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.HKDF (hkdfSHA256Extract, hkdfSHA256Expand)
import qualified UmbraVox.Crypto.Generated.FFI.HKDF as HKDFFFI

runTests :: IO Bool
runTests = do
    putStrLn "[HKDF] Running RFC 5869 KAT vectors..."
    katResults <- mapM runKATVec katVectors
    putStrLn "[HKDF] Running edge case tests..."
    edgeResults <- sequence
        [ assertEq "Edge: output len 0" "" (hexEncode (hkdfSHA256Expand (BS.replicate 32 0x0b) BS.empty 0))
        , assertEq "Edge: output len 1" 1 (BS.length (hkdfSHA256Expand (BS.replicate 32 0x0b) BS.empty 1))
        , assertEq "Edge: empty salt+info" 32 (BS.length (hkdfSHA256Extract BS.empty (BS.replicate 22 0x0b)))
        ]
    putStrLn "[HKDF] Running property/fuzz tests..."
    propResults <- sequence
        [ checkProperty "output length matches request (1000 random)" 1000 propOutputLen
        , checkProperty "deterministic (1000 random)" 1000 propDeterminism
        ]
    -- Finding:       M27.6.3 — HKDF FFI wrappers passed unchecked len to C;
    --                C silently returns early on len > 8160, leaving uninitialized
    --                memory to be read back as "derived key material".
    -- Vulnerability: Oversized len requests return garbage bytes masquerading as keys.
    -- Fix:           Haskell-side bounds check raises IOException before allocaBytes.
    -- Verified:      These tests confirm ioError is raised for len > RFC 5869 maximum.
    putStrLn "[HKDF] Running FFI bounds rejection tests..."
    boundsResults <- sequence
        [ testThrows "FFI SHA-256 expand rejects len=8161"
            (HKDFFFI.hkdfSHA256Expand (BS.replicate 32 0x0b) BS.empty 8161)
        , testThrows "FFI SHA-256 expand rejects len=maxBound"
            (HKDFFFI.hkdfSHA256Expand (BS.replicate 32 0x0b) BS.empty maxBound)
        , testThrows "FFI hkdf rejects len=8161"
            (HKDFFFI.hkdf BS.empty (BS.replicate 22 0x0b) BS.empty 8161)
        , testThrows "FFI hkdfExpand (SHA-512) rejects len=16321"
            (HKDFFFI.hkdfExpand (BS.replicate 64 0x0b) BS.empty 16321)
        , testThrows "FFI hkdfSHA512 rejects len=16321"
            (HKDFFFI.hkdfSHA512 BS.empty (BS.replicate 22 0x0b) BS.empty 16321)
        , testNoThrow "FFI SHA-256 expand accepts len=8160 (max valid)"
            (HKDFFFI.hkdfSHA256Expand (BS.replicate 32 0x0b) BS.empty 8160)
        , testNoThrow "FFI hkdfExpand (SHA-512) accepts len=16320 (max valid)"
            (HKDFFFI.hkdfExpand (BS.replicate 64 0x0b) BS.empty 16320)
        ]
    let results = katResults ++ edgeResults ++ propResults ++ boundsResults
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[HKDF] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Assert that an IO action throws an IOException.
testThrows :: String -> IO a -> IO Bool
testThrows label action = do
    threw <- (action >> pure False)
        `catch` (\(_ :: IOException) -> pure True)
    if threw
        then putStrLn ("  PASS: " ++ label) >> pure True
        else putStrLn ("  FAIL: " ++ label ++ " (expected IOException, got success)") >> pure False

-- | Assert that an IO action succeeds without throwing.
testNoThrow :: String -> IO a -> IO Bool
testNoThrow label action = do
    ok <- (evaluate =<< (action >> pure True))
        `catch` (\(e :: IOException) ->
            putStrLn ("  FAIL: " ++ label ++ " (unexpected IOException: " ++ show e ++ ")")
            >> pure False)
    if ok
        then putStrLn ("  PASS: " ++ label) >> pure True
        else pure False

data HKDFVec = HKDFVec
    { hvName :: String, hvIKM :: ByteString, hvSalt :: ByteString
    , hvInfo :: ByteString, hvLen :: Int, hvPRK :: String, hvOKM :: String }

runKATVec :: HKDFVec -> IO Bool
runKATVec v = do
    let prk = hkdfSHA256Extract (hvSalt v) (hvIKM v)
        okm = hkdfSHA256Expand prk (hvInfo v) (hvLen v)
        prkOk = hexEncode prk == hvPRK v
        okmOk = hexEncode okm == hvOKM v
    if prkOk && okmOk
        then putStrLn ("  PASS: " ++ hvName v) >> pure True
        else do
            putStrLn $ "  FAIL: " ++ hvName v
            if not prkOk then putStrLn $ "    PRK got: " ++ hexEncode prk else pure ()
            if not okmOk then putStrLn $ "    OKM got: " ++ hexEncode okm else pure ()
            pure False

katVectors :: [HKDFVec]
katVectors =
    [ HKDFVec "RFC5869 TC1" (BS.replicate 22 0x0b)
        (hexDecode "000102030405060708090a0b0c") (hexDecode "f0f1f2f3f4f5f6f7f8f9") 42
        "077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5"
        "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865"
    , HKDFVec "RFC5869 TC2" (BS.pack [0x00..0x4f]) (BS.pack [0x60..0xaf]) (BS.pack [0xb0..0xff]) 82
        "06a6b88c5853361a06104c9ceb35b45cef760014904671014a193f40c15fc244"
        "b11e398dc80327a1c8e7f78c596a49344f012eda2d4efad8a050cc4c19afa97c59045a99cac7827271cb41c65e590e09da3275600c2f09b8367793a9aca3db71cc30c58179ec3e87c14c01d5c1f3434f1d87"
    , HKDFVec "RFC5869 TC3: empty salt/info" (BS.replicate 22 0x0b) BS.empty BS.empty 42
        "19ef24a32c717b167f33a91d6f648bdf96596776afdb6377ac434c1c293ccb04"
        "8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8"
    ]

propOutputLen :: PRNG -> Bool
propOutputLen g =
    let (g1, g2) = splitPRNG g
        (ikm, _) = nextBytesRange 1 64 g1
        (w, g3) = nextWord32 g2
        len = fromIntegral (w `mod` 256) :: Int  -- [0..255]
        prk = hkdfSHA256Extract BS.empty ikm
    in BS.length (hkdfSHA256Expand prk BS.empty len) == len
  where
    mod = Prelude.mod

propDeterminism :: PRNG -> Bool
propDeterminism g =
    let (g1, g2) = splitPRNG g
        (ikm, _) = nextBytesRange 1 64 g1
        (salt, _) = nextBytesRange 0 32 g2
        prk = hkdfSHA256Extract salt ikm
    in hkdfSHA256Expand prk BS.empty 42 == hkdfSHA256Expand prk BS.empty 42
