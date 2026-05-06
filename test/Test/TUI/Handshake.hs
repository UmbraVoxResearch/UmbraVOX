-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.TUI.Handshake pure functions:
-- bsSlice, putW32BE, getW32BE, fingerprint, serializeBundle, deserializeBundle
module Test.TUI.Handshake (runTests) where

import qualified Data.ByteString as BS
import Data.Word (Word32)
import Test.Util (assertEq, checkProperty, checkPropertyIO, PRNG, nextWord32, nextBytes)
import UmbraVox.TUI.Handshake (bsSlice, putW32BE, getW32BE, fingerprint,
    serializeBundle, deserializeBundle)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..), generateIdentityKey)
import UmbraVox.Crypto.MLKEM (MLKEMEncapKey(..))
import UmbraVox.Crypto.Signal.PQXDH (PQPreKeyBundle(..))

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Handshake"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testPutW32BEZero
        , testPutW32BEOne
        , testPutW32BEMax
        , testPutW32BEKnown
        , testGetW32BEZero
        , testGetW32BEOne
        , testGetW32BEMax
        , testGetW32BEKnown
        , testBsSliceBasic
        , testBsSliceStart
        , testBsSliceEnd
        , testBsSliceEmpty
        , testBsSliceBeyond
        , testFingerprintZero
        , testFingerprintShort
        , testFingerprintFormat
        , testDeserializeBundleTooShort
        , testDeserializeBundleEmpty
        , propPutGetW32BERoundtrip
        , propBsSliceLength
        , propSerializeDeserializeRoundtrip
        ]
    pure (and results)

-- putW32BE / getW32BE ---------------------------------------------------------

testPutW32BEZero :: IO Bool
testPutW32BEZero = assertEq "putW32BE 0" (BS.pack [0,0,0,0]) (putW32BE 0)

testPutW32BEOne :: IO Bool
testPutW32BEOne = assertEq "putW32BE 1" (BS.pack [0,0,0,1]) (putW32BE 1)

testPutW32BEMax :: IO Bool
testPutW32BEMax = assertEq "putW32BE maxBound" (BS.pack [0xff,0xff,0xff,0xff]) (putW32BE maxBound)

testPutW32BEKnown :: IO Bool
testPutW32BEKnown = assertEq "putW32BE 0x01020304" (BS.pack [1,2,3,4]) (putW32BE 0x01020304)

testGetW32BEZero :: IO Bool
testGetW32BEZero = assertEq "getW32BE 0" (0 :: Word32) (getW32BE (BS.pack [0,0,0,0]))

testGetW32BEOne :: IO Bool
testGetW32BEOne = assertEq "getW32BE 1" (1 :: Word32) (getW32BE (BS.pack [0,0,0,1]))

testGetW32BEMax :: IO Bool
testGetW32BEMax = assertEq "getW32BE max" (maxBound :: Word32) (getW32BE (BS.pack [0xff,0xff,0xff,0xff]))

testGetW32BEKnown :: IO Bool
testGetW32BEKnown = assertEq "getW32BE 0x01020304" (0x01020304 :: Word32) (getW32BE (BS.pack [1,2,3,4]))

-- bsSlice ---------------------------------------------------------------------

testBsSliceBasic :: IO Bool
testBsSliceBasic = assertEq "bsSlice middle" (BS.pack [3,4,5])
    (bsSlice 2 3 (BS.pack [1,2,3,4,5,6]))

testBsSliceStart :: IO Bool
testBsSliceStart = assertEq "bsSlice from start" (BS.pack [1,2])
    (bsSlice 0 2 (BS.pack [1,2,3,4]))

testBsSliceEnd :: IO Bool
testBsSliceEnd = assertEq "bsSlice at end" (BS.pack [4])
    (bsSlice 3 1 (BS.pack [1,2,3,4]))

testBsSliceEmpty :: IO Bool
testBsSliceEmpty = assertEq "bsSlice zero len" BS.empty
    (bsSlice 2 0 (BS.pack [1,2,3]))

testBsSliceBeyond :: IO Bool
testBsSliceBeyond = assertEq "bsSlice beyond end" (BS.pack [3])
    (bsSlice 2 5 (BS.pack [1,2,3]))

-- fingerprint -----------------------------------------------------------------

testFingerprintZero :: IO Bool
testFingerprintZero = do
    let bs = BS.pack [0,0,0,0,0,0,0,0]
        fp = fingerprint bs
    assertEq "fingerprint zeros" "00:00:00:00:00:00:00:00:" fp

testFingerprintShort :: IO Bool
testFingerprintShort = do
    -- fingerprint takes first 8 bytes
    let bs = BS.pack [0xab, 0xcd]
        fp = fingerprint bs
    -- Should only fingerprint available bytes (2 bytes)
    assertEq "fingerprint short" "ab:cd:" fp

testFingerprintFormat :: IO Bool
testFingerprintFormat = do
    let bs = BS.pack [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xff]
        fp = fingerprint bs
    -- Takes first 8 bytes, each byte becomes "xx:"
    assertEq "fingerprint format" "01:23:45:67:89:ab:cd:ef:" fp

-- deserializeBundle -----------------------------------------------------------

testDeserializeBundleTooShort :: IO Bool
testDeserializeBundleTooShort = do
    let result = deserializeBundle (BS.replicate 100 0)
    case result of
        Nothing -> putStrLn "  PASS: deserializeBundle too short" >> pure True
        Just _  -> putStrLn "  FAIL: deserializeBundle too short (expected Nothing)" >> pure False

testDeserializeBundleEmpty :: IO Bool
testDeserializeBundleEmpty = do
    let result = deserializeBundle BS.empty
    case result of
        Nothing -> putStrLn "  PASS: deserializeBundle empty" >> pure True
        Just _  -> putStrLn "  FAIL: deserializeBundle empty (expected Nothing)" >> pure False

-- Property tests --------------------------------------------------------------

propPutGetW32BERoundtrip :: IO Bool
propPutGetW32BERoundtrip = checkProperty "putW32BE/getW32BE roundtrip" 200 $ \g ->
    let (w, _) = nextWord32 g
    in getW32BE (putW32BE w) == w

propBsSliceLength :: IO Bool
propBsSliceLength = checkProperty "bsSlice length <= requested" 100 $ \g ->
    let (w1, g1) = nextWord32 g
        (w2, g2) = nextWord32 g1
        (w3, _)  = nextWord32 g2
        totalLen = 1 + fromIntegral (w1 `mod` 50) :: Int
        off = fromIntegral (w2 `mod` fromIntegral totalLen) :: Int
        len = fromIntegral (w3 `mod` 50) :: Int
        bs = BS.replicate totalLen 0x42
        result = bsSlice off len bs
    in BS.length result <= len && BS.length result <= max 0 (totalLen - off)

propSerializeDeserializeRoundtrip :: IO Bool
propSerializeDeserializeRoundtrip = checkPropertyIO "serialize/deserialize bundle roundtrip" 20 $ \g -> do
    -- Generate deterministic key material
    let (edSec, g1) = nextBytes 32 g
        (xSec, g2)  = nextBytes 32 g1
        (spkPub, g3) = nextBytes 32 g2
        (spkSig, g4) = nextBytes 64 g3
        (pqKey, _)   = nextBytes 1184 g4  -- ML-KEM-768 encap key size
        ik = generateIdentityKey edSec xSec
        blob = serializeBundle ik spkPub spkSig (MLKEMEncapKey pqKey) Nothing
    case deserializeBundle blob of
        Nothing -> pure False
        Just bundle -> pure $
            pqpkbIdentityKey bundle == ikX25519Public ik
            && pqpkbIdentityEd25519 bundle == ikEd25519Public ik
            && pqpkbSignedPreKey bundle == spkPub
            && pqpkbSPKSignature bundle == spkSig
            && pqpkbOneTimePreKey bundle == Nothing
