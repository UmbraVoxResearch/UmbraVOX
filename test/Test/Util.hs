-- SPDX-License-Identifier: Apache-2.0
-- | Shared test utilities: deterministic PRNG, property-based test runner,
-- hex encoding/decoding, and common helpers.
--
-- The PRNG uses a 64-bit LCG (Knuth constants) with seed=42 for
-- reproducible property tests per attic/doc-legacy-2026-04-28/14-code-generation.md.
module Test.Util
    ( -- * Hex helpers
      hexEncode
    , hexDecode
    , strToBS
      -- * Deterministic PRNG
    , PRNG
    , mkPRNG
    , nextWord32
    , nextWord8
    , nextBytes
    , nextBytesRange
    , splitPRNG
      -- * Property test runner
    , checkProperty
    , checkPropertyIO
      -- * Test helpers
    , assertEq
    ) where

import Data.Bits ((.&.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, intToDigit)
import Data.Word (Word8, Word32, Word64)

------------------------------------------------------------------------
-- Hex encoding / decoding (consolidated from all test modules)
------------------------------------------------------------------------

hexEncode :: ByteString -> String
hexEncode = concatMap byteToHex . BS.unpack
  where
    byteToHex b = [intToDigit (fromIntegral (b `shiftR` 4)),
                   intToDigit (fromIntegral (b .&. 0x0f))]

hexDecode :: String -> ByteString
hexDecode [] = BS.empty
hexDecode [_] = BS.empty
hexDecode (h:l:r) = BS.cons (fromIntegral (digitToInt h * 16 + digitToInt l) :: Word8)
                            (hexDecode r)

strToBS :: String -> ByteString
strToBS = BS.pack . map (fromIntegral . fromEnum)

------------------------------------------------------------------------
-- Deterministic PRNG (64-bit LCG, Knuth constants)
------------------------------------------------------------------------

-- | Deterministic pseudo-random number generator.
-- Uses a 64-bit linear congruential generator with full period.
newtype PRNG = PRNG Word64

-- | Create a PRNG with the given seed. Default seed is 42.
mkPRNG :: Word64 -> PRNG
mkPRNG = PRNG

-- | Generate a random Word32 and advance the state.
nextWord32 :: PRNG -> (Word32, PRNG)
nextWord32 (PRNG s) =
    let !s' = s * 6364136223846793005 + 1442695040888963407
    in (fromIntegral (s' `shiftR` 32), PRNG s')

-- | Generate a random Word8 and advance the state.
nextWord8 :: PRNG -> (Word8, PRNG)
nextWord8 g = let (w, g') = nextWord32 g in (fromIntegral w, g')

-- | Generate a random ByteString of exactly @n@ bytes.
nextBytes :: Int -> PRNG -> (ByteString, PRNG)
nextBytes n g0 = go n g0 []
  where
    go 0 g acc = (BS.pack (reverse acc), g)
    go !remaining g acc =
        let (!w, !g') = nextWord32 g
            !b0 = fromIntegral w :: Word8
            !b1 = fromIntegral (w `shiftR` 8) :: Word8
            !b2 = fromIntegral (w `shiftR` 16) :: Word8
            !b3 = fromIntegral (w `shiftR` 24) :: Word8
            bytes = [b0, b1, b2, b3]
            take' = take (min 4 remaining) bytes
        in go (remaining - length take') g' (reverse take' ++ acc)

-- | Generate a random ByteString with length in @[lo, hi]@.
nextBytesRange :: Int -> Int -> PRNG -> (ByteString, PRNG)
nextBytesRange lo hi g =
    let (!w, !g') = nextWord32 g
        !len = lo + fromIntegral (w `mod` fromIntegral (hi - lo + 1))
    in nextBytes len g'

-- | Split a PRNG into two independent generators.
splitPRNG :: PRNG -> (PRNG, PRNG)
splitPRNG (PRNG s) =
    let !s1 = s * 6364136223846793005 + 1442695040888963407
        !s2 = s1 * 6364136223846793005 + 1442695040888963407
    in (PRNG (s1 `xor` 0xdeadbeef), PRNG (s2 `xor` 0xcafebabe))

------------------------------------------------------------------------
-- Property test runner
------------------------------------------------------------------------

-- | Run a pure property test @n@ times with deterministic PRNG (seed=42).
-- Prints pass/fail with iteration count.
checkProperty :: String -> Int -> (PRNG -> Bool) -> IO Bool
checkProperty name n prop = go 0 (mkPRNG 42)
  where
    go !i !g
        | i >= n = do
            putStrLn $ "  PASS: " ++ name ++ " (" ++ show n ++ " iterations)"
            pure True
        | prop g = let (_, g') = nextWord32 g in go (i + 1) g'
        | otherwise = do
            putStrLn $ "  FAIL: " ++ name ++ " (at iteration " ++ show i ++ ")"
            pure False

-- | Run an IO property test @n@ times with deterministic PRNG (seed=42).
checkPropertyIO :: String -> Int -> (PRNG -> IO Bool) -> IO Bool
checkPropertyIO name n prop = go 0 (mkPRNG 42)
  where
    go !i !g
        | i >= n = do
            putStrLn $ "  PASS: " ++ name ++ " (" ++ show n ++ " iterations)"
            pure True
        | otherwise = do
            ok <- prop g
            if ok
                then let (_, g') = nextWord32 g in go (i + 1) g'
                else do
                    putStrLn $ "  FAIL: " ++ name ++ " (at iteration " ++ show i ++ ")"
                    pure False

------------------------------------------------------------------------
-- Test assertion helper
------------------------------------------------------------------------

-- | Assert two values are equal, printing a message on failure.
assertEq :: (Eq a, Show a) => String -> a -> a -> IO Bool
assertEq name expected got =
    if expected == got
        then putStrLn ("  PASS: " ++ name) >> pure True
        else do
            putStrLn $ "  FAIL: " ++ name
            putStrLn $ "    expected: " ++ show expected
            putStrLn $ "    got:      " ++ show got
            pure False
