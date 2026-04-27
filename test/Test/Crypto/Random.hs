-- | Random module test suite: CSPRNG output quality, readEntropy sanity,
-- and randomBytes basic contract tests.
module Test.Crypto.Random (runTests) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Util
import UmbraVox.Crypto.Random (randomBytes, readEntropy)

runTests :: IO Bool
runTests = do
    putStrLn "Test.Crypto.Random"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testReadEntropyLength
        , testReadEntropyNonZero
        , testReadEntropyDistinct
        , testReadEntropyZeroLength
        , testRandomBytesLength
        , testRandomBytesNonZero
        , testRandomBytesDistinct
        , testRandomBytesZeroLength
        , testRandomBytesNegativeLength
        , testRandomBytesSmall
        , testRandomBytesLarge
        , testRandomBytesEntropyQuality
        ]
    pure (and results)

-- | readEntropy should return exactly the requested number of bytes.
testReadEntropyLength :: IO Bool
testReadEntropyLength = do
    e16 <- readEntropy 16
    e32 <- readEntropy 32
    e64 <- readEntropy 64
    r1 <- assertEq "readEntropy 16 -> 16 bytes" 16 (BS.length e16)
    r2 <- assertEq "readEntropy 32 -> 32 bytes" 32 (BS.length e32)
    r3 <- assertEq "readEntropy 64 -> 64 bytes" 64 (BS.length e64)
    pure (r1 && r2 && r3)

-- | readEntropy output should not be all zeros (vanishingly unlikely).
testReadEntropyNonZero :: IO Bool
testReadEntropyNonZero = do
    e <- readEntropy 32
    assertEq "readEntropy 32 not all zeros" True (e /= BS.replicate 32 0)

-- | Two calls to readEntropy should produce different output.
testReadEntropyDistinct :: IO Bool
testReadEntropyDistinct = do
    e1 <- readEntropy 32
    e2 <- readEntropy 32
    assertEq "readEntropy produces distinct outputs" True (e1 /= e2)

-- | readEntropy 0 should return empty.
testReadEntropyZeroLength :: IO Bool
testReadEntropyZeroLength = do
    e <- readEntropy 0
    assertEq "readEntropy 0 -> empty" BS.empty e

-- | randomBytes should return exactly the requested number of bytes.
testRandomBytesLength :: IO Bool
testRandomBytesLength = do
    r16 <- randomBytes 16
    r32 <- randomBytes 32
    r64 <- randomBytes 64
    r1 <- assertEq "randomBytes 16 -> 16 bytes" 16 (BS.length r16)
    r2 <- assertEq "randomBytes 32 -> 32 bytes" 32 (BS.length r32)
    r3 <- assertEq "randomBytes 64 -> 64 bytes" 64 (BS.length r64)
    pure (r1 && r2 && r3)

-- | randomBytes output should not be all zeros.
testRandomBytesNonZero :: IO Bool
testRandomBytesNonZero = do
    r <- randomBytes 32
    assertEq "randomBytes 32 not all zeros" True (r /= BS.replicate 32 0)

-- | Two calls to randomBytes should produce different output.
testRandomBytesDistinct :: IO Bool
testRandomBytesDistinct = do
    r1 <- randomBytes 32
    r2 <- randomBytes 32
    assertEq "randomBytes produces distinct outputs" True (r1 /= r2)

-- | randomBytes 0 should return empty.
testRandomBytesZeroLength :: IO Bool
testRandomBytesZeroLength = do
    r <- randomBytes 0
    assertEq "randomBytes 0 -> empty" BS.empty r

-- | randomBytes with negative length should return empty.
testRandomBytesNegativeLength :: IO Bool
testRandomBytesNegativeLength = do
    r <- randomBytes (-5)
    assertEq "randomBytes (-5) -> empty" BS.empty r

-- | randomBytes for small sizes (1, 2, 3 bytes).
testRandomBytesSmall :: IO Bool
testRandomBytesSmall = do
    r1 <- randomBytes 1
    r2 <- randomBytes 2
    r3 <- randomBytes 3
    a <- assertEq "randomBytes 1 -> 1 byte" 1 (BS.length r1)
    b <- assertEq "randomBytes 2 -> 2 bytes" 2 (BS.length r2)
    c <- assertEq "randomBytes 3 -> 3 bytes" 3 (BS.length r3)
    pure (a && b && c)

-- | randomBytes for a larger request spanning multiple ChaCha20 blocks.
testRandomBytesLarge :: IO Bool
testRandomBytesLarge = do
    r <- randomBytes 1024
    assertEq "randomBytes 1024 -> 1024 bytes" 1024 (BS.length r)

-- | Basic entropy quality: check that at least 8 distinct byte values appear
-- in a 256-byte sample (extremely conservative check).
testRandomBytesEntropyQuality :: IO Bool
testRandomBytesEntropyQuality = do
    r <- randomBytes 256
    let bytes = BS.unpack r
        distinctCount = length $ filter id [ any (== w) bytes | w <- [0..255 :: Word8] ]
    -- With 256 random bytes, we expect ~163 distinct values on average.
    -- Requiring at least 8 is extremely conservative.
    assertEq "randomBytes 256 has >= 8 distinct byte values" True (distinctCount >= 8)
