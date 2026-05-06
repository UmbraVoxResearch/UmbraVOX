-- | Tests for UmbraVox.Economics.Fees
module Test.Economics.Fees (runTests) where

import Test.Util (assertEq, checkProperty, nextWord32)
import UmbraVox.Economics.Fees
    ( messageFee
    , feeFloorDefault
    , feeCeilingDefault
    , clampFee
    )

runTests :: IO Bool
runTests = do
    putStrLn "Economics.Fees"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testFeeFloorDefault
        , testFeeCeilingDefault
        , testMessageFeeZeroBytes
        , testMessageFeeNegativeBytes
        , testMessageFee1Byte
        , testMessageFee1024Bytes
        , testMessageFee1025Bytes
        , testMessageFee2048Bytes
        , testMessageFeeLargeMessage
        , testClampFeeBelow
        , testClampFeeAbove
        , testClampFeeWithin
        , testClampFeeExactBounds
        , propMessageFeeNonNegative
        , propMessageFeeMonotonic
        , propMessageFeeStepAt1024
        ]
    pure (and results)

-- Constants
testFeeFloorDefault :: IO Bool
testFeeFloorDefault =
    assertEq "feeFloorDefault == 10" 10 feeFloorDefault

testFeeCeilingDefault :: IO Bool
testFeeCeilingDefault =
    assertEq "feeCeilingDefault == 10000" 10000 feeCeilingDefault

-- Edge cases
testMessageFeeZeroBytes :: IO Bool
testMessageFeeZeroBytes =
    assertEq "messageFee 0 == 0" 0 (messageFee 0)

testMessageFeeNegativeBytes :: IO Bool
testMessageFeeNegativeBytes =
    assertEq "messageFee (-1) == 0" 0 (messageFee (-1))

-- Formula: base_fee(10) * ceil(size/1024)
testMessageFee1Byte :: IO Bool
testMessageFee1Byte =
    assertEq "messageFee 1 == 10" 10 (messageFee 1)

testMessageFee1024Bytes :: IO Bool
testMessageFee1024Bytes =
    assertEq "messageFee 1024 == 10" 10 (messageFee 1024)

testMessageFee1025Bytes :: IO Bool
testMessageFee1025Bytes =
    assertEq "messageFee 1025 == 20" 20 (messageFee 1025)

testMessageFee2048Bytes :: IO Bool
testMessageFee2048Bytes =
    assertEq "messageFee 2048 == 20" 20 (messageFee 2048)

testMessageFeeLargeMessage :: IO Bool
testMessageFeeLargeMessage =
    -- 10 KB = 10 * 1024 bytes, ceil(10240/1024) = 10, fee = 10 * 10 = 100
    assertEq "messageFee 10240 == 100" 100 (messageFee 10240)

-- clampFee
testClampFeeBelow :: IO Bool
testClampFeeBelow =
    assertEq "clampFee 10 10000 5 == 10" 10 (clampFee 10 10000 5)

testClampFeeAbove :: IO Bool
testClampFeeAbove =
    assertEq "clampFee 10 10000 20000 == 10000" 10000 (clampFee 10 10000 20000)

testClampFeeWithin :: IO Bool
testClampFeeWithin =
    assertEq "clampFee 10 10000 500 == 500" 500 (clampFee 10 10000 500)

testClampFeeExactBounds :: IO Bool
testClampFeeExactBounds = do
    a <- assertEq "clampFee at floor" 10 (clampFee 10 10000 10)
    b <- assertEq "clampFee at ceiling" 10000 (clampFee 10 10000 10000)
    pure (a && b)

-- Property: messageFee is always >= 0
propMessageFeeNonNegative :: IO Bool
propMessageFeeNonNegative =
    checkProperty "messageFee >= 0 for random sizes" 200 $ \g ->
        let (w, _) = nextWord32 g
            sz = fromIntegral w :: Int
        in messageFee sz >= 0

-- Property: messageFee is monotonically non-decreasing for positive sizes
propMessageFeeMonotonic :: IO Bool
propMessageFeeMonotonic =
    checkProperty "messageFee monotonic" 200 $ \g ->
        let (w, _) = nextWord32 g
            sz = 1 + fromIntegral (w `mod` 100000) :: Int
        in messageFee sz <= messageFee (sz + 1024)

-- Property: fee increases at 1024-byte boundaries
propMessageFeeStepAt1024 :: IO Bool
propMessageFeeStepAt1024 =
    checkProperty "fee steps at 1024 boundaries" 100 $ \g ->
        let (w, _) = nextWord32 g
            n = 1 + fromIntegral (w `mod` 100) :: Int
            -- At exactly n*1024, fee should be 10*n
            -- At n*1024+1, fee should be 10*(n+1)
        in messageFee (n * 1024) == 10 * fromIntegral n
           && messageFee (n * 1024 + 1) == 10 * fromIntegral (n + 1)
