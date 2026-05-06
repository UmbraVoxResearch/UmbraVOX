-- | Tests for UmbraVox.Economics.Onboarding
module Test.Economics.Onboarding (runTests) where

import Test.Util (assertEq, checkProperty, nextWord32)
import UmbraVox.Economics.Onboarding
    ( faucetGrant
    , quadraticBondingStake
    , baseStake
    , inactiveReclaimThreshold
    )

runTests :: IO Bool
runTests = do
    putStrLn "Economics.Onboarding"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testBaseStake
        , testInactiveReclaimThreshold
        , testFaucetGrantNormal
        , testFaucetGrantCapped
        , testFaucetGrantLowReserve
        , testFaucetGrantZeroCapacity
        , testFaucetGrantNegativeCapacity
        , testQuadraticBonding1st
        , testQuadraticBonding2nd
        , testQuadraticBonding3rd
        , testQuadraticBonding10th
        , testQuadraticBondingZero
        , testQuadraticBondingNegative
        , propQuadraticBondingIncreasing
        , propFaucetGrantCapped
        , propQuadraticBondingFormula
        ]
    pure (and results)

-- Constants
testBaseStake :: IO Bool
testBaseStake =
    assertEq "baseStake == 50000" 50000 baseStake

testInactiveReclaimThreshold :: IO Bool
testInactiveReclaimThreshold =
    assertEq "inactiveReclaimThreshold == 5" 5 inactiveReclaimThreshold

-- faucetGrant
testFaucetGrantNormal :: IO Bool
testFaucetGrantNormal =
    -- reserve=1100000000, capacity=110000 -> 1100000000/110000 = 10000
    assertEq "faucetGrant normal" 10000 (faucetGrant 1100000000 110000)

testFaucetGrantCapped :: IO Bool
testFaucetGrantCapped =
    -- reserve=1100000000, capacity=1 -> min(10000, 1100000000) = 10000
    assertEq "faucetGrant capped at 10000" 10000 (faucetGrant 1100000000 1)

testFaucetGrantLowReserve :: IO Bool
testFaucetGrantLowReserve =
    -- reserve=50000, capacity=10 -> min(10000, 5000) = 5000
    assertEq "faucetGrant low reserve" 5000 (faucetGrant 50000 10)

testFaucetGrantZeroCapacity :: IO Bool
testFaucetGrantZeroCapacity =
    assertEq "faucetGrant zero capacity" 0 (faucetGrant 1000000 0)

testFaucetGrantNegativeCapacity :: IO Bool
testFaucetGrantNegativeCapacity =
    assertEq "faucetGrant negative capacity" 0 (faucetGrant 1000000 (-1))

-- quadraticBondingStake: 50000 * n^2
testQuadraticBonding1st :: IO Bool
testQuadraticBonding1st =
    assertEq "quadraticBondingStake 1 == 50000" 50000 (quadraticBondingStake 1)

testQuadraticBonding2nd :: IO Bool
testQuadraticBonding2nd =
    -- 50000 * 4 = 200000
    assertEq "quadraticBondingStake 2 == 200000" 200000 (quadraticBondingStake 2)

testQuadraticBonding3rd :: IO Bool
testQuadraticBonding3rd =
    -- 50000 * 9 = 450000
    assertEq "quadraticBondingStake 3 == 450000" 450000 (quadraticBondingStake 3)

testQuadraticBonding10th :: IO Bool
testQuadraticBonding10th =
    -- 50000 * 100 = 5000000
    assertEq "quadraticBondingStake 10 == 5000000" 5000000 (quadraticBondingStake 10)

testQuadraticBondingZero :: IO Bool
testQuadraticBondingZero =
    assertEq "quadraticBondingStake 0 == 0" 0 (quadraticBondingStake 0)

testQuadraticBondingNegative :: IO Bool
testQuadraticBondingNegative =
    assertEq "quadraticBondingStake (-1) == 0" 0 (quadraticBondingStake (-1))

-- Properties
propQuadraticBondingIncreasing :: IO Bool
propQuadraticBondingIncreasing =
    checkProperty "quadraticBonding strictly increasing for n >= 1" 200 $ \g ->
        let (w, _) = nextWord32 g
            n = 1 + fromIntegral (w `mod` 1000) :: Int
        in quadraticBondingStake (n + 1) > quadraticBondingStake n

propFaucetGrantCapped :: IO Bool
propFaucetGrantCapped =
    checkProperty "faucetGrant <= 10000" 200 $ \g ->
        let (w1, g1) = nextWord32 g
            (w2, _)  = nextWord32 g1
            reserve  = fromIntegral w1 :: Integer
            capacity = 1 + fromIntegral (w2 `mod` 10000) :: Integer
        in faucetGrant reserve capacity <= 10000

propQuadraticBondingFormula :: IO Bool
propQuadraticBondingFormula =
    checkProperty "quadraticBondingStake n == 50000 * n^2" 200 $ \g ->
        let (w, _) = nextWord32 g
            n = 1 + fromIntegral (w `mod` 1000) :: Int
        in quadraticBondingStake n == 50000 * fromIntegral (n * n)
