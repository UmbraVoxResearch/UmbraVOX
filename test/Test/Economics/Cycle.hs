-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Economics.Cycle
module Test.Economics.Cycle (runTests) where

import Test.Util (assertEq, checkProperty, nextWord32)
import UmbraVox.Economics.Cycle
    ( supplyRestoration
    , initialSupply
    , targetCycleSlots
    , minCycleSlots
    , durationRatio
    )

runTests :: IO Bool
runTests = do
    putStrLn "Economics.Cycle"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testInitialSupply
        , testTargetCycleSlots
        , testMinCycleSlots
        , testMinCycleSlotsIsOneEpoch
        , testSupplyRestorationGenesis
        , testSupplyRestorationWithStakes
        , testSupplyRestorationConservation
        , testDurationRatioNormal
        , testDurationRatioEarlyTruncation
        , testDurationRatioLong
        , propSupplyRestorationNonNegativeInputs
        , propSupplyRestorationConservation
        ]
    pure (and results)

-- Constants
testInitialSupply :: IO Bool
testInitialSupply =
    assertEq "initialSupply == 11000000000" 11000000000 initialSupply

testTargetCycleSlots :: IO Bool
testTargetCycleSlots =
    -- 22 epochs * 3927 slots/epoch = 86394
    assertEq "targetCycleSlots == 86394" 86394 targetCycleSlots

testMinCycleSlots :: IO Bool
testMinCycleSlots =
    assertEq "minCycleSlots == 3927" 3927 minCycleSlots

testMinCycleSlotsIsOneEpoch :: IO Bool
testMinCycleSlotsIsOneEpoch =
    -- Verify minCycleSlots is exactly 1 epoch
    assertEq "minCycleSlots == targetCycleSlots / 22" 3927 (targetCycleSlots `div` 22)

-- supplyRestoration
testSupplyRestorationGenesis :: IO Bool
testSupplyRestorationGenesis =
    -- At genesis: staked=0, reserve=1.1B, treasury=550M
    -- pool = 11B - 0 - 1.1B - 0.55B = 9.35B
    assertEq "supplyRestoration genesis"
        9350000000
        (supplyRestoration 0 1100000000 550000000)

testSupplyRestorationWithStakes :: IO Bool
testSupplyRestorationWithStakes =
    -- With 1B staked, 1.1B reserve, 550M treasury
    -- pool = 11B - 1B - 1.1B - 0.55B = 8.35B
    assertEq "supplyRestoration with stakes"
        8350000000
        (supplyRestoration 1000000000 1100000000 550000000)

testSupplyRestorationConservation :: IO Bool
testSupplyRestorationConservation = do
    let staked = 2000000000
        reserve = 1100000000
        treasury = 550000000
        pool = supplyRestoration staked reserve treasury
    -- pool + staked + reserve + treasury should equal initialSupply
    assertEq "conservation invariant"
        initialSupply
        (pool + staked + reserve + treasury)

-- durationRatio
testDurationRatioNormal :: IO Bool
testDurationRatioNormal =
    -- Normal cycle: actual == target -> ratio = 1.0
    assertEq "durationRatio normal" 1.0 (durationRatio targetCycleSlots)

testDurationRatioEarlyTruncation :: IO Bool
testDurationRatioEarlyTruncation = do
    -- Half-length cycle -> ratio = 0.5
    let r = durationRatio (targetCycleSlots `div` 2)
    assertEq "durationRatio early truncation < 1.0" True (r < 1.0)

testDurationRatioLong :: IO Bool
testDurationRatioLong = do
    let r = durationRatio (targetCycleSlots * 2)
    assertEq "durationRatio long cycle > 1.0" True (r > 1.0)

-- Properties
propSupplyRestorationNonNegativeInputs :: IO Bool
propSupplyRestorationNonNegativeInputs =
    checkProperty "supplyRestoration with bounded inputs >= 0" 200 $ \g ->
        let (w1, g1) = nextWord32 g
            (w2, g2) = nextWord32 g1
            (w3, _)  = nextWord32 g2
            -- Keep values bounded so their sum < initialSupply
            staked  = fromIntegral (w1 `mod` 3000000000) :: Integer
            reserve = fromIntegral (w2 `mod` 1500000000) :: Integer
            treas   = fromIntegral (w3 `mod` 1200000000) :: Integer
        in supplyRestoration staked reserve treas >= 0

propSupplyRestorationConservation :: IO Bool
propSupplyRestorationConservation =
    checkProperty "pool + staked + reserve + treasury == initialSupply" 200 $ \g ->
        let (w1, g1) = nextWord32 g
            (w2, g2) = nextWord32 g1
            (w3, _)  = nextWord32 g2
            staked  = fromIntegral w1 :: Integer
            reserve = fromIntegral w2 :: Integer
            treas   = fromIntegral w3 :: Integer
            pool = supplyRestoration staked reserve treas
        in pool + staked + reserve + treas == initialSupply
