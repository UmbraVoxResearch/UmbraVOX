-- | Tests for UmbraVox.Consensus.Types
--
-- Tests the newtype wrappers SlotNo, EpochNo, CycleNo, BlockNo
-- for correct Eq, Ord, Show instances and construction.
module Test.Consensus.Types (runTests) where

import Data.Word (Word64)

import Test.Util
import UmbraVox.Consensus.Types (SlotNo(..), EpochNo(..), CycleNo(..), BlockNo(..))

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.Types"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testSlotNoEq
        , testSlotNoOrd
        , testSlotNoShow
        , testEpochNoEq
        , testEpochNoOrd
        , testEpochNoShow
        , testCycleNoEq
        , testCycleNoOrd
        , testCycleNoShow
        , testBlockNoEq
        , testBlockNoOrd
        , testBlockNoShow
        , testSlotNoUnwrap
        , testEpochNoUnwrap
        , testCycleNoUnwrap
        , testBlockNoUnwrap
        , testSlotNoBoundary
        , testOrdConsistency
        , checkProperty "SlotNo Eq reflexive (100 iters)" 100 propSlotNoEqReflexive
        , checkProperty "BlockNo Ord transitive (100 iters)" 100 propBlockNoOrdTransitive
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.Types: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- SlotNo tests
testSlotNoEq :: IO Bool
testSlotNoEq = assertEq "SlotNo Eq: same values" (SlotNo 42) (SlotNo 42)

testSlotNoOrd :: IO Bool
testSlotNoOrd = assertEq "SlotNo Ord: 1 < 2" True (SlotNo 1 < SlotNo 2)

testSlotNoShow :: IO Bool
testSlotNoShow = assertEq "SlotNo Show" "SlotNo 0" (show (SlotNo 0))

-- EpochNo tests
testEpochNoEq :: IO Bool
testEpochNoEq = assertEq "EpochNo Eq: same values" (EpochNo 10) (EpochNo 10)

testEpochNoOrd :: IO Bool
testEpochNoOrd = assertEq "EpochNo Ord: 5 < 10" True (EpochNo 5 < EpochNo 10)

testEpochNoShow :: IO Bool
testEpochNoShow = assertEq "EpochNo Show" "EpochNo 100" (show (EpochNo 100))

-- CycleNo tests
testCycleNoEq :: IO Bool
testCycleNoEq = assertEq "CycleNo Eq: same values" (CycleNo 7) (CycleNo 7)

testCycleNoOrd :: IO Bool
testCycleNoOrd = assertEq "CycleNo Ord: 3 > 1" True (CycleNo 3 > CycleNo 1)

testCycleNoShow :: IO Bool
testCycleNoShow = assertEq "CycleNo Show" "CycleNo 99" (show (CycleNo 99))

-- BlockNo tests
testBlockNoEq :: IO Bool
testBlockNoEq = assertEq "BlockNo Eq: same values" (BlockNo 0) (BlockNo 0)

testBlockNoOrd :: IO Bool
testBlockNoOrd = assertEq "BlockNo Ord: 100 > 50" True (BlockNo 100 > BlockNo 50)

testBlockNoShow :: IO Bool
testBlockNoShow = assertEq "BlockNo Show" "BlockNo 1" (show (BlockNo 1))

-- Unwrap/wrap round-trip tests
testSlotNoUnwrap :: IO Bool
testSlotNoUnwrap =
    let SlotNo n = SlotNo 42
    in assertEq "SlotNo unwrap round-trip" (42 :: Word64) n

testEpochNoUnwrap :: IO Bool
testEpochNoUnwrap =
    let EpochNo n = EpochNo 99
    in assertEq "EpochNo unwrap round-trip" (99 :: Word64) n

testCycleNoUnwrap :: IO Bool
testCycleNoUnwrap =
    let CycleNo n = CycleNo 7
    in assertEq "CycleNo unwrap round-trip" (7 :: Word64) n

testBlockNoUnwrap :: IO Bool
testBlockNoUnwrap =
    let BlockNo n = BlockNo 1000
    in assertEq "BlockNo unwrap round-trip" (1000 :: Word64) n

-- Boundary value tests
testSlotNoBoundary :: IO Bool
testSlotNoBoundary = do
    r1 <- assertEq "SlotNo minBound" (SlotNo 0) (SlotNo 0)
    r2 <- assertEq "SlotNo maxBound" (SlotNo maxBound) (SlotNo (maxBound :: Word64))
    r3 <- assertEq "SlotNo Eq: different values not equal" False (SlotNo 0 == SlotNo 1)
    pure (r1 && r2 && r3)

-- Ord consistency: compare returns EQ for equal values
testOrdConsistency :: IO Bool
testOrdConsistency = do
    r1 <- assertEq "SlotNo compare EQ" EQ (compare (SlotNo 5) (SlotNo 5))
    r2 <- assertEq "SlotNo compare LT" LT (compare (SlotNo 1) (SlotNo 2))
    r3 <- assertEq "SlotNo compare GT" GT (compare (SlotNo 3) (SlotNo 2))
    r4 <- assertEq "EpochNo compare EQ" EQ (compare (EpochNo 0) (EpochNo 0))
    pure (r1 && r2 && r3 && r4)

-- Property: Eq is reflexive for SlotNo
propSlotNoEqReflexive :: PRNG -> Bool
propSlotNoEqReflexive g =
    let (w, _) = nextWord32 g
        s = SlotNo (fromIntegral w)
    in s == s

-- Property: Ord is transitive for BlockNo
propBlockNoOrdTransitive :: PRNG -> Bool
propBlockNoOrdTransitive g =
    let (w1, g1) = nextWord32 g
        (w2, g2) = nextWord32 g1
        (w3, _)  = nextWord32 g2
        -- Sort three values to get a <= b <= c, then verify transitivity
        vals = [fromIntegral w1, fromIntegral w2, fromIntegral w3] :: [Word64]
        sorted = foldr insertSorted [] vals
        [a, b, c] = map BlockNo (take 3 (sorted ++ repeat 0))
    in (a <= b) && (b <= c) && (a <= c)
  where
    insertSorted x [] = [x]
    insertSorted x (y:ys)
        | x <= y    = x : y : ys
        | otherwise = y : insertSorted x ys
