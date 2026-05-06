-- | Tests for UmbraVox.Economics.Token
module Test.Economics.Token (runTests) where

import Test.Util (assertEq)
import UmbraVox.Economics.Token (totalSupply)

runTests :: IO Bool
runTests = do
    putStrLn "Economics.Token"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testTotalSupplyValue
        , testTotalSupplyPositive
        , testTotalSupply11B
        ]
    pure (and results)

-- | totalSupply should be exactly 11 billion.
testTotalSupplyValue :: IO Bool
testTotalSupplyValue =
    assertEq "totalSupply == 11000000000" 11000000000 totalSupply

-- | totalSupply must be positive.
testTotalSupplyPositive :: IO Bool
testTotalSupplyPositive =
    assertEq "totalSupply > 0" True (totalSupply > 0)

-- | totalSupply should be 11 * 10^9.
testTotalSupply11B :: IO Bool
testTotalSupply11B =
    assertEq "totalSupply == 11 * 10^9" (11 * 10 ^ (9 :: Int)) totalSupply
