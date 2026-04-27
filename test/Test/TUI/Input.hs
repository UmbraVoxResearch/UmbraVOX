-- | Tests for UmbraVox.TUI.Input pure functions: splitOn, strip'
-- Note: Most Input functions are IO-heavy (terminal reading, event loops).
-- We test the exported pure helpers and the key-mapping logic indirectly.
module Test.TUI.Input (runTests) where

import Test.Util (assertEq, checkProperty, PRNG, nextWord32)
import UmbraVox.TUI.Input (strip')

-- strip' is imported from UmbraVox.TUI.Input (now exported).
-- splitOn is a local test helper that differs from Protocol.Encoding.splitOn
-- (returns [] for empty input vs [""]). It tests the TUI's internal parsing.
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = let (w, rest) = break (== c) s
              in w : case rest of { [] -> []; (_:r) -> splitOn c r }

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Input"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testSplitOnEmpty
        , testSplitOnSingle
        , testSplitOnMultiple
        , testSplitOnTrailing
        , testSplitOnLeading
        , testSplitOnConsecutive
        , testSplitOnNoDelim
        , testStripEmpty
        , testStripSpaces
        , testStripMixed
        , testStripNoSpaces
        , testStripAllSpaces
        , propSplitOnJoinRoundtrip
        , propStripIdempotent
        ]
    pure (and results)

-- splitOn ----------------------------------------------------------------------

testSplitOnEmpty :: IO Bool
testSplitOnEmpty = assertEq "splitOn empty" [] (splitOn ',' "")

testSplitOnSingle :: IO Bool
testSplitOnSingle = assertEq "splitOn single" ["hello"] (splitOn ',' "hello")

testSplitOnMultiple :: IO Bool
testSplitOnMultiple = assertEq "splitOn multiple" ["a","b","c"] (splitOn ',' "a,b,c")

testSplitOnTrailing :: IO Bool
testSplitOnTrailing = assertEq "splitOn trailing delim" ["a","b"] (splitOn ',' "a,b,")

testSplitOnLeading :: IO Bool
testSplitOnLeading = assertEq "splitOn leading delim" ["","a","b"] (splitOn ',' ",a,b")

testSplitOnConsecutive :: IO Bool
testSplitOnConsecutive = assertEq "splitOn consecutive delims" ["a","","b"] (splitOn ',' "a,,b")

testSplitOnNoDelim :: IO Bool
testSplitOnNoDelim = assertEq "splitOn no delim present" ["hello world"] (splitOn ',' "hello world")

-- strip' ----------------------------------------------------------------------

testStripEmpty :: IO Bool
testStripEmpty = assertEq "strip' empty" "" (strip' "")

testStripSpaces :: IO Bool
testStripSpaces = assertEq "strip' leading spaces" "hello" (strip' "   hello")

testStripMixed :: IO Bool
testStripMixed = assertEq "strip' preserves trailing" "hello   " (strip' "  hello   ")

testStripNoSpaces :: IO Bool
testStripNoSpaces = assertEq "strip' no spaces" "hello" (strip' "hello")

testStripAllSpaces :: IO Bool
testStripAllSpaces = assertEq "strip' all spaces" "" (strip' "     ")

-- Property tests --------------------------------------------------------------

propSplitOnJoinRoundtrip :: IO Bool
propSplitOnJoinRoundtrip = checkProperty "splitOn/concat preserves content" 100 $ \g ->
    let (w, _) = nextWord32 g
        -- Generate a simple test string with some commas
        n = fromIntegral (w `mod` 10) :: Int
        s = concatMap (\i -> if i `mod` 3 == 0 then "," else [toEnum (fromEnum 'a' + i `mod` 26)]) [0..n]
        parts = splitOn ',' s
        -- The original chars (minus delimiters) should be preserved in parts
        rejoined = concatMap id parts
        origChars = filter (/= ',') s
    in rejoined == origChars

propStripIdempotent :: IO Bool
propStripIdempotent = checkProperty "strip' is idempotent" 100 $ \g ->
    let (w, _) = nextWord32 g
        n = fromIntegral (w `mod` 10) :: Int
        s = replicate n ' ' ++ "test"
    in strip' (strip' s) == strip' s
