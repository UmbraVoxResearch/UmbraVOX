-- | Tests for UmbraVox.TUI.Render pure functions:
-- esc, padR, isPfx, clampSize, sizeValid, calcLayout
module Test.TUI.Render (runTests) where

import Test.Util (assertEq, checkProperty, PRNG, nextWord32)
import UmbraVox.TUI.Terminal (esc, padR, isPfx)
import UmbraVox.TUI.Layout (clampSize, sizeValid, calcLayout)
import UmbraVox.TUI.Types (Layout(..))

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Render"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testEscBasic
        , testEscEmpty
        , testPadRShort
        , testPadRExact
        , testPadRLong
        , testPadRZero
        , testIsPfxEmpty
        , testIsPfxMatch
        , testIsPfxMismatch
        , testIsPfxLonger
        , testIsPfxSelf
        , testClampSizeNormal
        , testClampSizeTooSmall
        , testClampSizeTooLarge
        , testClampSizeBoundary
        , testSizeValidNormal
        , testSizeValidTooSmall
        , testSizeValidTooLarge
        , testSizeValidBoundaryMin
        , testSizeValidBoundaryMax
        , testCalcLayoutMinimal
        , testCalcLayoutLarger
        , testCalcLayoutWidthSum
        , testCalcLayoutLeftMinimum
        , testCalcLayoutEdgeToEdge
        , propPadRLength
        , propClampSizeInRange
        , propLayoutWidthSum
        , propSizeValidClampIdempotent
        ]
    pure (and results)

-- esc -------------------------------------------------------------------------

testEscBasic :: IO Bool
testEscBasic = assertEq "esc basic" "\ESC[2J" (esc "2J")

testEscEmpty :: IO Bool
testEscEmpty = assertEq "esc empty" "\ESC[" (esc "")

-- padR ------------------------------------------------------------------------

testPadRShort :: IO Bool
testPadRShort = assertEq "padR short string" "hi   " (padR 5 "hi")

testPadRExact :: IO Bool
testPadRExact = assertEq "padR exact fit" "hello" (padR 5 "hello")

testPadRLong :: IO Bool
testPadRLong = assertEq "padR truncates long" "hel" (padR 3 "hello")

testPadRZero :: IO Bool
testPadRZero = assertEq "padR width 0" "" (padR 0 "hello")

-- isPfx -----------------------------------------------------------------------

testIsPfxEmpty :: IO Bool
testIsPfxEmpty = assertEq "isPfx empty prefix" True (isPfx "" "anything")

testIsPfxMatch :: IO Bool
testIsPfxMatch = assertEq "isPfx matching" True (isPfx "hel" "hello")

testIsPfxMismatch :: IO Bool
testIsPfxMismatch = assertEq "isPfx mismatch" False (isPfx "hex" "hello")

testIsPfxLonger :: IO Bool
testIsPfxLonger = assertEq "isPfx prefix longer" False (isPfx "hello world" "hello")

testIsPfxSelf :: IO Bool
testIsPfxSelf = assertEq "isPfx self" True (isPfx "abc" "abc")

-- clampSize -------------------------------------------------------------------

testClampSizeNormal :: IO Bool
testClampSizeNormal = assertEq "clampSize normal" (50, 120) (clampSize 50 120)

testClampSizeTooSmall :: IO Bool
testClampSizeTooSmall = assertEq "clampSize too small" (24, 80) (clampSize 10 40)

testClampSizeTooLarge :: IO Bool
testClampSizeTooLarge = assertEq "clampSize too large" (100, 300) (clampSize 200 500)

testClampSizeBoundary :: IO Bool
testClampSizeBoundary = assertEq "clampSize at boundary" (24, 80) (clampSize 24 80)

-- sizeValid -------------------------------------------------------------------

testSizeValidNormal :: IO Bool
testSizeValidNormal = assertEq "sizeValid normal" True (sizeValid 50 120)

testSizeValidTooSmall :: IO Bool
testSizeValidTooSmall = assertEq "sizeValid too small" False (sizeValid 23 79)

testSizeValidTooLarge :: IO Bool
testSizeValidTooLarge = assertEq "sizeValid too large" False (sizeValid 101 301)

testSizeValidBoundaryMin :: IO Bool
testSizeValidBoundaryMin = assertEq "sizeValid min boundary" True (sizeValid 24 80)

testSizeValidBoundaryMax :: IO Bool
testSizeValidBoundaryMax = assertEq "sizeValid max boundary" True (sizeValid 100 300)

-- calcLayout ------------------------------------------------------------------

testCalcLayoutMinimal :: IO Bool
testCalcLayoutMinimal = do
    let lay = calcLayout 24 80
    a <- assertEq "calcLayout 24x80 chatH > 0" True (lChatH lay > 0)
    b <- assertEq "calcLayout 24x80 lLeftW >= 20" True (lLeftW lay >= 20)
    pure (a && b)

testCalcLayoutLarger :: IO Bool
testCalcLayoutLarger = do
    let small = calcLayout 24 80
        large = calcLayout 80 200
    a <- assertEq "calcLayout larger cols" True (lCols large > lCols small)
    b <- assertEq "calcLayout larger rows" True (lRows large > lRows small)
    pure (a && b)

testCalcLayoutWidthSum :: IO Bool
testCalcLayoutWidthSum = do
    let lay = calcLayout 40 120
    assertEq "calcLayout width sum" (lCols lay) (lLeftW lay + lRightW lay)

testCalcLayoutLeftMinimum :: IO Bool
testCalcLayoutLeftMinimum = do
    -- Even at min size, lLeftW should be at least 20
    let lay = calcLayout 24 80
    assertEq "calcLayout lLeftW >= 20" True (lLeftW lay >= 20)

testCalcLayoutEdgeToEdge :: IO Bool
testCalcLayoutEdgeToEdge = do
    -- lCols should equal the raw terminal cols (edge-to-edge, no padding)
    let lay = calcLayout 50 100
    a <- assertEq "calcLayout lCols == cols" 100 (lCols lay)
    b <- assertEq "calcLayout lRows == rows" 50 (lRows lay)
    pure (a && b)

-- Property tests --------------------------------------------------------------

propPadRLength :: IO Bool
propPadRLength = checkProperty "padR always produces width-length string" 100 $ \g ->
    let (w32, _) = nextWord32 g
        w = fromIntegral (w32 `mod` 200) :: Int
    in length (padR w "test") == w

propClampSizeInRange :: IO Bool
propClampSizeInRange = checkProperty "clampSize always in valid range" 100 $ \g ->
    let (w1, g') = nextWord32 g
        (w2, _)  = nextWord32 g'
        r = fromIntegral (w1 `mod` 500) :: Int
        c = fromIntegral (w2 `mod` 500) :: Int
        (cr, cc) = clampSize r c
    in cr >= 24 && cr <= 100 && cc >= 80 && cc <= 300

propLayoutWidthSum :: IO Bool
propLayoutWidthSum = checkProperty "calcLayout lLeftW + lRightW == lCols" 100 $ \g ->
    let (w1, g') = nextWord32 g
        (w2, _)  = nextWord32 g'
        r = 24 + fromIntegral (w1 `mod` 77) :: Int
        c = 80 + fromIntegral (w2 `mod` 221) :: Int
        lay = calcLayout r c
    in lLeftW lay + lRightW lay == lCols lay

propSizeValidClampIdempotent :: IO Bool
propSizeValidClampIdempotent = checkProperty "clamped sizes are always valid" 100 $ \g ->
    let (w1, g') = nextWord32 g
        (w2, _)  = nextWord32 g'
        r = fromIntegral (w1 `mod` 500) :: Int
        c = fromIntegral (w2 `mod` 500) :: Int
        (cr, cc) = clampSize r c
    in sizeValid cr cc
