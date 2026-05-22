-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.TUI.Render pure functions:
-- esc, padR, isPfx, clampSize, sizeValid, calcLayout
module Test.TUI.Render (runTests) where

import Test.Util (assertEq, checkProperty, PRNG, nextWord32)
import UmbraVox.TUI.Terminal (esc, padR, isPfx)
import UmbraVox.TUI.Layout (clampSize, sizeValid, calcLayout)
import UmbraVox.TUI.PaginatedList (pageItemBySlot, pageMaxIndex, slicePage, psItems, psPage, psTotalPages)
import UmbraVox.TUI.Text (displayWidth, trimToWidth, splitAtWidth)
import UmbraVox.TUI.Types (Layout(..), ConnectionMode(..))
import UmbraVox.TUI.Render (statusBarConnTag)

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
        , testPadRWideGlyph
        , testTrimToWidthCombining
        , testSplitAtWidthCJK
        , testSplitAtWidthEmoji
        , testSplitAtWidthMixed
        , testSplitAtWidthNarrow
        , testDisplayWidthCJK
        , testDisplayWidthEmoji
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
        , testCalcLayoutResponsiveGridBias
        , testCalcLayoutHistoricalPaneProportion
        , testCalcLayoutKeepsSafetyRowsVisible
        , testStatusBarConnTagNormal
        , testStatusBarConnTagChaste
        , testStatusBarConnTagExplicitEphemeral
        , testPaginatedSliceClampsPage
        , testPaginatedSlotSelection
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

testPadRWideGlyph :: IO Bool
testPadRWideGlyph = do
    let rendered = padR 6 "\x4F60\x597D"
    a <- assertEq "padR wide glyph display width" 6 (displayWidth rendered)
    b <- assertEq "padR wide glyph content preserved" "\x4F60\x597D" (trimToWidth 4 rendered)
    pure (a && b)

testTrimToWidthCombining :: IO Bool
testTrimToWidthCombining =
    assertEq "trimToWidth keeps combining mark with base"
        "e\x0301"
        (trimToWidth 1 "e\x0301x")

-- R1.8.4: Wide character (CJK/emoji) wrapping tests
testSplitAtWidthCJK :: IO Bool
testSplitAtWidthCJK = do
    -- CJK chars are width 2. "你好世界" = 8 display columns.
    -- splitAtWidth 5 should take "你好" (4 cols) and leave "世界"
    -- because "你好世" would be 6 cols > 5.
    let (a, b) = splitAtWidth 5 "\x4F60\x597D\x4E16\x754C"
    x <- assertEq "splitAtWidth CJK first chunk" "\x4F60\x597D" a
    y <- assertEq "splitAtWidth CJK remainder" "\x4E16\x754C" b
    pure (x && y)

testSplitAtWidthEmoji :: IO Bool
testSplitAtWidthEmoji = do
    -- Common emoji (😀 U+1F600) is width 2.
    let (a, b) = splitAtWidth 3 "\x1F600\x1F601\x1F602"
    x <- assertEq "splitAtWidth emoji first chunk" "\x1F600" a
    y <- assertEq "splitAtWidth emoji remainder" "\x1F601\x1F602" b
    pure (x && y)

testSplitAtWidthMixed :: IO Bool
testSplitAtWidthMixed = do
    -- Mix of ASCII (width 1) and CJK (width 2): "hi你好" = 1+1+2+2 = 6 cols
    let (a, b) = splitAtWidth 4 "hi\x4F60\x597D"
    x <- assertEq "splitAtWidth mixed first chunk" "hi\x4F60" a
    y <- assertEq "splitAtWidth mixed remainder" "\x597D" b
    pure (x && y)

testSplitAtWidthNarrow :: IO Bool
testSplitAtWidthNarrow = do
    -- Width 1 can't fit a CJK char (width 2). Should return empty first chunk.
    let (a, b) = splitAtWidth 1 "\x4F60\x597D"
    x <- assertEq "splitAtWidth narrow can't fit wide char" "" a
    y <- assertEq "splitAtWidth narrow keeps all" "\x4F60\x597D" b
    pure (x && y)

testDisplayWidthCJK :: IO Bool
testDisplayWidthCJK =
    assertEq "displayWidth CJK 4 chars = 8 cols" 8 (displayWidth "\x4F60\x597D\x4E16\x754C")

testDisplayWidthEmoji :: IO Bool
testDisplayWidthEmoji =
    assertEq "displayWidth 3 emoji = 6 cols" 6 (displayWidth "\x1F600\x1F601\x1F602")

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

testCalcLayoutResponsiveGridBias :: IO Bool
testCalcLayoutResponsiveGridBias = do
    let compact = calcLayout 24 80
        wide = calcLayout 24 240
    a <- assertEq "responsive grid keeps left pane floor" True (lLeftW compact >= 20 && lLeftW wide >= 20)
    b <- assertEq "responsive grid allocates added width to chat pane" True (lRightW wide > lRightW compact)
    pure (a && b)

testCalcLayoutHistoricalPaneProportion :: IO Bool
testCalcLayoutHistoricalPaneProportion = do
    let lay = calcLayout 24 120
        pct = (lLeftW lay * 100) `div` max 1 (lCols lay)
    a <- assertEq "historical pane split keeps left pane substantial" True (pct >= 20)
    b <- assertEq "historical pane split keeps chat pane dominant" True (pct <= 50)
    pure (a && b)

testCalcLayoutKeepsSafetyRowsVisible :: IO Bool
testCalcLayoutKeepsSafetyRowsVisible = do
    -- Uses 42 rows so chatH = 32 (inputAreaRows=7, fixed overhead=10).
    -- With maxIdentityRows=20, separator(1)+QR(14)+header(1)+safety(3)+fp(1) = 20 rows.
    let lay = calcLayout 42 120
    assertEq "identity panel keeps QR, safety rows, and fingerprints visible" True (lIdentityH lay >= 20)

testStatusBarConnTagNormal :: IO Bool
testStatusBarConnTagNormal = do
    -- With persistence plugins enabled, non-Chaste non-ephemeral shows PERSISTENT
    let tag = statusBarConnTag Promiscuous False True True 0 3
    a <- assertEq "status bar normal mode with plugins shows PERSISTENT"
            True ("PERSISTENT" `contains` tag)
    b <- assertEq "status bar normal mode with plugins omits EPHEMERAL"
            False ("EPHEMERAL" `contains` tag)
    pure (a && b)

testStatusBarConnTagChaste :: IO Bool
testStatusBarConnTagChaste = do
    let tag = statusBarConnTag Chaste False True True 2 3
    a <- assertEq "status bar chaste shows ephemeral" True ("EPHEMERAL" `contains` tag)
    b <- assertEq "status bar chaste separates version with diamond" True ("\x25C6 UmbraVOX" `contains` tag)
    pure (a && b)

testStatusBarConnTagExplicitEphemeral :: IO Bool
testStatusBarConnTagExplicitEphemeral =
    assertEq "status bar explicit ephemeral flag shows tag"
        True
        ("EPHEMERAL" `contains` statusBarConnTag Promiscuous True True True 0 3)

testPaginatedSliceClampsPage :: IO Bool
testPaginatedSliceClampsPage = do
    let page = slicePage 10 5 ([0..11] :: [Int])
    a <- assertEq "paginated slice clamps page index" 1 (psPage page)
    b <- assertEq "paginated slice total pages" 2 (psTotalPages page)
    c <- assertEq "paginated slice keeps second page slots" [(0,10),(1,11)] (psItems page)
    pure (a && b && c)

testPaginatedSlotSelection :: IO Bool
testPaginatedSlotSelection = do
    let xs = ["a","b","c","d","e","f","g","h","i","j","k"]
    a <- assertEq "paginated slot picks visible second-page item" (Just "k") (pageItemBySlot 10 1 0 xs)
    b <- assertEq "paginated slot rejects missing slot" Nothing (pageItemBySlot 10 1 1 xs)
    c <- assertEq "paginated page max index" 1 (pageMaxIndex 10 xs)
    pure (a && b && c)

-- Property tests --------------------------------------------------------------

propPadRLength :: IO Bool
propPadRLength = checkProperty "padR always produces width-length string" 100 $ \g ->
    let (w32, _) = nextWord32 g
        w = fromIntegral (w32 `mod` 200) :: Int
    in displayWidth (padR w "test") == w

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

contains :: String -> String -> Bool
contains needle haystack = any (\i -> take (length needle) (drop i haystack) == needle)
    [0 .. max 0 (length haystack - length needle)]
