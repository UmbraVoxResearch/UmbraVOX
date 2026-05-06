-- | Tests for UmbraVox.TUI.Types and related layout logic.
module Test.TUI.Types (runTests) where

import Test.Util (assertEq)
import UmbraVox.TUI.Types (ContactStatus(..), statusTag, Layout(..))
import UmbraVox.TUI.Render (calcLayout, sizeValid)

runTests :: IO Bool
runTests = do
    putStrLn "TUI.Types"
    p1 <- testStatusTagOnline
    p2 <- testStatusTagOffline
    p3 <- testStatusTagLocal
    p4 <- testStatusTagGroup
    p5 <- testStatusTagLAN
    p6 <- testStatusTagPEX
    p7 <- testCalcLayoutDimensions
    p8 <- testCalcLayoutMinSize
    p9 <- testSizeValid
    pure (p1 && p2 && p3 && p4 && p5 && p6 && p7 && p8 && p9)

testStatusTagOnline :: IO Bool
testStatusTagOnline = assertEq "statusTag Online"  "[ON]"    (statusTag Online)

testStatusTagOffline :: IO Bool
testStatusTagOffline = assertEq "statusTag Offline" "[OFF]"   (statusTag Offline)

testStatusTagLocal :: IO Bool
testStatusTagLocal = assertEq "statusTag Local"  "[LOCAL]" (statusTag Local)

testStatusTagGroup :: IO Bool
testStatusTagGroup = assertEq "statusTag Group"  "[GRP]"   (statusTag Group)

testStatusTagLAN :: IO Bool
testStatusTagLAN = assertEq "statusTag LAN"    "[LAN]"   (statusTag LAN)

testStatusTagPEX :: IO Bool
testStatusTagPEX = assertEq "statusTag PEX"    "[PEX]"   (statusTag PEX)

-- | calcLayout at 80x24 should produce valid positive dimensions.
testCalcLayoutDimensions :: IO Bool
testCalcLayoutDimensions = do
    let lay = calcLayout 24 80
    a <- assertEq "layout lCols > 0"   True (lCols lay > 0)
    b <- assertEq "layout lRows > 0"   True (lRows lay > 0)
    c <- assertEq "layout lLeftW > 0"  True (lLeftW lay > 0)
    d <- assertEq "layout lRightW > 0" True (lRightW lay > 0)
    e <- assertEq "layout lChatH > 0"  True (lChatH lay > 0)
    f <- assertEq "layout lPadX > 0"   True (lPadX lay > 0)
    g <- assertEq "layout lPadY > 0"   True (lPadY lay > 0)
    -- lLeftW + lRightW should equal lCols
    h <- assertEq "layout width sum" (lCols lay) (lLeftW lay + lRightW lay)
    pure (a && b && c && d && e && f && g && h)

-- | calcLayout at larger terminal should produce larger usable area.
testCalcLayoutMinSize :: IO Bool
testCalcLayoutMinSize = do
    let small = calcLayout 24 80
        large = calcLayout 50 160
    a <- assertEq "larger terminal wider"  True (lCols large > lCols small)
    b <- assertEq "larger terminal taller" True (lRows large > lRows small)
    pure (a && b)

-- | sizeValid accepts normal sizes and rejects extremes.
testSizeValid :: IO Bool
testSizeValid = do
    a <- assertEq "sizeValid 24 80"  True  (sizeValid 24 80)
    b <- assertEq "sizeValid 10 40"  False (sizeValid 10 40)
    c <- assertEq "sizeValid 50 200" True  (sizeValid 50 200)
    pure (a && b && c)
