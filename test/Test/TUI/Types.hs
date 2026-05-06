-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.TUI.Types and related layout logic.
module Test.TUI.Types (runTests) where

import Test.Util (assertEq)
import UmbraVox.TUI.Types (ContactStatus(..), statusTag, Layout(..), MenuTab(..),
                           menuTabLabel, menuTabItems, menuTabUnderlineIndex)
import UmbraVox.TUI.Layout (dropdownCol)
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
    p10 <- testMenuTabEnum
    p11 <- testMenuTabItems
    p12 <- testMenuTabLabel
    p13 <- testMenuQuitUnderline
    p14 <- testDropdownAnchorTracksRightJustifiedTabs
    pure (p1 && p2 && p3 && p4 && p5 && p6 && p7 && p8 && p9 && p10 && p11 && p12 && p13 && p14)

testStatusTagOnline :: IO Bool
testStatusTagOnline = assertEq "statusTag Online"  " \x25CF"   (statusTag Online)

testStatusTagOffline :: IO Bool
testStatusTagOffline = assertEq "statusTag Offline" " \x25CB"   (statusTag Offline)

testStatusTagLocal :: IO Bool
testStatusTagLocal = assertEq "statusTag Local"  " \x1F512" (statusTag Local)

testStatusTagGroup :: IO Bool
testStatusTagGroup = assertEq "statusTag Group"  " \x1F465"   (statusTag Group)

testStatusTagLAN :: IO Bool
testStatusTagLAN = assertEq "statusTag LAN"    " \x1F5A7"   (statusTag LAN)

testStatusTagPEX :: IO Bool
testStatusTagPEX = assertEq "statusTag PEX"    " \x1F517"   (statusTag PEX)

-- | calcLayout at 80x24 should produce valid positive dimensions.
testCalcLayoutDimensions :: IO Bool
testCalcLayoutDimensions = do
    let lay = calcLayout 24 80
    a <- assertEq "layout lCols > 0"   True (lCols lay > 0)
    b <- assertEq "layout lRows > 0"   True (lRows lay > 0)
    c <- assertEq "layout lLeftW > 0"  True (lLeftW lay > 0)
    d <- assertEq "layout lRightW > 0" True (lRightW lay > 0)
    e <- assertEq "layout lChatH > 0"  True (lChatH lay > 0)
    -- lLeftW + lRightW should equal lCols
    f <- assertEq "layout width sum" (lCols lay) (lLeftW lay + lRightW lay)
    pure (a && b && c && d && e && f)

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

-- | MenuTab enum covers all 5 tabs.
testMenuTabEnum :: IO Bool
testMenuTabEnum = do
    let tabs = [minBound .. maxBound] :: [MenuTab]
    assertEq "MenuTab has 5 variants" 5 (length tabs)

-- | Each MenuTab has at least one item.
testMenuTabItems :: IO Bool
testMenuTabItems = do
    let tabs = [MenuHelp, MenuContacts, MenuChat, MenuPrefs, MenuQuit]
        allNonEmpty = all (\t -> not (null (menuTabItems t))) tabs
    assertEq "all MenuTab items non-empty" True allNonEmpty

-- | Each MenuTab has a non-empty label.
testMenuTabLabel :: IO Bool
testMenuTabLabel = do
    let tabs = [MenuHelp, MenuContacts, MenuChat, MenuPrefs, MenuQuit]
        allNonEmpty = all (\t -> not (null (menuTabLabel t))) tabs
    a <- assertEq "all MenuTab labels non-empty" True allNonEmpty
    -- Verify specific labels contain expected F-key references
    b <- assertEq "MenuHelp label contains F1" True ("F1" `isIn` menuTabLabel MenuHelp)
    c <- assertEq "MenuPrefs label contains F4" True ("F4" `isIn` menuTabLabel MenuPrefs)
    d <- assertEq "MenuQuit label contains Q" True ("Q" `isIn` menuTabLabel MenuQuit)
    pure (a && b && c && d)
  where
    isIn needle haystack = any (\i -> take (length needle) (drop i haystack) == needle)
                               [0..length haystack - length needle]

testMenuQuitUnderline :: IO Bool
testMenuQuitUnderline =
    assertEq "MenuQuit underline index" (Just 1) (menuTabUnderlineIndex MenuQuit)

testDropdownAnchorTracksRightJustifiedTabs :: IO Bool
testDropdownAnchorTracksRightJustifiedTabs = do
    let help80 = dropdownCol 80 MenuHelp
        help120 = dropdownCol 120 MenuHelp
        contacts120 = dropdownCol 120 MenuContacts
        chat120 = dropdownCol 120 MenuChat
        prefs120 = dropdownCol 120 MenuPrefs
        quit120 = dropdownCol 120 MenuQuit
    a <- assertEq "dropdown anchor shifts right on wider terminals" True (help120 > help80)
    b <- assertEq "dropdown anchors remain ordered by tab" True
        (help120 < contacts120 && contacts120 < chat120 && chat120 < prefs120 && prefs120 < quit120)
    pure (a && b)
