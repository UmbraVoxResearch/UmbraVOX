-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Text
    ( displayWidth
    , trimToWidth
    , padRight
    ) where

import Data.Char (GeneralCategory(..), generalCategory, ord)

displayWidth :: String -> Int
displayWidth = sum . map charWidth

trimToWidth :: Int -> String -> String
trimToWidth width = go 0
  where
    go _ [] = []
    go acc (ch:rest)
        | chWidth == 0 = ch : go acc rest
        | acc + chWidth <= width = ch : go (acc + chWidth) rest
        | otherwise = []
      where
        chWidth = charWidth ch

padRight :: Int -> String -> String
padRight width text =
    let clipped = trimToWidth width text
    in clipped ++ replicate (max 0 (width - displayWidth clipped)) ' '

charWidth :: Char -> Int
charWidth ch
    | isZeroWidth ch = 0
    | isWide ch = 2
    | otherwise = 1

isZeroWidth :: Char -> Bool
isZeroWidth ch =
    ord ch == 0
    || ord ch < 32
    || ord ch == 127
    || category `elem`
        [ NonSpacingMark
        , SpacingCombiningMark
        , EnclosingMark
        , Format
        , Control
        , Surrogate
        ]
  where
    category = generalCategory ch

isWide :: Char -> Bool
isWide ch =
    inRange 0x1100 0x115F code
    || inRange 0x231A 0x231B code
    || inRange 0x2329 0x232A code
    || inRange 0x23E9 0x23EC code
    || code == 0x23F0
    || code == 0x23F3
    || inRange 0x25FD 0x25FE code
    || inRange 0x2614 0x2615 code
    || inRange 0x2648 0x2653 code
    || inRange 0x267F 0x267F code
    || inRange 0x2693 0x2693 code
    || inRange 0x26A1 0x26A1 code
    || inRange 0x26AA 0x26AB code
    || inRange 0x26BD 0x26BE code
    || inRange 0x26C4 0x26C5 code
    || inRange 0x26CE 0x26CE code
    || inRange 0x26D4 0x26D4 code
    || inRange 0x26EA 0x26EA code
    || inRange 0x26F2 0x26F3 code
    || inRange 0x26F5 0x26F5 code
    || inRange 0x26FA 0x26FA code
    || inRange 0x26FD 0x26FD code
    || inRange 0x2705 0x2705 code
    || inRange 0x270A 0x270B code
    || inRange 0x2728 0x2728 code
    || inRange 0x274C 0x274C code
    || inRange 0x274E 0x274E code
    || inRange 0x2753 0x2755 code
    || inRange 0x2757 0x2757 code
    || inRange 0x2795 0x2797 code
    || inRange 0x27B0 0x27B0 code
    || inRange 0x27BF 0x27BF code
    || inRange 0x2B1B 0x2B1C code
    || inRange 0x2B50 0x2B50 code
    || inRange 0x2B55 0x2B55 code
    || inRange 0x2E80 0xA4CF code
    || inRange 0xAC00 0xD7A3 code
    || inRange 0xF900 0xFAFF code
    || inRange 0xFE10 0xFE19 code
    || inRange 0xFE30 0xFE6F code
    || inRange 0xFF00 0xFF60 code
    || inRange 0xFFE0 0xFFE6 code
    || inRange 0x1F004 0x1F004 code
    || inRange 0x1F0CF 0x1F0CF code
    || inRange 0x1F18E 0x1F18E code
    || inRange 0x1F191 0x1F19A code
    || inRange 0x1F200 0x1F251 code
    || inRange 0x1F300 0x1F64F code
    || inRange 0x1F680 0x1F6FF code
    || inRange 0x1F900 0x1F9FF code
    || inRange 0x1FA70 0x1FAFF code
    || inRange 0x20000 0x3FFFD code
  where
    code = ord ch
    inRange lo hi n = n >= lo && n <= hi
