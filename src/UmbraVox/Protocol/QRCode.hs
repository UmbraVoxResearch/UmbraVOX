-- | Simplified QR-like code generator for key fingerprint display.
--
-- This is NOT a standards-compliant QR encoder. It produces a visually
-- distinctive 25x25 module grid with finder patterns, suitable for
-- verification by a paired custom decoder. Encodes up to 128 hex chars.
module UmbraVox.Protocol.QRCode
    ( generateQR
    , renderQR
    ) where

import Data.Bits (testBit)
import Data.Char (digitToInt, isHexDigit, toLower)

-- | Grid size (Version 2 format: 25x25 modules).
gridSize :: Int
gridSize = 25

-- | Generate QR code matrix from a hex input string.
-- Returns list of rows; each row is a list of Bool (True = black).
generateQR :: String -> [[Bool]]
generateQR input = buildGrid (encodeHex (take 128 (filter isHexDigit (map toLower input))))

-- | Render QR matrix as compact ASCII art using Unicode half-block chars.
-- Two rows are packed into one terminal line:
--   top+bottom black = full block,  top only = upper half,
--   bottom only = lower half,  neither = space.
renderQR :: [[Bool]] -> [String]
renderQR rows = zipPairs (addQuiet rows)
  where
    -- Add 1-module quiet zone on each side
    addQuiet rs =
        let w = case rs of { [] -> 0; (r:_) -> length r }
            blank = replicate (w + 2) False
            pad r = False : r ++ [False]
        in  [blank] ++ map pad rs ++ [blank]

    zipPairs [] = []
    zipPairs [top] = [renderPair top (replicate (length top) False)]
    zipPairs (top:bot:rest) = renderPair top bot : zipPairs rest

    renderPair top bot = map halfBlock (zip top bot)

    halfBlock (True,  True)  = '\x2588'  -- full block
    halfBlock (True,  False) = '\x2580'  -- upper half
    halfBlock (False, True)  = '\x2584'  -- lower half
    halfBlock (False, False) = ' '

-- | Convert hex string to list of 4-bit nibbles.
encodeHex :: String -> [Int]
encodeHex = map digitToInt

-- | Build the 25x25 grid with finder patterns, alignment, and data.
buildGrid :: [Int] -> [[Bool]]
buildGrid nibbles = [[cellAt r c | c <- [0..gridSize-1]] | r <- [0..gridSize-1]]
  where
    -- Data bits: each nibble is 4 bits, padded to fill available cells
    dataBits :: [Bool]
    dataBits = concatMap nibbleToBits nibbles ++ repeat False

    nibbleToBits :: Int -> [Bool]
    nibbleToBits n = [testBit n 3, testBit n 2, testBit n 1, testBit n 0]

    -- Cells available for data (not reserved by patterns)
    dataCells :: [(Int,Int)]
    dataCells = [(r,c) | r <- [0..gridSize-1], c <- [0..gridSize-1]
                       , not (isReserved r c)]

    -- Map from (row,col) to data bit value
    dataMap :: [(Int,Int,Bool)]
    dataMap = zipWith (\(r,c) b -> (r,c,b)) dataCells dataBits

    lookupData :: Int -> Int -> Bool
    lookupData r c = case filter (\(r',c',_) -> r'==r && c'==c) dataMap of
        ((_,_,b):_) -> applyMask r c b
        []          -> False

    -- Simple checkerboard mask for visual balance
    applyMask :: Int -> Int -> Bool -> Bool
    applyMask r c b = b /= ((r + c) `mod` 2 == 0)

    cellAt :: Int -> Int -> Bool
    cellAt r c
        | isFinderArea r c   = finderPattern r c
        | isAlignArea r c    = alignPattern r c
        | isTimingH r c      = c `mod` 2 == 0
        | isTimingV r c      = r `mod` 2 == 0
        | otherwise          = lookupData r c

    isReserved :: Int -> Int -> Bool
    isReserved r c = isFinderArea r c || isAlignArea r c
                  || isTimingH r c   || isTimingV r c

    -- Finder patterns: 7x7 squares in three corners + 1-module separator
    isFinderArea :: Int -> Int -> Bool
    isFinderArea r c = (r < 8 && c < 8)          -- top-left
                    || (r < 8 && c >= gridSize-8) -- top-right
                    || (r >= gridSize-8 && c < 8) -- bottom-left

    finderPattern :: Int -> Int -> Bool
    finderPattern r c
        | r < 7 && c < 7                   = finderCell r c
        | r < 7 && c >= gridSize - 7       = finderCell r (c - (gridSize - 7))
        | r >= gridSize - 7 && c < 7       = finderCell (r - (gridSize - 7)) c
        | otherwise                        = False  -- separator zone = white

    finderCell :: Int -> Int -> Bool
    finderCell r c
        | r == 0 || r == 6 = c >= 0 && c <= 6
        | c == 0 || c == 6 = True
        | r >= 2 && r <= 4 && c >= 2 && c <= 4 = True
        | otherwise = False

    -- Alignment pattern: 5x5 at position (18,18) for Version 2
    isAlignArea :: Int -> Int -> Bool
    isAlignArea r c = r >= 16 && r <= 20 && c >= 16 && c <= 20

    alignPattern :: Int -> Int -> Bool
    alignPattern r c
        | ar == 0 || ar == 4 = True
        | ac == 0 || ac == 4 = True
        | ar == 2 && ac == 2 = True
        | otherwise          = False
      where ar = r - 16; ac = c - 16

    -- Timing patterns
    isTimingH :: Int -> Int -> Bool
    isTimingH r c = r == 6 && c >= 8 && c <= gridSize - 9

    isTimingV :: Int -> Int -> Bool
    isTimingV r c = c == 6 && r >= 8 && r <= gridSize - 9
