-- SPDX-License-Identifier: Apache-2.0
-- | Safety number, fingerprint display, and QR code generation for key
-- verification.
--
-- Verification methods:
--   1. Hex fingerprint — easy to read aloud over a phone call
--   2. Safety number   — 60 digits (12 groups of 5), like Signal
--   3. QR code         — scannable Version 2-L numeric QR code
module UmbraVox.Protocol.QRCode
    ( generateSafetyNumber
    , renderSafetyNumber
    , renderFingerprint
    , generateQRCode
    , renderQRCode
    ) where

import Data.Bits (shiftR, shiftL, (.&.), (.|.), xor, testBit)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char (digitToInt, isDigit)
import Data.List (intercalate, foldl')
import Data.Word (Word8)
import UmbraVox.Crypto.SHA256 (sha256)

-- | Generate a 60-digit safety number from two public keys.
--
-- The keys are sorted (lexicographic on raw bytes) before hashing so
-- that both parties derive the same number regardless of role.
-- SHA-256 of the sorted concatenation is used to extract 60 decimal
-- digits (each from 0-9), displayed as 12 groups of 5.
generateSafetyNumber :: ByteString -> ByteString -> String
generateSafetyNumber ours theirs =
    let (lo, hi) = if ours <= theirs then (ours, theirs) else (theirs, ours)
        hash     = sha256 (lo <> hi)
        -- We need 60 digits. Each hash byte gives one digit (mod 10).
        -- 32 bytes per hash; hash twice to get 64 bytes, take 60.
        hash2    = sha256 hash
        allBytes = BS.unpack hash <> BS.unpack hash2  -- 64 bytes
        digits   = take 60 (map byteToDigit allBytes)
    in digits
  where
    byteToDigit :: Word8 -> Char
    byteToDigit w = toEnum (fromEnum '0' + fromIntegral w `mod` 10)

-- | Format a 60-digit safety number as 6 rows of 2 groups (5 digits each).
renderSafetyNumber :: String -> [String]
renderSafetyNumber digits = map formatRow (groups 10 digits)
  where
    formatRow row = let (a, b) = splitAt 5 row
                    in "  " ++ a ++ " " ++ b
    groups _ [] = []
    groups n xs = let (g, rest) = splitAt n xs in g : groups n rest

-- | Format a public key as hex groups for display.
-- Produces 2 rows of 4 groups (4 hex chars each), covering 16 bytes.
renderFingerprint :: ByteString -> [String]
renderFingerprint bs =
    let hexStr = concatMap byte2hex (BS.unpack (BS.take 16 bs))
        row1   = intercalate " " (hexGroups (take 16 hexStr))
        row2   = intercalate " " (hexGroups (drop 16 hexStr))
    in ["  " ++ row1, "  " ++ row2]
  where
    byte2hex :: Word8 -> String
    byte2hex w = [hexC (w `shiftR` 4), hexC (w .&. 0x0f)]
    hexC n | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
           | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)
    hexGroups [] = []
    hexGroups xs = let (g, rest) = splitAt 4 xs in g : hexGroups rest

-- ---------------------------------------------------------------------------
-- QR Code Generation — Version 2-L, Numeric Mode
-- ---------------------------------------------------------------------------

-- | QR code size for Version 2.
qrSize :: Int
qrSize = 25

-- | Generate a QR code matrix for the given numeric string.
-- Returns a 25x25 grid of Bool (True = dark module).
-- Implements QR Version 2-L with numeric encoding.
generateQRCode :: String -> [[Bool]]
generateQRCode digits =
    let blank  = replicate qrSize (replicate qrSize False)
        -- Step 1: place function patterns
        grid1  = placeFunctionPatterns blank
        -- Step 2: encode data into codewords
        dataCW = encodeNumericData digits
        eccCW  = rsEncode 10 dataCW
        allCW  = dataCW ++ eccCW
        -- Step 3: place data bits in zigzag order
        grid2  = placeDataBits grid1 (codewordsToBits allCW)
        -- Step 4: apply mask pattern 0 (checkerboard)
        grid3  = applyMask grid2
        -- Step 5: place format information
        grid4  = placeFormatInfo grid3
    in grid4

-- | Render QR matrix as ASCII art using half-block Unicode characters.
-- Two matrix rows per terminal line for compact display.
-- Uses quiet zone of 1 module on each side.
renderQRCode :: [[Bool]] -> [String]
renderQRCode matrix =
    let sz   = length matrix
        -- Add 1-module quiet zone on each side
        padH row = [False] ++ row ++ [False]
        padded = [replicate (sz + 2) False]
              ++ map padH matrix
              ++ [replicate (sz + 2) False]
        rows = pairRows padded
    in map renderPair rows
  where
    pairRows []       = []
    pairRows [r]      = [(r, replicate (length r) False)]
    pairRows (a:b:rs) = (a, b) : pairRows rs
    renderPair (top, bot) = map renderCell (zip top bot)
    -- Unicode half-block rendering:
    -- upper=dark, lower=dark -> full block
    -- upper=dark, lower=light -> upper half
    -- upper=light, lower=dark -> lower half
    -- both light -> space
    renderCell (True,  True)  = '\x2588'  -- full block
    renderCell (True,  False) = '\x2580'  -- upper half block
    renderCell (False, True)  = '\x2584'  -- lower half block
    renderCell (False, False) = ' '

-- ---------------------------------------------------------------------------
-- Function pattern placement
-- ---------------------------------------------------------------------------

-- | Place all function patterns on the grid: finders, alignment,
-- timing, and dark module.
placeFunctionPatterns :: [[Bool]] -> [[Bool]]
placeFunctionPatterns g =
    let g1 = placeFinderPattern 0 0 g     -- top-left
        g2 = placeFinderPattern 0 18 g1   -- top-right
        g3 = placeFinderPattern 18 0 g2   -- bottom-left
        g4 = placeAlignmentPattern 18 18 g3  -- V2 alignment at (18,18)
        g5 = placeTimingPatterns g4
        g6 = setModule 17 8 True g5       -- dark module (V2)
    in g6

-- | Place a 7x7 finder pattern with top-left corner at (row, col).
placeFinderPattern :: Int -> Int -> [[Bool]] -> [[Bool]]
placeFinderPattern r0 c0 g = foldl' place g finderCells
  where
    finderCells =
        [ (r0 + dr, c0 + dc, val)
        | dr <- [0..6], dc <- [0..6]
        , let val = finderBit dr dc
        ]
    finderBit dr dc
        | dr == 0 || dr == 6 = True        -- top/bottom border
        | dc == 0 || dc == 6 = True        -- left/right border
        | dr >= 2 && dr <= 4
          && dc >= 2 && dc <= 4 = True      -- 3x3 center
        | otherwise = False
    place grid (r, c, v) = setModule r c v grid

-- | Place a 5x5 alignment pattern centered at (row, col).
placeAlignmentPattern :: Int -> Int -> [[Bool]] -> [[Bool]]
placeAlignmentPattern cr cc g = foldl' place g alignCells
  where
    alignCells =
        [ (cr + dr - 2, cc + dc - 2, val)
        | dr <- [0..4], dc <- [0..4]
        , let val = alignBit dr dc
        ]
    alignBit dr dc
        | dr == 0 || dr == 4 = True    -- top/bottom border
        | dc == 0 || dc == 4 = True    -- left/right border
        | dr == 2 && dc == 2 = True    -- center dot
        | otherwise = False
    place grid (r, c, v) = setModule r c v grid

-- | Place horizontal and vertical timing patterns on row 6 and column 6.
placeTimingPatterns :: [[Bool]] -> [[Bool]]
placeTimingPatterns g =
    let g1 = foldl' (\gr i -> setModule 6 i (even i) gr) g [8..16]
        g2 = foldl' (\gr i -> setModule i 6 (even i) gr) g1 [8..16]
    in g2

-- ---------------------------------------------------------------------------
-- Numeric data encoding
-- ---------------------------------------------------------------------------

-- | Encode a numeric string into data codewords (Version 2-L: 34 bytes).
encodeNumericData :: String -> [Word8]
encodeNumericData digits =
    let bitStream = numericModeBits digits
        padded   = padToCodewords 34 bitStream
    in bitsToBytes padded

-- | Produce the bit stream for numeric mode encoding.
numericModeBits :: String -> [Bool]
numericModeBits digits =
    let mode  = intToBits 4 1       -- 0001 = numeric mode
        count = intToBits 10 (length digits)
        dBits = encodeNumericGroups digits
        term  = intToBits 4 0       -- terminator
    in mode ++ count ++ dBits ++ term

-- | Encode groups of 3/2/1 digits into 10/7/4 bits respectively.
encodeNumericGroups :: String -> [Bool]
encodeNumericGroups [] = []
encodeNumericGroups ds
    | length ds >= 3 =
        let (grp, rest) = splitAt 3 ds
            val = safeReadInt grp
        in intToBits 10 val ++ encodeNumericGroups rest
    | length ds == 2 =
        let val = safeReadInt ds
        in intToBits 7 val
    | otherwise =
        let val = if isDigit (head ds) then digitToInt (head ds) else 0
        in intToBits 4 val

-- | Safely parse a numeric string, returning 0 on failure.
safeReadInt :: String -> Int
safeReadInt s = case reads s of { [(v,_)] -> v; _ -> 0 }

-- | Pad a bit stream to fill exactly n codewords (n * 8 bits).
padToCodewords :: Int -> [Bool] -> [Bool]
padToCodewords n bits =
    let target = n * 8
        -- Pad to byte boundary with zeros
        aligned = bits ++ replicate ((8 - length bits `mod` 8) `mod` 8) False
        -- Add alternating pad bytes 11101100, 00010001
        padBytes = cycle [0xEC, 0x11]
        needed   = (target - length aligned) `div` 8
        extra    = concatMap (intToBits 8) (take needed padBytes)
    in take target (aligned ++ extra)

-- ---------------------------------------------------------------------------
-- Reed-Solomon error correction in GF(256)
-- ---------------------------------------------------------------------------

-- | GF(256) with primitive polynomial 0x11D (x^8 + x^4 + x^3 + x^2 + 1).

-- | Multiply by alpha (x) in GF(256) with reduction polynomial 0x11D.
gfStep :: Word8 -> Word8
gfStep v =
    let shifted = fromIntegral v `shiftL` 1 :: Int
    in fromIntegral (if shifted >= 256 then shifted `xor` 0x11D else shifted)

-- | GF(256) exp table: gfExpTbl !! i = alpha^i for i in [0..511].
-- Extended to 512 entries to avoid modular indexing during multiply.
gfExpTbl :: [Word8]
gfExpTbl =
    let first255 = take 255 (iterate gfStep 1)
    in first255 ++ first255 ++ [1]  -- 511 entries

-- | GF(256) log table: gfLogTbl !! v = i such that alpha^i = v.
-- Index 0 is unused (log(0) is undefined).
gfLogTbl :: [Int]
gfLogTbl = buildTable (replicate 256 0) 0 (1 :: Word8)
  where
    buildTable tbl 255 _ = tbl
    buildTable tbl i v =
        let tbl' = replaceIdx (fromIntegral v) i tbl
        in buildTable tbl' (i + 1) (gfStep v)
    replaceIdx idx val lst =
        take idx lst ++ [val] ++ drop (idx + 1) lst

-- | Multiply two elements in GF(256).
gfMul :: Word8 -> Word8 -> Word8
gfMul 0 _ = 0
gfMul _ 0 = 0
gfMul a b =
    gfExpTbl !! (gfLogTbl !! fromIntegral a + gfLogTbl !! fromIntegral b)

-- | Raise a GF(256) element to a power.
gfPow :: Word8 -> Int -> Word8
gfPow _ 0 = 1
gfPow b e = gfMul b (gfPow b (e - 1))

-- | Build the RS generator polynomial for the given number of EC codewords.
-- Returns coefficients in order [x^0, x^1, ..., x^n] where coeff of x^n = 1.
rsGeneratorPoly :: Int -> [Word8]
rsGeneratorPoly n = foldl' mulByRoot [1] [0..n-1]
  where
    mulByRoot poly i = polyMulLinear poly (gfPow 2 i)

-- | Multiply polynomial by (x + root) in GF(256).
-- Polynomial represented as [coeff_0, coeff_1, ...] (ascending degree).
polyMulLinear :: [Word8] -> Word8 -> [Word8]
polyMulLinear [] _ = [0]
polyMulLinear poly root =
    let shifted  = 0 : poly            -- multiply by x
        extended = poly ++ [0]         -- extend for root*poly term
        scaled   = map (gfMul root) extended
    in zipWith xor shifted scaled

-- | Compute Reed-Solomon ECC codewords for the given data.
-- Uses systematic encoding: data codewords are not modified,
-- returns only the ECC codewords.
rsEncode :: Int -> [Word8] -> [Word8]
rsEncode nEcc dataCW =
    let gen = rsGeneratorPoly nEcc
        -- Process data bytes through the LFSR-style division
        initRegs = replicate nEcc (0 :: Word8)
        finalRegs = foldl' (rsStep gen nEcc) initRegs dataCW
    in finalRegs

-- | One step of RS polynomial division (LFSR style).
-- Register layout: regs !! 0 is the highest-degree coefficient.
rsStep :: [Word8] -> Int -> [Word8] -> Word8 -> [Word8]
rsStep gen nEcc regs dataByte =
    let feedback = dataByte `xor` head regs
        -- Shift register left and XOR with generator coefficients
        shifted = tail regs ++ [0]
        -- gen has n+1 coefficients; we use gen[n-1] down to gen[0]
        genCoeffs = reverse (take nEcc gen)
    in zipWith (\s gc -> s `xor` gfMul feedback gc) shifted genCoeffs

-- ---------------------------------------------------------------------------
-- Data placement (zigzag)
-- ---------------------------------------------------------------------------

-- | Place data bits onto the grid in the standard QR zigzag pattern.
placeDataBits :: [[Bool]] -> [Bool] -> [[Bool]]
placeDataBits grid bits =
    let coords = dataModuleCoords
        pairs  = zip coords (bits ++ repeat False)
    in foldl' (\g ((r,c), v) -> setModule r c v g) grid pairs

-- | Generate the list of (row, col) coordinates for data modules
-- in the standard QR zigzag order, skipping function pattern areas.
dataModuleCoords :: [(Int, Int)]
dataModuleCoords = concatMap columnPair colPairs
  where
    -- Column pairs from right to left, skipping column 6 (timing)
    allCols = [24, 22, 20, 18, 16, 14, 12, 10, 8, 5, 3, 1]
    colPairs = zip [0 :: Int ..] allCols
    columnPair (idx, c) =
        let rows = if even idx then [qrSize-1, qrSize-2 .. 0]
                               else [0 .. qrSize-1]
        in [ (r, cc)
           | r <- rows
           , cc <- [c, c - 1]
           , cc >= 0
           , not (isFunctionModule r cc)
           ]

-- | Check if a module position is reserved for function patterns.
isFunctionModule :: Int -> Int -> Bool
isFunctionModule r c =
    inFinderArea r c
    || inAlignmentArea r c
    || isTimingModule r c
    || isDarkModule r c
    || isFormatModule r c

-- | Check if (r,c) is within a finder pattern area (including separator).
inFinderArea :: Int -> Int -> Bool
inFinderArea r c =
    (r <= 8 && c <= 8)          -- top-left finder + separator
    || (r <= 8 && c >= 17)      -- top-right finder + separator
    || (r >= 17 && c <= 8)      -- bottom-left finder + separator

-- | Check if (r,c) is within the alignment pattern area (V2: centered at 18,18).
inAlignmentArea :: Int -> Int -> Bool
inAlignmentArea r c =
    r >= 16 && r <= 20 && c >= 16 && c <= 20

-- | Check if (r,c) is a timing pattern module.
isTimingModule :: Int -> Int -> Bool
isTimingModule r c = (r == 6 && c >= 8 && c <= 16)
                  || (c == 6 && r >= 8 && r <= 16)

-- | The dark module for Version 2 is at (17, 8).
isDarkModule :: Int -> Int -> Bool
isDarkModule 17 8 = True
isDarkModule _  _ = False

-- | Check if (r,c) is a format information module.
-- Format info is placed at specific positions that are not already
-- covered by the finder areas. These are the "extra" positions:
-- Row 8 cols 17-24 (already in finder area but also format)
-- Col 8 rows 18-24 (already in finder area but also format)
-- Since inFinderArea already covers all format positions, this
-- function only needs to return False for non-finder format positions.
-- In practice, all format info positions fall within inFinderArea
-- for V2, so this is a no-op.
isFormatModule :: Int -> Int -> Bool
isFormatModule _ _ = False

-- ---------------------------------------------------------------------------
-- Masking
-- ---------------------------------------------------------------------------

-- | Apply mask pattern 0: (row + col) mod 2 == 0 inverts data modules.
applyMask :: [[Bool]] -> [[Bool]]
applyMask grid =
    [ [ if not (isFunctionModule r c) && (r + c) `mod` 2 == 0
        then not (grid !! r !! c)
        else grid !! r !! c
      | c <- [0..qrSize-1]
      ]
    | r <- [0..qrSize-1]
    ]

-- ---------------------------------------------------------------------------
-- Format information
-- ---------------------------------------------------------------------------

-- | Place format information bits for ECC level L, mask pattern 0.
placeFormatInfo :: [[Bool]] -> [[Bool]]
placeFormatInfo grid =
    let fmtBits = formatInfoBits
        g1 = placeFormatCopy1 grid fmtBits
        g2 = placeFormatCopy2 g1 fmtBits
    in g2

-- | Compute the 15-bit format information for ECC L (01), mask 0 (000).
-- Data: 01 000 = 8 (in 5 bits). BCH(15,5) then XOR with mask pattern.
formatInfoBits :: [Bool]
formatInfoBits =
    let dataBits = 0x08  -- 01000 in binary: ECC L=01, mask=000
        encoded  = bchEncode dataBits
        masked   = encoded `xor` 0x5412  -- XOR with 101010000010010
    in intToBits 15 masked

-- | BCH(15,5) encoding. Generator polynomial: x^10+x^8+x^5+x^4+x^2+x+1 = 0x537.
bchEncode :: Int -> Int
bchEncode dat =
    let shifted = dat `shiftL` 10
        remainder = bchMod shifted
    in shifted .|. remainder

-- | Compute remainder of division by BCH generator 0x537 (degree 10).
-- Only reduces bits 14 down to 10; the result is the 10-bit remainder.
bchMod :: Int -> Int
bchMod val = go val 14
  where
    go v bit
        | bit < 10  = v .&. 0x3FF  -- mask to 10 bits
        | testBit v bit = go (v `xor` (0x537 `shiftL` (bit - 10))) (bit - 1)
        | otherwise     = go v (bit - 1)

-- | Place both copies of format information.
-- Copy 1: near top-left finder (split across row 8 and column 8).
-- Copy 2: near other two finders (row 8 right side, column 8 bottom side).
--
-- Per ISO 18004, the 15-bit format string f[0]..f[14] is placed at
-- these exact (row, col) positions. f[0] is the MSB (ECC level high bit).
placeFormatCopy1 :: [[Bool]] -> [Bool] -> [[Bool]]
placeFormatCopy1 grid bits =
    let -- Copy 1 horizontal: row 8, columns in specific order
        -- Copy 1 vertical: column 8, rows in specific order
        -- These interleave around the top-left finder.
        positions =
            -- bit index -> (row, col) per Table 9 of ISO 18004
            [ (0,  (8, 0))
            , (1,  (8, 1))
            , (2,  (8, 2))
            , (3,  (8, 3))
            , (4,  (8, 4))
            , (5,  (8, 5))
            , (6,  (8, 7))    -- skip col 6 (timing)
            , (7,  (8, 8))
            , (8,  (7, 8))    -- now going up column 8
            , (9,  (5, 8))    -- skip row 6 (timing)
            , (10, (4, 8))
            , (11, (3, 8))
            , (12, (2, 8))
            , (13, (1, 8))
            , (14, (0, 8))
            ]
    in foldl' (\g (i, (r,c)) -> setModule r c (bits !! i) g) grid positions

placeFormatCopy2 :: [[Bool]] -> [Bool] -> [[Bool]]
placeFormatCopy2 grid bits =
    let -- Copy 2: bits placed near the other two finders
        -- Horizontal part: row 8, right side (near top-right finder)
        -- Vertical part: column 8, bottom side (near bottom-left finder)
        positions =
            [ (0,  (24, 8))   -- bottom of col 8
            , (1,  (23, 8))
            , (2,  (22, 8))
            , (3,  (21, 8))
            , (4,  (20, 8))
            , (5,  (19, 8))
            , (6,  (18, 8))
            , (7,  (8, 17))   -- right side of row 8
            , (8,  (8, 18))
            , (9,  (8, 19))
            , (10, (8, 20))
            , (11, (8, 21))
            , (12, (8, 22))
            , (13, (8, 23))
            , (14, (8, 24))
            ]
    in foldl' (\g (i, (r,c)) -> setModule r c (bits !! i) g) grid positions

-- ---------------------------------------------------------------------------
-- Bit manipulation helpers
-- ---------------------------------------------------------------------------

-- | Convert an integer to a list of Bool bits (MSB first).
intToBits :: Int -> Int -> [Bool]
intToBits nBits val =
    [ testBit val (nBits - 1 - i) | i <- [0..nBits-1] ]

-- | Convert a list of bits to bytes (8 bits per byte, MSB first).
bitsToBytes :: [Bool] -> [Word8]
bitsToBytes [] = []
bitsToBytes bits =
    let (byte, rest) = splitAt 8 bits
        val = foldl' (\acc b -> acc `shiftL` 1 .|. if b then 1 else 0) 0 byte
    in val : bitsToBytes rest

-- | Convert codewords to a flat bit list.
codewordsToBits :: [Word8] -> [Bool]
codewordsToBits = concatMap (\w -> intToBits 8 (fromIntegral w))

-- ---------------------------------------------------------------------------
-- Grid helpers
-- ---------------------------------------------------------------------------

-- | Set a single module in the grid.
setModule :: Int -> Int -> Bool -> [[Bool]] -> [[Bool]]
setModule r c val grid
    | r < 0 || r >= qrSize || c < 0 || c >= qrSize = grid
    | otherwise =
        let row  = grid !! r
            row' = take c row ++ [val] ++ drop (c + 1) row
        in take r grid ++ [row'] ++ drop (r + 1) grid
