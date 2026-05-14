-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.RichText
    ( RichStyle(..)
    , RichChar(..)
    , WrappedRichLine(..)
    , RichInputLayout(..)
    , computeRichInputLayout
    , richCursorScreenOffset
    , richCursorIndexForScreenOffset
    , richCursorIndexForLineColumn
    , richEnsureCursorVisible
    , richCursorLineIndex
    , richCursorTextDisplayCol
    , richClampCursor
    , richMoveCursorLeft
    , richMoveCursorRight
    , richVisibleLines
    , richShowScrollbar
    , richContentW
    , richMaxScroll
    , renderRichCharsPadded
    , renderRichCharsPaddedSel
    , renderMarkdownLinePadded
    , parseEmojiInput
    ) where

import Data.Char (isAlphaNum, ord, toLower)
import Data.List (find, nub, stripPrefix)
import UmbraVox.TUI.Terminal (csi, resetSGR, setFg, bold)
import UmbraVox.TUI.Text (displayWidth, splitAtWidth)

data RichStyle = RichStyle
    { rsBold :: Bool
    , rsItalic :: Bool
    , rsUnderline :: Bool
    , rsColor :: Maybe Int
    } deriving stock (Eq, Show)

data RichChar = RichChar
    { rcChar :: Char
    , rcStyle :: RichStyle
    , rcRawBefore :: Int
    , rcRawAfter :: Int
    } deriving stock (Eq, Show)

data WrappedRichLine = WrappedRichLine
    { wrlChars :: [RichChar]
    , wrlStartRaw :: Int
    , wrlEndRaw :: Int
    } deriving stock (Eq, Show)

data RichInputLayout = RichInputLayout
    { rilWrapped :: [WrappedRichLine]
    , rilShowScrollbar :: Bool
    , rilBodyW :: Int
    , rilContentW :: Int
    , rilTextW :: Int
    , rilEntryRows :: Int
    , rilMaxScroll :: Int
    , rilCursorStops :: [Int]
    } deriving stock (Eq, Show)

defaultStyle :: RichStyle
defaultStyle = RichStyle False False False Nothing

inputPrefix :: String
inputPrefix = " \x25B8 "

continuationPrefix :: String
continuationPrefix = replicate (displayWidth inputPrefix) ' '

fontCloseTag :: String
fontCloseTag = "</font>"

computeRichInputLayout :: Int -> Int -> String -> RichInputLayout
computeRichInputLayout bodyW entryRows buf =
    let prefixW = displayWidth inputPrefix
        baseTextW = max 1 (bodyW - prefixW)
        baseWrapped = wrapRichBuffer baseTextW buf
        showScrollbar = length baseWrapped > max 1 entryRows
        sbW = if showScrollbar then 1 else 0
        textW = max 1 (bodyW - prefixW - sbW)
        wrapped = wrapRichBuffer textW buf
        maxOff = max 0 (length wrapped - max 1 entryRows)
    in RichInputLayout
        { rilWrapped = wrapped
        , rilShowScrollbar = showScrollbar
        , rilBodyW = bodyW
        , rilContentW = bodyW - sbW
        , rilTextW = textW
        , rilEntryRows = max 1 entryRows
        , rilMaxScroll = maxOff
        , rilCursorStops = cursorStopsFor wrapped
        }

richCursorScreenOffset :: RichInputLayout -> Int -> Int -> Maybe (Int, Int)
richCursorScreenOffset layout scrollOff rawCursor =
    let cursor = richClampCursor layout rawCursor
        lineIx = richCursorLineIndex layout cursor
        start = richVisibleStart layout scrollOff
        rowOff = lineIx - start
    in if rowOff < 0 || rowOff >= rilEntryRows layout
        then Nothing
        else
            let prefixW = if lineIx == 0 then displayWidth inputPrefix else displayWidth continuationPrefix
                colOff = prefixW + richCursorTextDisplayCol layout cursor
            in Just (rowOff, colOff)

richCursorIndexForScreenOffset :: RichInputLayout -> Int -> Int -> Int -> Int
richCursorIndexForScreenOffset layout scrollOff rowOff colOff =
    case richLineAt layout lineIx of
        Nothing -> 0
        Just line ->
            let prefixW = if lineIx == 0 then displayWidth inputPrefix else displayWidth continuationPrefix
                textCol = max 0 (colOff - prefixW)
            in richCursorIndexForChars line textCol
  where
    start = richVisibleStart layout scrollOff
    lineIx = max 0 (min (max 0 (length (rilWrapped layout) - 1)) (start + rowOff))

richCursorIndexForLineColumn :: RichInputLayout -> Int -> Int -> Int
richCursorIndexForLineColumn layout lineIx textCol =
    case richLineAt layout clamped of
        Nothing -> 0
        Just line -> richCursorIndexForChars line textCol
  where
    clamped = max 0 (min (max 0 (length (rilWrapped layout) - 1)) lineIx)

richEnsureCursorVisible :: RichInputLayout -> Int -> Int -> Int
richEnsureCursorVisible layout scrollOff rawCursor =
    let total = length (rilWrapped layout)
        entryRows = rilEntryRows layout
        maxOff = rilMaxScroll layout
        currentOff = clampScroll layout scrollOff
        currentStart = richVisibleStart layout currentOff
        currentEnd = currentStart + entryRows - 1
        lineIx = richCursorLineIndex layout rawCursor
        targetStart
            | lineIx < currentStart = lineIx
            | lineIx > currentEnd = lineIx - entryRows + 1
            | otherwise = currentStart
        newOff = total - entryRows - targetStart
    in max 0 (min maxOff newOff)

richClampCursor :: RichInputLayout -> Int -> Int
richClampCursor layout rawCursor =
    case rilCursorStops layout of
        [] -> 0
        stops -> nearestStop rawCursor stops

richMoveCursorLeft :: RichInputLayout -> Int -> Int
richMoveCursorLeft layout rawCursor =
    case filter (< richClampCursor layout rawCursor) (rilCursorStops layout) of
        [] -> richClampCursor layout rawCursor
        xs -> last xs

richMoveCursorRight :: RichInputLayout -> Int -> Int
richMoveCursorRight layout rawCursor =
    case filter (> richClampCursor layout rawCursor) (rilCursorStops layout) of
        [] -> richClampCursor layout rawCursor
        (x:_) -> x

richVisibleLines :: RichInputLayout -> Int -> [[RichChar]]
richVisibleLines layout scrollOff =
    let start = richVisibleStart layout scrollOff
    in map wrlChars (take (rilEntryRows layout) (drop start (rilWrapped layout)))

richShowScrollbar :: RichInputLayout -> Bool
richShowScrollbar = rilShowScrollbar

richContentW :: RichInputLayout -> Int
richContentW = rilContentW

richMaxScroll :: RichInputLayout -> Int
richMaxScroll = rilMaxScroll

renderRichCharsPadded :: Int -> Maybe Int -> [RichChar] -> IO ()
renderRichCharsPadded width mCursor chars =
    renderRichCharsPaddedSel width mCursor Nothing Nothing chars

-- | Render rich chars with optional cursor and optional selection highlight.
-- mSelRange is (selLo, selHi) in raw buffer indices; characters whose
-- rcRawBefore falls in [selLo, selHi) are rendered with inverted colours.
renderRichCharsPaddedSel :: Int -> Maybe Int -> Maybe Int -> Maybe Int -> [RichChar] -> IO ()
renderRichCharsPaddedSel width mCursor mSelLo mSelHi chars = do
    let endCol = sum (map (displayWidth . (:[] ) . rcChar) chars)
    go 0 chars
    let usedW = if mCursor == Just endCol then min width (endCol + 1) else min width endCol
    if mCursor == Just endCol && endCol < width
        then putStr "\x2588" >> putStr (replicate (max 0 (width - usedW)) ' ')
        else putStr (replicate (max 0 (width - usedW)) ' ')
    resetSGR
  where
    inSelection ch = case (mSelLo, mSelHi) of
        (Just lo, Just hi) -> rcRawBefore ch >= lo && rcRawBefore ch < hi
        _ -> False
    go _ [] = pure ()
    go col (ch:rest)
        | col >= width = pure ()
        | otherwise = do
            let charW = displayWidth [rcChar ch]
                drawCursor = mCursor == Just col
                sel = inSelection ch
            applyStyle (rcStyle ch)
            if drawCursor
                then csi "7m" >> putChar (rcChar ch) >> resetSGR
                else if sel
                    then csi "7m" >> putChar (rcChar ch) >> resetSGR
                    else putChar (rcChar ch)
            go (col + charW) rest

renderMarkdownLinePadded :: Int -> Bool -> String -> IO ()
renderMarkdownLinePadded width enabled rawLine =
    if enabled
        then renderRichCharsPadded width Nothing (trimRichCharsToWidth width (projectLine 0 rawLine))
        else putStr (padToWidth width rawLine)

parseEmojiInput :: String -> Maybe String
parseEmojiInput input
    | null trimmed = Nothing
    | otherwise =
        case stripPrefix "U+" upper <|> stripPrefix "0X" upper of
            Just hexDigits -> (:[]) <$> codepointToChar hexDigits
            Nothing
                | all isHex trimmed -> (:[]) <$> codepointToChar upper
                | otherwise -> Just trimmed
  where
    trimmed = trim input
    upper = map toLower trimmed
    isHex ch = isAlphaNum ch && ch `elem` ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

padToWidth :: Int -> String -> String
padToWidth width txt =
    let (shown, _) = splitAtWidth width txt
    in shown ++ replicate (max 0 (width - displayWidth shown)) ' '

trimRichCharsToWidth :: Int -> [RichChar] -> [RichChar]
trimRichCharsToWidth width = go 0
  where
    go _ [] = []
    go acc (ch:rest)
        | next > width = []
        | otherwise = ch : go next rest
      where
        next = acc + displayWidth [rcChar ch]

wrapRichBuffer :: Int -> String -> [WrappedRichLine]
wrapRichBuffer width txt
    | null txt = [WrappedRichLine [] 0 0]
    | otherwise = go 0 txt
  where
    go start [] = [WrappedRichLine [] start start]
    go start s =
        case break (== '\n') s of
            (line, []) ->
                wrapProjected start (projectLine start line)
            (line, _:rest) ->
                wrapProjected start (projectLine start line)
                ++ go (start + length line + 1) rest

    wrapProjected start [] = [WrappedRichLine [] start start]
    wrapProjected start chars =
        let (chunk, rest) = splitRichAtWidth width chars
            lineStart = rcRawBefore (head chunk)
            lineEnd = rcRawAfter (last chunk)
            current = WrappedRichLine chunk lineStart lineEnd
        in if null rest then [current] else current : wrapProjected lineEnd rest

splitRichAtWidth :: Int -> [RichChar] -> ([RichChar], [RichChar])
splitRichAtWidth width = go 0 []
  where
    go _ acc [] = (reverse acc, [])
    go accW acc xs@(ch:rest)
        | null acc = keepOrTake xs
        | accW + charW <= width = go (accW + charW) (ch:acc) rest
        | otherwise = (reverse acc, xs)
      where
        charW = displayWidth [rcChar ch]
        keepOrTake ys@(y:ysRest)
            | displayWidth [rcChar y] > width = ([y], ysRest)
            | otherwise = go (displayWidth [rcChar y]) [y] ysRest
        keepOrTake [] = (reverse acc, [])

cursorStopsFor :: [WrappedRichLine] -> [Int]
cursorStopsFor wrapped =
    nub (concatMap lineStops wrapped)
  where
    lineStops line =
        case wrlChars line of
            [] -> [wrlStartRaw line]
            xs -> wrlStartRaw line : map rcRawAfter xs

nearestStop :: Int -> [Int] -> Int
nearestStop rawCursor stops =
    case break (> rawCursor) stops of
        ([], next:_) -> next
        (prevs, []) -> last prevs
        (prevs, next:_) ->
            let prev = last prevs
            in if rawCursor - prev <= next - rawCursor then prev else next

richVisibleStart :: RichInputLayout -> Int -> Int
richVisibleStart layout scrollOff =
    let total = length (rilWrapped layout)
        entryRows = rilEntryRows layout
        off = clampScroll layout scrollOff
    in max 0 (total - entryRows - off)

clampScroll :: RichInputLayout -> Int -> Int
clampScroll layout off = max 0 (min (rilMaxScroll layout) off)

richCursorLineIndex :: RichInputLayout -> Int -> Int
richCursorLineIndex layout rawCursor =
    let cursor = richClampCursor layout rawCursor
        wrapped = rilWrapped layout
    in case findIndexLine 0 wrapped cursor of
        Just ix -> ix
        Nothing -> max 0 (length wrapped - 1)

findIndexLine :: Int -> [WrappedRichLine] -> Int -> Maybe Int
findIndexLine _ [] _ = Nothing
findIndexLine ix (line:rest) cursor
    | cursor <= wrlEndRaw line || null (wrlChars line) = Just ix
    | otherwise = findIndexLine (ix + 1) rest cursor

richCursorTextDisplayCol :: RichInputLayout -> Int -> Int
richCursorTextDisplayCol layout rawCursor =
    case richLineAt layout (richCursorLineIndex layout rawCursor) of
        Nothing -> 0
        Just line -> widthBeforeCursor line (richClampCursor layout rawCursor)

richLineAt :: RichInputLayout -> Int -> Maybe WrappedRichLine
richLineAt layout ix
    | ix < 0 || ix >= length (rilWrapped layout) = Nothing
    | otherwise = Just (rilWrapped layout !! ix)

widthBeforeCursor :: WrappedRichLine -> Int -> Int
widthBeforeCursor line cursor =
    go 0 (wrlChars line)
  where
    go acc [] = acc
    go acc (ch:rest)
        | cursor <= rcRawBefore ch = acc
        | cursor < rcRawAfter ch = acc
        | otherwise = go (acc + displayWidth [rcChar ch]) rest

richCursorIndexForChars :: WrappedRichLine -> Int -> Int
richCursorIndexForChars line targetCol =
    go (wrlStartRaw line) 0 (wrlChars line)
  where
    go current acc [] = current
    go _ acc (ch:rest)
        | acc >= targetCol = rcRawBefore ch
        | next > targetCol = rcRawBefore ch
        | otherwise = go (rcRawAfter ch) next rest
      where
        next = acc + displayWidth [rcChar ch]

projectLine :: Int -> String -> [RichChar]
projectLine start txt = go defaultStyle start txt
  where
    go _ _ [] = []
    go style rawPos s
        | Just (colorName, afterOpen) <- parseFontOpen s
        , Just (inner, afterClose) <- breakOn fontCloseTag afterOpen =
            let innerStart = rawPos + (length s - length afterOpen)
                closingPos = innerStart + length inner
                styled = style { rsColor = colorCode colorName <|> rsColor style }
            in go styled innerStart inner
               ++ go style (closingPos + length fontCloseTag) afterClose
        | Just rest <- stripPrefix "**" s
        , Just (inner, afterClose) <- breakOn "**" rest =
            let innerStart = rawPos + 2
                closingPos = innerStart + length inner
                styled = style { rsBold = True }
            in go styled innerStart inner
               ++ go style (closingPos + 2) afterClose
        | Just rest <- stripPrefix "*" s
        , Just (inner, afterClose) <- breakOn "*" rest =
            let innerStart = rawPos + 1
                closingPos = innerStart + length inner
                styled = style { rsItalic = True }
            in go styled innerStart inner
               ++ go style (closingPos + 1) afterClose
        | Just rest <- stripPrefix "[" s
        , Just (label, afterLabel) <- breakOn "](" rest
        , Just (url, afterUrl) <- breakOn ")" afterLabel =
            let labelStart = rawPos + 1
                urlStart = labelStart + length label + 2
                styled = style
                    { rsUnderline = True
                    , rsColor = rsColor style <|> Just 36
                    }
            in go styled labelStart label
               ++ go style (urlStart + length url + 1) afterUrl
        | otherwise =
            let ch = head s
                rest = tail s
            in RichChar ch style rawPos (rawPos + 1) : go style (rawPos + 1) rest

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack = go [] haystack
  where
    go _ [] = Nothing
    go acc s
        | Just rest <- stripPrefix needle s = Just (reverse acc, rest)
        | otherwise = go (head s : acc) (tail s)

parseFontOpen :: String -> Maybe (String, String)
parseFontOpen s = do
    rest0 <- stripPrefix "<font" s
    rest1 <- stripLeadingSpace rest0
    rest2 <- stripPrefix "color" rest1
    rest3 <- stripLeadingSpace rest2
    rest4 <- stripPrefix "=" rest3
    quoteRest <- stripLeadingSpace rest4
    case quoteRest of
        ('"':xs) -> do
            let (colorName, suffix) = span (/= '"') xs
            rest5 <- stripPrefix "\"" suffix
            rest6 <- stripLeadingSpace rest5
            rest7 <- stripPrefix ">" rest6
            Just (map toLower colorName, rest7)
        _ -> Nothing

stripLeadingSpace :: String -> Maybe String
stripLeadingSpace = Just . dropWhile (== ' ')

colorCode :: String -> Maybe Int
colorCode name =
    lookup (map toLower name)
        [ ("black", 30), ("red", 31), ("green", 32), ("yellow", 33)
        , ("blue", 34), ("magenta", 35), ("cyan", 36), ("white", 37)
        ]

applyStyle :: RichStyle -> IO ()
applyStyle style = do
    resetSGR
    case rsColor style of
        Just color -> setFg color
        Nothing -> pure ()
    if rsBold style then bold else pure ()
    if rsItalic style then csi "3m" else pure ()
    if rsUnderline style then csi "4m" else pure ()

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

codepointToChar :: String -> Maybe Char
codepointToChar hexDigits =
    case readHexInt hexDigits of
        Just n | n >= 0 && n <= 0x10FFFF -> Just (toEnum n)
        _ -> Nothing

readHexInt :: String -> Maybe Int
readHexInt [] = Nothing
readHexInt xs = foldl step (Just 0) xs
  where
    step Nothing _ = Nothing
    step (Just acc) ch
        | ch >= '0' && ch <= '9' = Just (acc * 16 + ord ch - ord '0')
        | lower >= 'a' && lower <= 'f' = Just (acc * 16 + 10 + ord lower - ord 'a')
        | otherwise = Nothing
      where
        lower = toLower ch

infixl 3 <|>
(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) (Just x) _ = Just x
(<|>) Nothing y = y
