-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.InputBuffer
    ( WrappedInputLine(..)
    , InputBufferLayout(..)
    , inputPrefix
    , continuationPrefix
    , computeInputBufferLayout
    , clampInputCursor
    , clampInputScroll
    , visibleInputStart
    , visibleInputLines
    , cursorLineIndex
    , cursorTextDisplayCol
    , cursorScreenOffset
    , cursorIndexForScreenOffset
    , cursorIndexForLineColumn
    , ensureCursorVisible
    ) where

import UmbraVox.TUI.Text (displayWidth, splitAtWidth)

data WrappedInputLine = WrappedInputLine
    { wilText :: String
    , wilStart :: Int
    , wilEnd :: Int
    } deriving stock (Eq, Show)

data InputBufferLayout = InputBufferLayout
    { iblWrapped :: [WrappedInputLine]
    , iblRendered :: [String]
    , iblShowScrollbar :: Bool
    , iblBodyW :: Int
    , iblContentW :: Int
    , iblTextW :: Int
    , iblEntryRows :: Int
    , iblMaxScroll :: Int
    } deriving stock (Eq, Show)

inputPrefix :: String
inputPrefix = " \x25B8 "

continuationPrefix :: String
continuationPrefix = replicate (displayWidth inputPrefix) ' '

-- | Two-pass layout: first wrap at full width to decide scrollbar visibility,
-- then re-wrap at narrower width if a scrollbar is needed.
--
-- R1.8.3 safety note: the scrollbar decision is locked from pass 1 and never
-- re-evaluated.  Pass 2 (narrower width) can only produce equal or more
-- wrapped lines, so the decision "needs scrollbar" remains valid.  When
-- pass 1 says "no scrollbar", no narrowing occurs (textW == baseTextW),
-- so there is no oscillation.
computeInputBufferLayout :: Int -> Int -> String -> InputBufferLayout
computeInputBufferLayout bodyW entryRows buf =
    let prefixW = displayWidth inputPrefix
        baseTextW = max 1 (bodyW - prefixW)
        baseWrapped = wrapInputBuffer baseTextW buf
        baseRendered = renderWrappedLines baseWrapped
        showScrollbar = length baseRendered > entryRows
        sbW = if showScrollbar then 1 else 0
        textW = max 1 (bodyW - prefixW - sbW)
        wrapped = wrapInputBuffer textW buf
        rendered = renderWrappedLines wrapped
        maxOff = max 0 (length rendered - entryRows)
    in InputBufferLayout
        { iblWrapped = wrapped
        , iblRendered = rendered
        , iblShowScrollbar = showScrollbar
        , iblBodyW = bodyW
        , iblContentW = bodyW - sbW
        , iblTextW = textW
        , iblEntryRows = max 1 entryRows
        , iblMaxScroll = maxOff
        }

clampInputCursor :: String -> Int -> Int
clampInputCursor buf cursor = max 0 (min (length buf) cursor)

clampInputScroll :: InputBufferLayout -> Int -> Int
clampInputScroll layout off = max 0 (min (iblMaxScroll layout) off)

visibleInputStart :: InputBufferLayout -> Int -> Int
visibleInputStart layout off =
    let total = length (iblRendered layout)
        entryRows = iblEntryRows layout
        scrollOff = clampInputScroll layout off
    in max 0 (total - entryRows - scrollOff)

visibleInputLines :: InputBufferLayout -> Int -> [String]
visibleInputLines layout off =
    let start = visibleInputStart layout off
    in take (iblEntryRows layout) (drop start (iblRendered layout))

cursorLineIndex :: InputBufferLayout -> Int -> Int
cursorLineIndex layout cursor =
    let wrapped = iblWrapped layout
        clamped = clampCursorForLayout layout cursor
    in case wrapped of
        [] -> 0
        _ -> go clamped wrapped 0
      where
        go _ [] ix = max 0 (ix - 1)
        go target (line:rest) ix
            | target <= wilEnd line = ix
            | otherwise = go target rest (ix + 1)

cursorTextDisplayCol :: InputBufferLayout -> Int -> Int
cursorTextDisplayCol layout cursor =
    case safeLineAt layout (cursorLineIndex layout cursor) of
        Nothing -> 0
        Just line ->
            let clamped = clampCursorForLayout layout cursor
                localChars = max 0 (min (length (wilText line)) (clamped - wilStart line))
            in displayColForCharCount localChars (wilText line)

cursorScreenOffset :: InputBufferLayout -> Int -> Int -> Maybe (Int, Int)
cursorScreenOffset layout scrollOff cursor =
    let lineIx = cursorLineIndex layout cursor
        start = visibleInputStart layout scrollOff
        rowOff = lineIx - start
    in if rowOff < 0 || rowOff >= iblEntryRows layout
        then Nothing
        else
            let prefixW = if lineIx == 0 then displayWidth inputPrefix else displayWidth continuationPrefix
                colOff = prefixW + cursorTextDisplayCol layout cursor
            in Just (rowOff, colOff)

cursorIndexForScreenOffset :: InputBufferLayout -> Int -> Int -> Int -> Int
cursorIndexForScreenOffset layout scrollOff rowOff colOff =
    case safeLineAt layout lineIx of
        Nothing -> 0
        Just line ->
            let prefixW = if lineIx == 0 then displayWidth inputPrefix else displayWidth continuationPrefix
                textCol = max 0 (colOff - prefixW)
                charOff = charCountAtDisplayCol textCol (wilText line)
            in wilStart line + charOff
  where
    start = visibleInputStart layout scrollOff
    lineIx = max 0 (min (max 0 (length (iblWrapped layout) - 1)) (start + rowOff))

cursorIndexForLineColumn :: InputBufferLayout -> Int -> Int -> Int
cursorIndexForLineColumn layout lineIx textCol =
    case safeLineAt layout clampedLine of
        Nothing -> 0
        Just line ->
            let charOff = charCountAtDisplayCol textCol (wilText line)
            in wilStart line + charOff
  where
    clampedLine = max 0 (min (max 0 (length (iblWrapped layout) - 1)) lineIx)

ensureCursorVisible :: InputBufferLayout -> Int -> Int -> Int
ensureCursorVisible layout scrollOff cursor =
    let total = length (iblRendered layout)
        entryRows = iblEntryRows layout
        maxOff = iblMaxScroll layout
        currentOff = clampInputScroll layout scrollOff
        currentStart = visibleInputStart layout currentOff
        currentEnd = currentStart + entryRows - 1
        lineIx = cursorLineIndex layout cursor
        targetStart
            | lineIx < currentStart = lineIx
            | lineIx > currentEnd = lineIx - entryRows + 1
            | otherwise = currentStart
        newOff = total - entryRows - targetStart
    in max 0 (min maxOff newOff)

wrapInputBuffer :: Int -> String -> [WrappedInputLine]
wrapInputBuffer width txt
    | null txt = [WrappedInputLine "" 0 0]
    | otherwise = go 0 txt
  where
    go start [] = [WrappedInputLine "" start start]
    go start s =
        case break (== '\n') s of
            (line, []) ->
                wrapSegment start line
            (line, _:rest) ->
                wrapSegment start line ++ go (start + length line + 1) rest

    wrapSegment start line
        | null line = [WrappedInputLine "" start start]
        | otherwise = loop start line

    loop start s =
        let (chunk, rest) = splitAtWidth width s
            end = start + length chunk
            current = WrappedInputLine chunk start end
        in if null rest then [current] else current : loop end rest

renderWrappedLines :: [WrappedInputLine] -> [String]
renderWrappedLines wrapped =
    case wrapped of
        [] -> [inputPrefix]
        _ -> zipWith renderLine [0 :: Int ..] wrapped
  where
    renderLine 0 line = inputPrefix ++ wilText line
    renderLine _ line = continuationPrefix ++ wilText line

clampCursorForLayout :: InputBufferLayout -> Int -> Int
clampCursorForLayout layout cursor =
    case iblWrapped layout of
        [] -> 0
        xs -> let maxCursor = maximum (map wilEnd xs)
              in max 0 (min maxCursor cursor)

safeLineAt :: InputBufferLayout -> Int -> Maybe WrappedInputLine
safeLineAt layout ix
    | ix < 0 || ix >= length (iblWrapped layout) = Nothing
    | otherwise = Just (iblWrapped layout !! ix)

displayColForCharCount :: Int -> String -> Int
displayColForCharCount n = go 0 0
  where
    go acc _ [] = acc
    go acc count xs
        | count >= n = acc
    go acc count (ch:rest) = go (acc + displayWidth [ch]) (count + 1) rest

charCountAtDisplayCol :: Int -> String -> Int
charCountAtDisplayCol target = go 0 0
  where
    go count _ [] = count
    go count acc xs
        | acc >= target = count
    go count acc (ch:rest) =
        let next = acc + displayWidth [ch]
        in if next > target
            then count
            else go (count + 1) next rest
