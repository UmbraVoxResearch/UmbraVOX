-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Layout
    ( clampSize, sizeValid, calcLayout, dropdownCol, inputAreaRows
    , chatPaneBounds, contactsPaneBounds, actionsPaneBounds, inputToolbarBounds, inputEntryBounds
    ) where

import UmbraVox.TUI.Constants
import UmbraVox.TUI.LayoutGrid (Bounds(..), TrackSpec(..), resolveTrackSizes)
import UmbraVox.TUI.Types (Layout(..), MenuTab(..), menuTabLabel)

inputAreaRows :: Int
inputAreaRows = 7

-- | Clamp terminal dimensions to supported bounds.
clampSize :: Int -> Int -> (Int, Int)
clampSize rows cols = (clamp minTermRows maxTermRows rows,
                       clamp minTermCols maxTermCols cols)
  where clamp lo hi v = max lo (min hi v)

-- | Check whether terminal dimensions are within supported bounds.
sizeValid :: Int -> Int -> Bool
sizeValid rows cols = rows >= minTermRows && rows <= maxTermRows
                   && cols >= minTermCols && cols <= maxTermCols

-- | Height of the identity panel (including the leading separator row).
-- Buttons removed (now in F5 Identity menu); panel shows QR, safety number,
-- and fingerprints only.  Fixed at exact content height to waste no rows.
-- Content: 14 QR + 1 safety label + safetyRows + 1 fp header + 2 fp rows
-- plus 1 separator = total identityPanelH.
identityPanelH :: Int -> Int -> Int
identityPanelH chatH leftW = max 0 (min (chatH - 3) exactRows)
  where
    qrRows = 14
    headerRows = 1       -- "Standard: X3DH safety number"
    fpHeaderRows = 1     -- "X25519:          Ed25519:"
    fpDataRows = 2       -- two rows of hex fingerprints
    innerW = max 1 (leftW - 2)
    groupsPerRow = max 1 (min 5 ((innerW + 1) `div` 6))
    safetyRows = (12 + groupsPerRow - 1) `div` groupsPerRow
    -- 1 separator + exact content rows — zero padding
    exactRows = 1 + qrRows + headerRows + safetyRows + fpHeaderRows + fpDataRows

-- | Compute the layout geometry from terminal dimensions.
-- Row budget: 1 menu + 1 separator + (chatH rows of content) + inputAreaRows + 1 status
-- inputAreaRows includes: 1 toolbar row + 1 box-top + entryRows content + 1 box-bottom
calcLayout :: Int -> Int -> Layout
calcLayout rows cols = Layout
    { lCols        = cols
    , lRows        = rows
    , lLeftW       = leftW
    , lRightW      = rightW
    , lChatH       = chatH
    , lIdentityH   = identityPanelH chatH leftW
    , lToolbarRow  = 2 + chatH + 1  -- contentTop(2) + chatH rows + mid-border(1) = chatH+3
    }
  where
    rowTracks =
        [ Fixed 1
        , Fixed 1
        , Flexible Bounds { bMin = 0, bMax = max 0 rows }
        , Fixed inputAreaRows
        , Fixed 1
        ]
    [_, _, chatH, _, _] = resolveTrackSizes rows rowTracks
    -- Left pane: just wide enough for QR (~27 chars) and fingerprints (~40 chars).
    -- Formula: max 20 (min 42 (cols/4)) — compact but usable.
    minRightW = 48
    leftW = max minLeftPaneW (min 42 (min (cols `div` 4) (cols - minRightW)))
    rightW = max minRightW (cols - leftW)

-- | Compute the column position for a dropdown menu under the given tab.
-- Tabs are right-justified in the header, so include left-side fill padding.
dropdownCol :: Int -> MenuTab -> Int
dropdownCol totalW tab =
    let labels = map menuTabLabel [minBound..maxBound]
        idx = fromEnum tab
        preceding = take idx labels
        tabsContentW = 1 + sum (map (\l -> length l + 1) labels)
        fillW = max 0 (totalW - tabsContentW - 2)
    in 3 + fillW + sum (map (\l -> length l + 1) preceding)

-- | Interior bounds of the chat pane (right side), excluding borders.
-- Returns (row0, col0, width, height).
chatPaneBounds :: Layout -> (Int, Int, Int, Int)
chatPaneBounds lay =
    let r0 = 2                    -- row 1 is menu bar
        c0 = lLeftW lay + 1       -- divider at lLeftW, interior starts after it
        w  = max 1 (lRightW lay - 1)
        h  = max 1 (lChatH lay)
    in (r0, c0, w, h)

-- | Interior bounds of the contacts pane (left side), excluding borders.
-- Returns (row0, col0, width, height).
contactsPaneBounds :: Layout -> (Int, Int, Int, Int)
contactsPaneBounds lay =
    let r0 = 2
        c0 = 2
        w  = max 1 (lLeftW lay - 2)
        h  = max 1 (lChatH lay - lIdentityH lay)
    in (r0, c0, w, h)

-- | Interior bounds of the action strip in the left pane, excluding borders.
-- Returns (row0, col0, width, height).
actionsPaneBounds :: Layout -> (Int, Int, Int, Int)
actionsPaneBounds lay =
    let inputTop = lChatH lay + 3
        actionRows = 4
        r0 = inputTop + max 0 (inputAreaRows - actionRows)
        c0 = 2
        w  = max 1 (lLeftW lay - 2)
        h  = actionRows
    in (r0, c0, w, h)

-- | Interior bounds of the right-side input text-entry box, excluding borders.
-- Row 0 of inputAreaRows is the toolbar; row 1 is the box top border;
-- content starts at row 2.  Height = inputAreaRows - 3 (toolbar + top + bottom).
-- Returns (row0, col0, width, height).
inputEntryBounds :: Layout -> (Int, Int, Int, Int)
inputEntryBounds lay =
    let inputTop = lChatH lay + 3
        c0 = lLeftW lay + 1
        w = max 1 (lRightW lay - 1)
        h = max 1 (inputAreaRows - 3)
    in (inputTop + 2, c0, w, h)

-- | Interior bounds of the right-side input toolbar row, excluding borders.
-- Returns (row0, col0, width, height).
inputToolbarBounds :: Layout -> (Int, Int, Int, Int)
inputToolbarBounds lay =
    let inputTop = lChatH lay + 3
        c0 = lLeftW lay + 1
        w = max 1 (lRightW lay - 1)
    in (inputTop, c0, w, 1)
