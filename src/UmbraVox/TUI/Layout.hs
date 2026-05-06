-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Layout
    ( clampSize, sizeValid, calcLayout, dropdownCol
    ) where

import UmbraVox.TUI.Constants
import UmbraVox.TUI.Types (Layout(..), MenuTab(..), menuTabLabel)

-- | Clamp terminal dimensions to supported bounds.
clampSize :: Int -> Int -> (Int, Int)
clampSize rows cols = (clamp minTermRows maxTermRows rows,
                       clamp minTermCols maxTermCols cols)
  where clamp lo hi v = max lo (min hi v)

-- | Check whether terminal dimensions are within supported bounds.
sizeValid :: Int -> Int -> Bool
sizeValid rows cols = rows >= minTermRows && rows <= maxTermRows
                   && cols >= minTermCols && cols <= maxTermCols

-- | Compute the layout geometry from terminal dimensions.
-- Row budget: 1 menu + 1 separator + (chatH rows of content) + 1 input + 1 status
calcLayout :: Int -> Int -> Layout
calcLayout rows cols = Layout
    { lCols   = cols
    , lRows   = rows
    , lLeftW  = leftW
    , lRightW = cols - leftW
    , lChatH  = rows - 4  -- 1 menu + 1 separator + 1 input + 1 status
    }
  where
    leftW = max minLeftPaneW (cols `div` leftPaneRatio)

-- | Compute the column position for a dropdown menu under the given tab.
dropdownCol :: MenuTab -> Int
dropdownCol tab =
    let labels = map menuTabLabel [minBound..maxBound]
        idx = fromEnum tab
        preceding = take idx labels
    in 2 + sum (map (\l -> length l + 1) preceding)
