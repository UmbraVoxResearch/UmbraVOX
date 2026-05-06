-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Constants
    ( maxInputLen, maxDialogBufLen
    , minTermRows, maxTermRows, minTermCols, maxTermCols
    , defaultTermRows, defaultTermCols
    , minLeftPaneW, leftPaneRatio
    , maxOverlayW, overlayMinMargin, minDropdownW
    ) where

maxInputLen, maxDialogBufLen :: Int
maxInputLen = 4096
maxDialogBufLen = 4096

minTermRows, maxTermRows, minTermCols, maxTermCols :: Int
minTermRows = 24; maxTermRows = 100; minTermCols = 80; maxTermCols = 300

defaultTermRows, defaultTermCols :: Int
defaultTermRows = 24; defaultTermCols = 80

minLeftPaneW, leftPaneRatio :: Int
minLeftPaneW = 20; leftPaneRatio = 4

maxOverlayW, overlayMinMargin, minDropdownW :: Int
maxOverlayW = 60; overlayMinMargin = 4; minDropdownW = 16
