-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.RuntimeEvent
    ( RuntimeEvent(..)
    , applyRuntimeEvent
    , applyRuntimeEvents
    ) where

import Control.Monad (forM_)
import Data.IORef (writeIORef)
import UmbraVox.TUI.Types

data RuntimeEvent
    = EventSetStatus String
    | EventSetDialog (Maybe DialogMode)
    | EventSetInput String
    | EventSetDialogTab Int
    | EventResetBrowse
    deriving stock (Eq, Show)

applyRuntimeEvent :: AppState -> RuntimeEvent -> IO ()
applyRuntimeEvent st evt =
    case evt of
        EventSetStatus msg ->
            writeIORef (asStatusMsg st) msg
        EventSetDialog mode ->
            writeIORef (asDialogMode st) mode
        EventSetInput buf ->
            writeIORef (asInputBuf st) buf
        EventSetDialogTab tabIx ->
            writeIORef (asDialogTab st) tabIx
        EventResetBrowse -> do
            writeIORef (asBrowsePage st) 0
            writeIORef (asBrowseFilter st) ""

applyRuntimeEvents :: AppState -> [RuntimeEvent] -> IO ()
applyRuntimeEvents st evts = forM_ evts (applyRuntimeEvent st)
