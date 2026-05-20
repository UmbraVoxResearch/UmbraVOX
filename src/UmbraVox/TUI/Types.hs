-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Types
    ( SessionId
    , ContactStatus(..)
    , statusTag
    , SessionInfo(..)
    , SessionCrypto(..)
    , BridgeState(..)
    , Pane(..)
    , ConnectionMode(..)
    , MenuTab(..)
    , menuTabLabel
    , menuTabUnderlineIndex
    , menuTabItems
    , AppState(..)
    , asConfig
    , asRunning
    , DialogMode(..)
    , AppConfig(..)
    , Layout(..)
    , Rect(..)
    , rectContains
    , InputEvent(..)
    ) where

import Data.IORef (IORef)

import UmbraVox.BuildProfile
    ( BuildPluginId(..), pluginEnabled )

-- Re-export domain types from their new neutral namespaces.
-- Existing imports of UmbraVox.TUI.Types continue to work unchanged.
import UmbraVox.App.Types
    ( ContactStatus(..), statusTag, SessionInfo(..), SessionCrypto(..), BridgeState(..) )
import UmbraVox.App.Config
    ( SessionId, ConnectionMode(..), AppConfig(..) )
import UmbraVox.App.State
    ( CoreState(..) )

data Pane = ContactPane | ChatPane | IdentityPane deriving stock (Eq, Show, Enum, Bounded)

data MenuTab = MenuHelp | MenuPrefs | MenuIdentity | MenuQuit
    deriving stock (Eq, Show, Enum, Bounded)

menuTabLabel :: MenuTab -> String
menuTabLabel MenuHelp     = " F1 Help "
menuTabLabel MenuPrefs    = " F2 Prefs "
menuTabLabel MenuIdentity = " F3 Identity "
menuTabLabel MenuQuit     = " Q Quit "

menuTabUnderlineIndex :: MenuTab -> Maybe Int
menuTabUnderlineIndex MenuQuit = Just 1
menuTabUnderlineIndex _        = Nothing

menuTabItems :: MenuTab -> [String]
menuTabItems MenuHelp     = ["Help", "About"]
menuTabItems MenuPrefs
    = ["Settings", "Toggle Rich Text"]
    ++ (if pluginEnabled PluginChatTransfer then ["Export Chat", "Import Chat"] else [])
    ++ ["New Bridge Chat"]
menuTabItems MenuIdentity = ["Regenerate Key", "Export Keys", "Import Keys", "Toggle Key Info"]
menuTabItems MenuQuit     = ["Quit"]

-- | Full application state: domain state wrapped in 'CoreState' plus
-- TUI-only presentation fields.
--
-- Use 'asConfig' to reach the 'AppConfig' and 'asRunning' to reach the
-- lifecycle flag — both delegate to 'asCoreState'.  Existing code that
-- uses @asConfig st@ or @asRunning st@ continues to compile without change.
data AppState = AppState
    { asCoreState :: !CoreState
      -- ^ Domain state (config, sessions, DB handle, running flag).
      --   Shared with the headless runtime via 'UmbraVox.App.State.CoreState'.
    , asSelected :: IORef Int, asFocus :: IORef Pane
    , asInputBuf :: IORef String, asDialogBuf :: IORef String
    , asRichText :: IORef Bool
    , asInputCursor :: IORef Int
    , asChatScroll :: IORef Int
    , asInputScroll :: IORef Int
    , asStatusMsg :: IORef String
    , asDialogMode :: IORef (Maybe DialogMode)
    , asBrowsePage :: IORef Int
    , asBrowseFilter :: IORef String
    , asLayout :: IORef Layout
    , asContactScroll :: IORef Int
    , asTermSize :: IORef (Int, Int)
    , asMenuOpen :: IORef (Maybe MenuTab)
    , asMenuIndex :: IORef Int
    , asDialogTab :: IORef Int
    , asLastRenderToken :: IORef (Maybe String)
      -- ^ Strict: writers must evaluate the token with a bang pattern
      --   before storing (e.g. @let !tok = …; writeIORef ref (Just tok)@)
      --   to avoid IORef space leak.
    , asRegenCheckbox :: IORef Bool
      -- ^ Checkbox state for the key-regeneration confirmation dialog.
    , asDialogScroll :: !(IORef Int)
      -- ^ Scroll offset (in lines) within the currently open dialog overlay.
      --   Reset to 0 whenever a dialog is opened or closed.
    , asSelectionStart :: !(IORef (Maybe Int))
    , asShowIdentity :: !(IORef Bool)
      -- ^ Whether the identity panel (QR + fingerprints) is visible.
      --   Toggled via F5 Identity → "Toggle Key Info".
      -- ^ Raw buffer index where the current text selection began.
      --   Nothing means no active selection.
    , asLinkText :: !(IORef String)
      -- ^ "Text" field contents for the Insert Link modal.
    , asLinkUrl :: !(IORef String)
      -- ^ "URL" field contents for the Insert Link modal.
    , asLinkFocus :: !(IORef Int)
      -- ^ Which field/button is focused in DlgInsertLink:
      --   0 = Text, 1 = URL, 2 = Insert btn, 3 = Cancel btn.
    , asEmojiPage :: !(IORef Int)
      -- ^ Current page index in the emoji picker dialog (0-based).
    , asEmojiSearch :: !(IORef String)
      -- ^ Active search filter text in the emoji picker dialog.
    , asEmojiCategory :: !(IORef Int)
      -- ^ Current category index in the emoji picker dialog (0-based).
    }

-- | Accessor: domain configuration, delegating to 'asCoreState'.
-- Keeps all existing @asConfig st@ call sites working without modification.
asConfig :: AppState -> AppConfig
asConfig = csConfig . asCoreState

-- | Accessor: running flag, delegating to 'asCoreState'.
-- Keeps all existing @asRunning st@ call sites working without modification.
asRunning :: AppState -> IORef Bool
asRunning = csRunning . asCoreState

data DialogMode = DlgHelp | DlgAbout | DlgSettings | DlgVerify | DlgNewConn
    | DlgKeys | DlgBrowse | DlgRegenKey | DlgExportWarn | DlgExportKeys
    | DlgInsertLink | DlgEmojiPicker
    | DlgBridgeSelect | DlgBridgeAuth | DlgBridgeContacts
    | DlgPrompt String (String -> IO ())

instance Eq DialogMode where
    DlgHelp       == DlgHelp       = True
    DlgAbout      == DlgAbout      = True
    DlgSettings   == DlgSettings   = True
    DlgVerify     == DlgVerify     = True
    DlgNewConn    == DlgNewConn    = True
    DlgKeys       == DlgKeys       = True
    DlgBrowse     == DlgBrowse     = True
    DlgRegenKey   == DlgRegenKey   = True
    DlgExportWarn == DlgExportWarn = True
    DlgExportKeys == DlgExportKeys = True
    DlgInsertLink  == DlgInsertLink  = True
    DlgEmojiPicker == DlgEmojiPicker = True
    DlgBridgeSelect   == DlgBridgeSelect   = True
    DlgBridgeAuth     == DlgBridgeAuth     = True
    DlgBridgeContacts == DlgBridgeContacts = True
    DlgPrompt a _ == DlgPrompt b _ = a == b
    _             == _             = False

instance Show DialogMode where
    show DlgHelp         = "DlgHelp"
    show DlgAbout        = "DlgAbout"
    show DlgSettings     = "DlgSettings"
    show DlgVerify       = "DlgVerify"
    show DlgNewConn      = "DlgNewConn"
    show DlgKeys         = "DlgKeys"
    show DlgBrowse       = "DlgBrowse"
    show DlgRegenKey     = "DlgRegenKey"
    show DlgExportWarn   = "DlgExportWarn"
    show DlgExportKeys   = "DlgExportKeys"
    show DlgInsertLink   = "DlgInsertLink"
    show DlgEmojiPicker  = "DlgEmojiPicker"
    show DlgBridgeSelect   = "DlgBridgeSelect"
    show DlgBridgeAuth     = "DlgBridgeAuth"
    show DlgBridgeContacts = "DlgBridgeContacts"
    show (DlgPrompt s _) = "DlgPrompt " ++ show s ++ " <callback>"

data Layout = Layout
    { lCols :: Int        -- ^ total terminal columns
    , lRows :: Int        -- ^ total terminal rows
    , lLeftW :: Int       -- ^ contacts pane width (including border)
    , lRightW :: Int      -- ^ chat pane width (including border)
    , lChatH :: Int       -- ^ number of content rows (between header and input)
    , lIdentityH :: Int   -- ^ height of the inline identity panel (rows, including separator)
    , lToolbarRow :: Int  -- ^ absolute terminal row of the editor toolbar (1-based)
    } deriving stock (Eq)

-- | Terminal-space rectangle used by input hit-testing and menu/dropdown geometry.
data Rect = Rect
    { rectTop :: Int
    , rectBottom :: Int
    , rectLeft :: Int
    , rectRight :: Int
    } deriving stock (Eq, Show)

rectContains :: Rect -> Int -> Int -> Bool
rectContains r row col =
    row >= rectTop r && row <= rectBottom r && col >= rectLeft r && col <= rectRight r

data InputEvent = KeyChar Char | KeyEnter | KeyTab | KeyBackspace | KeyEscape
    | KeyUp | KeyDown | KeyLeft | KeyRight | KeyHome | KeyEnd
    | KeyPageUp | KeyPageDown
    | KeyShiftLeft | KeyShiftRight | KeyShiftHome | KeyShiftEnd
    | KeyCtrlN | KeyCtrlG | KeyCtrlQ | KeyCtrlD
    | KeyCtrlB | KeyCtrlI | KeyCtrlU | KeyCtrlK
    | KeyMouseLeft Int Int        -- row, col (1-based terminal coordinates)
    | KeyMouseDrag Int Int        -- row, col (drag with button held)
    | KeyMouseRelease Int Int     -- row, col (mouse release)
    | KeyMouseScrollUp Int Int    -- row, col (wheel scroll up)
    | KeyMouseScrollDown Int Int  -- row, col (wheel scroll down)
    | KeyIgnored
    | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5
    | KeyUnknown
