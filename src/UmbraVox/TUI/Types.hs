-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Types
    ( SessionId
    , ContactStatus(..)
    , statusTag
    , SessionInfo(..)
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
    , InputEvent(..)
    ) where

import Data.IORef (IORef)

import UmbraVox.BuildProfile
    ( BuildPluginId(..), pluginEnabled )

-- Re-export domain types from their new neutral namespaces.
-- Existing imports of UmbraVox.TUI.Types continue to work unchanged.
import UmbraVox.App.Types
    ( ContactStatus(..), statusTag, SessionInfo(..) )
import UmbraVox.App.Config
    ( SessionId, ConnectionMode(..), AppConfig(..) )
import UmbraVox.App.State
    ( CoreState(..) )

data Pane = ContactPane | ChatPane deriving stock (Eq, Show, Enum, Bounded)

data MenuTab = MenuHelp | MenuContacts | MenuChat | MenuPrefs | MenuQuit
    deriving stock (Eq, Show, Enum, Bounded)

menuTabLabel :: MenuTab -> String
menuTabLabel MenuHelp     = " F1 Help "
menuTabLabel MenuContacts = " F2 Contacts "
menuTabLabel MenuChat     = " F3 Chat "
menuTabLabel MenuPrefs    = " F4 Prefs "
menuTabLabel MenuQuit     = " Q Quit "

menuTabUnderlineIndex :: MenuTab -> Maybe Int
menuTabUnderlineIndex MenuQuit = Just 1
menuTabUnderlineIndex _        = Nothing

menuTabItems :: MenuTab -> [String]
menuTabItems MenuHelp     = ["Help", "About"]
menuTabItems MenuContacts = ["Browse", "Verify"]
menuTabItems MenuChat     = ["New", "Rename", "Send", "Clear Input"]
menuTabItems MenuPrefs
    = ["Settings", "Keys"]
    ++ (if pluginEnabled PluginDiscovery then ["mDNS Toggle"] else [])
    ++ if pluginEnabled PluginChatTransfer then ["Export Chat", "Import Chat"] else []
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
    , asChatScroll :: IORef Int
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
    | DlgKeys | DlgBrowse | DlgPrompt String (String -> IO ())

instance Eq DialogMode where
    DlgHelp       == DlgHelp       = True
    DlgAbout      == DlgAbout      = True
    DlgSettings   == DlgSettings   = True
    DlgVerify     == DlgVerify     = True
    DlgNewConn    == DlgNewConn    = True
    DlgKeys       == DlgKeys       = True
    DlgBrowse     == DlgBrowse     = True
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
    show (DlgPrompt s _) = "DlgPrompt " ++ show s ++ " <callback>"

data Layout = Layout
    { lCols :: Int     -- ^ total terminal columns
    , lRows :: Int     -- ^ total terminal rows
    , lLeftW :: Int    -- ^ contacts pane width (including border)
    , lRightW :: Int   -- ^ chat pane width (including border)
    , lChatH :: Int    -- ^ number of content rows (between header and input)
    } deriving stock (Eq)

data InputEvent = KeyChar Char | KeyEnter | KeyTab | KeyBackspace | KeyEscape
    | KeyUp | KeyDown | KeyLeft | KeyRight
    | KeyPageUp | KeyPageDown
    | KeyCtrlN | KeyCtrlG | KeyCtrlQ | KeyCtrlD
    | KeyMouseLeft Int Int  -- row, col (1-based terminal coordinates)
    | KeyIgnored
    | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5
    | KeyUnknown
