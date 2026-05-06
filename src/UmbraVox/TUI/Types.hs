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
    , menuTabItems
    , AppState(..)
    , DialogMode(..)
    , AppConfig(..)
    , Layout(..)
    , InputEvent(..)
    ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import UmbraVox.Chat.Session (ChatSession)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey)
import UmbraVox.Network.TransportClass (AnyTransport)
import UmbraVox.Network.MDNS (MDNSPeer)
import UmbraVox.Storage.Anthony (AnthonyDB)

type SessionId = Int

data ContactStatus = Online | Offline | Local | Group | LAN | PEX
    deriving stock (Eq)

statusTag :: ContactStatus -> String
statusTag Online  = " \x25CF"   -- ● filled circle
statusTag Offline = " \x25CB"   -- ○ empty circle
statusTag Local   = " \x1F512"  -- 🔒 lock
statusTag Group   = " \x1F465"  -- 👥 people
statusTag LAN     = " \x1F5A7"  -- 🖧 network
statusTag PEX     = " \x1F517"  -- 🔗 link

data SessionInfo = SessionInfo
    { siTransport :: Maybe AnyTransport, siSession :: IORef ChatSession
    , siSessionLock :: MVar ()
    , siRecvTid :: Maybe ThreadId, siPeerName :: String
    , siHistory :: IORef [String], siStatus :: IORef ContactStatus }

data Pane = ContactPane | ChatPane deriving stock (Eq, Show, Enum, Bounded)

-- | Connection trust modes, ordered most open to most locked down.
-- Swing:       Accept all + auto PEX peer exchange + mDNS on
-- Promiscuous: Accept all connections automatically + mDNS on
-- Selective:   Accept with fingerprint confirmation + mDNS on
-- Chaste:      Trusted keys only, silent reject, mDNS off
-- Chastity:    Trusted keys only + ephemeral (no DB, no persistence)
data ConnectionMode = Swing | Promiscuous | Selective | Chaste | Chastity
    deriving stock (Eq, Show, Enum, Bounded)

data MenuTab = MenuHelp | MenuContacts | MenuChat | MenuPrefs | MenuQuit
    deriving stock (Eq, Show, Enum, Bounded)

menuTabLabel :: MenuTab -> String
menuTabLabel MenuHelp     = " F1 Help "
menuTabLabel MenuContacts = " F2 Contacts "
menuTabLabel MenuChat     = " F3 Chat "
menuTabLabel MenuPrefs    = " F4 Prefs "
menuTabLabel MenuQuit     = " Q̲uit "

menuTabItems :: MenuTab -> [String]
menuTabItems MenuHelp     = ["Help", "About"]
menuTabItems MenuContacts = ["Browse", "Verify"]
menuTabItems MenuChat     = ["New", "Rename", "Send", "Clear Input"]
menuTabItems MenuPrefs    = ["Settings", "Keys", "mDNS Toggle", "Export Chat", "Import Chat"]
menuTabItems MenuQuit     = ["Quit"]

data AppState = AppState
    { asConfig :: AppConfig, asSelected :: IORef Int, asFocus :: IORef Pane
    , asInputBuf :: IORef String, asDialogBuf :: IORef String
    , asChatScroll :: IORef Int
    , asStatusMsg :: IORef String, asRunning :: IORef Bool
    , asDialogMode :: IORef (Maybe DialogMode)
    , asLayout :: IORef Layout
    , asContactScroll :: IORef Int
    , asTermSize :: IORef (Int, Int)
    , asMenuOpen :: IORef (Maybe MenuTab)
    , asMenuIndex :: IORef Int }

data DialogMode = DlgHelp | DlgSettings | DlgVerify | DlgNewConn
    | DlgKeys | DlgBrowse | DlgWelcome | DlgPrompt String (String -> IO ())

instance Eq DialogMode where
    DlgHelp       == DlgHelp       = True
    DlgSettings   == DlgSettings   = True
    DlgVerify     == DlgVerify     = True
    DlgNewConn    == DlgNewConn    = True
    DlgKeys       == DlgKeys       = True
    DlgBrowse     == DlgBrowse     = True
    DlgWelcome    == DlgWelcome    = True
    DlgPrompt a _ == DlgPrompt b _ = a == b
    _             == _             = False

instance Show DialogMode where
    show DlgHelp         = "DlgHelp"
    show DlgSettings     = "DlgSettings"
    show DlgVerify       = "DlgVerify"
    show DlgNewConn      = "DlgNewConn"
    show DlgKeys         = "DlgKeys"
    show DlgBrowse       = "DlgBrowse"
    show DlgWelcome      = "DlgWelcome"
    show (DlgPrompt s _) = "DlgPrompt " ++ show s ++ " <callback>"

data AppConfig = AppConfig
    { cfgListenPort  :: IORef Int
    , cfgDisplayName :: IORef String
    , cfgIdentity    :: IORef (Maybe IdentityKey)
    , cfgSessions    :: IORef (Map SessionId SessionInfo)
    , cfgNextId      :: IORef SessionId
    , cfgDebugLogging :: IORef Bool
    , cfgDebugLogPath :: IORef FilePath
    -- Discovery settings
    , cfgMDNSEnabled :: IORef Bool
    , cfgPEXEnabled  :: IORef Bool
    , cfgDBEnabled   :: IORef Bool
    , cfgDBPath      :: IORef String
    -- Discovery state
    , cfgListenerThread :: IORef (Maybe ThreadId)
    , cfgMDNSThread  :: IORef (Maybe ThreadId)
    , cfgMDNSPeers   :: IORef [MDNSPeer]
    -- Retention settings
    , cfgRetentionDays    :: IORef Int              -- days to keep messages (0 = forever)
    , cfgAutoSaveMessages :: IORef Bool             -- auto-save messages to DB
    , cfgAnthonyDB        :: IORef (Maybe AnthonyDB) -- DB handle
    -- Security
    , cfgConnectionMode   :: IORef ConnectionMode
    , cfgTrustedKeys      :: IORef [ByteString]
    }

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
    | KeyCtrlN | KeyCtrlQ | KeyCtrlD
    | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5
    | KeyUnknown
