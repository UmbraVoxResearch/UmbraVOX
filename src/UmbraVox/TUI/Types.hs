module UmbraVox.TUI.Types
    ( SessionId
    , ContactStatus(..)
    , statusTag
    , SessionInfo(..)
    , Pane(..)
    , AppState(..)
    , DialogMode(..)
    , AppConfig(..)
    , Layout(..)
    , InputEvent(..)
    ) where

import Control.Concurrent (ThreadId)
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
statusTag Online  = "[ON]"
statusTag Offline = "[OFF]"
statusTag Local   = "[LOCAL]"
statusTag Group   = "[GRP]"
statusTag LAN     = "[LAN]"
statusTag PEX     = "[PEX]"

data SessionInfo = SessionInfo
    { siTransport :: Maybe AnyTransport, siSession :: IORef ChatSession
    , siRecvTid :: Maybe ThreadId, siPeerName :: String
    , siHistory :: IORef [String], siStatus :: IORef ContactStatus }

data Pane = ContactPane | ChatPane deriving stock (Eq)

data AppState = AppState
    { asConfig :: AppConfig, asSelected :: IORef Int, asFocus :: IORef Pane
    , asInputBuf :: IORef String, asDialogBuf :: IORef String
    , asChatScroll :: IORef Int
    , asStatusMsg :: IORef String, asRunning :: IORef Bool
    , asDialogMode :: IORef (Maybe DialogMode)
    , asLayout :: IORef Layout
    , asContactScroll :: IORef Int
    , asTermSize :: IORef (Int, Int) }

data DialogMode = DlgHelp | DlgSettings | DlgVerify | DlgNewConn
    | DlgKeys | DlgPrompt String (String -> IO ())

data AppConfig = AppConfig
    { cfgListenPort  :: IORef Int
    , cfgDisplayName :: IORef String
    , cfgIdentity    :: IORef (Maybe IdentityKey)
    , cfgSessions    :: IORef (Map SessionId SessionInfo)
    , cfgNextId      :: IORef SessionId
    -- Discovery settings
    , cfgMDNSEnabled :: IORef Bool
    , cfgPEXEnabled  :: IORef Bool
    , cfgDBEnabled   :: IORef Bool
    , cfgDBPath      :: IORef String
    -- Discovery state
    , cfgMDNSThread  :: IORef (Maybe ThreadId)
    , cfgMDNSPeers   :: IORef [MDNSPeer]
    -- Retention settings
    , cfgRetentionDays    :: IORef Int              -- days to keep messages (0 = forever)
    , cfgAutoSaveMessages :: IORef Bool             -- auto-save messages to DB
    , cfgAnthonyDB        :: IORef (Maybe AnthonyDB) -- DB handle
    }

data Layout = Layout
    { lCols :: Int, lRows :: Int
    , lLeftW :: Int, lRightW :: Int, lChatH :: Int
    , lPadX :: Int, lPadY :: Int }
    deriving stock (Eq)

data InputEvent = KeyChar Char | KeyEnter | KeyTab | KeyBackspace | KeyEscape
    | KeyUp | KeyDown | KeyPageUp | KeyPageDown
    | KeyCtrlN | KeyCtrlW | KeyCtrlR | KeyCtrlX
    | KeyCtrlP | KeyCtrlQ | KeyCtrlL | KeyCtrlD
    | KeyUnknown
