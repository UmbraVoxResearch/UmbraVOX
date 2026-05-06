-- | Shared test utilities for TUI simulation tests.
-- Provides mock state construction and dialog mode helpers.
module Test.TUI.Sim.Util
    ( mkTestState
    , mkTestConfig
    , addTestSession
    , addTestSessionWithHistory
    , isDlgNewConn, isDlgSettings, isDlgHelp, isDlgKeys
    , isDlgVerify, isDlgBrowse, isDlgWelcome, isDlgNothing
    , isDlgPrompt, isDlgPromptWithSubstring
    ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import UmbraVox.TUI.Types
import UmbraVox.TUI.Layout (calcLayout)
import UmbraVox.Chat.Session (initChatSession)
import UmbraVox.Crypto.Random (randomBytes)

-- | Create a minimal AppState suitable for testing.
mkTestState :: IO AppState
mkTestState = do
    cfg <- mkTestConfig
    let lay = calcLayout 40 120
    AppState cfg
        <$> newIORef 0           -- asSelected
        <*> newIORef ContactPane -- asFocus
        <*> newIORef ""          -- asInputBuf
        <*> newIORef ""          -- asDialogBuf
        <*> newIORef 0           -- asChatScroll
        <*> newIORef ""          -- asStatusMsg
        <*> newIORef True        -- asRunning
        <*> newIORef Nothing     -- asDialogMode
        <*> newIORef lay         -- asLayout
        <*> newIORef 0           -- asContactScroll
        <*> newIORef (40, 120)   -- asTermSize
        <*> newIORef Nothing     -- asMenuOpen
        <*> newIORef 0           -- asMenuIndex

mkTestConfig :: IO AppConfig
mkTestConfig =
    AppConfig
        <$> newIORef 1111        -- cfgListenPort
        <*> newIORef "testuser"  -- cfgDisplayName
        <*> newIORef Nothing     -- cfgIdentity
        <*> newIORef Map.empty   -- cfgSessions
        <*> newIORef 1           -- cfgNextId
        <*> newIORef False       -- cfgMDNSEnabled
        <*> newIORef False       -- cfgPEXEnabled
        <*> newIORef False       -- cfgDBEnabled
        <*> newIORef ""          -- cfgDBPath
        <*> newIORef Nothing     -- cfgMDNSThread
        <*> newIORef []          -- cfgMDNSPeers
        <*> newIORef 30          -- cfgRetentionDays
        <*> newIORef False       -- cfgAutoSaveMessages
        <*> newIORef Nothing     -- cfgAnthonyDB
        <*> newIORef False       -- cfgTrustedOnly

-- | Add a loopback session with empty history.
addTestSession :: AppConfig -> String -> IO SessionId
addTestSession cfg label = addTestSessionWithHistory cfg label []

-- | Add a loopback session with pre-populated history.
addTestSessionWithHistory :: AppConfig -> String -> [String] -> IO SessionId
addTestSessionWithHistory cfg label history = do
    sid <- readIORef (cfgNextId cfg)
    writeIORef (cfgNextId cfg) (sid + 1)
    secret <- randomBytes 32
    dhSec <- randomBytes 32
    peerPub <- randomBytes 32
    session <- initChatSession secret dhSec peerPub
    ref <- newIORef session
    histRef <- newIORef history
    stRef <- newIORef Local
    let si = SessionInfo Nothing ref Nothing label histRef stRef
    modifyIORef' (cfgSessions cfg) (Map.insert sid si)
    pure sid
  where
    modifyIORef' r f = readIORef r >>= \v -> writeIORef r (f v)

------------------------------------------------------------------------
-- Dialog mode pattern matchers
------------------------------------------------------------------------

isDlgNewConn :: Maybe DialogMode -> Bool
isDlgNewConn (Just DlgNewConn) = True; isDlgNewConn _ = False

isDlgSettings :: Maybe DialogMode -> Bool
isDlgSettings (Just DlgSettings) = True; isDlgSettings _ = False

isDlgHelp :: Maybe DialogMode -> Bool
isDlgHelp (Just DlgHelp) = True; isDlgHelp _ = False

isDlgKeys :: Maybe DialogMode -> Bool
isDlgKeys (Just DlgKeys) = True; isDlgKeys _ = False

isDlgVerify :: Maybe DialogMode -> Bool
isDlgVerify (Just DlgVerify) = True; isDlgVerify _ = False

isDlgBrowse :: Maybe DialogMode -> Bool
isDlgBrowse (Just DlgBrowse) = True; isDlgBrowse _ = False

isDlgWelcome :: Maybe DialogMode -> Bool
isDlgWelcome (Just DlgWelcome) = True; isDlgWelcome _ = False

isDlgNothing :: Maybe DialogMode -> Bool
isDlgNothing Nothing = True; isDlgNothing _ = False

isDlgPrompt :: Maybe DialogMode -> Bool
isDlgPrompt (Just (DlgPrompt _ _)) = True; isDlgPrompt _ = False

isDlgPromptWithSubstring :: String -> Maybe DialogMode -> Bool
isDlgPromptWithSubstring sub (Just (DlgPrompt title _)) = sub `isInfixOf` title
isDlgPromptWithSubstring _ _ = False
