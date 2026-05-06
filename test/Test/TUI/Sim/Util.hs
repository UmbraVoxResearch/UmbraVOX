-- SPDX-License-Identifier: Apache-2.0
-- | Shared test utilities for TUI simulation tests.
-- Provides mock state construction and dialog mode helpers.
module Test.TUI.Sim.Util
    ( mkTestState
    , mkTestConfig
    , calcTestLayout
    , addTestSession
    , addTestSessionWithHistory
    , seedBrowsePeers
    , isDlgNewConn, isDlgSettings, isDlgHelp, isDlgAbout, isDlgKeys
    , isDlgVerify, isDlgBrowse, isDlgNothing
    , isDlgPrompt, isDlgPromptWithSubstring
    ) where

import Control.Concurrent.MVar (newMVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import UmbraVox.TUI.Types
import UmbraVox.TUI.Layout (calcLayout)
import UmbraVox.Chat.Session (initChatSession)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Network.MDNS (MDNSPeer(..))

-- | Create a minimal AppState suitable for testing.
mkTestState :: IO AppState
mkTestState = do
    cfg <- mkTestConfig
    let lay = calcTestLayout
    AppState cfg
        <$> newIORef 0           -- asSelected
        <*> newIORef ContactPane -- asFocus
        <*> newIORef ""          -- asInputBuf
        <*> newIORef ""          -- asDialogBuf
        <*> newIORef 0           -- asChatScroll
        <*> newIORef ""          -- asStatusMsg
        <*> newIORef True        -- asRunning
        <*> newIORef Nothing     -- asDialogMode
        <*> newIORef 0           -- asBrowsePage
        <*> newIORef ""          -- asBrowseFilter
        <*> newIORef lay         -- asLayout
        <*> newIORef 0           -- asContactScroll
        <*> newIORef (40, 120)   -- asTermSize
        <*> newIORef Nothing     -- asMenuOpen
        <*> newIORef 0           -- asMenuIndex
        <*> newIORef 0           -- asDialogTab
        <*> newIORef Nothing     -- asLastRenderToken

calcTestLayout :: Layout
calcTestLayout = calcLayout 40 120

mkTestConfig :: IO AppConfig
mkTestConfig =
    AppConfig
        <$> newIORef 1111        -- cfgListenPort
        <*> newIORef "testuser"  -- cfgDisplayName
        <*> newIORef Nothing     -- cfgIdentity
        <*> newIORef Map.empty   -- cfgSessions
        <*> newIORef 1           -- cfgNextId
        <*> newIORef False       -- cfgDebugLogging
        <*> newIORef "build/test-runtime.log" -- cfgDebugLogPath
        <*> newIORef False       -- cfgMDNSEnabled
        <*> newIORef False       -- cfgPEXEnabled
        <*> newIORef False       -- cfgDBEnabled
        <*> newIORef ""          -- cfgDBPath
        <*> newIORef Nothing     -- cfgListenerThread
        <*> newIORef Nothing     -- cfgMDNSThread
        <*> newIORef []          -- cfgMDNSPeers
        <*> newIORef 30          -- cfgRetentionDays
        <*> newIORef False       -- cfgAutoSaveMessages
        <*> newIORef Nothing     -- cfgAnthonyDB
        <*> newIORef Promiscuous -- cfgConnectionMode
        <*> newIORef []          -- cfgTrustedKeys

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
    lock <- newMVar ()
    histRef <- newIORef history
    stRef <- newIORef Local
    let si = SessionInfo Nothing ref lock Nothing label histRef stRef
    modifyIORef' (cfgSessions cfg) (Map.insert sid si)
    pure sid
  where
    modifyIORef' r f = readIORef r >>= \v -> writeIORef r (f v)

seedBrowsePeers :: AppState -> Int -> IO ()
seedBrowsePeers st count =
    writeIORef (cfgMDNSPeers (asConfig st))
        [ MDNSPeer
            { mdnsPubkey = BS.pack [fromIntegral i, 0xAA]
            , mdnsName = Just ("peer-" ++ show i)
            , mdnsIP = "127.0.0.1"
            , mdnsPort = 3000 + i
            , mdnsLastSeen = 0
            }
        | i <- [0 .. count - 1]
        ]

------------------------------------------------------------------------
-- Dialog mode pattern matchers
------------------------------------------------------------------------

isDlgNewConn :: Maybe DialogMode -> Bool
isDlgNewConn (Just DlgNewConn) = True; isDlgNewConn _ = False

isDlgSettings :: Maybe DialogMode -> Bool
isDlgSettings (Just DlgSettings) = True; isDlgSettings _ = False

isDlgHelp :: Maybe DialogMode -> Bool
isDlgHelp (Just DlgHelp) = True; isDlgHelp _ = False

isDlgAbout :: Maybe DialogMode -> Bool
isDlgAbout (Just DlgAbout) = True; isDlgAbout _ = False

isDlgKeys :: Maybe DialogMode -> Bool
isDlgKeys (Just DlgKeys) = True; isDlgKeys _ = False

isDlgVerify :: Maybe DialogMode -> Bool
isDlgVerify (Just DlgVerify) = True; isDlgVerify _ = False

isDlgBrowse :: Maybe DialogMode -> Bool
isDlgBrowse (Just DlgBrowse) = True; isDlgBrowse _ = False

isDlgNothing :: Maybe DialogMode -> Bool
isDlgNothing Nothing = True; isDlgNothing _ = False

isDlgPrompt :: Maybe DialogMode -> Bool
isDlgPrompt (Just (DlgPrompt _ _)) = True; isDlgPrompt _ = False

isDlgPromptWithSubstring :: String -> Maybe DialogMode -> Bool
isDlgPromptWithSubstring sub (Just (DlgPrompt title _)) = sub `isInfixOf` title
isDlgPromptWithSubstring _ _ = False
