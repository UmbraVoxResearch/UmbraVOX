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
    , isDlgInsertLink, isDlgEmojiPicker
    ) where

import Control.Concurrent.MVar (newMVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import UmbraVox.App.Startup (newDefaultAppConfig)
import UmbraVox.App.State (newCoreState)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Layout (calcLayout)
import UmbraVox.Chat.OutboundQueue (newQueue, maxQueueDepth, maxMessageAge)
import UmbraVox.Chat.Session (initChatSession)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Network.MDNS (MDNSPeer(..))

-- | Create a minimal AppState suitable for testing.
mkTestState :: IO AppState
mkTestState = do
    cfg <- mkTestConfig
    coreState <- newCoreState cfg
    let lay = calcTestLayout
    AppState coreState
        <$> newIORef 0           -- asSelected
        <*> newIORef ContactPane -- asFocus
        <*> newIORef ""          -- asInputBuf
        <*> newIORef ""          -- asDialogBuf
        <*> newIORef True        -- asRichText
        <*> newIORef 0           -- asInputCursor
        <*> newIORef 0           -- asChatScroll
        <*> newIORef 0           -- asInputScroll
        <*> newIORef ""          -- asStatusMsg
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
        <*> newIORef False       -- asRegenCheckbox
        <*> newIORef 0           -- asDialogScroll
        <*> newIORef Nothing     -- asSelectionStart
        <*> newIORef True        -- asShowIdentity
        <*> newIORef ""          -- asLinkText
        <*> newIORef ""          -- asLinkUrl
        <*> newIORef 0           -- asLinkFocus
        <*> newIORef 0           -- asEmojiPage
        <*> newIORef ""          -- asEmojiSearch
        <*> newIORef 0           -- asEmojiCategory

calcTestLayout :: Layout
calcTestLayout = calcLayout 40 120

mkTestConfig :: IO AppConfig
mkTestConfig = do
    cfg <- newDefaultAppConfig
    writeIORef (cfgDisplayName cfg) "testuser"
    writeIORef (cfgDebugLogPath cfg) "build/test-runtime.log"
    -- Tests use ephemeral mode by default (in-memory storage, no disk
    -- writes).  newDefaultAppConfig defaults this to False (production
    -- default); restore the test-isolation default that was dropped when
    -- mkTestConfig was refactored to delegate to newDefaultAppConfig.
    writeIORef (cfgEphemeral cfg) True
    -- Pin the listen port deterministically.  newDefaultAppConfig calls
    -- findAvailablePort, which probes the host and returns a non-deterministic
    -- port; several settings-overlay tests assert the rendered "Listen port:
    -- 1111" line.  Restore the fixed test-isolation port the pre-refactor
    -- mkTestConfig set explicitly.
    writeIORef (cfgListenPort cfg) 1111
    -- Start mDNS disabled deterministically.  newDefaultAppConfig derives the
    -- initial mDNS state from pluginEnabled PluginDiscovery (enabled in the
    -- default profile); the pre-refactor mkTestConfig pinned it OFF so the
    -- mDNS-toggle tests observe a known starting state ("enabled and applied"
    -- after the first toggle).
    writeIORef (cfgMDNSEnabled cfg) False
    return cfg

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
    mSession <- initChatSession secret dhSec peerPub
    let session = case mSession of
                      Just s  -> s
                      Nothing -> error "addTestSessionWithHistory: initChatSession returned Nothing (impossible with random keys)"
    ref <- newIORef session
    lock <- newMVar ()
    histRef <- newIORef history
    stRef <- newIORef Local
    oq <- newQueue maxQueueDepth maxMessageAge
    let si = SessionInfo Nothing (RatchetCrypto ref) lock Nothing label histRef stRef oq
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

isDlgInsertLink :: Maybe DialogMode -> Bool
isDlgInsertLink (Just DlgInsertLink) = True; isDlgInsertLink _ = False

isDlgEmojiPicker :: Maybe DialogMode -> Bool
isDlgEmojiPicker (Just DlgEmojiPicker) = True; isDlgEmojiPicker _ = False
