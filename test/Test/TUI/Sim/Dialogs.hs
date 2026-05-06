-- SPDX-License-Identifier: Apache-2.0
-- | TUI simulation tests: dialog open/close lifecycle and prompt handling
module Test.TUI.Sim.Dialogs (runTests) where

import qualified Data.ByteString as BS
import Control.Exception (SomeException, catch)
import Control.Concurrent.MVar (newMVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Util (assertEq)
import Test.TUI.Sim.Util
import UmbraVox.Chat.Session (initChatSession)
import UmbraVox.Network.MDNS (MDNSPeer(..))
import UmbraVox.Network.Transport.Loopback (newLoopbackPair)
import UmbraVox.Network.TransportClass (AnyTransport(..), anyClose)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Storage.Anthony (openDB, closeDB, saveMessage, messageCount)
import UmbraVox.TUI.Actions (startBrowse, startSettings)
import UmbraVox.TUI.Dialog
    ( browseOverlayLines, overlayBounds, overlayCloseBounds, settingsOverlayLines
    , helpOverlayLines, newConnOverlayLines, promptOverlayLines
    )
import UmbraVox.TUI.Types
import UmbraVox.TUI.Input (handleDialog, handleSettingsDlg, handleNewConnDlg, handleNormal)

runTests :: IO Bool
runTests = do
    putStrLn "Test.TUI.Sim.Dialogs"
    putStrLn (replicate 40 '-')
    results <- sequence
        -- Dialog lifecycle
        [ testDlgHelpAnyKeyCloses
        , testDlgAboutAnyKeyCloses
        , testDlgKeysAnyKeyCloses
        , testDlgVerifyAnyKeyCloses
        , testDlgHelpMouseTopXCloses
        , testDlgBrowseEscCloses
        , testBrowseSearchPrompt
        , testBrowseMouseFooterPrev
        , testBrowseMouseFooterNext
        , testBrowseMouseFooterClear
        , testBrowseMouseFooterClose
        , testBrowseDigitConnectsVisiblePeer
        , testBrowseDigitClampsStalePage
        , testBrowseInvalidDigitSetsStatus
        , testBrowseOverlayFiltersByName
        , testBrowseOverlayMatchesNameAndPubkeyTerms
        , testBrowseOverlayPaginateTenPerPage
        , testBrowseMouseSearchClickOpensPrompt
        , testDialogBlocksNormalInput
        -- Prompt dialog
        , testPromptTyping
        , testPromptBackspace
        , testPromptBackspaceEmpty
        , testPromptEnterCallback
        , testPromptEnterClears
        , testPromptEnterCloses
        , testPromptEscCloses
        , testPromptMouseOk
        , testPromptMouseCancel
        , testPromptMaxLength
        -- Settings dialog
        , testSettingsPortPrompt
        , testSettingsMDNSToggle
        , testSettingsPEXToggle
        , testSettingsAutoSaveToggle
        , testSettingsConnectionModeCycles
        , testSettingsConnectionModeDisconnectsRemoteSessions
        , testSettingsRetentionAppliesAndPrunes
        , testSettingsOverlayNumbering
        , testSettingsTabRightSwitchesOverlay
        , testSettingsTabMouseSwitchesOverlay
        , testStartSettingsResetsTab
        , testSettingsKeysOpens
        , testSettingsUnknownCloses
        , testSettingsMouseOptionOpensPrompt
        , testSettingsMouseCloseButton
        -- NewConn dialog
        , testNewConnSecureNotes
        , testNewConnSingleOpensPrompt
        , testNewConnGroupOpensPrompt
        , testNewConnUnknownCloses
        , testNewConnMouseOptionOpensPrompt
        , testNewConnMouseCancelCloses
        ]
    pure (and results)

-- Dialog lifecycle --------------------------------------------------------

testDlgHelpAnyKeyCloses :: IO Bool
testDlgHelpAnyKeyCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgHelp)
    handleDialog st (KeyChar 'x')
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgHelp any key closes" True (isDlgNothing dlg)

testDlgAboutAnyKeyCloses :: IO Bool
testDlgAboutAnyKeyCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgAbout)
    handleDialog st (KeyChar 'x')
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgAbout any key closes" True (isDlgNothing dlg)

testDlgKeysAnyKeyCloses :: IO Bool
testDlgKeysAnyKeyCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgKeys)
    handleDialog st (KeyChar 'x')
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgKeys any key closes" True (isDlgNothing dlg)

testDlgVerifyAnyKeyCloses :: IO Bool
testDlgVerifyAnyKeyCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgVerify)
    handleDialog st (KeyChar 'x')
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgVerify any key closes" True (isDlgNothing dlg)

testDlgHelpMouseTopXCloses :: IO Bool
testDlgHelpMouseTopXCloses = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgHelp)
    let (r, cStart, _) = overlayCloseBounds calcTestLayout (length helpOverlayLines)
    handleNormal st (KeyMouseLeft r cStart)
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgHelp mouse top [X] closes" True (isDlgNothing dlg)

testDlgBrowseEscCloses :: IO Bool
testDlgBrowseEscCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgBrowse)
    handleDialog st KeyEscape
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgBrowse esc closes" True (isDlgNothing dlg)

testBrowseSearchPrompt :: IO Bool
testBrowseSearchPrompt = do
    st <- mkTestState
    startBrowse st
    handleDialog st (KeyChar '/')
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgBrowse slash opens prompt" True
        (isDlgPromptWithSubstring "Search peers" dlg)

testBrowseMouseFooterPrev :: IO Bool
testBrowseMouseFooterPrev = do
    st <- mkTestState
    seedBrowsePeers st 12
    startBrowse st
    writeIORef (asBrowsePage st) 1
    lines' <- browseOverlayLines st
    let lineIx = length lines' - 1
        line = lines' !! lineIx
        (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
        row = r0 + 1 + lineIx
        col = c0 + 2 + findSubstringPos "[ Prev ]" line + 1
    handleNormal st (KeyMouseLeft row col)
    page <- readIORef (asBrowsePage st)
    assertEq "DlgBrowse mouse footer prev decrements page" 0 page

testBrowseMouseFooterNext :: IO Bool
testBrowseMouseFooterNext = do
    st <- mkTestState
    seedBrowsePeers st 12
    startBrowse st
    lines' <- browseOverlayLines st
    let lineIx = length lines' - 1
        line = lines' !! lineIx
        (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
        row = r0 + 1 + lineIx
        col = c0 + 2 + findSubstringPos "[ Next ]" line + 1
    handleNormal st (KeyMouseLeft row col)
    page <- readIORef (asBrowsePage st)
    assertEq "DlgBrowse mouse footer next increments page" 1 page

testBrowseMouseFooterClear :: IO Bool
testBrowseMouseFooterClear = do
    st <- mkTestState
    writeIORef (asBrowseFilter st) "peer"
    startBrowse st
    lines' <- browseOverlayLines st
    let lineIx = length lines' - 1
        line = lines' !! lineIx
        (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
        row = r0 + 1 + lineIx
        col = c0 + 2 + findSubstringPos "[ Clear ]" line + 1
    handleNormal st (KeyMouseLeft row col)
    q <- readIORef (asBrowseFilter st)
    assertEq "DlgBrowse mouse footer clear resets filter" "" q

testBrowseMouseFooterClose :: IO Bool
testBrowseMouseFooterClose = do
    st <- mkTestState
    startBrowse st
    lines' <- browseOverlayLines st
    let lineIx = length lines' - 1
        line = lines' !! lineIx
        (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
        row = r0 + 1 + lineIx
        col = c0 + 2 + findSubstringPos "[ Close ]" line + 1
    handleNormal st (KeyMouseLeft row col)
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgBrowse mouse footer close closes dialog" True (isDlgNothing dlg)

testBrowseDigitConnectsVisiblePeer :: IO Bool
testBrowseDigitConnectsVisiblePeer = do
    st <- mkTestState
    writeIORef (cfgMDNSPeers (asConfig st))
        [ MDNSPeer
            { mdnsPubkey = BS.pack [0xAA, 0xBB]
            , mdnsName = Just "peer-zero"
            , mdnsIP = "127.0.0.1"
            , mdnsPort = 19421
            , mdnsLastSeen = 0
            }
        ]
    startBrowse st
    handleDialog st (KeyChar '0')
    dlg <- readIORef (asDialogMode st)
    status <- readIORef (asStatusMsg st)
    ok1 <- assertEq "DlgBrowse digit closes" True (isDlgNothing dlg)
    ok2 <- assertEq "DlgBrowse digit starts connect" True
        ("Connecting to 127.0.0.1:19421" `prefixOf` status)
    pure (ok1 && ok2)

testBrowseDigitClampsStalePage :: IO Bool
testBrowseDigitClampsStalePage = do
    st <- mkTestState
    writeIORef (cfgMDNSPeers (asConfig st))
        [ MDNSPeer
            { mdnsPubkey = BS.pack [0xAA, 0xBB]
            , mdnsName = Just "peer-zero"
            , mdnsIP = "127.0.0.1"
            , mdnsPort = 19421
            , mdnsLastSeen = 0
            }
        ]
    writeIORef (asBrowsePage st) 3
    startBrowse st
    writeIORef (asBrowsePage st) 3
    handleDialog st (KeyChar '0')
    dlg <- readIORef (asDialogMode st)
    status <- readIORef (asStatusMsg st)
    page <- readIORef (asBrowsePage st)
    ok1 <- assertEq "DlgBrowse stale page digit closes" True (isDlgNothing dlg)
    ok2 <- assertEq "DlgBrowse stale page digit clamps page" 0 page
    ok3 <- assertEq "DlgBrowse stale page digit starts connect" True
        ("Connecting to 127.0.0.1:19421" `prefixOf` status)
    pure (ok1 && ok2 && ok3)

testBrowseInvalidDigitSetsStatus :: IO Bool
testBrowseInvalidDigitSetsStatus = do
    st <- mkTestState
    writeIORef (cfgMDNSPeers (asConfig st))
        [ MDNSPeer
            { mdnsPubkey = BS.pack [0xAA]
            , mdnsName = Just "peer-zero"
            , mdnsIP = "127.0.0.1"
            , mdnsPort = 19421
            , mdnsLastSeen = 0
            }
        ]
    startBrowse st
    handleDialog st (KeyChar '9')
    dlg <- readIORef (asDialogMode st)
    status <- readIORef (asStatusMsg st)
    ok1 <- assertEq "DlgBrowse invalid digit stays open" True (isDlgBrowse dlg)
    ok2 <- assertEq "DlgBrowse invalid digit status" "No peer on slot 9 for the current page" status
    pure (ok1 && ok2)

testBrowseOverlayFiltersByName :: IO Bool
testBrowseOverlayFiltersByName = do
    st <- mkTestState
    writeIORef (cfgMDNSPeers (asConfig st))
        [ MDNSPeer (BS.pack [0xAA]) (Just "alpha-node") "127.0.0.1" 1111 0
        , MDNSPeer (BS.pack [0xBB]) (Just "beta-node") "127.0.0.1" 2222 0
        ]
    writeIORef (asBrowseFilter st) "beta"
    lines' <- browseOverlayLines st
    let has needle = any (needle `isIn`) lines'
    a <- assertEq "browse filter keeps matching peer" True (has "beta-node")
    b <- assertEq "browse filter drops non-matching peer" False (has "alpha-node")
    pure (a && b)

testBrowseOverlayMatchesNameAndPubkeyTerms :: IO Bool
testBrowseOverlayMatchesNameAndPubkeyTerms = do
    st <- mkTestState
    writeIORef (cfgMDNSPeers (asConfig st))
        [ MDNSPeer (BS.pack [0xAA, 0x10]) (Just "alpha-node") "127.0.0.1" 1111 0
        , MDNSPeer (BS.pack [0xBB, 0x22]) (Just "beta-node") "127.0.0.1" 2222 0
        ]
    writeIORef (asBrowseFilter st) "beta bb22"
    lines' <- browseOverlayLines st
    let has needle = any (needle `isIn`) lines'
    a <- assertEq "browse combined query keeps matching peer" True (has "beta-node")
    b <- assertEq "browse combined query drops non-matching peer" False (has "alpha-node")
    pure (a && b)

testBrowseOverlayPaginateTenPerPage :: IO Bool
testBrowseOverlayPaginateTenPerPage = do
    st <- mkTestState
    let mkPeer i = MDNSPeer (BS.pack [fromIntegral i]) (Just ("peer-" ++ show i)) "127.0.0.1" (3000 + i) 0
    writeIORef (cfgMDNSPeers (asConfig st)) (map mkPeer [0..11])
    writeIORef (asBrowsePage st) 1
    lines' <- browseOverlayLines st
    let has needle = any (needle `isIn`) lines'
    a <- assertEq "browse page 2 shows index 0" True (has "0. peer-10")
    b <- assertEq "browse page 2 shows index 1" True (has "1. peer-11")
    c <- assertEq "browse page 2 omits page 1 entries" False (has "0. peer-0")
    pure (a && b && c)

testBrowseMouseSearchClickOpensPrompt :: IO Bool
testBrowseMouseSearchClickOpensPrompt = do
    st <- mkTestState
    startBrowse st
    lines' <- browseOverlayLines st
    let (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
    handleNormal st (KeyMouseLeft (r0 + 3) (c0 + 10))
    dlg <- readIORef (asDialogMode st)
    assertEq "DlgBrowse mouse search opens prompt" True
        (isDlgPromptWithSubstring "Search peers" dlg)

testDialogBlocksNormalInput :: IO Bool
testDialogBlocksNormalInput = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgHelp)
    let origFocus = ContactPane
    writeIORef (asFocus st) origFocus
    handleDialog st KeyTab  -- dialog should intercept, not toggle focus
    f <- readIORef (asFocus st)
    assertEq "dialog blocks Tab" origFocus f

-- Prompt dialog -----------------------------------------------------------

testPromptTyping :: IO Bool
testPromptTyping = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    mapM_ (\c -> handleDialog st (KeyChar c)) ("test" :: String)
    buf <- readIORef (asDialogBuf st)
    assertEq "prompt typing" "test" buf

testPromptBackspace :: IO Bool
testPromptBackspace = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    writeIORef (asDialogBuf st) "abc"
    handleDialog st KeyBackspace
    buf <- readIORef (asDialogBuf st)
    assertEq "prompt backspace" "ab" buf

testPromptBackspaceEmpty :: IO Bool
testPromptBackspaceEmpty = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    handleDialog st KeyBackspace
    buf <- readIORef (asDialogBuf st)
    assertEq "prompt backspace empty" "" buf

testPromptEnterCallback :: IO Bool
testPromptEnterCallback = do
    st <- mkTestState
    result <- newIORef ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\v -> writeIORef result v)))
    writeIORef (asDialogBuf st) "val"
    handleDialog st KeyEnter
    v <- readIORef result
    assertEq "prompt enter callback" "val" v

testPromptEnterClears :: IO Bool
testPromptEnterClears = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    writeIORef (asDialogBuf st) "val"
    handleDialog st KeyEnter
    buf <- readIORef (asDialogBuf st)
    assertEq "prompt enter clears buf" "" buf

testPromptEnterCloses :: IO Bool
testPromptEnterCloses = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    handleDialog st KeyEnter
    dlg <- readIORef (asDialogMode st)
    assertEq "prompt enter closes" True (isDlgNothing dlg)

testPromptEscCloses :: IO Bool
testPromptEscCloses = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    handleDialog st KeyEscape
    dlg <- readIORef (asDialogMode st)
    assertEq "prompt esc closes" True (isDlgNothing dlg)

testPromptMouseOk :: IO Bool
testPromptMouseOk = do
    st <- mkTestState
    result <- newIORef ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\v -> writeIORef result v)))
    writeIORef (asDialogBuf st) "value"
    let lines' = promptOverlayLines "test" "value"
        lineIx = length lines' - 1
        line = lines' !! lineIx
        (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
        row = r0 + 1 + lineIx
        col = c0 + 2 + findSubstringPos "[ OK ]" line + 1
    handleNormal st (KeyMouseLeft row col)
    v <- readIORef result
    dlg <- readIORef (asDialogMode st)
    ok1 <- assertEq "prompt mouse OK submits callback" "value" v
    ok2 <- assertEq "prompt mouse OK closes dialog" True (isDlgNothing dlg)
    pure (ok1 && ok2)

testPromptMouseCancel :: IO Bool
testPromptMouseCancel = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    writeIORef (asDialogBuf st) "value"
    let lines' = promptOverlayLines "test" "value"
        lineIx = length lines' - 1
        line = lines' !! lineIx
        (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
        row = r0 + 1 + lineIx
        col = c0 + 2 + findSubstringPos "[ Cancel ]" line + 1
    handleNormal st (KeyMouseLeft row col)
    buf <- readIORef (asDialogBuf st)
    dlg <- readIORef (asDialogMode st)
    ok1 <- assertEq "prompt mouse Cancel clears input" "" buf
    ok2 <- assertEq "prompt mouse Cancel closes dialog" True (isDlgNothing dlg)
    pure (ok1 && ok2)

testPromptMaxLength :: IO Bool
testPromptMaxLength = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just (DlgPrompt "test" (\_ -> pure ())))
    writeIORef (asDialogBuf st) (replicate 4096 'x')
    handleDialog st (KeyChar 'y')
    buf <- readIORef (asDialogBuf st)
    assertEq "prompt max length" 4096 (length buf)

-- Settings dialog ---------------------------------------------------------

testSettingsPortPrompt :: IO Bool
testSettingsPortPrompt = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '1')
    dlg <- readIORef (asDialogMode st)
    assertEq "settings '1' opens port prompt" True (isDlgPrompt dlg)

testSettingsMDNSToggle :: IO Bool
testSettingsMDNSToggle = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    before <- readIORef (cfgMDNSEnabled (asConfig st))
    handleSettingsDlg st (KeyChar '3')
    after <- readIORef (cfgMDNSEnabled (asConfig st))
    assertEq "settings '3' toggles mDNS" True (before /= after)

testSettingsPEXToggle :: IO Bool
testSettingsPEXToggle = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    before <- readIORef (cfgPEXEnabled (asConfig st))
    handleSettingsDlg st (KeyChar '4')
    after <- readIORef (cfgPEXEnabled (asConfig st))
    assertEq "settings '4' toggles PEX" True (before /= after)

testSettingsAutoSaveToggle :: IO Bool
testSettingsAutoSaveToggle = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    before <- readIORef (cfgAutoSaveMessages (asConfig st))
    handleSettingsDlg st (KeyChar '8')
    after <- readIORef (cfgAutoSaveMessages (asConfig st))
    assertEq "settings '8' toggles auto-save" True (before /= after)

testSettingsConnectionModeCycles :: IO Bool
testSettingsConnectionModeCycles = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    writeIORef (cfgConnectionMode (asConfig st)) Swing
    handleSettingsDlg st (KeyChar 'c')
    mode1 <- readIORef (cfgConnectionMode (asConfig st))
    handleSettingsDlg st (KeyChar 'c')
    mode2 <- readIORef (cfgConnectionMode (asConfig st))
    ok1 <- assertEq "settings 'c' first cycle" Promiscuous mode1
    ok2 <- assertEq "settings 'c' second cycle" Selective mode2
    pure (ok1 && ok2)

testSettingsConnectionModeDisconnectsRemoteSessions :: IO Bool
testSettingsConnectionModeDisconnectsRemoteSessions = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    (txA, txB) <- newLoopbackPair "settings-mode"
    secret <- randomBytes 32
    dhSecret <- randomBytes 32
    peerPub <- randomBytes 32
    session <- initChatSession secret dhSecret peerPub
    sessionRef <- newIORef session
    lock <- newMVar ()
    histRef <- newIORef []
    statusRef <- newIORef Online
    let sid = 1
        si = SessionInfo
            { siTransport = Just (AnyTransport txA)
            , siSession = sessionRef
            , siSessionLock = lock
            , siRecvTid = Nothing
            , siPeerName = "peer-1"
            , siHistory = histRef
            , siStatus = statusRef
            }
    writeIORef (cfgSessions (asConfig st)) (Map.singleton sid si)
    writeIORef (cfgNextId (asConfig st)) 2
    sessionsBefore <- readIORef (cfgSessions (asConfig st))
    let hadTransport = case Map.lookup sid sessionsBefore of
            Just si -> case siTransport si of
                Just _ -> True
                Nothing -> False
            Nothing -> False
    handleSettingsDlg st (KeyChar 'c')
    sessionsAfter <- readIORef (cfgSessions (asConfig st))
    let disconnected = case Map.lookup sid sessionsAfter of
            Just si -> case siTransport si of
                Nothing -> True
                Just _ -> False
            Nothing -> False
    status <- readIORef (asStatusMsg st)
    anyClose (AnyTransport txB)
    ok1 <- assertEq "settings 'c' starts from remote session" True hadTransport
    ok2 <- assertEq "settings 'c' disconnects session for renegotiation" True disconnected
    ok3 <- assertEq "settings 'c' status mentions reconnect count" True
        ("reconnect 1 session(s)" `isIn` status)
    pure (ok1 && ok2 && ok3)

testSettingsRetentionAppliesAndPrunes :: IO Bool
testSettingsRetentionAppliesAndPrunes = withDB "tui-settings-retention.db" $ \dbPath -> do
    st <- mkTestState
    db <- openDB dbPath
    writeIORef (cfgAnthonyDB (asConfig st)) (Just db)
    writeIORef (cfgDBEnabled (asConfig st)) True
    -- two very old rows that should be pruned when retention is set to 1 day
    saveMessage db 1 "Alice" "old-1" 1
    saveMessage db 1 "Bob" "old-2" 2
    handleSettingsDlg st (KeyChar '7')
    dlg <- readIORef (asDialogMode st)
    case dlg of
        Just (DlgPrompt _ callback) -> callback "1"
        _ -> pure ()
    remaining <- messageCount db 1
    status <- readIORef (asStatusMsg st)
    closeDB db
    ok1 <- assertEq "settings retention prunes old persisted rows" 0 remaining
    ok2 <- assertEq "settings retention status updated" True
        ("Retention set to 1 days and applied" `isIn` status)
    pure (ok1 && ok2)

testSettingsOverlayNumbering :: IO Bool
testSettingsOverlayNumbering = do
    st <- mkTestState
    writeIORef (asDialogTab st) 2
    lines' <- settingsOverlayLines st
    let hasPrefix needle = any (needle `prefixOf`) lines'
    a <- assertEq "settings overlay 5 is persistent db" True
        (hasPrefix "   5. Persistent DB:")
    b <- assertEq "settings overlay 9 is clear history" True
        (hasPrefix "   9. Clear history...")
    pure (a && b)

testSettingsTabRightSwitchesOverlay :: IO Bool
testSettingsTabRightSwitchesOverlay = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleDialog st KeyRight
    tabIx <- readIORef (asDialogTab st)
    lines' <- settingsOverlayLines st
    let hasLine needle = any (needle `prefixOf`) lines'
    ok1 <- assertEq "settings Right switches to discovery tab" 1 tabIx
    ok2 <- assertEq "settings discovery tab shows peer exchange control" True
        (hasLine "   4. Peer Exchange:")
    pure (ok1 && ok2)

testSettingsTabMouseSwitchesOverlay :: IO Bool
testSettingsTabMouseSwitchesOverlay = do
    st <- mkTestState
    writeIORef (asDialogMode st) (Just DlgSettings)
    lines' <- settingsOverlayLines st
    let tabLine = head lines'
        (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
        row = r0 + 1
        col = c0 + 2 + findSubstringPos "[Advanced]" tabLine + 1
    handleNormal st (KeyMouseLeft row col)
    tabIx <- readIORef (asDialogTab st)
    updated <- settingsOverlayLines st
    let hasLine needle = any (needle `prefixOf`) updated
    ok1 <- assertEq "settings mouse switches to advanced tab" 4 tabIx
    ok2 <- assertEq "settings advanced tab shows debug logging control" True
        (hasLine "   a. Debug logging:")
    ok3 <- assertEq "settings advanced tab shows packaged plugin artifact status" True
        (any ("artifact-missing" `isIn`) updated)
    pure (ok1 && ok2 && ok3)

testStartSettingsResetsTab :: IO Bool
testStartSettingsResetsTab = do
    st <- mkTestState
    writeIORef (asDialogTab st) 4
    startSettings st
    tabIx <- readIORef (asDialogTab st)
    dlg <- readIORef (asDialogMode st)
    ok1 <- assertEq "startSettings resets tab to simple" 0 tabIx
    ok2 <- assertEq "startSettings opens settings dialog" True (isDlgSettings dlg)
    pure (ok1 && ok2)

testSettingsKeysOpens :: IO Bool
testSettingsKeysOpens = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar '0')
    dlg <- readIORef (asDialogMode st)
    assertEq "settings '0' opens keys" True (isDlgKeys dlg)

testSettingsUnknownCloses :: IO Bool
testSettingsUnknownCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgSettings)
    handleSettingsDlg st (KeyChar 'z')
    dlg <- readIORef (asDialogMode st)
    assertEq "settings unknown keeps dialog open" True (isDlgSettings dlg)

testSettingsMouseOptionOpensPrompt :: IO Bool
testSettingsMouseOptionOpensPrompt = do
    st <- mkTestState
    lines' <- settingsOverlayLines st
    let portLine = findLineIndex "   1. Listen port:    1111" lines'
        (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleNormal st (KeyMouseLeft (r0 + portLine + 1) (c0 + 8))
    dlg <- readIORef (asDialogMode st)
    assertEq "settings mouse port option opens prompt" True (isDlgPrompt dlg)

testSettingsMouseCloseButton :: IO Bool
testSettingsMouseCloseButton = do
    st <- mkTestState
    lines' <- settingsOverlayLines st
    let lineIx = length lines' - 1
        line = lines' !! lineIx
        (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
        row = r0 + 1 + lineIx
        col = c0 + 2 + findSubstringPos "[ Close ]" line + 1
    writeIORef (asDialogMode st) (Just DlgSettings)
    handleNormal st (KeyMouseLeft row col)
    dlg <- readIORef (asDialogMode st)
    assertEq "settings mouse Close closes dialog" True (isDlgNothing dlg)

-- NewConn dialog ----------------------------------------------------------

testNewConnSecureNotes :: IO Bool
testNewConnSecureNotes = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar '1')
    sessions <- readIORef (cfgSessions (asConfig st))
    assertEq "newconn '1' creates session" True (Map.size sessions > 0)

testNewConnSingleOpensPrompt :: IO Bool
testNewConnSingleOpensPrompt = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar '2')
    dlg <- readIORef (asDialogMode st)
    assertEq "newconn '2' opens prompt" True (isDlgPromptWithSubstring "host" dlg)

testNewConnGroupOpensPrompt :: IO Bool
testNewConnGroupOpensPrompt = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar '3')
    dlg <- readIORef (asDialogMode st)
    assertEq "newconn '3' opens group prompt" True (isDlgPromptWithSubstring "Group" dlg)

testNewConnUnknownCloses :: IO Bool
testNewConnUnknownCloses = do
    st <- mkTestState; writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNewConnDlg st (KeyChar 'z')
    dlg <- readIORef (asDialogMode st)
    assertEq "newconn unknown keeps dialog open" True (isDlgNewConn dlg)

testNewConnMouseOptionOpensPrompt :: IO Bool
testNewConnMouseOptionOpensPrompt = do
    st <- mkTestState
    let (r0, c0, _, _) = overlayBounds calcTestLayout 5
    writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNormal st (KeyMouseLeft (r0 + 2) (c0 + 8))
    dlg <- readIORef (asDialogMode st)
    assertEq "newconn mouse single option opens prompt" True (isDlgPromptWithSubstring "host" dlg)

testNewConnMouseCancelCloses :: IO Bool
testNewConnMouseCancelCloses = do
    st <- mkTestState
    let lines' = newConnOverlayLines
        lineIx = length lines' - 1
        line = lines' !! lineIx
        (r0, c0, _, _) = overlayBounds calcTestLayout (length lines')
        row = r0 + 1 + lineIx
        col = c0 + 2 + findSubstringPos "[ Cancel ]" line + 1
    writeIORef (asDialogMode st) (Just DlgNewConn)
    handleNormal st (KeyMouseLeft row col)
    dlg <- readIORef (asDialogMode st)
    assertEq "newconn mouse Cancel closes dialog" True (isDlgNothing dlg)

prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

isIn :: String -> String -> Bool
isIn needle haystack = any (\i -> take (length needle) (drop i haystack) == needle)
    [0 .. length haystack - length needle]

findLineIndex :: String -> [String] -> Int
findLineIndex needle = go 0
  where
    go _ [] = 0
    go n (line:rest)
        | line == needle = n
        | otherwise = go (n + 1) rest

findSubstringPos :: String -> String -> Int
findSubstringPos needle = go 0
  where
    go _ [] = 0
    go n hay
        | needle `prefixOf` hay = n
        | otherwise = go (n + 1) (tail hay)

withDB :: FilePath -> (FilePath -> IO Bool) -> IO Bool
withDB name action = do
    tmp <- getTemporaryDirectory
    let path = tmp </> name
    result <- action path `catch` \(e :: SomeException) -> do
        putStrLn $ "  FAIL: dialogs DB exception: " ++ show e
        pure False
    cleanup path
    pure result
  where
    cleanup path = removeFile path `catch` (\(_ :: SomeException) -> pure ())
