-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Input
    ( readKey, readCSI, readSS3, drainSeq
    , eventLoop
    , handleNormal, handleContact, handleChat, handleDialog
    , handleSettingsDlg, handleNewConnDlg
    , strip'
    , startListenerIfNeeded
    , acceptLoopTUI
    ) where

import Control.Concurrent (forkIO)
import Control.Exception (catch, finally, SomeException)
import Control.Monad (void, when, unless)
import Data.ByteString (ByteString)
import Data.List (isPrefixOf)
import Data.IORef (IORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import System.IO (stdin, hReady)
import UmbraVox.App.RuntimeLog (logEvent)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Render (render, clearScreen)
import UmbraVox.TUI.Menu (toggleMenu, openMenu, closeMenu, handleMenu)
import UmbraVox.TUI.Constants (maxInputLen, maxDialogBufLen)
import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey)
import UmbraVox.TUI.Actions (startNewConn, startVerify, startExport,
    startSettings, startKeysView, startBrowse, addSecureNotes, showHelp,
    renameContact, sendCurrentMessage, quitApp, setStatus,
    adjustContactScroll, addSession, selectLast, recvLoopTUI)
import UmbraVox.TUI.Handshake (handshakeInitiator, handshakeResponder,
    genIdentity, fingerprint)
import UmbraVox.Network.Transport (listen, connect, connectTryPorts)
import UmbraVox.Network.TransportClass (AnyTransport(..))
import UmbraVox.Storage.Anthony (clearConversation)
import UmbraVox.Protocol.Encoding (splitOn, safeReadPort, defaultPorts, parseHostPort)

-- Input handling ----------------------------------------------------------
readKey :: IO InputEvent
readKey = do
    c <- getChar
    case c of
        '\n'   -> pure KeyEnter
        '\r'   -> pure KeyEnter
        '\t'   -> pure KeyTab
        '\DEL' -> pure KeyBackspace
        '\x7F' -> pure KeyBackspace
        '\x08' -> pure KeyBackspace
        '\x0E' -> pure KeyCtrlN
        '\x11' -> pure KeyCtrlQ
        '\x04' -> pure KeyCtrlD
        '\ESC' -> do
            ready <- hReady stdin
            if not ready then pure KeyEscape else do
                c2 <- getChar
                case c2 of
                    '[' -> readCSI; 'O' -> readSS3; _ -> pure KeyEscape
        _ -> pure (KeyChar c)

readCSI :: IO InputEvent
readCSI = do
    c <- getChar
    case c of
        'A' -> pure KeyUp; 'B' -> pure KeyDown
        'C' -> pure KeyRight; 'D' -> pure KeyLeft
        '5' -> drainTilde >> pure KeyPageUp
        '6' -> drainTilde >> pure KeyPageDown
        '1' -> readCSIExtended
        _ -> drainSeq >> pure KeyUnknown
  where drainTilde = hReady stdin >>= \r -> when r (void getChar)

-- | Handle extended CSI sequences like ESC[1x~ for F-keys
-- F1=ESC[11~, F2=ESC[12~, F3=ESC[13~, F4=ESC[14~, F5=ESC[15~
readCSIExtended :: IO InputEvent
readCSIExtended = do
    c2 <- getChar
    case c2 of
        '1' -> drainTilde' >> pure KeyF1   -- ESC[11~
        '2' -> drainTilde' >> pure KeyF2   -- ESC[12~
        '3' -> drainTilde' >> pure KeyF3   -- ESC[13~
        '4' -> drainTilde' >> pure KeyF4   -- ESC[14~
        '5' -> drainTilde' >> pure KeyF5   -- ESC[15~
        _   -> drainSeq >> pure KeyUnknown
  where drainTilde' = hReady stdin >>= \r -> when r (void getChar)

readSS3 :: IO InputEvent
readSS3 = do
    c <- getChar
    case c of
        'P' -> pure KeyF1; 'Q' -> pure KeyF2
        'R' -> pure KeyF3; 'S' -> pure KeyF4
        _   -> pure KeyUnknown

drainSeq :: IO ()
drainSeq = hReady stdin >>= \r -> when r (void getChar >> drainSeq)

-- Main event loop ---------------------------------------------------------
eventLoop :: AppState -> IO ()
eventLoop st = do
    running <- readIORef (asRunning st)
    when running $ do
        key <- readKey
        dlg <- readIORef (asDialogMode st)
        case dlg of
            Just _  -> handleDialog st key
            Nothing -> do
                mOpen <- readIORef (asMenuOpen st)
                case mOpen of
                    Just _  -> handleMenu st key
                    Nothing -> handleNormal st key
        readIORef (asRunning st) >>= \r -> when r (render st >> eventLoop st)

-- Normal key handling -----------------------------------------------------
handleNormal :: AppState -> InputEvent -> IO ()
handleNormal st key = do
    focus <- readIORef (asFocus st)
    case key of
        KeyTab   -> modifyIORef' (asFocus st) (\p ->
            if p == ContactPane then ChatPane else ContactPane)
        KeyCtrlN -> startNewConn st
        KeyCtrlQ -> quitApp st
        KeyCtrlD -> quitApp st
        KeyF1    -> toggleMenu st MenuHelp
        KeyF2    -> toggleMenu st MenuContacts
        KeyF3    -> toggleMenu st MenuChat
        KeyF4    -> toggleMenu st MenuPrefs
        KeyF5    -> pure ()
        KeyEscape -> do
            dlg <- readIORef (asDialogMode st)
            case dlg of
                Just _  -> writeIORef (asDialogMode st) Nothing
                Nothing -> pure ()
        _ -> case focus of
            ContactPane -> handleContact st key
            ChatPane    -> handleChat st key

-- Contact key handling ----------------------------------------------------

handleContact :: AppState -> InputEvent -> IO ()
handleContact st key = do
    n <- Map.size <$> readIORef (cfgSessions (asConfig st))
    lay <- readIORef (asLayout st)
    let visRows = lChatH lay
    case key of
        KeyUp    -> do
            modifyIORef' (asSelected st) (\i -> max 0 (i-1))
            adjustContactScroll st visRows
        KeyDown  -> do
            modifyIORef' (asSelected st) (\i -> min (max 0 (n-1)) (i+1))
            adjustContactScroll st visRows
        KeyEnter -> writeIORef (asFocus st) ChatPane >> writeIORef (asChatScroll st) 0
        _ -> pure ()

handleChat :: AppState -> InputEvent -> IO ()
handleChat st key = do
    lay <- readIORef (asLayout st)
    let ch = lChatH lay
    case key of
        KeyChar c    -> modifyIORef' (asInputBuf st) (\s ->
            if length s >= maxInputLen then s else s ++ [c])
        KeyBackspace -> modifyIORef' (asInputBuf st) (\s -> if null s then s else init s)
        KeyEnter     -> sendCurrentMessage st
        KeyUp        -> modifyIORef' (asChatScroll st) (+1)
        KeyDown      -> modifyIORef' (asChatScroll st) (\s -> max 0 (s-1))
        KeyPageUp    -> modifyIORef' (asChatScroll st) (+ch)
        KeyPageDown  -> modifyIORef' (asChatScroll st) (\s -> max 0 (s-ch))
        _            -> pure ()

-- Dialogs -----------------------------------------------------------------
handleDialog :: AppState -> InputEvent -> IO ()
handleDialog st KeyEscape = writeIORef (asDialogMode st) Nothing
handleDialog st key = do
    dlg <- readIORef (asDialogMode st)
    case dlg of
        Just DlgHelp    -> writeIORef (asDialogMode st) Nothing
        Just DlgKeys    -> writeIORef (asDialogMode st) Nothing
        Just DlgVerify  -> writeIORef (asDialogMode st) Nothing
        Just DlgBrowse  -> writeIORef (asDialogMode st) Nothing
        Just DlgWelcome -> writeIORef (asDialogMode st) Nothing
        Just DlgSettings -> handleSettingsDlg st key
        Just DlgNewConn  -> handleNewConnDlg st key
        Just (DlgPrompt _ cb) -> case key of
            KeyEnter -> do
                b <- readIORef (asDialogBuf st); cb b
                writeIORef (asDialogBuf st) ""
                writeIORef (asDialogMode st) Nothing
            KeyChar c -> modifyIORef' (asDialogBuf st) (\s ->
                if length s >= maxDialogBufLen then s else s ++ [c])
            KeyBackspace -> modifyIORef' (asDialogBuf st) (\s -> if null s then s else init s)
            _ -> pure ()
        _ -> writeIORef (asDialogMode st) Nothing

handleSettingsDlg :: AppState -> InputEvent -> IO ()
handleSettingsDlg st (KeyChar '1') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Set Port" $ \val ->
        case reads val of
            [(p,_)] -> do
                writeIORef (cfgListenPort (asConfig st)) (p :: Int)
                logEvent (asConfig st) "settings.listen_port"
                    [("port", show p), ("restart_required", "true")]
                setStatus st ("Listen port set to " ++ show p ++ " (restart required)")
            _ -> pure ()))
handleSettingsDlg st (KeyChar '2') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Set Display Name" $ \val ->
        unless (null val) $ do
            writeIORef (cfgDisplayName (asConfig st)) val
            logEvent (asConfig st) "settings.display_name" [("updated", "true")]
            ))
handleSettingsDlg st (KeyChar '3') =
    toggleSettingWithStatus st "settings.mdns" (cfgMDNSEnabled (asConfig st))
        "mDNS enabled (restart required)"
        "mDNS disabled (restart required)"
handleSettingsDlg st (KeyChar '4') =
    toggleSettingWithStatus st "settings.pex" (cfgPEXEnabled (asConfig st))
        "Peer exchange enabled"
        "Peer exchange disabled"
handleSettingsDlg st (KeyChar '5') =
    toggleSettingWithStatus st "settings.db_enabled" (cfgDBEnabled (asConfig st))
        "Persistent storage enabled (restart required)"
        "Persistent storage disabled (restart required)"
handleSettingsDlg st (KeyChar '6') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Set DB Path" $ \val ->
        unless (null val) $ do
            writeIORef (cfgDBPath (asConfig st)) val
            logEvent (asConfig st) "settings.db_path"
                [("path", val), ("restart_required", "true")]
            setStatus st "Database path updated (restart required)"))
handleSettingsDlg st (KeyChar '7') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Retention (days, 0=forever)" $ \val ->
        case reads val of
            [(d,_)] -> do
                let days = max 0 (d :: Int)
                writeIORef (cfgRetentionDays (asConfig st)) days
                logEvent (asConfig st) "settings.retention_days" [("days", show days)]
            _ -> pure () ))
handleSettingsDlg st (KeyChar '8') =
    toggleSettingWithStatus st "settings.auto_save" (cfgAutoSaveMessages (asConfig st))
        "Auto-save messages enabled"
        "Auto-save messages disabled"
handleSettingsDlg st (KeyChar '9') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Clear history? Type YES to confirm" $ \val ->
        when (val == "YES") $ do
            mDb <- readIORef (cfgAnthonyDB (asConfig st))
            sel <- readIORef (asSelected st)
            case mDb of
                Just db -> do
                    sessions <- readIORef (cfgSessions (asConfig st))
                    let entries = Map.toList sessions
                    when (sel < length entries) $ do
                        let (sid, _) = entries !! sel
                        clearConversation db sid
                Nothing -> pure ()
            -- Also clear the in-memory history for the selected session
            sessions <- readIORef (cfgSessions (asConfig st))
            let entries = Map.toList sessions
            when (sel < length entries) $ do
                let (_, si) = entries !! sel
                writeIORef (siHistory si) []
                logEvent (asConfig st) "history.clear" [("selected_index", show sel)]
            ))
handleSettingsDlg st (KeyChar 'a') =
    toggleSettingWithStatus st "settings.debug_logging" (cfgDebugLogging (asConfig st))
        "Runtime debug logging enabled"
        "Runtime debug logging disabled"
handleSettingsDlg st (KeyChar 'A') = handleSettingsDlg st (KeyChar 'a')
handleSettingsDlg st (KeyChar 'b') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Set Log Path" $ \val ->
        unless (null val) $ do
            writeIORef (cfgDebugLogPath (asConfig st)) val
            logEvent (asConfig st) "settings.debug_log_path" [("path", val)]
            setStatus st "Runtime log path updated"))
handleSettingsDlg st (KeyChar 'B') = handleSettingsDlg st (KeyChar 'b')
handleSettingsDlg st (KeyChar '0') =
    writeIORef (asDialogMode st) (Just DlgKeys)
handleSettingsDlg st _ = writeIORef (asDialogMode st) Nothing

handleNewConnDlg :: AppState -> InputEvent -> IO ()
-- 1 = Private (secure notes, local only)
handleNewConnDlg st (KeyChar '1') = do
    writeIORef (asDialogMode st) Nothing
    addSecureNotes st
-- 2 = Single (connect to one peer via host:port or listen)
handleNewConnDlg st (KeyChar '2') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "host:port (or 'listen' or 'listen:PORT')" $ \val ->
        if val == "listen" || "listen:" `isPrefixOf` val then do
            port <- case break (== ':') val of
                (_, ':':p) -> case reads p of
                    [(n, _)] -> pure (n :: Int)
                    _        -> readIORef (cfgListenPort (asConfig st))
                _          -> readIORef (cfgListenPort (asConfig st))
            ik <- getOrCreateIdentity (asConfig st)
            void (startListenerIfNeeded st ik port "dialog")
        else do
            let (h, mPort) = parseHostPort val
            case mPort of
                Just port -> do
                    logEvent (asConfig st) "transport.connect.attempt"
                        [("host", h), ("port", show port)]
                    setStatus st ("Connecting to " ++ h ++ ":" ++ show port ++ "...")
                    void $ forkIO $ (do
                        ik <- getOrCreateIdentity (asConfig st)
                        t <- connect h port; let at = AnyTransport t
                        session <- handshakeInitiator at ik
                        sid <- addSession (asConfig st) at session (h ++ ":" ++ show port)
                        selectLast st; setStatus st ("Connected #" ++ show sid)
                        ) `catch` (\(e::SomeException) -> setStatus st ("Failed: "++show e))
                Nothing -> do
                    logEvent (asConfig st) "transport.connect.attempt_defaults"
                        [("host", h)]
                    setStatus st ("Connecting to " ++ h ++ " (trying default ports)...")
                    void $ forkIO $ (do
                        ik <- getOrCreateIdentity (asConfig st)
                        t <- connectTryPorts h defaultPorts; let at = AnyTransport t
                        session <- handshakeInitiator at ik
                        sid <- addSession (asConfig st) at session h
                        selectLast st; setStatus st ("Connected #" ++ show sid)
                        ) `catch` (\(e::SomeException) -> setStatus st ("Failed: "++show e))
        ))
-- 3 = Group (multiple peers)
handleNewConnDlg st (KeyChar '3') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Group: host:port (comma-separated)" $ \val -> do
        let peers = filter (not . null) $ splitOn ',' val
        setStatus st ("Connecting to " ++ show (length peers) ++ " peers...")
        void $ forkIO $ do
            ik <- getOrCreateIdentity (asConfig st)
            successes <- connectGroupPeers st ik peers 0
            when (successes > 0) $ selectLast st
            setStatus st ("Group connected: " ++ show successes ++ "/" ++ show (length peers))
        ))
handleNewConnDlg st _ = writeIORef (asDialogMode st) Nothing

strip' :: String -> String
strip' = dropWhile (== ' ')

-- | Get the persistent identity key, generating one if it doesn't exist yet.
getOrCreateIdentity :: AppConfig -> IO IdentityKey
getOrCreateIdentity cfg = do
    mIk <- readIORef (cfgIdentity cfg)
    case mIk of
        Just ik -> pure ik
        Nothing -> do
            ik <- genIdentity
            writeIORef (cfgIdentity cfg) (Just ik)
            pure ik

acceptLoopTUI :: AppState -> IdentityKey -> Int -> IO ()
acceptLoopTUI st ik port = do
    logEvent (asConfig st) "listener.awaiting_connection" [("port", show port)]
    t <- listen port
    let at = AnyTransport t
        trustCheck :: ByteString -> IO Bool
        trustCheck peerKey = do
            mode <- readIORef (cfgConnectionMode (asConfig st))
            case mode of
                Swing       -> do
                    setStatus st ("Swing: accepted " ++ fingerprint peerKey)
                    pure True
                Promiscuous -> pure True
                Selective   -> do
                    setStatus st ("Peer: " ++ fingerprint peerKey)
                    pure True
                Chaste      -> do
                    keys <- readIORef (cfgTrustedKeys (asConfig st))
                    pure (any (constantEq peerKey) keys)
                Chastity    -> do
                    keys <- readIORef (cfgTrustedKeys (asConfig st))
                    pure (any (constantEq peerKey) keys)
    logEvent (asConfig st) "listener.accepted_connection" [("port", show port)]
    session <- handshakeResponder at ik trustCheck
    sid <- addSession (asConfig st) at session ("peer:" ++ show port)
    selectLast st
    setStatus st ("Session #" ++ show sid)
    acceptLoopTUI st ik port

startListenerIfNeeded :: AppState -> IdentityKey -> Int -> String -> IO Bool
startListenerIfNeeded st ik port source = do
    mTid <- readIORef (cfgListenerThread (asConfig st))
    case mTid of
        Just _ -> do
            logEvent (asConfig st) "listener.start.skipped"
                [ ("port", show port)
                , ("source", source)
                , ("reason", "already_running")
                ]
            setStatus st ("Listener already running on " ++ show port)
            pure False
        Nothing -> do
            logEvent (asConfig st) "listener.start"
                [ ("port", show port)
                , ("source", source)
                ]
            setStatus st ("Listening on " ++ show port ++ "...")
            tid <- forkIO (listenerWorker st ik port)
            writeIORef (cfgListenerThread (asConfig st)) (Just tid)
            pure True

listenerWorker :: AppState -> IdentityKey -> Int -> IO ()
listenerWorker st ik port =
    (acceptLoopTUI st ik port
        `catch` (\(e :: SomeException) -> do
            logEvent (asConfig st) "listener.stop"
                [ ("port", show port)
                , ("reason", show e)
                ]
            setStatus st ("Listener stopped: " ++ show e)))
    `finally` writeIORef (cfgListenerThread (asConfig st)) Nothing

connectGroupPeers :: AppState -> IdentityKey -> [String] -> Int -> IO Int
connectGroupPeers _ _ [] successes = pure successes
connectGroupPeers st ik (p:ps) successes =
    ((do
        let peer = strip' p
            (h, mPort) = parseHostPort peer
        t <- case mPort of
            Just port -> connect h port
            Nothing   -> connectTryPorts h defaultPorts
        let at = AnyTransport t
        session <- handshakeInitiator at ik
        void $ addSession (asConfig st) at session peer
        connectGroupPeers st ik ps (successes + 1)
        ) `catch` (\(_ :: SomeException) -> connectGroupPeers st ik ps successes))

toggleSettingWithStatus :: AppState -> String -> IORef Bool -> String -> String -> IO ()
toggleSettingWithStatus st eventName ref enabledMsg disabledMsg = do
    modifyIORef' ref not
    enabled <- readIORef ref
    logEvent (asConfig st) eventName [("enabled", show enabled)]
    setStatus st (if enabled then enabledMsg else disabledMsg)
