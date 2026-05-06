module UmbraVox.TUI.Input
    ( readKey, readCSI, readSS3, drainSeq
    , eventLoop
    , handleNormal, handleContact, handleChat, handleDialog
    , handleSettingsDlg, handleNewConnDlg
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad (void, when, unless)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import System.IO (stdin, hReady)
import UmbraVox.TUI.Types
import UmbraVox.TUI.Render (render, clearScreen)
import UmbraVox.TUI.Actions (startNewConn, startVerify, startExport,
    startSettings, startKeysView, addSecureNotes, showHelp, renameContact,
    sendCurrentMessage, quitApp, setStatus, adjustContactScroll,
    addSession, selectLast, recvLoopTUI)
import UmbraVox.TUI.Handshake (handshakeInitiator, handshakeResponder)
import UmbraVox.Network.Transport (listen, connect)

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
        '\x17' -> pure KeyCtrlW
        '\x12' -> pure KeyCtrlR
        '\x18' -> pure KeyCtrlX
        '\x10' -> pure KeyCtrlP
        '\x11' -> pure KeyCtrlQ
        '\x0C' -> pure KeyCtrlL
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
        '5' -> drainTilde >> pure KeyPageUp
        '6' -> drainTilde >> pure KeyPageDown
        _ -> drainSeq >> pure KeyUnknown
  where drainTilde = hReady stdin >>= \r -> when r (void getChar)

readSS3 :: IO InputEvent
readSS3 = getChar >> pure KeyUnknown

drainSeq :: IO ()
drainSeq = hReady stdin >>= \r -> when r (void getChar >> drainSeq)

-- Main event loop ---------------------------------------------------------
eventLoop :: AppState -> IO ()
eventLoop st = do
    running <- readIORef (asRunning st)
    when running $ do
        key <- readKey
        dlg <- readIORef (asDialogMode st)
        case dlg of { Just _ -> handleDialog st key; Nothing -> handleNormal st key }
        readIORef (asRunning st) >>= \r -> when r (render st >> eventLoop st)

-- Normal key handling -----------------------------------------------------
handleNormal :: AppState -> InputEvent -> IO ()
handleNormal st key = do
    focus <- readIORef (asFocus st)
    case key of
        KeyTab   -> modifyIORef' (asFocus st) (\p -> if p==ContactPane then ChatPane else ContactPane)
        KeyCtrlN -> startNewConn st
        KeyCtrlR -> startVerify st
        KeyCtrlX -> startExport st
        KeyCtrlP -> startSettings st
        KeyCtrlQ -> quitApp st
        KeyCtrlW -> quitApp st
        KeyCtrlL -> clearScreen
        KeyCtrlD -> quitApp st
        KeyEscape -> do
            dlg <- readIORef (asDialogMode st)
            case dlg of
                Just _  -> writeIORef (asDialogMode st) Nothing
                Nothing -> pure ()
        _ -> case focus of { ContactPane -> handleContact st key; ChatPane -> handleChat st key }

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
            modifyIORef' (asSelected st) (\i -> min (n-1) (i+1))
            adjustContactScroll st visRows
        KeyEnter -> writeIORef (asFocus st) ChatPane >> writeIORef (asChatScroll st) 0
        KeyChar 'n' -> startNewConn st; KeyChar 'N' -> startNewConn st
        KeyChar 'g' -> setStatus st "Group: add peers with N, msgs go to all"
        KeyChar 'G' -> setStatus st "Group: add peers with N, msgs go to all"
        KeyChar 'r' -> renameContact st; KeyChar 'R' -> renameContact st
        KeyChar 'k' -> startKeysView st; KeyChar 'K' -> startKeysView st
        KeyChar 's' -> addSecureNotes st; KeyChar 'S' -> addSecureNotes st
        KeyChar '?' -> showHelp st
        _ -> pure ()

handleChat :: AppState -> InputEvent -> IO ()
handleChat st key = do
    lay <- readIORef (asLayout st)
    let ch = lChatH lay
    case key of
        KeyChar c    -> modifyIORef' (asInputBuf st) (++[c])
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
        Just DlgSettings -> handleSettingsDlg st key
        Just DlgNewConn  -> handleNewConnDlg st key
        Just (DlgPrompt _ cb) -> case key of
            KeyEnter -> do
                b <- readIORef (asDialogBuf st); cb b
                writeIORef (asDialogBuf st) ""
                writeIORef (asDialogMode st) Nothing
            KeyChar c -> modifyIORef' (asDialogBuf st) (++[c])
            KeyBackspace -> modifyIORef' (asDialogBuf st) (\s -> if null s then s else init s)
            _ -> pure ()
        _ -> writeIORef (asDialogMode st) Nothing

handleSettingsDlg :: AppState -> InputEvent -> IO ()
handleSettingsDlg st (KeyChar '1') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Set Port" $ \val ->
        case reads val of { [(p,_)] -> writeIORef (cfgListenPort (asConfig st)) (p::Int); _ -> pure () }))
handleSettingsDlg st (KeyChar '2') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Set Display Name" $ \val ->
        unless (null val) $ writeIORef (cfgDisplayName (asConfig st)) val))
handleSettingsDlg st (KeyChar '3') =
    modifyIORef' (cfgMDNSEnabled (asConfig st)) not
handleSettingsDlg st (KeyChar '4') =
    modifyIORef' (cfgPEXEnabled (asConfig st)) not
handleSettingsDlg st (KeyChar '5') =
    modifyIORef' (cfgDBEnabled (asConfig st)) not
handleSettingsDlg st (KeyChar '6') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Set DB Path" $ \val ->
        unless (null val) $ writeIORef (cfgDBPath (asConfig st)) val))
handleSettingsDlg st (KeyChar '7') =
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
    writeIORef (asDialogMode st) (Just (DlgPrompt "host:port (or 'listen')" $ \val ->
        if val == "listen" then do
            port <- readIORef (cfgListenPort (asConfig st))
            setStatus st ("Listening on " ++ show port ++ "...")
            void $ forkIO $ (do
                t <- listen port; session <- handshakeResponder t
                sid <- addSession (asConfig st) t session ("peer:" ++ show port)
                selectLast st; setStatus st ("Session #" ++ show sid)
                ) `catch` (\(e::SomeException) -> setStatus st ("Failed: "++show e))
        else do
            let (host, portStr) = break (==':') val
                port = if null portStr then 9999 else read (drop 1 portStr) :: Int
                h = if null host then "127.0.0.1" else host
            setStatus st ("Connecting to " ++ h ++ ":" ++ show port ++ "...")
            void $ forkIO $ (do
                t <- connect h port; session <- handshakeInitiator t
                sid <- addSession (asConfig st) t session (h ++ ":" ++ show port)
                selectLast st; setStatus st ("Connected #" ++ show sid)
                ) `catch` (\(e::SomeException) -> setStatus st ("Failed: "++show e))
        ))
-- 3 = Group (multiple peers)
handleNewConnDlg st (KeyChar '3') = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Group: host:port (comma-separated)" $ \val -> do
        let peers = filter (not . null) $ splitOn ',' val
        setStatus st ("Connecting to " ++ show (length peers) ++ " peers...")
        void $ forkIO $ mapM_ (\p -> (do
            let (host, portStr) = break (==':') (strip' p)
                port = if null portStr then 9999 else read (drop 1 portStr) :: Int
                h = if null host then "127.0.0.1" else host
            t <- connect h port; session <- handshakeInitiator t
            void $ addSession (asConfig st) t session (h ++ ":" ++ show port)
            ) `catch` (\(e::SomeException) -> setStatus st ("Failed: "++show e))
            ) peers
        selectLast st; setStatus st ("Group with " ++ show (length peers) ++ " peers")
        ))
handleNewConnDlg st _ = writeIORef (asDialogMode st) Nothing

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = let (w, rest) = break (== c) s
              in w : case rest of { [] -> []; (_:r) -> splitOn c r }

strip' :: String -> String
strip' = dropWhile (== ' ')
