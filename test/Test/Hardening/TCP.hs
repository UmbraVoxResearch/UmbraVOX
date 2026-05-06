-- SPDX-License-Identifier: Apache-2.0
-- | Real TCP end-to-end hardening scenarios.
module Test.Hardening.TCP (runTests) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, finally)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Posix.Files
    ( fileMode
    , getFileStatus
    , groupModes
    , otherModes
    , ownerReadMode
    , ownerWriteMode
    )

import Test.Harness
    ( TransportBackend(..), TestClient(..), closeClient
    , createClientPairWith, createNamedClientPairWith
    , handshakeClients, clientSend, clientRecv
    )
import Test.TUI.Sim.Util (mkTestState)
import Test.Util (assertEq)
import UmbraVox.Network.TransportClass (anyClose)
import UmbraVox.TUI.Actions.Session (sendCurrentMessage)
import UmbraVox.TUI.Handshake (genIdentity)
import UmbraVox.TUI.Input
    ( acceptLoopTUI, handleDialog, handleNewConnDlg, startListenerIfNeeded )
import UmbraVox.TUI.Types
    ( AppState(..), AppConfig(..), DialogMode(..), InputEvent(..)
    , SessionInfo(..), ContactStatus(..)
    )

runTests :: IO Bool
runTests = do
    putStrLn "[Hardening/TCP] Running real TCP hardening tests..."
    results <- sequence
        [ testTCPHandshakeRoundTrip
        , testTCPBidirectional
        , testTUIBidirectionalRuntime
        , testTUIRuntimeLogging
        , testTUIListenerSingleOwner
        , testTUIDisconnectMarksOffline
        , testTCPReconnectWithStableIdentities
        , testTCPLargePayload
        ]
    let passed = length (filter id results)
        total = length results
    putStrLn $ "[Hardening/TCP] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

testTCPHandshakeRoundTrip :: IO Bool
testTCPHandshakeRoundTrip = do
    logRef <- newIORef []
    (alice, bob) <- createClientPairWith logRef (TCPBackend 19401)
    handshakeClients alice bob
    clientSend alice (BC.pack "tcp-hello")
    result <- newEmptyMVar
    _ <- forkIO $ putMVar result =<< clientRecv bob
    got <- clientRecvResult "tcp round-trip" result
    closeClient alice
    closeClient bob
    assertEq "tcp round-trip plaintext" (Just (BC.pack "tcp-hello")) got

testTCPBidirectional :: IO Bool
testTCPBidirectional = do
    logRef <- newIORef []
    (alice, bob) <- createClientPairWith logRef (TCPBackend 19402)
    handshakeClients alice bob
    clientSend alice (BC.pack "from-alice")
    clientSend bob (BC.pack "from-bob")
    aResult <- newEmptyMVar
    bResult <- newEmptyMVar
    _ <- forkIO $ putMVar aResult =<< clientRecv alice
    _ <- forkIO $ putMVar bResult =<< clientRecv bob
    aMsg <- clientRecvResult "tcp bidir alice" aResult
    bMsg <- clientRecvResult "tcp bidir bob" bResult
    closeClient alice
    closeClient bob
    ok1 <- assertEq "tcp bidir alice recv" (Just (BC.pack "from-bob")) aMsg
    ok2 <- assertEq "tcp bidir bob recv" (Just (BC.pack "from-alice")) bMsg
    pure (ok1 && ok2)

testTUIBidirectionalRuntime :: IO Bool
testTUIBidirectionalRuntime = do
    alice <- mkTestState
    bob <- mkTestState
    aliceIk <- genIdentity
    bobIk <- genIdentity
    writeIORef (cfgIdentity (asConfig alice)) (Just aliceIk)
    writeIORef (cfgIdentity (asConfig bob)) (Just bobIk)
    writeIORef (cfgListenPort (asConfig alice)) 19406
    writeIORef (cfgListenPort (asConfig bob)) 19407
    listenerTid <- forkIO (acceptLoopTUI alice aliceIk 19406)
    let cleanup = do
            cleanupSessions alice
            cleanupSessions bob
            killThread listenerTid `catch` ignoreError
            threadDelay 50000
    (`finally` cleanup) $ do
        threadDelay 50000
        writeIORef (asDialogMode bob) (Just DlgNewConn)
        handleNewConnDlg bob (KeyChar '2')
        feedPrompt bob "127.0.0.1:19406"
        connectedAlice <- waitForSessionCount alice 1 5000
        connectedBob <- waitForSessionCount bob 1 5000
        okConnectedAlice <- assertEq "tui runtime alice connected" True connectedAlice
        okConnectedBob <- assertEq "tui runtime bob connected" True connectedBob

        writeIORef (asInputBuf bob) "from-bob"
        sendCurrentMessage bob
        bobLoggedSend <- waitForHistoryLine bob "You: from-bob" 5000
        aliceSawBob <- waitForHistoryLine alice "Peer: from-bob" 5000
        okBobLoggedSend <- assertEq "tui runtime bob local echo" True bobLoggedSend
        okAliceSawBob <- assertEq "tui runtime alice recv" True aliceSawBob

        writeIORef (asInputBuf alice) "from-alice"
        sendCurrentMessage alice
        aliceLoggedSend <- waitForHistoryLine alice "You: from-alice" 5000
        bobSawAlice <- waitForHistoryLine bob "Peer: from-alice" 5000
        okAliceLoggedSend <- assertEq "tui runtime alice local echo" True aliceLoggedSend
        okBobSawAlice <- assertEq "tui runtime bob recv" True bobSawAlice

        pure
            ( okConnectedAlice && okConnectedBob
           && okBobLoggedSend && okAliceSawBob
           && okAliceLoggedSend && okBobSawAlice
            )

testTUIRuntimeLogging :: IO Bool
testTUIRuntimeLogging = do
    let logPath = "build/test-runtime-events.log"
    createDirectoryIfMissing True "build"
    exists <- doesFileExist logPath
    whenExists exists (removeFile logPath)
    alice <- mkTestState
    bob <- mkTestState
    writeIORef (cfgDebugLogging (asConfig alice)) True
    writeIORef (cfgDebugLogging (asConfig bob)) True
    writeIORef (cfgDebugLogPath (asConfig alice)) logPath
    writeIORef (cfgDebugLogPath (asConfig bob)) logPath
    aliceIk <- genIdentity
    bobIk <- genIdentity
    writeIORef (cfgIdentity (asConfig alice)) (Just aliceIk)
    writeIORef (cfgIdentity (asConfig bob)) (Just bobIk)
    writeIORef (cfgListenPort (asConfig alice)) 19408
    listenerTid <- forkIO (acceptLoopTUI alice aliceIk 19408)
    let cleanup = do
            cleanupSessions alice
            cleanupSessions bob
            killThread listenerTid `catch` ignoreError
            threadDelay 50000
    (`finally` cleanup) $ do
        threadDelay 50000
        writeIORef (asDialogMode bob) (Just DlgNewConn)
        handleNewConnDlg bob (KeyChar '2')
        feedPrompt bob "127.0.0.1:19408"
        connectedAlice <- waitForSessionCount alice 1 5000
        connectedBob <- waitForSessionCount bob 1 5000
        writeIORef (asInputBuf bob) "log-from-bob"
        sendCurrentMessage bob
        _ <- waitForHistoryLine alice "Peer: log-from-bob" 5000
        writeIORef (asInputBuf alice) "log-from-alice"
        sendCurrentMessage alice
        _ <- waitForHistoryLine bob "Peer: log-from-alice" 5000
        logExists <- waitForFile logPath 5000
        logReady <- waitForLogEvents logPath
            [ "session.add.remote"
            , "message.send"
            , "message.recv"
            ]
            5000
        permsOk <- logFilePrivate logPath
        okConnectedAlice <- assertEq "tui runtime log alice connected" True connectedAlice
        okConnectedBob <- assertEq "tui runtime log bob connected" True connectedBob
        okLogExists <- assertEq "tui runtime log file exists" True logExists
        okPerms <- assertEq "tui runtime log file perms" True permsOk
        okLogReady <- assertEq "tui runtime log events" True logReady
        pure
            ( okConnectedAlice && okConnectedBob
           && okLogExists && okPerms && okLogReady
            )

testTUIListenerSingleOwner :: IO Bool
testTUIListenerSingleOwner = do
    st <- mkTestState
    ik <- genIdentity
    writeIORef (cfgIdentity (asConfig st)) (Just ik)
    let port = 19410
    started <- startListenerIfNeeded st ik port "test"
    mTid1 <- waitForListenerThread st 5000
    startedAgain <- startListenerIfNeeded st ik port "duplicate"
    mTid2 <- readIORef (cfgListenerThread (asConfig st))
    status <- readIORef (asStatusMsg st)
    cleanupListener st
    okStarted <- assertEq "tui listener first start" True started
    okTracked <- assertEq "tui listener tracked" True (isJust mTid1)
    okSkipped <- assertEq "tui listener duplicate skipped" False startedAgain
    okPreserved <- assertEq "tui listener tid preserved" True (sameThread mTid1 mTid2)
    okStatus <- assertEq "tui listener duplicate status"
        ("Listener already running on " ++ show port)
        status
    pure (okStarted && okTracked && okSkipped && okPreserved && okStatus)

testTUIDisconnectMarksOffline :: IO Bool
testTUIDisconnectMarksOffline = do
    alice <- mkTestState
    bob <- mkTestState
    aliceIk <- genIdentity
    bobIk <- genIdentity
    writeIORef (cfgIdentity (asConfig alice)) (Just aliceIk)
    writeIORef (cfgIdentity (asConfig bob)) (Just bobIk)
    writeIORef (cfgListenPort (asConfig alice)) 19409
    listenerTid <- forkIO (acceptLoopTUI alice aliceIk 19409)
    let cleanup = do
            cleanupSessions alice
            cleanupSessions bob
            killThread listenerTid `catch` ignoreError
            threadDelay 50000
    (`finally` cleanup) $ do
        threadDelay 50000
        writeIORef (asDialogMode bob) (Just DlgNewConn)
        handleNewConnDlg bob (KeyChar '2')
        feedPrompt bob "127.0.0.1:19409"
        connectedAlice <- waitForSessionCount alice 1 5000
        connectedBob <- waitForSessionCount bob 1 5000
        closeOnlyTransport bob
        aliceOffline <- waitForSessionOffline alice 5000
        aliceLostHistory <- waitForAnyHistoryLine alice
            ["[Connection lost]", "[Peer disconnected]"]
            5000
        okConnectedAlice <- assertEq "tui disconnect alice connected" True connectedAlice
        okConnectedBob <- assertEq "tui disconnect bob connected" True connectedBob
        okAliceOffline <- assertEq "tui disconnect alice offline" True aliceOffline
        okAliceLostHistory <- assertEq "tui disconnect alice history" True aliceLostHistory
        pure
            ( okConnectedAlice && okConnectedBob
           && okAliceOffline && okAliceLostHistory
            )

testTCPReconnectWithStableIdentities :: IO Bool
testTCPReconnectWithStableIdentities = do
    logRef1 <- newIORef []
    (alice1, bob1) <- createClientPairWith logRef1 (TCPBackend 19403)
    handshakeClients alice1 bob1
    aliceId <- pure (tcIdentity alice1)
    bobId <- pure (tcIdentity bob1)
    closeClient alice1
    closeClient bob1

    logRef2 <- newIORef []
    (alice2, bob2) <-
        createNamedClientPairWith logRef2 (TCPBackend 19404)
            "alice" (Just aliceId) "bob" (Just bobId)
    handshakeClients alice2 bob2
    clientSend alice2 (BC.pack "reconnected")
    result <- newEmptyMVar
    _ <- forkIO $ putMVar result =<< clientRecv bob2
    got <- clientRecvResult "tcp reconnect" result
    closeClient alice2
    closeClient bob2
    assertEq "tcp reconnect payload" (Just (BC.pack "reconnected")) got

testTCPLargePayload :: IO Bool
testTCPLargePayload = do
    logRef <- newIORef []
    (alice, bob) <- createClientPairWith logRef (TCPBackend 19405)
    handshakeClients alice bob
    let payload = BS.replicate 32768 0x5a
    clientSend alice payload
    result <- newEmptyMVar
    _ <- forkIO $ putMVar result =<< clientRecv bob
    got <- clientRecvResult "tcp large payload" result
    closeClient alice
    closeClient bob
    assertEq "tcp large payload preserved" (Just payload) got

clientRecvResult :: String -> MVar (Maybe BS.ByteString) -> IO (Maybe BS.ByteString)
clientRecvResult _label = takeMVar

feedPrompt :: AppState -> String -> IO ()
feedPrompt st chars = do
    mapM_ (\c -> handleDialog st (KeyChar c)) chars
    handleDialog st KeyEnter

waitForSessionCount :: AppState -> Int -> Int -> IO Bool
waitForSessionCount st expected timeoutMs
    | timeoutMs <= 0 = do
        sessions <- readIORef (cfgSessions (asConfig st))
        pure (Map.size sessions >= expected)
    | otherwise = do
        sessions <- readIORef (cfgSessions (asConfig st))
        if Map.size sessions >= expected
            then pure True
            else threadDelay 10000 >> waitForSessionCount st expected (timeoutMs - 10)

waitForHistoryLine :: AppState -> String -> Int -> IO Bool
waitForHistoryLine st needle timeoutMs
    | timeoutMs <= 0 = maybe (pure False) hasLine =<< getOnlySession st
    | otherwise = do
        found <- maybe (pure False) hasLine =<< getOnlySession st
        if found
            then pure True
            else threadDelay 10000 >> waitForHistoryLine st needle (timeoutMs - 10)
  where
    hasLine si = any (isInfixOf needle) <$> readIORef (siHistory si)

waitForAnyHistoryLine :: AppState -> [String] -> Int -> IO Bool
waitForAnyHistoryLine st needles timeoutMs
    | timeoutMs <= 0 = maybe (pure False) hasAnyLine =<< getOnlySession st
    | otherwise = do
        found <- maybe (pure False) hasAnyLine =<< getOnlySession st
        if found
            then pure True
            else threadDelay 10000 >> waitForAnyHistoryLine st needles (timeoutMs - 10)
  where
    hasAnyLine si = do
        history <- readIORef (siHistory si)
        pure (any (\needle -> any (isInfixOf needle) history) needles)

waitForFile :: FilePath -> Int -> IO Bool
waitForFile path timeoutMs
    | timeoutMs <= 0 = doesFileExist path
    | otherwise = do
        exists <- doesFileExist path
        if exists
            then pure True
            else threadDelay 10000 >> waitForFile path (timeoutMs - 10)

waitForSessionOffline :: AppState -> Int -> IO Bool
waitForSessionOffline st timeoutMs
    | timeoutMs <= 0 = maybe (pure False) isOffline =<< getOnlySession st
    | otherwise = do
        offline <- maybe (pure False) isOffline =<< getOnlySession st
        if offline
            then pure True
            else threadDelay 10000 >> waitForSessionOffline st (timeoutMs - 10)
  where
    isOffline si = do
        status <- readIORef (siStatus si)
        let noTransport = case siTransport si of
                Nothing -> True
                Just _ -> False
            noRecvThread = case siRecvTid si of
                Nothing -> True
                Just _ -> False
        pure (status == Offline && noTransport && noRecvThread)

logFilePrivate :: FilePath -> IO Bool
logFilePrivate path = do
    exists <- doesFileExist path
    if not exists
        then pure False
        else do
            status <- getFileStatus path
            let mode = fileMode status
                ownerOk = mode .&. ownerReadMode /= 0 && mode .&. ownerWriteMode /= 0
                groupClear = mode .&. groupModes == 0
                otherClear = mode .&. otherModes == 0
            pure (ownerOk && groupClear && otherClear)

getOnlySession :: AppState -> IO (Maybe SessionInfo)
getOnlySession st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    pure $ snd <$> Map.lookupMin sessions

cleanupSessions :: AppState -> IO ()
cleanupSessions st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    mapM_ cleanupSession (Map.elems sessions)

cleanupSession :: SessionInfo -> IO ()
cleanupSession si = do
    case siTransport si of
        Just t -> anyClose t `catch` ignoreError
        Nothing -> pure ()
    case siRecvTid si of
        Just tid -> killThread tid `catch` ignoreError
        Nothing -> pure ()

cleanupListener :: AppState -> IO ()
cleanupListener st = do
    mTid <- readIORef (cfgListenerThread (asConfig st))
    case mTid of
        Just tid -> killThread tid `catch` ignoreError
        Nothing -> pure ()
    writeIORef (cfgListenerThread (asConfig st)) Nothing

waitForListenerThread :: AppState -> Int -> IO (Maybe ThreadId)
waitForListenerThread st timeoutMs
    | timeoutMs <= 0 = readIORef (cfgListenerThread (asConfig st))
    | otherwise = do
        mTid <- readIORef (cfgListenerThread (asConfig st))
        case mTid of
            Just _ -> pure mTid
            Nothing -> threadDelay 10000 >> waitForListenerThread st (timeoutMs - 10)

sameThread :: Maybe ThreadId -> Maybe ThreadId -> Bool
sameThread (Just tid1) (Just tid2) = tid1 == tid2
sameThread _ _ = False

closeOnlyTransport :: AppState -> IO ()
closeOnlyTransport st = do
    mSession <- getOnlySession st
    case mSession >>= siTransport of
        Just t -> do
            _ <- forkIO (anyClose t `catch` ignoreError)
            pure ()
        Nothing -> pure ()

ignoreError :: SomeException -> IO ()
ignoreError _ = pure ()

whenExists :: Bool -> IO () -> IO ()
whenExists True action = action
whenExists False _ = pure ()

waitForLogEvents :: FilePath -> [String] -> Int -> IO Bool
waitForLogEvents path needles timeoutMs
    | timeoutMs <= 0 = checkLogFile path needles
    | otherwise = do
        found <- checkLogFile path needles
        if found
            then pure True
            else threadDelay 10000 >> waitForLogEvents path needles (timeoutMs - 10)

checkLogFile :: FilePath -> [String] -> IO Bool
checkLogFile path needles = do
    exists <- doesFileExist path
    if not exists
        then pure False
        else do
            contents <- readFile path
            pure (all (`isInfixOf` contents) needles)
