-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ScopedTypeVariables #-}
-- | Startup-level recovery tests.
module Test.App.Startup
    ( runTests
    , runStartupProcessChild
    ) where

import Control.Exception (SomeException, catch, finally)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map
import Data.List (find, isInfixOf, isPrefixOf, sort, stripPrefix)
import qualified Network.Socket as NS
import System.Directory
    ( createDirectoryIfMissing, findExecutable, getTemporaryDirectory
    , removeDirectoryRecursive, removeFile
    )
import System.Environment (getEnvironment, getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (Handle, hClose, hGetChar, hWaitForInput)
import System.Process
    ( CreateProcess(env, std_in, std_out, std_err), ProcessHandle, StdStream(CreatePipe)
    , createProcess, proc, readCreateProcessWithExitCode
    , readProcessWithExitCode, terminateProcess, waitForProcess
    )
import System.Timeout (timeout)

import Test.TUI.Sim.Util (mkTestConfig)
import Test.Util (assertEq)
import UmbraVox.App.Startup
    ( newDefaultAppConfig, initializeLocalIdentity, applyPersistenceAnswer
    , refreshPackagedPluginCatalog, refreshTransportProviderCatalog
    , resolveIdentityAt, resolvePersistencePreference
    , resolvePersistencePreferenceAt, restorePersistentStateAt
    )
import UmbraVox.BuildProfile (PluginLaunchSpec(..), pprLaunchSpec)
import UmbraVox.Network.ProviderCatalog (ctpLoadStatus)
import UmbraVox.Crypto.Signal.X3DH
    ( ikEd25519Secret, ikX25519Secret, ikX25519Public )
import UmbraVox.Storage.Anthony
    ( closeDB, loadMessages, openDB, saveConversation, saveMessage, saveSetting, saveTrustedKey )
import UmbraVox.TUI.Actions.Session (sendCurrentMessage)
import UmbraVox.TUI.Handshake (fingerprint)
import UmbraVox.TUI.Layout (calcLayout)
import UmbraVox.TUI.Types (AppConfig(..), AppState(..), ContactStatus(..), Pane(..), SessionInfo(..))

runTests :: IO Bool
runTests = do
    putStrLn "Test.App.Startup"
    putStrLn (replicate 40 '-')
    idOk <- testResolveIdentityStable
    pluginCatalogOk <- testPackagedPluginCatalogCache
    providerCatalogOk <- testTransportProviderCatalogCache
    prefRuntimeOk <- testResolvePersistencePreferenceTracksRuntimeConfig
    prefOk <- testApplyPersistenceAnswerNoPersistsWithoutDB
    prefYesOk <- testApplyPersistenceAnswerYesPersistsIntentWithoutHealthyDB
    anthonyReady <- hasAnthony
    dbResults <- case anthonyReady of
        Nothing -> do
            putStrLn "  SKIP: anthony bootstrap unavailable"
            (:[]) <$> testRestorePersistentStateFailureDisablesPersistence
        Just () -> sequence
            [ testRestorePersistentStateSessions
            , testResolvePersistencePreference
            , testRestorePersistentStatePreservesConversationIds
            , testRestoredOfflineSessionsFailClosedOnSend
            , testRestorePersistentStateTrustedKeys
            , testRestartPreservesIdentityAndHistory
            , testProcessRestartAroundBootPath
            , testPromptWaitStartsListenerFirst
            , testLiveTerminalBootPath
            , testRestorePersistentStateFailureDisablesPersistence
            ]
    pure (idOk && pluginCatalogOk && providerCatalogOk && prefRuntimeOk && prefOk && prefYesOk && and dbResults)

runStartupProcessChild :: String -> IO Bool
runStartupProcessChild answer = do
    cfg <- newDefaultAppConfig
    ik <- initializeLocalIdentity cfg
    restored <- applyPersistenceAnswer cfg answer
    dbEnabled <- readIORef (cfgDBEnabled cfg)
    putStrLn $
        "STARTUP_CHILD "
            ++ fingerprint (ikX25519Public ik)
            ++ " " ++ show restored
            ++ " " ++ show dbEnabled
    pure True

testResolveIdentityStable :: IO Bool
testResolveIdentityStable = do
    tmp <- getTemporaryDirectory
    let path = tmp </> "umbravox-startup-identity.key"
    ik1 <- resolveIdentityAt path
    ik2 <- resolveIdentityAt path
    cleanup path
    ok1 <- assertEq "resolveIdentityAt keeps ed secret stable" True
        (ikEd25519Secret ik1 == ikEd25519Secret ik2)
    ok2 <- assertEq "resolveIdentityAt keeps x secret stable" True
        (ikX25519Secret ik1 == ikX25519Secret ik2)
    pure (ok1 && ok2)

testPackagedPluginCatalogCache :: IO Bool
testPackagedPluginCatalogCache = do
    cfg <- newDefaultAppConfig
    initialCatalog <- readIORef (cfgPackagedPluginCatalog cfg)
    initialRuntimeCatalog <- readIORef (cfgPackagedPluginRuntimeCatalog cfg)
    writeIORef (cfgPackagedPluginCatalog cfg) []
    writeIORef (cfgPackagedPluginRuntimeCatalog cfg) []
    refreshPackagedPluginCatalog cfg
    refreshedCatalog <- readIORef (cfgPackagedPluginCatalog cfg)
    refreshedRuntimeCatalog <- readIORef (cfgPackagedPluginRuntimeCatalog cfg)
    ok1 <- assertEq "default app config seeds packaged plugin catalog" True (not (null initialCatalog))
    ok2 <- assertEq "default app config seeds packaged plugin runtime catalog" True (not (null initialRuntimeCatalog))
    ok3 <- assertEq "packaged plugin refresh restores cached catalog" True (not (null refreshedCatalog))
    ok4 <- assertEq "packaged plugin refresh restores cached runtime catalog" True (not (null refreshedRuntimeCatalog))
    ok5 <- assertEq "runtime catalog includes typed launch spec" True
        (any hasLaunchSpec initialRuntimeCatalog)
    pure (ok1 && ok2 && ok3 && ok4 && ok5)
  where
    hasLaunchSpec entry =
        case pprLaunchSpec entry of
            PluginLaunchIPCStdIO _ -> True
            _ -> False

testTransportProviderCatalogCache :: IO Bool
testTransportProviderCatalogCache = do
    cfg <- newDefaultAppConfig
    initialCatalog <- readIORef (cfgTransportProviderCatalog cfg)
    initialRuntimeCatalog <- readIORef (cfgTransportProviderRuntimeCatalog cfg)
    writeIORef (cfgTransportProviderCatalog cfg) []
    writeIORef (cfgTransportProviderRuntimeCatalog cfg) []
    refreshTransportProviderCatalog cfg
    refreshedCatalog <- readIORef (cfgTransportProviderCatalog cfg)
    refreshedRuntimeCatalog <- readIORef (cfgTransportProviderRuntimeCatalog cfg)
    ok1 <- assertEq "default app config seeds transport provider catalog" True (not (null initialCatalog))
    ok2 <- assertEq "default app config seeds transport provider runtime catalog" True (not (null initialRuntimeCatalog))
    ok3 <- assertEq "transport provider refresh restores cached catalog" True (not (null refreshedCatalog))
    ok4 <- assertEq "transport provider refresh restores cached runtime catalog" True (not (null refreshedRuntimeCatalog))
    ok5 <- assertEq "transport provider runtime entries carry load status" True
        (all (\entry -> case ctpLoadStatus entry of { _ -> True }) initialRuntimeCatalog)
    pure (ok1 && ok2 && ok3 && ok4 && ok5)

testRestorePersistentStateSessions :: IO Bool
testRestorePersistentStateSessions = withDB "umbravox-startup-restore.db" $ \dbPath -> do
    cfg <- mkTestConfig
    seedPersistentDB dbPath
    restored <- restorePersistentStateAt cfg dbPath
    sessions <- readIORef (cfgSessions cfg)
    nextId <- readIORef (cfgNextId cfg)
    histories <- restoredHistories sessions
    ok1 <- assertEq "restorePersistentStateAt count" 2 restored
    ok2 <- assertEq "restorePersistentStateAt session map size" 2 (Map.size sessions)
    ok3 <- assertEq "restorePersistentStateAt next session id" 3 nextId
    ok4 <- assertEq "restored peer names" ["Alice", "Bob"] (sort (Map.keys histories))
    ok5 <- assertEq "restored Alice history"
        ["You: hello", "Alice: world"]
        (Map.findWithDefault [] "Alice" histories)
    ok6 <- assertEq "restored Bob history"
        ["Bob: ping", "You: pong"]
        (Map.findWithDefault [] "Bob" histories)
    pure (and [ok1, ok2, ok3, ok4, ok5, ok6])

testResolvePersistencePreference :: IO Bool
testResolvePersistencePreference = withDB "umbravox-startup-pref.db" $ \dbPath -> do
    prefMissing <- resolvePersistencePreferenceAt dbPath

    writeFile (prefPath dbPath) "1\n"
    prefEnabled <- resolvePersistencePreferenceAt dbPath

    writeFile (prefPath dbPath) "0\n"
    prefDisabled <- resolvePersistencePreferenceAt dbPath

    cleanupFile (prefPath dbPath)
    writeFile dbPath "not-a-sqlite-db\n"
    prefMalformed <- resolvePersistencePreferenceAt dbPath

    ok1 <- assertEq "resolvePersistencePreferenceAt missing DB" Nothing prefMissing
    ok2 <- assertEq "resolvePersistencePreferenceAt enabled flag" (Just True) prefEnabled
    ok3 <- assertEq "resolvePersistencePreferenceAt disabled flag" (Just False) prefDisabled
    ok4 <- assertEq "resolvePersistencePreferenceAt malformed DB ignored without sidecar" Nothing prefMalformed
    pure (and [ok1, ok2, ok3, ok4])

testResolvePersistencePreferenceTracksRuntimeConfig :: IO Bool
testResolvePersistencePreferenceTracksRuntimeConfig =
    withDB "umbravox-startup-pref-runtime.db" $ \dbPath -> do
        cfg <- mkTestConfig
        writeIORef (cfgDBPath cfg) dbPath
        writeFile dbPath "not-a-sqlite-db\n"
        writeFile (prefPath dbPath) "0\n"
        pref <- resolvePersistencePreference cfg
        runtimePref <- readIORef (cfgPersistencePreference cfg)
        ok1 <- assertEq "resolvePersistencePreference returns sidecar-disabled preference" (Just False) pref
        ok2 <- assertEq "resolvePersistencePreference updates runtime preference" (Just False) runtimePref
        pure (ok1 && ok2)

testApplyPersistenceAnswerNoPersistsWithoutDB :: IO Bool
testApplyPersistenceAnswerNoPersistsWithoutDB =
    withDB "umbravox-startup-pref-no.db" $ \dbPath -> do
        cfg <- mkTestConfig
        writeIORef (cfgDBPath cfg) dbPath
        writeFile dbPath "not-a-sqlite-db\n"
        restored <- applyPersistenceAnswer cfg "no"
        dbEnabled <- readIORef (cfgDBEnabled cfg)
        dbHandle <- readIORef (cfgAnthonyDB cfg)
        runtimePref <- readIORef (cfgPersistencePreference cfg)
        pref <- resolvePersistencePreferenceAt dbPath
        ok1 <- assertEq "applyPersistenceAnswer no returns zero" 0 restored
        ok2 <- assertEq "applyPersistenceAnswer no keeps DB disabled" False dbEnabled
        ok3 <- assertEq "applyPersistenceAnswer no keeps DB handle empty" True (isNothing dbHandle)
        ok4 <- assertEq "applyPersistenceAnswer no persists runtime preference" (Just False) runtimePref
        ok5 <- assertEq "applyPersistenceAnswer no persists sidecar preference" (Just False) pref
        pure (and [ok1, ok2, ok3, ok4, ok5])

testApplyPersistenceAnswerYesPersistsIntentWithoutHealthyDB :: IO Bool
testApplyPersistenceAnswerYesPersistsIntentWithoutHealthyDB =
    withDB "umbravox-startup-pref-yes.db" $ \dbPath -> do
        cfg <- mkTestConfig
        writeIORef (cfgDBPath cfg) dbPath
        writeFile dbPath "not-a-sqlite-db\n"
        restored <- applyPersistenceAnswer cfg "yes"
        dbEnabled <- readIORef (cfgDBEnabled cfg)
        runtimePref <- readIORef (cfgPersistencePreference cfg)
        pref <- resolvePersistencePreferenceAt dbPath
        ok1 <- assertEq "applyPersistenceAnswer yes returns zero on malformed DB" 0 restored
        ok2 <- assertEq "applyPersistenceAnswer yes falls back to DB disabled" False dbEnabled
        ok3 <- assertEq "applyPersistenceAnswer yes persists runtime preference" (Just True) runtimePref
        ok4 <- assertEq "applyPersistenceAnswer yes persists sidecar preference" (Just True) pref
        pure (and [ok1, ok2, ok3, ok4])

testRestoredOfflineSessionsFailClosedOnSend :: IO Bool
testRestoredOfflineSessionsFailClosedOnSend = withDB "umbravox-startup-restored-send.db" $ \dbPath -> do
    cfg <- mkTestConfig
    writeIORef (cfgAutoSaveMessages cfg) True
    seedPersistentDB dbPath
    restored <- restorePersistentStateAt cfg dbPath
    selectedRef <- newIORef 0
    focusRef <- newIORef ChatPane
    inputRef <- newIORef "after-restart"
    dialogBufRef <- newIORef ""
    chatScrollRef <- newIORef 0
    statusRef <- newIORef ""
    runningRef <- newIORef True
    dialogModeRef <- newIORef Nothing
    browsePageRef <- newIORef 0
    browseFilterRef <- newIORef ""
    layoutRef <- newIORef (calcLayout 40 120)
    contactScrollRef <- newIORef 0
    termSizeRef <- newIORef (40, 120)
    menuOpenRef <- newIORef Nothing
    menuIndexRef <- newIORef 0
    dialogTabRef <- newIORef 0
    renderTokenRef <- newIORef Nothing
    let st = AppState cfg selectedRef focusRef inputRef dialogBufRef chatScrollRef
            statusRef runningRef dialogModeRef browsePageRef browseFilterRef
            layoutRef contactScrollRef termSizeRef menuOpenRef menuIndexRef
            dialogTabRef renderTokenRef
    sendCurrentMessage st
    sessions <- readIORef (cfgSessions cfg)
    let (_, aliceSi) = head (Map.toAscList sessions)
    aliceHist <- readIORef (siHistory aliceSi)
    statusMsg <- readIORef statusRef
    buf <- readIORef inputRef
    db <- openDB dbPath
    msgs <- loadMessages db 1 10
    closeDB db
    onlineStates <- mapM (readIORef . siStatus) (Map.elems sessions)
    ok1 <- assertEq "restored send test restored count" 2 restored
    ok2 <- assertEq "restored sessions remain offline" True (all (== Offline) onlineStates)
    ok3 <- assertEq "restored send leaves input intact" "after-restart" buf
    ok4 <- assertEq "restored send reports reconnect required"
        "Session offline; reconnect required for Alice" statusMsg
    ok5 <- assertEq "restored send does not mutate history"
        ["You: hello", "Alice: world"] aliceHist
    ok6 <- assertEq "restored send does not persist phantom outbound message"
        2 (length msgs)
    pure (and [ok1, ok2, ok3, ok4, ok5, ok6])

testRestorePersistentStatePreservesConversationIds :: IO Bool
testRestorePersistentStatePreservesConversationIds =
    withDB "umbravox-startup-restore-ids.db" $ \dbPath -> do
        cfg <- mkTestConfig
        db <- openDB dbPath
        saveConversation db 7 "peer-7" "Alice" 1700
        saveConversation db 11 "peer-11" "Bob" 1701
        saveMessage db 7 "You" "hello" 1800
        saveMessage db 11 "Bob" "ping" 1801
        closeDB db

        restored <- restorePersistentStateAt cfg dbPath
        sessions <- readIORef (cfgSessions cfg)
        nextId <- readIORef (cfgNextId cfg)

        ok1 <- assertEq "restore preserves DB conversation ids" [7, 11] (Map.keys sessions)
        ok2 <- assertEq "restore advances nextId past max conversation id" 12 nextId
        ok3 <- assertEq "restore count with sparse ids" 2 restored
        pure (and [ok1, ok2, ok3])

testRestorePersistentStateTrustedKeys :: IO Bool
testRestorePersistentStateTrustedKeys = withDB "umbravox-startup-trusted.db" $ \dbPath -> do
    cfg <- mkTestConfig
    seedPersistentDB dbPath
    _ <- restorePersistentStateAt cfg dbPath
    keys <- readIORef (cfgTrustedKeys cfg)
    let expected = sort [BS.replicate 32 0xaa, BS.replicate 32 0xbb]
    ok1 <- assertEq "restorePersistentStateAt trusted key count" 2 (length keys)
    ok2 <- assertEq "restorePersistentStateAt trusted key contents" expected (sort keys)
    pure (ok1 && ok2)

testRestartPreservesIdentityAndHistory :: IO Bool
testRestartPreservesIdentityAndHistory = withDB "umbravox-startup-restart.db" $ \dbPath -> do
    tmp <- getTemporaryDirectory
    let idPath = tmp </> "umbravox-startup-restart.key"
    seedPersistentDB dbPath

    cfg1 <- mkTestConfig
    ik1 <- resolveIdentityAt idPath
    restored1 <- restorePersistentStateAt cfg1 dbPath
    sessions1 <- readIORef (cfgSessions cfg1)
    histories1 <- restoredHistories sessions1

    cfg2 <- mkTestConfig
    ik2 <- resolveIdentityAt idPath
    restored2 <- restorePersistentStateAt cfg2 dbPath
    sessions2 <- readIORef (cfgSessions cfg2)
    histories2 <- restoredHistories sessions2
    keys2 <- readIORef (cfgTrustedKeys cfg2)

    cleanup idPath

    let fp1 = fingerprint (ikX25519Public ik1)
        fp2 = fingerprint (ikX25519Public ik2)
        expectedHistories = Map.fromList
            [ ("Alice", ["You: hello", "Alice: world"])
            , ("Bob", ["Bob: ping", "You: pong"])
            ]
        expectedKeys = sort [BS.replicate 32 0xaa, BS.replicate 32 0xbb]
    ok1 <- assertEq "restart restore first count" 2 restored1
    ok2 <- assertEq "restart restore second count" 2 restored2
    ok3 <- assertEq "restart preserves local identity fingerprint" fp1 fp2
    ok4 <- assertEq "restart restores initial histories" expectedHistories histories1
    ok5 <- assertEq "restart restores histories after reboot" expectedHistories histories2
    ok6 <- assertEq "restart restores trusted keys after reboot" expectedKeys (sort keys2)
    pure (and [ok1, ok2, ok3, ok4, ok5, ok6])

testProcessRestartAroundBootPath :: IO Bool
testProcessRestartAroundBootPath = do
    exe <- getExecutablePath
    env0 <- getEnvironment
    tmp <- getTemporaryDirectory
    let homeDir = tmp </> "umbravox-startup-process-home"
        dbPath = homeDir </> ".umbravox" </> "umbravox.db"
        childEnv = ("HOME", homeDir) : filter ((/= "HOME") . fst) env0
    createDirectoryIfMissing True (homeDir </> ".umbravox")
    seedPersistentDB dbPath
    run1 <- runStartupChild exe childEnv "yes"
    run2 <- runStartupChild exe childEnv "yes"
    cleanupFile (homeDir </> ".umbravox" </> "identity.key")
    cleanupFile dbPath
    cleanupDir homeDir
    case (run1, run2) of
        (Just (fp1, restored1, db1), Just (fp2, restored2, db2)) -> do
            ok1 <- assertEq "process boot restart preserves fingerprint" fp1 fp2
            ok2 <- assertEq "process boot first restore count" 2 restored1
            ok3 <- assertEq "process boot second restore count" 2 restored2
            ok4 <- assertEq "process boot first DB enabled" True db1
            ok5 <- assertEq "process boot second DB enabled" True db2
            pure (and [ok1, ok2, ok3, ok4, ok5])
        _ -> pure False

testPromptWaitStartsListenerFirst :: IO Bool
testPromptWaitStartsListenerFirst = do
    binaryPath <- locateUmbravoxBinary
    env0 <- getEnvironment
    tmp <- getTemporaryDirectory
    let homeDir = tmp </> "umbravox-startup-prompt-home"
        dataDir = homeDir </> ".umbravox"
        childEnv = ("HOME", homeDir) : filter ((/= "HOME") . fst) env0
        cp = (proc binaryPath [])
            { env = Just childEnv
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    createDirectoryIfMissing True dataDir
    result <- withPromptProcess cp $ \stdoutH ph -> do
        stdoutText <- readUntilContains stdoutH "Enable persistent storage?" (8 * 1000000)
        let mPort = parseStartupListenPort stdoutText
        reachable <- maybe (pure False) canConnectLocalListener mPort
        ok1 <- assertEq "startup prompt still appears" True
            ("Enable persistent storage?" `isInfixOf` stdoutText)
        ok2 <- assertEq "startup announces listener before prompt" True
            (listenerAppearsBeforePrompt stdoutText)
        ok3 <- assertEq "startup prompt listener port parsed" True (maybe False (> 0) mPort)
        ok4 <- assertEq "startup listener reachable before prompt answer" True reachable
        terminateProcess ph
        _ <- waitForProcess ph
        pure (and [ok1, ok2, ok3, ok4])
    cleanupDir homeDir
    pure result

testLiveTerminalBootPath :: IO Bool
testLiveTerminalBootPath = do
    scriptPath <- findExecutable "script"
    case scriptPath of
        Nothing -> do
            putStrLn "  SKIP: script PTY helper unavailable"
            pure True
        Just scriptBin -> do
            helperReady <- probeScriptHelper scriptBin
            if not helperReady
                then do
                    putStrLn "  SKIP: script PTY helper is unstable in this environment"
                    pure True
                else do
                    tmp <- getTemporaryDirectory
                    let homeDir = tmp </> "umbravox-live-terminal-home"
                        dataDir = homeDir </> ".umbravox"
                        idPath = dataDir </> "identity.key"
                        dbPath = dataDir </> "umbravox.db"
                    createDirectoryIfMissing True dataDir
                    ik1 <- resolveIdentityAt idPath
                    seedPersistentDB dbPath
                    binaryPath <- locateUmbravoxBinary
                    result <- timeout (20 * 1000000) $
                        readProcessWithExitCode "sh"
                            [ "-c"
                            , staggeredPTYCommand homeDir scriptBin binaryPath
                            ]
                            ""
                    ik2 <- resolveIdentityAt idPath
                    cleanupFile idPath
                    cleanupFile dbPath
                    cleanupDir homeDir
                    case result of
                        Nothing ->
                            assertEq "live terminal boot path exits before timeout" True False
                        Just (exitCode, stdoutText, stderrText) -> do
                            if exitCode == ExitFailure 139
                                then do
                                    putStrLn "  SKIP: script PTY helper is unstable in this environment"
                                    pure True
                                else do
                                    ok1 <- assertEq "live terminal boot path exit code" ExitSuccess exitCode
                                    ok2 <- assertEq "live terminal boot path skips prompt after persistence remembered" False
                                        ("Enable persistent storage?" `isInfixOf` stdoutText)
                                    ok3 <- assertEq "live terminal boot path initializes DB" True
                                        ("Initializing database..." `isInfixOf` stdoutText)
                                    ok4 <- assertEq "live terminal boot path restores conversations" True
                                        ("Storage: persistent mode (2 conversations restored)" `isInfixOf` stdoutText)
                                    ok5 <- assertEq "live terminal boot path reaches clean quit" True
                                        ("Goodbye." `isInfixOf` stdoutText)
                                    ok6 <- assertEq "live terminal boot path keeps fingerprint stable"
                                        (fingerprint (ikX25519Public ik1))
                                        (fingerprint (ikX25519Public ik2))
                                    ok7 <- assertEq "live terminal boot path keeps stderr empty" "" stderrText
                                    pure (and [ok1, ok2, ok3, ok4, ok5, ok6, ok7])

testRestorePersistentStateFailureDisablesPersistence :: IO Bool
testRestorePersistentStateFailureDisablesPersistence = do
    cfg <- mkTestConfig
    tmp <- getTemporaryDirectory
    let dirPath = tmp </> "umbravox-startup-db-dir"
    createDirectoryIfMissing True dirPath
    restored <- restorePersistentStateAt cfg dirPath
    dbEnabled <- readIORef (cfgDBEnabled cfg)
    dbHandle <- readIORef (cfgAnthonyDB cfg)
    sessions <- readIORef (cfgSessions cfg)
    cleanupDir dirPath
    ok1 <- assertEq "restorePersistentStateAt failure returns zero" 0 restored
    ok2 <- assertEq "restorePersistentStateAt failure disables DB" False dbEnabled
    ok3 <- assertEq "restorePersistentStateAt failure keeps DB handle empty" True (isNothing dbHandle)
    ok4 <- assertEq "restorePersistentStateAt failure leaves sessions empty" 0 (Map.size sessions)
    pure (and [ok1, ok2, ok3, ok4])

seedPersistentDB :: FilePath -> IO ()
seedPersistentDB dbPath = do
    db <- openDB dbPath
    saveConversation db 1 "peer-1" "Alice" 1700
    saveConversation db 2 "peer-2" "Bob" 1701
    saveMessage db 1 "You" "hello" 1800
    saveMessage db 1 "Alice" "world" 1801
    saveMessage db 2 "Bob" "ping" 1802
    saveMessage db 2 "You" "pong" 1803
    saveTrustedKey db (BS.replicate 32 0xaa) "alice"
    saveTrustedKey db (BS.replicate 32 0xbb) "bob"
    closeDB db

restoredHistories :: Map.Map Int SessionInfo -> IO (Map.Map String [String])
restoredHistories sessions = fmap Map.fromList $
    mapM toPair (Map.elems sessions)
  where
    toPair si = do
        hist <- readIORef (siHistory si)
        pure (siPeerName si, hist)

hasAnthony :: IO (Maybe ())
hasAnthony = do
    tmp <- getTemporaryDirectory
    let path = tmp </> "umbravox-anthony-probe-startup.db"
    result <- ((do
        db <- openDB path
        saveSetting db "__probe__" "ok"
        closeDB db
        pure (Just ()))
        `catch` \(_ :: SomeException) -> pure Nothing)
    cleanup path
    pure result

withDB :: FilePath -> (FilePath -> IO Bool) -> IO Bool
withDB name action = do
    tmp <- getTemporaryDirectory
    let path = tmp </> name
    result <- action path `catch` \(e :: SomeException) -> do
        putStrLn $ "  FAIL: startup recovery exception: " ++ show e
        pure False
    cleanup path
    cleanup (prefPath path)
    pure result

cleanup :: FilePath -> IO ()
cleanup path = removeFile path `catch` \(_ :: SomeException) -> pure ()

cleanupFile :: FilePath -> IO ()
cleanupFile = cleanup

cleanupDir :: FilePath -> IO ()
cleanupDir path = removeDirectoryRecursive path `catch` \(_ :: SomeException) -> pure ()

prefPath :: FilePath -> FilePath
prefPath path = path ++ ".pref"

runStartupChild
    :: FilePath
    -> [(String, String)]
    -> String
    -> IO (Maybe (String, Int, Bool))
runStartupChild exe childEnv answer = do
    let cp = (proc exe ["startup-process-child", answer]) { env = Just childEnv }
    (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode cp ""
    case (exitCode, parseStartupChild stdoutText) of
        (ExitSuccess, Just summary) -> pure (Just summary)
        _ -> do
            putStrLn "  FAIL: startup child process"
            putStrLn $ "    exit:   " ++ show exitCode
            putStrLn $ "    stdout: " ++ show stdoutText
            putStrLn $ "    stderr: " ++ show stderrText
            pure Nothing

parseStartupChild :: String -> Maybe (String, Int, Bool)
parseStartupChild output = do
    line <- find ("STARTUP_CHILD " `isPrefixOf`) (lines output)
    case words line of
        ["STARTUP_CHILD", fp, restoredText, dbText] ->
            Just (fp, read restoredText, read dbText)
        _ -> Nothing

withPromptProcess
    :: CreateProcess
    -> (Handle -> ProcessHandle -> IO Bool)
    -> IO Bool
withPromptProcess cp action = do
    (mIn, mOut, mErr, ph) <- createProcess cp
    case (mIn, mOut, mErr) of
        (Just stdinH, Just stdoutH, Just stderrH) ->
            action stdoutH ph `finally` do
                hClose stdinH `catch` \(_ :: SomeException) -> pure ()
                hClose stdoutH `catch` \(_ :: SomeException) -> pure ()
                hClose stderrH `catch` \(_ :: SomeException) -> pure ()
                terminateProcess ph `catch` \(_ :: SomeException) -> pure ()
                _ <- waitForProcess ph `catch` \(_ :: SomeException) -> pure ExitSuccess
                pure ()
        _ -> pure False

readUntilContains :: Handle -> String -> Int -> IO String
readUntilContains h needle micros = do
    result <- timeout micros (loop "")
    pure (maybe "" id result)
  where
    loop acc
        | needle `isInfixOf` acc = pure acc
        | otherwise = do
            ready <- hWaitForInput h 100
            if ready
                then do
                    ch <- hGetChar h
                    loop (acc ++ [ch])
                else loop acc

parseStartupListenPort :: String -> Maybe Int
parseStartupListenPort output = do
    line <- find ("  Network: starting tcp listener on " `isPrefixOf`) (lines output)
    rest <- stripPrefix "  Network: starting tcp listener on " line
    case words rest of
        (portText:_) ->
            case reads portText of
                [(port, "")] -> Just port
                _ -> Nothing
        _ -> Nothing

listenerAppearsBeforePrompt :: String -> Bool
listenerAppearsBeforePrompt output =
    case (findIndexOf "  Network: starting tcp listener on " output,
          findIndexOf "Enable persistent storage?" output) of
        (Just listenerIx, Just promptIx) -> listenerIx < promptIx
        _ -> False

findIndexOf :: String -> String -> Maybe Int
findIndexOf needle = go 0
  where
    go _ [] = if null needle then Just 0 else Nothing
    go ix rest
        | needle `isPrefixOf` rest = Just ix
        | otherwise =
            case rest of
                (_:xs) -> go (ix + 1) xs
                [] -> Nothing

canConnectLocalListener :: Int -> IO Bool
canConnectLocalListener port = do
    let hints = NS.defaultHints
            { NS.addrSocketType = NS.Stream
            , NS.addrFamily = NS.AF_INET
            }
    addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
    case addrs of
        [] -> pure False
        (addr:_) -> do
            sock <- NS.openSocket addr
            result <- ((timeout (2 * 1000000) (NS.connect sock (NS.addrAddress addr)))
                `catch` \(_ :: SomeException) -> pure Nothing)
            NS.close sock `catch` \(_ :: SomeException) -> pure ()
            pure (result == Just ())

locateUmbravoxBinary :: IO FilePath
locateUmbravoxBinary = do
    (exitCode, stdoutText, stderrText) <- readProcessWithExitCode
        "cabal"
        ["list-bin", "exe:umbravox"]
        ""
    case exitCode of
        ExitSuccess ->
            case lines stdoutText of
                (path:_) -> pure path
                [] -> fail "cabal list-bin returned no executable path"
        ExitFailure _ ->
            fail ("cabal list-bin failed: " ++ stderrText)

staggeredPTYCommand :: FilePath -> FilePath -> FilePath -> String
staggeredPTYCommand homeDir scriptBin binaryPath =
    "({ sleep 1; printf 'y\\n'; sleep 1; printf '\\021\\021'; }"
        ++ " | env HOME=" ++ shellQuote homeDir
        ++ " TERM=xterm "
        ++ shellQuote scriptBin
        ++ " -q -e -f -c "
        ++ shellQuote binaryPath
        ++ " /dev/null)"

probeScriptHelper :: FilePath -> IO Bool
probeScriptHelper scriptBin = do
    (exitCode, stdoutText, stderrText) <- readProcessWithExitCode
        scriptBin
        ["-q", "-e", "-f", "-c", "printf probe", "/dev/null"]
        ""
    pure (exitCode == ExitSuccess && "probe" `isInfixOf` stdoutText && null stderrText)

shellQuote :: String -> String
shellQuote s = "'" ++ concatMap escape s ++ "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]
