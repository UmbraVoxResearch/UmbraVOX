-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.RuntimeSettings
    ( restartMDNS
    , stopMDNSManager
    , toggleMDNSSetting
    , togglePersistentStorage
    , closeCurrentDB
    , toggleSettingWithStatus
    , applyDisplayName
    , applyDBPath
    , applyRetentionSetting
    , applyRetentionDays
    , applyDebugLogPath
    , clearSelectedHistoryConfirmed
    , cycleConnectionMode
    ) where

import Control.Concurrent (killThread, threadDelay, forkIO)
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forever, when)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.IORef (IORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import UmbraVox.App.RuntimeLog (logEvent)
import UmbraVox.BuildProfile
    ( BuildPluginId(..), pluginEnabled, pluginUnavailableStatus
    )
import UmbraVox.App.Startup (restorePersistentState, setPersistencePreference)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))
import UmbraVox.Network.MDNS (startMDNS, stopMDNS, getDiscoveredPeers)
import UmbraVox.Network.TransportClass (anyClose)
import UmbraVox.Storage.Anthony (clearConversation, closeDB, pruneMessages)
import UmbraVox.TUI.RuntimeEvent (RuntimeEvent(..), applyRuntimeEvents)
import UmbraVox.TUI.Types

emitStatus :: AppState -> String -> IO ()
emitStatus st msg = applyRuntimeEvents st [EventSetStatus msg]

toggleMDNSSetting :: AppState -> IO ()
toggleMDNSSetting st = do
    if not (pluginEnabled PluginDiscovery)
        then emitStatus st (pluginUnavailableStatus PluginDiscovery)
        else do
            modifyIORef' (cfgMDNSEnabled (asConfig st)) not
            enabled <- readIORef (cfgMDNSEnabled (asConfig st))
            if enabled
                then restartMDNS st
                else stopMDNSManager st
            logEvent (asConfig st) "settings.mdns" [("enabled", show enabled)]
            emitStatus st (if enabled then "mDNS enabled and applied" else "mDNS disabled and applied")

restartMDNS :: AppState -> IO ()
restartMDNS st = do
    if not (pluginEnabled PluginDiscovery)
        then pure ()
        else do
            stopMDNSManager st
            port <- readIORef (cfgListenPort (asConfig st))
            name <- readIORef (cfgDisplayName (asConfig st))
            mIk <- readIORef (cfgIdentity (asConfig st))
            case mIk of
                Nothing -> pure ()
                Just ik -> do
                    managerTid <- forkIO (mdnsManagerWorker st port name (ikX25519Public ik))
                    writeIORef (cfgMDNSThread (asConfig st)) (Just managerTid)

stopMDNSManager :: AppState -> IO ()
stopMDNSManager st = do
    mTid <- readIORef (cfgMDNSThread (asConfig st))
    maybe (pure ()) killThread mTid
    writeIORef (cfgMDNSThread (asConfig st)) Nothing
    writeIORef (cfgMDNSPeers (asConfig st)) []

mdnsManagerWorker :: AppState -> Int -> String -> ByteString -> IO ()
mdnsManagerWorker st port name pubkey = do
    (peersRef, mdnsTid) <- startMDNS port name pubkey
    (forever $ do
        peers <- getDiscoveredPeers peersRef
        writeIORef (cfgMDNSPeers (asConfig st)) peers
        threadDelay 5000000)
        `finally` stopMDNS mdnsTid

togglePersistentStorage :: AppState -> IO ()
togglePersistentStorage st = do
    if not (pluginEnabled PluginPersistentStorage)
        then emitStatus st (pluginUnavailableStatus PluginPersistentStorage)
        else do
            currentlyEnabled <- readIORef (cfgDBEnabled (asConfig st))
            currentPreference <- readIORef (cfgPersistencePreference (asConfig st))
            let wantsPersistent = currentlyEnabled || currentPreference == Just True
            if wantsPersistent
                then do
                    closeCurrentDB st
                    writeIORef (cfgDBEnabled (asConfig st)) False
                    setPersistencePreference (asConfig st) False
                    emitStatus st "Persistent storage disabled and applied"
                    logEvent (asConfig st) "settings.db_enabled" [("enabled", "False")]
                else do
                    setPersistencePreference (asConfig st) True
                    restored <- restorePersistentState (asConfig st)
                    enabled <- readIORef (cfgDBEnabled (asConfig st))
                    if enabled
                        then emitStatus st ("Persistent storage enabled and applied (" ++ show restored ++ " restored)")
                        else emitStatus st "Persistent storage unavailable; still ephemeral"
                    logEvent (asConfig st) "settings.db_enabled" [("enabled", show enabled)]

closeCurrentDB :: AppState -> IO ()
closeCurrentDB st = do
    mDb <- readIORef (cfgAnthonyDB (asConfig st))
    case mDb of
        Nothing -> pure ()
        Just db -> closeDB db `catch` (\(_ :: SomeException) -> pure ())
    writeIORef (cfgAnthonyDB (asConfig st)) Nothing

toggleSettingWithStatus :: AppState -> String -> IORef Bool -> String -> String -> IO ()
toggleSettingWithStatus st eventName ref enabledMsg disabledMsg = do
    modifyIORef' ref not
    enabled <- readIORef ref
    logEvent (asConfig st) eventName [("enabled", show enabled)]
    emitStatus st (if enabled then enabledMsg else disabledMsg)

applyDisplayName :: AppState -> String -> IO ()
applyDisplayName st val = do
    writeIORef (cfgDisplayName (asConfig st)) val
    mdnsOn <- readIORef (cfgMDNSEnabled (asConfig st))
    when mdnsOn (restartMDNS st)
    logEvent (asConfig st) "settings.display_name" [("updated", "true")]

applyDBPath :: AppState -> String -> IO ()
applyDBPath st val = do
    if not (pluginEnabled PluginPersistentStorage)
        then emitStatus st (pluginUnavailableStatus PluginPersistentStorage)
        else do
            writeIORef (cfgDBPath (asConfig st)) val
            dbOn <- readIORef (cfgDBEnabled (asConfig st))
            if dbOn
                then do
                    closeCurrentDB st
                    restored <- restorePersistentState (asConfig st)
                    enabled <- readIORef (cfgDBEnabled (asConfig st))
                    if enabled
                        then emitStatus st ("Database path updated and applied (" ++ show restored ++ " restored)")
                        else emitStatus st "Database path update failed; switched to ephemeral mode"
                else emitStatus st "Database path updated"
            logEvent (asConfig st) "settings.db_path" [("path", val)]

applyRetentionSetting :: AppState -> Int -> IO ()
applyRetentionSetting st days = do
    if not (pluginEnabled PluginPersistentStorage)
        then emitStatus st (pluginUnavailableStatus PluginPersistentStorage)
        else do
            let cfg = asConfig st
            mDb <- readIORef (cfgAnthonyDB cfg)
            case mDb of
                Nothing ->
                    emitStatus st ("Retention set to " ++ retentionLabel days ++ " (ephemeral mode)")
                Just db ->
                    if days <= 0
                        then emitStatus st "Retention set to forever and applied"
                        else do
                            pruneMessages db days `catch` \(_ :: SomeException) -> pure ()
                            emitStatus st ("Retention set to " ++ retentionLabel days ++ " and applied")
  where
    retentionLabel 0 = "forever"
    retentionLabel n = show n ++ " days"

applyRetentionDays :: AppState -> Int -> IO ()
applyRetentionDays st days = do
    if not (pluginEnabled PluginPersistentStorage)
        then emitStatus st (pluginUnavailableStatus PluginPersistentStorage)
        else do
            writeIORef (cfgRetentionDays (asConfig st)) days
            applyRetentionSetting st days
            logEvent (asConfig st) "settings.retention_days" [("days", show days)]

applyDebugLogPath :: AppState -> FilePath -> IO ()
applyDebugLogPath st val = do
    if not (pluginEnabled PluginRuntimeLogging)
        then emitStatus st (pluginUnavailableStatus PluginRuntimeLogging)
        else do
            writeIORef (cfgDebugLogPath (asConfig st)) val
            logEvent (asConfig st) "settings.debug_log_path" [("path", val)]
            emitStatus st "Runtime log path updated"

clearSelectedHistoryConfirmed :: AppState -> IO ()
clearSelectedHistoryConfirmed st = do
    let cfg = asConfig st
    mDb <- readIORef (cfgAnthonyDB cfg)
    sel <- readIORef (asSelected st)
    case mDb of
        Just db -> do
            sessions <- readIORef (cfgSessions cfg)
            let entries = Map.toList sessions
            when (sel < length entries) $ do
                let (sid, _) = entries !! sel
                clearConversation db sid
        Nothing -> pure ()
    sessions <- readIORef (cfgSessions cfg)
    let entries = Map.toList sessions
    when (sel < length entries) $ do
        let (_, si) = entries !! sel
        writeIORef (siHistory si) []
        logEvent cfg "history.clear" [("selected_index", show sel)]

cycleConnectionMode :: AppState -> IO ()
cycleConnectionMode st = do
    let cfg = asConfig st
    if not (pluginEnabled PluginConnectionModeSelection)
        then do
            writeIORef (cfgConnectionMode cfg) Chastity
            emitStatus st (pluginUnavailableStatus PluginConnectionModeSelection)
        else do
            current <- readIORef (cfgConnectionMode cfg)
            let modes = [minBound .. maxBound] :: [ConnectionMode]
                next = case dropWhile (/= current) modes of
                    (_:m:_) -> m
                    _       -> head modes
            writeIORef (cfgConnectionMode cfg) next
            applyConnectionModeSideEffects st next
            disconnected <- disconnectRemoteSessions st
            logEvent cfg "settings.connection_mode"
                [ ("mode", show next)
                , ("disconnected_sessions", show disconnected)
                ]
            emitStatus st
                ("Connection mode set to " ++ map toLower (show next)
                    ++ " and applied; reconnect " ++ show disconnected ++ " session(s)")

applyConnectionModeSideEffects :: AppState -> ConnectionMode -> IO ()
applyConnectionModeSideEffects st mode = do
    let cfg = asConfig st
        setDiscovery mdns pex = do
            writeIORef (cfgMDNSEnabled cfg) mdns
            writeIORef (cfgPEXEnabled cfg) pex
            if mdns then restartMDNS st else stopMDNSManager st
    case mode of
        Swing -> setDiscovery True True
        Promiscuous -> setDiscovery True True
        Selective -> setDiscovery True True
        Chaste -> setDiscovery False False
        Chastity -> do
            setDiscovery False False
            closeCurrentDB st
            writeIORef (cfgDBEnabled cfg) False
            writeIORef (cfgPersistencePreference cfg) (Just False)
            writeIORef (cfgAutoSaveMessages cfg) False

disconnectRemoteSessions :: AppState -> IO Int
disconnectRemoteSessions st = do
    let cfg = asConfig st
    sessions <- readIORef (cfgSessions cfg)
    now <- (round <$> getPOSIXTime) :: IO Int
    updated <- mapM (disconnectOne now) (Map.toList sessions)
    writeIORef (cfgSessions cfg) (Map.fromList (map (\(sid, si, _) -> (sid, si)) updated))
    pure (length [() | (_, _, True) <- updated])
  where
    disconnectOne now (sid, si) =
        case siTransport si of
            Nothing -> pure (sid, si, False)
            Just t -> do
                maybe (pure ()) killThread (siRecvTid si)
                anyClose t `catch` \(_ :: SomeException) -> pure ()
                writeIORef (siStatus si) Offline
                modifyIORef' (siHistory si)
                    (("[" ++ show now ++ "] [system] Session requires reconnect after security mode change") :)
                pure (sid, si { siTransport = Nothing, siRecvTid = Nothing }, True)
