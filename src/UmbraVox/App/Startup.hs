-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
-- | Startup helpers for identity and persistence recovery.
module UmbraVox.App.Startup
    ( newDefaultAppConfig
    , initializeLocalIdentity
    , applyPersistenceAnswer
    , persistenceAnswerEnables
    , setPersistencePreference
    , refreshPackagedPluginCatalog
    , refreshTransportProviderCatalog
    , resolvePersistencePreference
    , resolvePersistencePreferenceAt
    , resolveIdentity
    , resolveIdentityAt
    , restorePersistentState
    , restorePersistentStateAt
    , expandHome
    ) where

import Control.Exception (SomeException, catch)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (unless, when)
import Data.Char (isSpace, toLower)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (dropWhileEnd, isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
#ifdef mingw32_HOST_OS
import System.Directory (setPermissions, emptyPermissions, setOwnerReadable, setOwnerWritable)
#else
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
#endif
import qualified Network.Socket as NS

import UmbraVox.App.RuntimeLog (logEvent)
import UmbraVox.Network.Discovery (newDiscoveryManager)
import UmbraVox.Network.PeerManager (newPeerManager)
import UmbraVox.BuildProfile
    ( BuildPlugin, BuildPluginId(..), PackagedPluginRuntime(..), PluginManifest
    , loadPackagedPluginRuntimeCatalog, pluginEnabled
    )
import UmbraVox.Crypto.BIP39 (generatePassphrase)
import UmbraVox.Chat.OutboundQueue (newQueue, maxQueueDepth, maxMessageAge)
import UmbraVox.Chat.Session (initChatSession)
import UmbraVox.Crypto.KeyStore
    ( loadIdentityKey, saveIdentityKey
    , loadIdentityKeyAt, saveIdentityKeyAt
    )
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey, ikEd25519Secret)
import UmbraVox.Storage.Anthony
    ( AnthonyDB, loadConversations, loadMessages
    , loadTrustedKeys, openDB, openDBWithKey, saveSetting
    )
import UmbraVox.Storage.AnthonyAdapter (anthonyStorageHandle)
import UmbraVox.Storage.Encryption
    ( getOrCreateSalt, deriveStorageKey )
import UmbraVox.Storage.InMemory (newInMemoryStorage)
import UmbraVox.App.Config
    ( AppConfig(..), ConnectionMode(..) )
import UmbraVox.Plugin.Registry (defaultPersistencePlugins, enablePlugin, resolveEnable)
import qualified UmbraVox.Plugin.Registry as Registry
import UmbraVox.App.Types
    ( SessionInfo(..), SessionCrypto(..), ContactStatus(..) )
import UmbraVox.Protocol.Handshake (genIdentity)
import UmbraVox.Protocol.Encoding (defaultPorts)
import UmbraVox.Network.ProviderCatalog
    ( CachedTransportProvider(..), ProviderManifest, TransportProvider
    , loadTransportProviderRuntimeCatalog
    )

persistenceSettingKey :: String
persistenceSettingKey = "storage.persistent.enabled"

newDefaultAppConfig :: IO AppConfig
newDefaultAppConfig = do
    randomName <- generatePassphrase 1
    listenPort <- findAvailablePort defaultPorts
    dataDir <- lookupEnv "UMBRAVOX_DATA" >>= \case
        Just d  -> pure d
        Nothing -> (++ "/.umbravox") <$> getHomeDirectory
    let logPath = dataDir ++ "/umbravox.log"
    packagedPluginRuntimeCatalog <- loadPackagedPluginRuntimeCatalog
    let packagedPluginCatalog = map toCatalogEntry packagedPluginRuntimeCatalog
    transportProviderRuntimeCatalog <- loadTransportProviderRuntimeCatalog
    let transportProviderCatalog = map toProviderCatalogEntry transportProviderRuntimeCatalog
    debugEnv <- lookupEnv "UMBRAVOX_DEBUG_LOG"
    let debugEnabled = case debugEnv of
            Just raw -> raw `elem` ["1", "true", "TRUE", "yes", "YES", "on", "ON"]
            Nothing -> False
        initialDebug = if pluginEnabled PluginRuntimeLogging then debugEnabled else False
        initialMDNS = pluginEnabled PluginDiscovery
        initialPEX = False
        initialPersistencePref = if pluginEnabled PluginPersistentStorage then Nothing else Just False
        initialAutoSave = pluginEnabled PluginPersistentStorage
        initialMode = if pluginEnabled PluginConnectionModeSelection then Selective else Chaste
    initialStorage <- newInMemoryStorage
    peerMgr <- newPeerManager
    discoveryMgr <- newDiscoveryManager peerMgr
    AppConfig
        <$> newIORef listenPort
        <*> newIORef randomName
        <*> newIORef Nothing
        <*> newIORef Map.empty
        <*> newIORef 1
        <*> newIORef initialDebug
        <*> newIORef logPath
        <*> newIORef initialMDNS
        <*> newIORef initialPEX
        <*> newIORef False
        <*> newIORef (dataDir ++ "/umbravox.db")
        <*> newIORef initialPersistencePref
        <*> newIORef packagedPluginCatalog
        <*> newIORef packagedPluginRuntimeCatalog
        <*> newIORef transportProviderCatalog
        <*> newIORef transportProviderRuntimeCatalog
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newIORef []
        <*> newIORef 30
        <*> newIORef initialAutoSave
        <*> newIORef Nothing
        <*> newIORef initialMode
        <*> newIORef []
        <*> newIORef Set.empty
        <*> newIORef Map.empty  -- cfgTofoAddrKeys: M27.4.2 address→key tracking
        <*> newIORef False  -- cfgEphemeral: off by default
        <*> newIORef initialStorage  -- cfgStorage: starts as in-memory; upgraded to Anthony on DB open
        <*> newIORef defaultPersistencePlugins  -- cfgPluginRegistry: all persistence plugins disabled
        <*> newMVar ()      -- cfgLogLock: runtime log serialization
        <*> newIORef 0      -- cfgLogWriterPID: single-writer PID tracking
        <*> pure peerMgr               -- cfgPeerManager: M24.2
        <*> pure discoveryMgr          -- cfgDiscoveryManager: M24.2
        <*> newIORef Set.empty         -- cfgDiscoverySources: M24.2 (populated by discoverPeers)
        <*> newIORef Nothing           -- cfgDNSDiscoveryDomain: M24.2
        <*> newIORef False             -- cfgDHTEnabled: M24.4 (off by default)
        <*> newIORef []                -- cfgDHTBootstrapNodes: M24.4 (no bootstrap nodes)

initializeLocalIdentity :: AppConfig -> IO IdentityKey
initializeLocalIdentity cfg = do
    ephemeral <- readIORef (cfgEphemeral cfg)
    identity <- resolveIdentityEphemeral ephemeral
    writeIORef (cfgIdentity cfg) (Just identity)
    logEvent cfg "identity.ready" []
    pure identity

applyPersistenceAnswer :: AppConfig -> String -> IO Int
applyPersistenceAnswer cfg answer = do
    ephemeral <- readIORef (cfgEphemeral cfg)
    if not (pluginEnabled PluginPersistentStorage) && ephemeral
        then do
            writeIORef (cfgPersistencePreference cfg) (Just False)
            writeIORef (cfgDBEnabled cfg) False
            pure 0
        else if persistenceAnswerEnables answer
            then do
                setPersistencePreference cfg True
                restorePersistentState cfg `catch` \(_ :: SomeException) -> do
                    logEvent cfg "persistence.restore.failed" []
                    writeIORef (cfgDBEnabled cfg) False
                    pure 0
            else do
                setPersistencePreference cfg False
                logEvent cfg "persistence.mode" [("enabled", "false")]
                writeIORef (cfgDBEnabled cfg) False
                pure 0

persistenceAnswerEnables :: String -> Bool
persistenceAnswerEnables raw =
    normalize raw `elem` ["y", "yes"]
  where
    normalize = map toLower . dropWhileEnd isSpace . dropWhile isSpace

resolvePersistencePreference :: AppConfig -> IO (Maybe Bool)
resolvePersistencePreference cfg = do
    if not (pluginEnabled PluginPersistentStorage)
        then do
            writeIORef (cfgPersistencePreference cfg) (Just False)
            pure (Just False)
        else do
            dbPath <- readIORef (cfgDBPath cfg)
            home <- getHomeDirectory
            let path = expandHome home dbPath
            pref <- resolvePersistencePreferenceAt path
            writeIORef (cfgPersistencePreference cfg) pref
            pure pref

resolvePersistencePreferenceAt :: FilePath -> IO (Maybe Bool)
resolvePersistencePreferenceAt "" = pure Nothing
resolvePersistencePreferenceAt path = do
    let prefPath = persistencePreferencePath path
    exists <- doesFileExist prefPath
    if not exists
        then pure Nothing
        else do
            raw <- (do
                contents <- readFile prefPath
                length contents `seq` pure contents
                ) `catch` \(_ :: SomeException) -> pure ""
            pure (parsePersistenceFlag raw)

refreshPackagedPluginCatalog :: AppConfig -> IO ()
refreshPackagedPluginCatalog cfg = do
    packagedPluginRuntimeCatalog <- loadPackagedPluginRuntimeCatalog
    let packagedPluginCatalog = map toCatalogEntry packagedPluginRuntimeCatalog
    writeIORef (cfgPackagedPluginCatalog cfg) packagedPluginCatalog
    writeIORef (cfgPackagedPluginRuntimeCatalog cfg) packagedPluginRuntimeCatalog

refreshTransportProviderCatalog :: AppConfig -> IO ()
refreshTransportProviderCatalog cfg = do
    transportProviderRuntimeCatalog <- loadTransportProviderRuntimeCatalog
    let transportProviderCatalog = map toProviderCatalogEntry transportProviderRuntimeCatalog
    writeIORef (cfgTransportProviderCatalog cfg) transportProviderCatalog
    writeIORef (cfgTransportProviderRuntimeCatalog cfg) transportProviderRuntimeCatalog

toCatalogEntry :: PackagedPluginRuntime -> (BuildPlugin, PluginManifest)
toCatalogEntry runtimeEntry = (pprPlugin runtimeEntry, pprManifest runtimeEntry)

toProviderCatalogEntry :: CachedTransportProvider -> (TransportProvider, ProviderManifest)
toProviderCatalogEntry runtimeEntry = (ctpProvider runtimeEntry, ctpManifest runtimeEntry)

resolveIdentity :: IO IdentityKey
resolveIdentity = do
    if not (pluginEnabled PluginIdentityPersistence)
        then genIdentity
        else do
            mIdentity <- loadIdentityKey
            case mIdentity of
                Just ik -> pure ik
                Nothing -> do
                    ik <- genIdentity
                    saveIdentityKey ik
                    pure ik

-- | Like 'resolveIdentity' but skips the disk write when ephemeral is set.
resolveIdentityEphemeral :: Bool -> IO IdentityKey
resolveIdentityEphemeral ephemeral = do
    if not (pluginEnabled PluginIdentityPersistence) || ephemeral
        then genIdentity
        else do
            mIdentity <- loadIdentityKey
            case mIdentity of
                Just ik -> pure ik
                Nothing -> do
                    ik <- genIdentity
                    saveIdentityKey ik
                    pure ik

resolveIdentityAt :: FilePath -> IO IdentityKey
resolveIdentityAt path = do
    mIdentity <- loadIdentityKeyAt path
    case mIdentity of
        Just ik -> pure ik
        Nothing -> do
            ik <- genIdentity
            saveIdentityKeyAt path ik
            pure ik

restorePersistentState :: AppConfig -> IO Int
restorePersistentState cfg = do
    if not (pluginEnabled PluginPersistentStorage)
        then do
            writeIORef (cfgDBEnabled cfg) False
            pure 0
        else do
            dbPath <- readIORef (cfgDBPath cfg)
            home <- getHomeDirectory
            let path = expandHome home dbPath
            restorePersistentStateAt cfg path

restorePersistentStateAt :: AppConfig -> FilePath -> IO Int
restorePersistentStateAt cfg path =
    restorePersistentStateAtUnsafe cfg path `catch` \(_ :: SomeException) -> do
        writeIORef (cfgDBEnabled cfg) False
        pure 0

-- Finding: M1.1.3 — restorePersistentStateAtUnsafe opened the database with
-- 'openDB', which stores no encryption key in the 'AnthonyDB' handle.  Even
-- though 'Storage.Encryption' and the encrypted 'saveMessage'/'loadMessages'
-- paths existed, the application never supplied a key at startup, so all
-- production data was written and read as plaintext.
--
-- Vulnerability: At-rest encryption for message content and conversation names
-- was fully implemented in the storage layer but never activated at the
-- application level, making the feature a dead letter.  Any process with read
-- access to the database file (backup, snooping daemon, offline disk access)
-- could read all messages and peer names verbatim.
--
-- Fix: When the local identity key is available in 'cfgIdentity', derive a
-- 'StorageKey' from the identity's Ed25519 secret and a per-install random
-- salt (loaded or created by 'getOrCreateSalt').  Open the database with
-- 'openDBWithKey' so that all subsequent reads and writes use AEAD encryption.
-- When no identity is loaded (e.g. during isolated unit tests that seed the DB
-- without an identity), fall back to 'openDB' to preserve backward
-- compatibility.
--
-- Verified: Existing tests that use 'mkTestConfig' (cfgIdentity = Nothing) and
-- 'seedPersistentDB' (openDB, plaintext) continue to pass via the fallback path.
-- Production flow (initializeLocalIdentity → restorePersistentState) activates
-- the encrypted path.
restorePersistentStateAtUnsafe :: AppConfig -> FilePath -> IO Int
restorePersistentStateAtUnsafe cfg path = do
    mIdentity <- readIORef (cfgIdentity cfg)
    ephemeral <- readIORef (cfgEphemeral cfg)
    -- M17.3.7: only create DB directory when persistence is active (not ephemeral)
    unless ephemeral $ createDirectoryIfMissing True (takeDirectory path)
    db <- case mIdentity of
        Just ik -> do
            salt <- if ephemeral
                then randomBytes 32  -- in-memory salt, never written to disk
                else do
                    let saltPath = path ++ ".salt"
                    getOrCreateSalt saltPath
            let storageKey = deriveStorageKey salt (ikEd25519Secret ik)
            openDBWithKey path storageKey
        Nothing ->
            openDB path
    writeIORef (cfgAnthonyDB cfg) (Just db)
    writeIORef (cfgStorage cfg) (anthonyStorageHandle db)
    writeIORef (cfgDBEnabled cfg) True
    writeIORef (cfgPersistencePreference cfg) (Just True)
    -- M17.3.6: enable message-storage (and deps) in the runtime plugin
    -- registry now that the DB is open, then guard the .pref sidecar write
    -- behind the plugin check so no preference file touches disk when the
    -- message-storage plugin is disabled.
    enableMessageStoragePlugin cfg
    pluginReg <- readIORef (cfgPluginRegistry cfg)
    when (not ephemeral && Registry.pluginEnabled "message-storage" pluginReg) $
        rememberPersistencePreferenceAt path True
    saveSetting db persistenceSettingKey "1"
    logEvent cfg "persistence.mode" [("enabled", "true"), ("path", path)]
    trustedPairs <- loadTrustedKeys db
    writeIORef (cfgTrustedKeys cfg) (map fst trustedPairs)
    convs <- loadConversations db
    results <- mapM (restoreConversationSafe cfg db) convs
    let failCount = length (filter not results)
    when (failCount > 0) $
        logEvent cfg "persistence.decrypt.failed"
            [("conversations_failed", show failCount)]
    Map.size <$> readIORef (cfgSessions cfg)

-- | Update the in-memory persistence preference and, when the
-- message-storage runtime plugin is active, persist the choice to the
-- .pref sidecar file (M17.3.6).
setPersistencePreference :: AppConfig -> Bool -> IO ()
setPersistencePreference cfg enabled = do
    writeIORef (cfgPersistencePreference cfg) (Just enabled)
    ephemeral <- readIORef (cfgEphemeral cfg)
    pluginReg <- readIORef (cfgPluginRegistry cfg)
    when (pluginEnabled PluginPersistentStorage
          && not ephemeral
          && Registry.pluginEnabled "message-storage" pluginReg) $ do
        dbPath <- readIORef (cfgDBPath cfg)
        home <- getHomeDirectory
        rememberPersistencePreferenceAt (expandHome home dbPath) enabled

-- | Enable the message-storage plugin (and its transitive dependencies)
-- in the runtime plugin registry.  Called when the database is
-- successfully opened so that subsequent .pref writes are allowed.
enableMessageStoragePlugin :: AppConfig -> IO ()
enableMessageStoragePlugin cfg = do
    reg <- readIORef (cfgPluginRegistry cfg)
    case resolveEnable "message-storage" reg of
        Right toEnable ->
            writeIORef (cfgPluginRegistry cfg) (foldr enablePlugin reg toEnable)
        Left _ -> pure ()  -- unknown plugin; do nothing

rememberPersistencePreferenceAt :: FilePath -> Bool -> IO ()
rememberPersistencePreferenceAt "" _ = pure ()
rememberPersistencePreferenceAt path enabled = do
    let prefPath = persistencePreferencePath path
        value = if enabled then "1\n" else "0\n"
    createDirectoryIfMissing True (takeDirectory prefPath)
    writeFile prefPath value `catch` \(_ :: SomeException) -> pure ()
#ifdef mingw32_HOST_OS
    setPermissions prefPath (setOwnerWritable True (setOwnerReadable True emptyPermissions))
        `catch` \(_ :: SomeException) -> pure ()
#else
    setFileMode prefPath (ownerReadMode `unionFileModes` ownerWriteMode)
        `catch` \(_ :: SomeException) -> pure ()
#endif

persistencePreferencePath :: FilePath -> FilePath
persistencePreferencePath path = path ++ ".pref"

parsePersistenceFlag :: String -> Maybe Bool
parsePersistenceFlag raw =
    case map toLower (dropWhileEnd isSpace (dropWhile isSpace raw)) of
        "1"     -> Just True
        "true"  -> Just True
        "yes"   -> Just True
        "y"     -> Just True
        "0"     -> Just False
        "false" -> Just False
        "no"    -> Just False
        "n"     -> Just False
        _       -> Nothing

restoreConversationSafe :: AppConfig -> AnthonyDB -> (Int, String, String, Int) -> IO Bool
restoreConversationSafe cfg db conv =
    (restoreConversation cfg db conv >> pure True)
    `catch` \(_ :: SomeException) -> pure False

restoreConversation
    :: AppConfig
    -> AnthonyDB
    -> (Int, String, String, Int)
    -> IO ()
restoreConversation cfg db (convId, _pubkey, name, _created) = do
    chatSec <- randomBytes 32
    dhSec   <- randomBytes 32
    dhPub   <- randomBytes 32
    -- Random keys are never all-zero, so initChatSession cannot return Nothing here.
    mSession <- initChatSession chatSec dhSec dhPub
    let session = case mSession of
                      Just s  -> s
                      Nothing -> error "restoreConversation: ratchet init with random keys returned Nothing (impossible)"
    sessRef <- newIORef session
    lockRef <- newMVar ()
    histRef <- newIORef []
    statRef <- newIORef Offline
    oq <- newQueue maxQueueDepth maxMessageAge
    let si = SessionInfo
            { siTransport = Nothing
            , siCrypto = RatchetCrypto sessRef
            , siSessionLock = lockRef
            , siRecvTid = Nothing
            , siPeerName = name
            , siHistory = histRef
            , siStatus = statRef
            , siOutboundQueue = oq
            }
    msgs <- loadMessages db convId 500
    let formatted = map (\(sender, content, _ts) -> sender ++ ": " ++ content) msgs
    writeIORef histRef formatted
    modifyIORef' (cfgNextId cfg) (max (convId + 1))
    modifyIORef' (cfgSessions cfg) (Map.insert convId si)
    logEvent cfg "persistence.restore.session"
        [ ("session_id", show convId)
        , ("peer", name)
        , ("messages", show (length formatted))
        ]

expandHome :: FilePath -> FilePath -> FilePath
expandHome home ('~':'/':rest)
    | ".." `isInfixOf` rest = home ++ "/.umbravox/umbravox.db"  -- safe fallback
    | otherwise = home ++ '/' : rest
expandHome _ path = path

findAvailablePort :: [Int] -> IO Int
findAvailablePort [] = pure 1111
findAvailablePort (p:ps) = do
    ok <- tryBindPort p
    if ok then pure p else findAvailablePort ps

tryBindPort :: Int -> IO Bool
tryBindPort port = (do
    let hints = NS.defaultHints
            { NS.addrFlags = [NS.AI_PASSIVE]
            , NS.addrSocketType = NS.Stream
            , NS.addrFamily = NS.AF_UNSPEC
            }
    addrs <- NS.getAddrInfo (Just hints) Nothing (Just (show port)) :: IO [NS.AddrInfo]
    results <- mapM tryBindAddr addrs
    pure (or results)
    ) `catch` \(_ :: SomeException) -> pure False

tryBindAddr :: NS.AddrInfo -> IO Bool
tryBindAddr addr =
    (do
        sock <- NS.openSocket addr
        when (NS.addrFamily addr == NS.AF_INET6) $
            NS.setSocketOption sock NS.IPv6Only 1 `catch` \(_ :: SomeException) -> pure ()
        result <- ((do
            NS.setSocketOption sock NS.ReuseAddr 1
            NS.bind sock (NS.addrAddress addr)
            pure True
            ) `catch` \(_ :: SomeException) -> pure False)
        NS.close sock `catch` \(_ :: SomeException) -> pure ()
        pure result
        ) `catch` \(_ :: SomeException) -> pure False
