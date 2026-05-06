-- SPDX-License-Identifier: Apache-2.0
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
    ) where

import Control.Exception (SomeException, catch)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (when)
import Data.Char (isSpace, toLower)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import qualified Network.Socket as NS

import UmbraVox.App.RuntimeLog (logEvent)
import UmbraVox.BuildProfile
    ( BuildPlugin, BuildPluginId(..), PackagedPluginRuntime(..), PluginManifest
    , loadPackagedPluginRuntimeCatalog, pluginEnabled
    )
import UmbraVox.Crypto.BIP39 (generatePassphrase)
import UmbraVox.Chat.Session (initChatSession)
import UmbraVox.Crypto.KeyStore
    ( loadIdentityKey, saveIdentityKey
    , loadIdentityKeyAt, saveIdentityKeyAt
    )
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey)
import UmbraVox.Storage.Anthony
    ( AnthonyDB, loadConversations, loadMessages
    , loadTrustedKeys, openDB, saveSetting
    )
import UmbraVox.TUI.Types
    ( AppConfig(..), SessionInfo(..), ContactStatus(..), ConnectionMode(..) )
import UmbraVox.TUI.Handshake (genIdentity)
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
    home <- getHomeDirectory
    let logPath = expandHome home "~/.umbravox/umbravox.log"
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
        initialMode = if pluginEnabled PluginConnectionModeSelection then Selective else Chastity
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
        <*> newIORef "~/.umbravox/umbravox.db"
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

initializeLocalIdentity :: AppConfig -> IO IdentityKey
initializeLocalIdentity cfg = do
    identity <- resolveIdentity
    writeIORef (cfgIdentity cfg) (Just identity)
    logEvent cfg "identity.ready" []
    pure identity

applyPersistenceAnswer :: AppConfig -> String -> IO Int
applyPersistenceAnswer cfg answer
    | not (pluginEnabled PluginPersistentStorage) = do
        writeIORef (cfgPersistencePreference cfg) (Just False)
        writeIORef (cfgDBEnabled cfg) False
        pure 0
    | persistenceAnswerEnables answer = do
        setPersistencePreference cfg True
        restorePersistentState cfg `catch` \(_ :: SomeException) -> do
            logEvent cfg "persistence.restore.failed" []
            writeIORef (cfgDBEnabled cfg) False
            pure 0
    | otherwise = do
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

restorePersistentStateAtUnsafe :: AppConfig -> FilePath -> IO Int
restorePersistentStateAtUnsafe cfg path = do
    createDirectoryIfMissing True (takeDirectory path)
    db <- openDB path
    writeIORef (cfgAnthonyDB cfg) (Just db)
    writeIORef (cfgDBEnabled cfg) True
    writeIORef (cfgPersistencePreference cfg) (Just True)
    rememberPersistencePreferenceAt path True
    saveSetting db persistenceSettingKey "1"
    logEvent cfg "persistence.mode" [("enabled", "true"), ("path", path)]
    trustedPairs <- loadTrustedKeys db
    writeIORef (cfgTrustedKeys cfg) (map fst trustedPairs)
    convs <- loadConversations db
    mapM_ (restoreConversation cfg db) convs
    Map.size <$> readIORef (cfgSessions cfg)

setPersistencePreference :: AppConfig -> Bool -> IO ()
setPersistencePreference cfg enabled = do
    writeIORef (cfgPersistencePreference cfg) (Just enabled)
    when (pluginEnabled PluginPersistentStorage) $ do
        dbPath <- readIORef (cfgDBPath cfg)
        home <- getHomeDirectory
        rememberPersistencePreferenceAt (expandHome home dbPath) enabled

rememberPersistencePreferenceAt :: FilePath -> Bool -> IO ()
rememberPersistencePreferenceAt "" _ = pure ()
rememberPersistencePreferenceAt path enabled = do
    let prefPath = persistencePreferencePath path
        value = if enabled then "1\n" else "0\n"
    createDirectoryIfMissing True (takeDirectory prefPath)
    writeFile prefPath value `catch` \(_ :: SomeException) -> pure ()

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

restoreConversation
    :: AppConfig
    -> AnthonyDB
    -> (Int, String, String, Int)
    -> IO ()
restoreConversation cfg db (convId, _pubkey, name, _created) = do
    chatSec <- randomBytes 32
    dhSec   <- randomBytes 32
    dhPub   <- randomBytes 32
    session <- initChatSession chatSec dhSec dhPub
    sessRef <- newIORef session
    lockRef <- newMVar ()
    histRef <- newIORef []
    statRef <- newIORef Offline
    let si = SessionInfo
            { siTransport = Nothing
            , siSession = sessRef
            , siSessionLock = lockRef
            , siRecvTid = Nothing
            , siPeerName = name
            , siHistory = histRef
            , siStatus = statRef
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
expandHome home ('~':'/':rest) = home ++ '/' : rest
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
            , NS.addrFamily = NS.AF_INET
            }
    addr : _ <- NS.getAddrInfo (Just hints) (Just "0.0.0.0") (Just (show port))
    sock <- NS.openSocket addr
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress addr)
    NS.close sock
    pure True
    ) `catch` \(_ :: SomeException) -> pure False
