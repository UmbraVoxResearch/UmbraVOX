-- SPDX-License-Identifier: Apache-2.0
-- | Startup helpers for identity and persistence recovery.
module UmbraVox.App.Startup
    ( newDefaultAppConfig
    , initializeLocalIdentity
    , applyPersistenceAnswer
    , resolvePersistencePreference
    , resolvePersistencePreferenceAt
    , resolveIdentity
    , resolveIdentityAt
    , restorePersistentState
    , restorePersistentStateAt
    ) where

import Control.Exception (SomeException, catch)
import Control.Concurrent.MVar (newMVar)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import qualified Network.Socket as NS

import UmbraVox.App.RuntimeLog (logEvent)
import UmbraVox.Crypto.BIP39 (generatePassphrase)
import UmbraVox.Chat.Session (initChatSession)
import UmbraVox.Crypto.KeyStore
    ( loadIdentityKey, saveIdentityKey
    , loadIdentityKeyAt, saveIdentityKeyAt
    )
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey)
import UmbraVox.Storage.Anthony
    ( AnthonyDB, closeDB, loadConversations, loadMessages, loadSetting
    , loadTrustedKeys, openDB, saveSetting
    )
import UmbraVox.TUI.Types
    ( AppConfig(..), SessionInfo(..), ContactStatus(..), ConnectionMode(..) )
import UmbraVox.TUI.Handshake (genIdentity)
import UmbraVox.Protocol.Encoding (defaultPorts)

persistenceSettingKey :: String
persistenceSettingKey = "storage.persistent.enabled"

newDefaultAppConfig :: IO AppConfig
newDefaultAppConfig = do
    randomName <- generatePassphrase 1
    listenPort <- findAvailablePort defaultPorts
    home <- getHomeDirectory
    let logPath = expandHome home "~/.umbravox/umbravox.log"
    debugEnv <- lookupEnv "UMBRAVOX_DEBUG_LOG"
    let debugEnabled = case debugEnv of
            Just raw -> raw `elem` ["1", "true", "TRUE", "yes", "YES", "on", "ON"]
            Nothing -> False
    AppConfig
        <$> newIORef listenPort
        <*> newIORef randomName
        <*> newIORef Nothing
        <*> newIORef Map.empty
        <*> newIORef 1
        <*> newIORef debugEnabled
        <*> newIORef logPath
        <*> newIORef True
        <*> newIORef False
        <*> newIORef False
        <*> newIORef "~/.umbravox/umbravox.db"
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newIORef []
        <*> newIORef 30
        <*> newIORef True
        <*> newIORef Nothing
        <*> newIORef Selective
        <*> newIORef []

initializeLocalIdentity :: AppConfig -> IO IdentityKey
initializeLocalIdentity cfg = do
    identity <- resolveIdentity
    writeIORef (cfgIdentity cfg) (Just identity)
    logEvent cfg "identity.ready" []
    pure identity

applyPersistenceAnswer :: AppConfig -> String -> IO Int
applyPersistenceAnswer cfg answer
    | answer `elem` ["y", "Y", "yes", "Yes", "YES"] =
        restorePersistentState cfg `catch` \(_ :: SomeException) -> do
            logEvent cfg "persistence.restore.failed" []
            writeIORef (cfgDBEnabled cfg) False
            pure 0
    | otherwise = do
        logEvent cfg "persistence.mode" [("enabled", "false")]
        writeIORef (cfgDBEnabled cfg) False
        pure 0

resolvePersistencePreference :: AppConfig -> IO (Maybe Bool)
resolvePersistencePreference cfg = do
    dbPath <- readIORef (cfgDBPath cfg)
    home <- getHomeDirectory
    resolvePersistencePreferenceAt (expandHome home dbPath)

resolvePersistencePreferenceAt :: FilePath -> IO (Maybe Bool)
resolvePersistencePreferenceAt path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else (do
            db <- openDB path
            mValue <- loadSetting db persistenceSettingKey
            closeDB db
            pure (Just (parsePersistenceValue mValue))
            ) `catch` \(_ :: SomeException) -> pure Nothing

resolveIdentity :: IO IdentityKey
resolveIdentity = do
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
    saveSetting db persistenceSettingKey "1"
    logEvent cfg "persistence.mode" [("enabled", "true"), ("path", path)]
    trustedPairs <- loadTrustedKeys db
    writeIORef (cfgTrustedKeys cfg) (map fst trustedPairs)
    convs <- loadConversations db
    mapM_ (restoreConversation cfg db) convs
    Map.size <$> readIORef (cfgSessions cfg)

parsePersistenceValue :: Maybe String -> Bool
parsePersistenceValue Nothing = True
parsePersistenceValue (Just raw) =
    case raw of
        "0"     -> False
        "false" -> False
        "False" -> False
        "no"    -> False
        "No"    -> False
        _       -> True

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
