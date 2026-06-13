-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

-- | Runtime event logging for operational diagnostics.
--
-- WARNING: Debug logging captures sensitive operational metadata.
-- Even with payload redaction and restrictive file permissions,
-- log files should not be treated as production-safe telemetry.
-- This facility is intended for troubleshooting only and is
-- disabled by default. Enable via UMBRAVOX_DEBUG_LOG=1.
module UmbraVox.App.RuntimeLog
    ( logEvent
    , runtimeLoggingEnabled
    , redactedFieldKeys
    , ensureLogPermissions
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (withMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
#ifdef mingw32_HOST_OS
import System.Directory (setPermissions, emptyPermissions, setOwnerReadable, setOwnerWritable)
import System.Process (getCurrentPid)
#else
import System.Posix.Files
    ( ownerReadMode
    , ownerWriteMode
    , setFileMode
    , unionFileModes
    )
import System.Posix.Process (getProcessID)
#endif

import UmbraVox.BuildProfile (BuildPluginId(..), pluginEnabled)
import UmbraVox.App.Config (AppConfig(..))

runtimeLoggingEnabled :: AppConfig -> IO Bool
runtimeLoggingEnabled cfg = do
    if not (pluginEnabled PluginRuntimeLogging)
        then pure False
        else do
            ephemeral <- readIORef (cfgEphemeral cfg)
            if ephemeral
                then pure False
                else do
                    env <- lookupEnv "UMBRAVOX_DEBUG_LOG"
                    case env of
                        Just raw | raw `elem` ["1", "true", "TRUE", "yes", "YES", "on", "ON"] -> pure True
                        Just raw | raw `elem` ["0", "false", "FALSE", "no", "NO", "off", "OFF"] -> pure False
                        _ -> readIORef (cfgDebugLogging cfg)

logEvent :: AppConfig -> String -> [(String, String)] -> IO ()
logEvent cfg name fields = do
    enabled <- runtimeLoggingEnabled cfg
    if not enabled
        then pure ()
        else do
            path <- readIORef (cfgDebugLogPath cfg)
            ts <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime
            let rendered =
                    unwords $
                        [ts, name] ++
                        map renderField fields
            withMVar (cfgLogLock cfg) $ \_ ->
                writeLogLine cfg path (rendered ++ "\n")
                    `catch` \(_ :: SomeException) -> pure ()

writeLogLine :: AppConfig -> FilePath -> String -> IO ()
writeLogLine cfg path line = do
    createDirectoryIfMissing True (takeDirectory path)
    appendFile path line `catch` \(_ :: SomeException) ->
        -- Retry once after a brief delay (handles transient file locks)
        threadDelay 10000 >> appendFile path line `catch` \(_ :: SomeException) -> pure ()
    -- Enforce restrictive permissions on every write
    ensureLogPermissions path
    -- Single-writer PID tracking
#ifdef mingw32_HOST_OS
    currentPID <- fromIntegral <$> getCurrentPid
#else
    currentPID <- fromIntegral <$> getProcessID
#endif
    previousPID <- readIORef (cfgLogWriterPID cfg)
    writeIORef (cfgLogWriterPID cfg) currentPID
    when (previousPID /= 0 && previousPID /= currentPID) $
        appendFile path $ "WARN: multiple log writer PIDs detected ("
            ++ show previousPID ++ " -> " ++ show currentPID ++ ")\n"

-- | Ensure the log file has restrictive permissions (0600).
-- Re-applied on every write to prevent permission drift.
ensureLogPermissions :: FilePath -> IO ()
ensureLogPermissions path = do
    exists <- doesFileExist path
#ifdef mingw32_HOST_OS
    when exists $
        setPermissions path (setOwnerWritable True (setOwnerReadable True emptyPermissions))
#else
    when exists $
        setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)
#endif

quoteValue :: String -> String
quoteValue raw = "\"" ++ concatMap escapeChar raw ++ "\""
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]

renderField :: (String, String) -> String
renderField (key, value) = key ++ "=" ++ quoteValue (sanitizeFieldValue key value)

sanitizeFieldValue :: String -> String -> String
sanitizeFieldValue key value
    | key `elem` safeFieldKeys = value
    | otherwise = "[redacted]"

-- | Fields whose values are safe to show in logs (denylist-by-default).
-- All field values are redacted unless the key appears in this list.
--
-- Criteria for inclusion: value carries no secret material (not a key,
-- password, peer identity, or session token) and provides operational
-- value for incident diagnosis.  Non-secret counts, booleans, and
-- human-readable error descriptions qualify; network topology (port,
-- host) does not.
safeFieldKeys :: [String]
safeFieldKeys =
    [ "active"     -- connection/session boolean or count
    , "count"
    , "direction"
    , "drained"    -- drain-complete boolean
    , "level"
    , "mode"
    , "reason"     -- human-readable error/close reason; no secret content
    , "result"
    , "status"
    , "type"
    , "version"
    ]

-- | Legacy alias kept for export compatibility.
-- Under the denylist model every key NOT in 'safeFieldKeys' is redacted,
-- so this list is no longer the source of truth for redaction decisions.
redactedFieldKeys :: [String]
redactedFieldKeys =
    [ "answer"
    , "content"
    , "host"
    , "key"
    , "messages"
    , "passphrase"
    , "password"
    , "path"
    , "peer"
    , "port"
    , "secret"
    , "selected_index"
    , "sender"
    , "session_id"
    , "token"
    ]
