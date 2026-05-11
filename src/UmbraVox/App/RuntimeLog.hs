-- SPDX-License-Identifier: Apache-2.0

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
import Control.Concurrent.MVar (MVar, withMVar, newMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files
    ( ownerReadMode
    , ownerWriteMode
    , setFileMode
    , unionFileModes
    )
import System.Posix.Process (getProcessID)

import UmbraVox.BuildProfile (BuildPluginId(..), pluginEnabled)
import UmbraVox.App.Config (AppConfig(..))

runtimeLogLock :: MVar ()
runtimeLogLock = unsafePerformIO (newMVar ())
{-# NOINLINE runtimeLogLock #-}

logWriterPID :: IORef Int
logWriterPID = unsafePerformIO (newIORef 0)
{-# NOINLINE logWriterPID #-}

runtimeLoggingEnabled :: AppConfig -> IO Bool
runtimeLoggingEnabled cfg = do
    if not (pluginEnabled PluginRuntimeLogging)
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
            withMVar runtimeLogLock $ \_ ->
                writeLogLine path (rendered ++ "\n")
                    `catch` \(_ :: SomeException) -> pure ()

writeLogLine :: FilePath -> String -> IO ()
writeLogLine path line = do
    createDirectoryIfMissing True (takeDirectory path)
    appendFile path line `catch` \(_ :: SomeException) ->
        -- Retry once after a brief delay (handles transient file locks)
        threadDelay 10000 >> appendFile path line `catch` \(_ :: SomeException) -> pure ()
    -- Enforce restrictive permissions on every write
    ensureLogPermissions path
    -- Single-writer PID tracking
    currentPID <- fromIntegral <$> getProcessID
    previousPID <- readIORef logWriterPID
    writeIORef logWriterPID currentPID
    when (previousPID /= 0 && previousPID /= currentPID) $
        appendFile path $ "WARN: multiple log writer PIDs detected ("
            ++ show previousPID ++ " -> " ++ show currentPID ++ ")\n"

-- | Ensure the log file has restrictive permissions (0600).
-- Re-applied on every write to prevent permission drift.
ensureLogPermissions :: FilePath -> IO ()
ensureLogPermissions path = do
    exists <- doesFileExist path
    when exists $
        setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)

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
    | key `elem` redactedFieldKeys = "[redacted]"
    | otherwise = value

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
