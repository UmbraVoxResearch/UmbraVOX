-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.App.RuntimeLog
    ( logEvent
    , runtimeLoggingEnabled
    ) where

import Control.Exception (SomeException, catch)
import Control.Concurrent.MVar (MVar, withMVar, newMVar)
import Data.IORef (readIORef)
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

import UmbraVox.TUI.Types (AppConfig(..))

runtimeLogLock :: MVar ()
runtimeLogLock = unsafePerformIO (newMVar ())
{-# NOINLINE runtimeLogLock #-}

runtimeLoggingEnabled :: AppConfig -> IO Bool
runtimeLoggingEnabled cfg = do
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
                        map (\(k, v) -> k ++ "=" ++ quoteValue v) fields
            withMVar runtimeLogLock $ \_ ->
                writeLogLine path (rendered ++ "\n")
                    `catch` \(_ :: SomeException) -> pure ()

writeLogLine :: FilePath -> String -> IO ()
writeLogLine path line = do
    createDirectoryIfMissing True (takeDirectory path)
    existed <- doesFileExist path
    appendFile path line
    if existed
        then pure ()
        else setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)

quoteValue :: String -> String
quoteValue raw = "\"" ++ concatMap escapeChar raw ++ "\""
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]
