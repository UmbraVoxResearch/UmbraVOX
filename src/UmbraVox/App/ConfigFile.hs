-- SPDX-License-Identifier: Apache-2.0
-- | Config file loading for UmbraVOX.
--
-- Reads @~\/.umbravox\/config@ if it exists.
-- Format: one @key = value@ pair per line; @#@ introduces a comment;
-- blank lines are ignored.
--
-- Supported keys (matching 'UmbraVox.App.Defaults'):
--   port, max_connections, max_frame_size, max_skip, max_total_skipped,
--   sqlite_timeout_ms, mdns_announce_interval, mdns_peer_timeout, debug_log
module UmbraVox.App.ConfigFile
    ( loadConfigFile
    , applyConfigFile
    , verifyConfigHash
    ) where

import Control.Exception (SomeException, catch)
import Data.Char (isSpace, toLower)
import System.IO (hPutStrLn, stderr)
import Data.IORef (writeIORef)
import Data.List (dropWhileEnd, isPrefixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)

import UmbraVox.App.Config (AppConfig(..))
import qualified UmbraVox.Crypto.Generated.FFI.SHA256 as SHA256FFI

-- | Parse a single line into a @(key, value)@ pair.
-- Returns 'Nothing' for blank lines, comment lines, and malformed lines.
parseLine :: String -> Maybe (String, String)
parseLine raw =
    let stripped = dropWhile isSpace raw
    in if null stripped || "#" `isPrefixOf` stripped
        then Nothing
        else case break (== '=') stripped of
            (_, [])    -> Nothing          -- no '=' found
            (k, _:v)   ->
                let key = dropWhileEnd isSpace (dropWhile isSpace k)
                    val = dropWhileEnd isSpace (dropWhile isSpace v)
                in if null key then Nothing else Just (key, val)

-- | Load @~\/.umbravox\/config@ and return a map of key\/value pairs.
-- Returns an empty map if the file does not exist or cannot be read.
loadConfigFile :: IO (Map.Map String String)
loadConfigFile = do
    dataDir <- lookupEnv "UMBRAVOX_DATA" >>= \case
        Just d  -> pure d
        Nothing -> (++ "/.umbravox") <$> getHomeDirectory
    let path = dataDir ++ "/config"
    exists <- doesFileExist path
    if not exists
        then pure Map.empty
        else (do
            contents <- readFile path
            length contents `seq` pure ()
            let pairs = [ p | Just p <- map parseLine (lines contents) ]
            pure (Map.fromList pairs)
            ) `catch` \(e :: SomeException) -> do
                hPutStrLn stderr $ "Warning: config parse failed, using defaults: " ++ show e
                pure Map.empty

-- | Apply a config map (from 'loadConfigFile') to an 'AppConfig', overriding
-- only the fields for which a key is present in the map.
applyConfigFile :: Map.Map String String -> AppConfig -> IO ()
applyConfigFile cfg appCfg = do
    applyInt  "port"                  (cfgListenPort appCfg)
    applyBool "debug_log"             (cfgDebugLogging appCfg)
    -- max_connections, max_frame_size, max_skip, max_total_skipped,
    -- sqlite_timeout_ms, mdns_announce_interval, and mdns_peer_timeout are
    -- global constants (not stored as IORefs in AppConfig); they are honoured
    -- by the fields below where a direct IORef equivalent exists.
    -- Currently only port and debug_log have corresponding IORefs.
    -- Future work: expose remaining Defaults as IORef fields in AppConfig.
    pure ()
  where
    applyInt key ref =
        case Map.lookup key cfg of
            Nothing -> pure ()
            Just v  -> case reads v of
                ((n, rest) : _) | all isSpace rest -> writeIORef ref n
                _                                  -> pure ()

    applyBool key ref =
        case Map.lookup key cfg of
            Nothing -> pure ()
            Just v  -> case normalize v of
                "1"     -> writeIORef ref True
                "true"  -> writeIORef ref True
                "yes"   -> writeIORef ref True
                "on"    -> writeIORef ref True
                "0"     -> writeIORef ref False
                "false" -> writeIORef ref False
                "no"    -> writeIORef ref False
                "off"   -> writeIORef ref False
                _       -> pure ()

    normalize = map toLower . dropWhileEnd isSpace . dropWhile isSpace

-- | Verify config file integrity against a pinned SHA-256 hash (M17.7.4,
-- M20.4.6).
--
-- If the config file contains a @config_hash_pin = <hex>@ entry, the file's
-- SHA-256 hash (computed over all lines excluding the pin line) is compared
-- against the pinned value.  Returns:
--
-- * @Right ()@ — no pin present (verification skipped) or pin matches
-- * @Left msg@ — pin present but hash does NOT match (tampered)
verifyConfigHash :: IO (Either String ())
verifyConfigHash = do
    dataDir <- lookupEnv "UMBRAVOX_DATA" >>= \case
        Just d  -> pure d
        Nothing -> (++ "/.umbravox") <$> getHomeDirectory
    let path = dataDir ++ "/config"
    exists <- doesFileExist path
    if not exists
        then pure (Right ())
        else (do
            contents <- readFile path
            length contents `seq` pure ()
            let allLines = lines contents
                cfg = Map.fromList [ p | Just p <- map parseLine allLines ]
            case Map.lookup "config_hash_pin" cfg of
                Nothing  -> pure (Right ())
                Just pin -> do
                    -- Hash all lines except the pin line itself.
                    -- Use exact key match via parseLine, not prefix match.
                    let filtered = unlines [ l | l <- allLines
                                           , maybe True ((/= "config_hash_pin") . fst) (parseLine l) ]
                    hashBytes <- SHA256FFI.sha256 (BS8.pack filtered)
                    let hashHex = concatMap byteToHex (BS.unpack hashBytes)
                    if map toLower pin == map toLower hashHex
                        then pure (Right ())
                        else pure (Left $ "config hash mismatch: expected "
                                       ++ map toLower pin ++ " but got "
                                       ++ map toLower hashHex)
            ) `catch` \(_ :: SomeException) ->
                pure (Left "config hash verification failed: could not read config file")
  where
    byteToHex w = [hexC (fromIntegral w `div` 16), hexC (fromIntegral w `mod` 16)]
    hexC n | n < 10    = toEnum (fromEnum '0' + n)
           | otherwise = toEnum (fromEnum 'a' + n - 10)
