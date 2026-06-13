-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
module UmbraVox.TUI.Actions.Export
    ( startExport, exportToPath, startImport
    ) where

import qualified Data.ByteString as BS
import Data.IORef (readIORef, writeIORef, modifyIORef')
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist, canonicalizePath, getHomeDirectory)
import System.IO (hPutStrLn, stderr)
#ifdef mingw32_HOST_OS
import System.Directory (setPermissions, emptyPermissions, setOwnerReadable, setOwnerWritable)
#else
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
#endif
import UmbraVox.BuildProfile (BuildPluginId(..), pluginEnabled, pluginUnavailableStatus)
import UmbraVox.TUI.Types
import UmbraVox.Crypto.BIP39 (generatePassphrase)
import UmbraVox.Crypto.Export (encryptExport, decryptExport)
import UmbraVox.Protocol.UTF8 (encodeStringUtf8, decodeUtf8String)
import UmbraVox.TUI.RuntimeEvent (RuntimeEvent(..), applyRuntimeEvents)

-- | Prompt user for export path, or switch to import flow.
startExport :: AppState -> IO ()
startExport st = do
    if not (pluginEnabled PluginChatTransfer)
        then setStatusLocal st (pluginUnavailableStatus PluginChatTransfer)
        else do
            mSel <- selectedSession st
            case mSel of
              Just _ -> do
                applyRuntimeEvents st
                    [ EventSetInput ""
                    , EventSetDialog (Just (DlgPrompt "Export path (or 'import')" $ \val ->
                        if val == "import" then startImport st
                        else exportToPath st val))
                    ]
              Nothing -> setStatusLocal st "No contact selected"

-- | Encrypt and write selected session history to a file.
exportToPath :: AppState -> String -> IO ()
exportToPath st path = do
    if not (pluginEnabled PluginChatTransfer)
        then setStatusLocal st (pluginUnavailableStatus PluginChatTransfer)
        else do
            let path' = if null path then "umbravox_export.enc" else path
            -- M40.2: Reject paths outside $HOME or the working directory before
            -- prompting for a password, mirroring TUI.Actions.Session (/file).
            contained <- resolveContainedPath path'
            case contained of
              Left err   -> setStatusLocal st err
              Right canon -> applyRuntimeEvents st
                [ EventSetInput ""
                , EventSetDialog (Just (DlgPrompt "Password (empty=BIP39)" $ \pw -> do
                    password <- if null pw then do
                        phrase <- generatePassphrase 6
                        -- M35-R8-B01: Print to stderr so the user can record
                        -- the passphrase before it is used to encrypt.  Without
                        -- this, the exported file is permanently unrecoverable.
                        -- (TUI stdout is in raw mode; stderr is the safe channel.)
                        hPutStrLn stderr ("BIP39 export passphrase: " ++ phrase)
                        setStatusLocal st "BIP39 export ready — passphrase printed to stderr"
                        pure (encodeStringUtf8 phrase)
                      else pure (encodeStringUtf8 pw)
                    mSel <- selectedSession st
                    case mSel of
                      Just (_,si) -> do
                        hist <- readIORef (siHistory si)
                        let plaintext = encodeStringUtf8 (unlines (reverse hist))
                        blob <- encryptExport password plaintext
                        BS.writeFile canon blob
                        -- M40.1: Restrict the encrypted export to owner-only (0600).
                        setExportFileMode canon
                        setStatusLocal st ("Encrypted " ++ show (length hist)
                                     ++ " msgs to " ++ canon)
                      Nothing -> setStatusLocal st "No contact selected"
                    ))
                ]

-- | Prompt user for import file path and password, then decrypt.
startImport :: AppState -> IO ()
startImport st = do
    if not (pluginEnabled PluginChatTransfer)
        then setStatusLocal st (pluginUnavailableStatus PluginChatTransfer)
        else do
            mSel <- selectedSession st
            case mSel of
              Nothing -> setStatusLocal st "No contact selected"
              Just _ -> do
                applyRuntimeEvents st
                    [ EventSetInput ""
                    , EventSetDialog (Just (DlgPrompt "Import file path" $ \path -> do
                        exists <- doesFileExist path
                        -- M40.2: Require the import path to stay under $HOME or CWD.
                        contained <- resolveContainedPath path
                        case (exists, contained) of
                          (False, _)      -> setStatusLocal st ("File not found: " ++ path)
                          (_, Left err)    -> setStatusLocal st err
                          (_, Right canon) ->
                            applyRuntimeEvents st
                                [ EventSetInput ""
                                , EventSetDialog (Just (DlgPrompt "Import password" $ \pw -> do
                                    blob <- BS.readFile canon
                                    mPlaintext <- decryptExport (encodeStringUtf8 pw) blob
                                    case mPlaintext of
                                        Nothing -> setStatusLocal st "Decryption failed (wrong password?)"
                                        Just plaintext ->
                                            case decodeUtf8String plaintext of
                                                Left _ -> setStatusLocal st "Import payload is not valid UTF-8"
                                                Right decoded -> do
                                                    mSel' <- selectedSession st
                                                    case mSel' of
                                                      Just (_,si) -> do
                                                        let msgs = lines decoded
                                                        modifyIORef' (siHistory si) (reverse msgs ++)
                                                        setStatusLocal st ("Imported " ++ show (length msgs)
                                                                     ++ " msgs from " ++ canon)
                                                      Nothing -> setStatusLocal st "No contact selected"
                                    ))
                                ]
                        ))
                    ]

-- Internal helper (duplicated to avoid circular imports) -----------------

-- | M40.2: Resolve a user-supplied path and require it to remain under the
-- user's home directory or the current working directory.  Returns the
-- canonical path on success.  Mirrors the containment check used for the
-- @/file@ command in 'UmbraVox.TUI.Actions.Session' so export/import cannot
-- read or overwrite arbitrary files (e.g. ~/.ssh/authorized_keys).
resolveContainedPath :: FilePath -> IO (Either String FilePath)
resolveContainedPath path = do
    canon   <- canonicalizePath path
    homeDir <- getHomeDirectory
    cwd     <- canonicalizePath "."
    let allowed = homeDir `isPrefixOf` canon || cwd `isPrefixOf` canon
    pure $ if allowed
              then Right canon
              else Left "Access denied: path must be under home or working directory"

-- | M40.1: Restrict an exported file to owner read/write only (0600).
setExportFileMode :: FilePath -> IO ()
setExportFileMode path =
#ifdef mingw32_HOST_OS
    setPermissions path (setOwnerWritable True (setOwnerReadable True emptyPermissions))
#else
    setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)
#endif

setStatusLocal :: AppState -> String -> IO ()
setStatusLocal st msg = applyRuntimeEvents st [EventSetStatus msg]

selectedSession :: AppState -> IO (Maybe (SessionId, SessionInfo))
selectedSession st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    sel <- readIORef (asSelected st)
    let entries = Map.toList sessions
    pure $
        if sel >= 0 && sel < length entries
            then Just (entries !! sel)
            else Nothing
