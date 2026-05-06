-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Actions.Export
    ( startExport, exportToPath, startImport
    ) where

import qualified Data.ByteString as BS
import Data.IORef (readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)
import UmbraVox.TUI.Types
import UmbraVox.Crypto.BIP39 (generatePassphrase)
import UmbraVox.Crypto.Export (encryptExport, decryptExport)
import UmbraVox.Protocol.UTF8 (encodeStringUtf8, decodeUtf8String)

-- | Prompt user for export path, or switch to import flow.
startExport :: AppState -> IO ()
startExport st = do
    mSel <- selectedSession st
    case mSel of
      Just _ -> do
        writeIORef (asDialogBuf st) ""
        writeIORef (asDialogMode st) (Just (DlgPrompt "Export path (or 'import')" $ \val ->
            if val == "import" then startImport st
            else exportToPath st val))
      Nothing -> setStatusLocal st "No contact selected"

-- | Encrypt and write selected session history to a file.
exportToPath :: AppState -> String -> IO ()
exportToPath st path = do
    let path' = if null path then "umbravox_export.enc" else path
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Password (empty=BIP39)" $ \pw -> do
        password <- if null pw then do
            phrase <- generatePassphrase 6
            setStatusLocal st ("BIP39 passphrase: " ++ phrase)
            pure (encodeStringUtf8 phrase)
          else pure (encodeStringUtf8 pw)
        mSel <- selectedSession st
        case mSel of
          Just (_,si) -> do
            hist <- readIORef (siHistory si)
            let plaintext = encodeStringUtf8 (unlines (reverse hist))
            blob <- encryptExport password plaintext
            BS.writeFile path' blob
            setStatusLocal st ("Encrypted " ++ show (length hist)
                         ++ " msgs to " ++ path')
          Nothing -> setStatusLocal st "No contact selected"
        ))

-- | Prompt user for import file path and password, then decrypt.
startImport :: AppState -> IO ()
startImport st = do
    mSel <- selectedSession st
    case mSel of
      Nothing -> setStatusLocal st "No contact selected"
      Just _ -> do
        writeIORef (asDialogBuf st) ""
        writeIORef (asDialogMode st) (Just (DlgPrompt "Import file path" $ \path -> do
            exists <- doesFileExist path
            if not exists then setStatusLocal st ("File not found: " ++ path)
            else do
                writeIORef (asDialogBuf st) ""
                writeIORef (asDialogMode st) (Just (DlgPrompt "Import password" $ \pw -> do
                    blob <- BS.readFile path
                    case decryptExport (encodeStringUtf8 pw) blob of
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
                                                     ++ " msgs from " ++ path)
                                      Nothing -> setStatusLocal st "No contact selected"
                    ))
            ))

-- Internal helper (duplicated to avoid circular imports) -----------------

setStatusLocal :: AppState -> String -> IO ()
setStatusLocal st msg = writeIORef (asStatusMsg st) msg

selectedSession :: AppState -> IO (Maybe (SessionId, SessionInfo))
selectedSession st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    sel <- readIORef (asSelected st)
    let entries = Map.toList sessions
    pure $
        if sel >= 0 && sel < length entries
            then Just (entries !! sel)
            else Nothing
