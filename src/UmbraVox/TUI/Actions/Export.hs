-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Actions.Export
    ( startExport, exportToPath, startImport
    ) where

import Control.Monad (when, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)
import UmbraVox.TUI.Types
import UmbraVox.Crypto.BIP39 (generatePassphrase)
import UmbraVox.Crypto.Export (encryptExport, decryptExport)

-- | Prompt user for export path, or switch to import flow.
startExport :: AppState -> IO ()
startExport st = do
    sessions <- readIORef (cfgSessions (asConfig st))
    sel <- readIORef (asSelected st)
    let entries = Map.toList sessions
    if sel < length entries then do
        writeIORef (asDialogBuf st) ""
        writeIORef (asDialogMode st) (Just (DlgPrompt "Export path (or 'import')" $ \val ->
            if val == "import" then startImport st
            else exportToPath st val))
    else setStatusLocal st "No contact selected"

-- | Encrypt and write selected session history to a file.
exportToPath :: AppState -> String -> IO ()
exportToPath st path = do
    let path' = if null path then "umbravox_export.enc" else path
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Password (empty=BIP39)" $ \pw -> do
        password <- if null pw then do
            phrase <- generatePassphrase 6
            setStatusLocal st ("BIP39 passphrase: " ++ phrase)
            pure (BC.pack phrase)
          else pure (BC.pack pw)
        sessions <- readIORef (cfgSessions (asConfig st))
        sel <- readIORef (asSelected st)
        let entries = Map.toList sessions
        when (sel < length entries) $ do
            let (_,si) = entries !! sel
            hist <- readIORef (siHistory si)
            let plaintext = BC.pack (unlines (reverse hist))
            blob <- encryptExport password plaintext
            BS.writeFile path' blob
            setStatusLocal st ("Encrypted " ++ show (length hist)
                         ++ " msgs to " ++ path')
        ))

-- | Prompt user for import file path and password, then decrypt.
startImport :: AppState -> IO ()
startImport st = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt "Import file path" $ \path -> do
        exists <- doesFileExist path
        if not exists then setStatusLocal st ("File not found: " ++ path)
        else do
            writeIORef (asDialogBuf st) ""
            writeIORef (asDialogMode st) (Just (DlgPrompt "Import password" $ \pw -> do
                blob <- BS.readFile path
                case decryptExport (BC.pack pw) blob of
                    Nothing -> setStatusLocal st "Decryption failed (wrong password?)"
                    Just plaintext -> do
                        sessions <- readIORef (cfgSessions (asConfig st))
                        sel <- readIORef (asSelected st)
                        let entries = Map.toList sessions
                        when (sel < length entries) $ do
                            let (_,si) = entries !! sel
                                msgs = lines (BC.unpack plaintext)
                            modifyIORef' (siHistory si) (reverse msgs ++)
                            setStatusLocal st ("Imported " ++ show (length msgs)
                                         ++ " msgs from " ++ path)
                ))
        ))

-- Internal helper (duplicated to avoid circular imports) -----------------

setStatusLocal :: AppState -> String -> IO ()
setStatusLocal st msg = writeIORef (asStatusMsg st) msg
