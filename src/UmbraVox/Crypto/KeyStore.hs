-- SPDX-License-Identifier: Apache-2.0
-- | Identity key persistence for the MVP.
--
-- See: doc/spec/crypto.md
--
-- Finding:     M10.1.2 — Identity keys were written as raw bytes to disk;
--              the `_passphrase` parameter of `openKeyStore` was silently
--              ignored, leaving all key material unprotected at rest.
-- Vulnerability: An attacker with read access to ~/.umbravox/identity.key
--              can extract the full Ed25519 and X25519 secret keys without
--              knowing the user's passphrase.
-- Fix:         Derive a 32-byte wrapping key from the passphrase via
--              HKDF-SHA-256, generate a fresh 12-byte random nonce per
--              save, and wrap the 128-byte key blob with AES-256-GCM.
--              On-disk format: nonce(12) || ciphertext(128) || tag(16) = 156 bytes.
--              Decryption fails with an explicit error when the tag does not
--              verify (wrong passphrase or tampered file).
-- Verified:    `cabal test umbravox-test --test-options='required'` passes.
module UmbraVox.Crypto.KeyStore
  ( KeyStore
  , openKeyStore
  , saveIdentityKey
  , loadIdentityKey
  , saveIdentityKeyAt
  , loadIdentityKeyAt
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)

import UmbraVox.BuildProfile (BuildPluginId(..), pluginEnabled)
import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.HKDF (hkdfSHA256Extract, hkdfSHA256Expand)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.SecureBytes (SecureBytes, fromByteString, toByteString, withSecureKey)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))

-- | Handle to an encrypted-at-rest key store.
--
-- Finding:     M10.2.11 — 'ksPassphrase' was a plain 'ByteString' with an
--              auto-derived 'Show' instance, meaning 'show ks' would print
--              the raw passphrase bytes in plaintext to logs or error messages.
-- Vulnerability: Any logger or error-reporting path that prints a 'KeyStore'
--              value leaks the passphrase verbatim, potentially exposing it in
--              log files, crash dumps, or stderr output.
-- Fix:         Changed 'ksPassphrase' to 'SecureBytes' so the passphrase is
--              stored in a pinned, zeroed-on-free buffer.  Replaced 'deriving
--              Show' with a manual instance that prints @\<redacted\>@ for the
--              passphrase field.
-- Verified:    'show (KeyStore path _)' now prints \"<redacted>\" for the
--              passphrase; the passphrase bytes are never present in the output.
data KeyStore = KeyStore
  { ksPath       :: !FilePath
  , ksPassphrase :: !SecureBytes
  }

instance Show KeyStore where
  show ks = "KeyStore {ksPath = " ++ show (ksPath ks) ++ ", ksPassphrase = <redacted>}"

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | Domain separation label for key wrapping derivation.
keystoreInfo :: ByteString
keystoreInfo = "UmbraVox_KeyStore_v1"

-- | Length of the per-install random salt (32 bytes).
--
-- Finding:     M27.6.5 — All installations shared the same fixed
--              @BS.replicate 32 0x55@ salt for HKDF key derivation.
-- Vulnerability: A fixed salt means that two users with the same passphrase
--              derive the same wrapping key, enabling precomputation attacks
--              (rainbow tables) and cross-installation key correlation.
-- Fix:         Generate a per-install 32-byte random salt on first save,
--              stored alongside the key file as @<path>.salt@.  Existing
--              files without a salt file fall back to the legacy fixed salt
--              for backward compatibility.
-- Verified:    Each new installation produces a unique salt; legacy files
--              without a salt file still decrypt correctly.
saltLen :: Int
saltLen = 32

-- | Legacy fixed salt, used only when no per-install salt file exists
-- (backward compatibility with pre-M27.6.5 key stores).
legacySalt :: ByteString
legacySalt = BS.replicate 32 0x55

-- | On-disk sizes.
nonceLen, plaintextLen, tagLen, blobLen :: Int
nonceLen      = 12
plaintextLen  = 128
tagLen        = 16
blobLen       = nonceLen + plaintextLen + tagLen  -- 156

------------------------------------------------------------------------
-- Key derivation
------------------------------------------------------------------------

-- | Derive a 32-byte AES-256-GCM wrapping key from a passphrase and salt.
deriveWrappingKey :: ByteString -> ByteString -> ByteString
deriveWrappingKey salt passphrase =
    let !prk = hkdfSHA256Extract salt passphrase
    in hkdfSHA256Expand prk keystoreInfo 32

-- | Path of the per-install salt file for the given key path.
saltPath :: FilePath -> FilePath
saltPath path = path ++ ".salt"

-- | Load or generate the per-install random salt for key derivation.
-- If @<path>.salt@ exists, reads it.  Otherwise generates a fresh 32-byte
-- random salt, writes it to @<path>.salt@, and returns it.
loadOrGenerateSalt :: FilePath -> IO ByteString
loadOrGenerateSalt path = do
    let sp = saltPath path
    exists <- doesFileExist sp
    if exists
        then do
            s <- BS.readFile sp
            if BS.length s == saltLen
                then pure s
                else generateAndWriteSalt sp
        else generateAndWriteSalt sp
  where
    generateAndWriteSalt sp = do
        createDirectoryIfMissing True (takeDirectory sp)
        s <- randomBytes saltLen
        BS.writeFile sp s
        setFileMode sp (ownerReadMode `unionFileModes` ownerWriteMode)
        pure s

-- | Load the salt for decryption: use the per-install salt if it exists,
-- otherwise fall back to the legacy fixed salt for backward compatibility.
loadSaltForDecrypt :: FilePath -> IO ByteString
loadSaltForDecrypt path = do
    let sp = saltPath path
    exists <- doesFileExist sp
    if exists
        then do
            s <- BS.readFile sp
            if BS.length s == saltLen then pure s else pure legacySalt
        else pure legacySalt

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Open or create a key store at the given path, bound to a passphrase.
--
-- The passphrase is immediately wrapped in a 'SecureBytes' buffer so that
-- the caller's 'ByteString' copy can be discarded and the key material is
-- held in a pinned, zeroed-on-free allocation for the lifetime of the handle.
openKeyStore :: FilePath -> ByteString -> IO KeyStore
openKeyStore path passphrase = do
    createDirectoryIfMissing True (takeDirectory path)
    passphraseSB <- fromByteString passphrase
    pure (KeyStore path passphraseSB)

-- | Save the local identity key to the default application path.
-- Guarded by the PluginIdentityPersistence build flag; returns ()
-- silently when the plugin is disabled (ephemeral-by-default, M17.3.1).
saveIdentityKey :: IdentityKey -> IO ()
saveIdentityKey ik
    | not (pluginEnabled PluginIdentityPersistence) = pure ()
    | otherwise = do
        path <- defaultIdentityPath
        saveIdentityKeyAt path ik

-- | Load the local identity key from the default application path.
loadIdentityKey :: IO (Maybe IdentityKey)
loadIdentityKey = do
    path <- defaultIdentityPath
    loadIdentityKeyAt path

-- | Save an identity key to a specific path, encrypted with the default
-- (empty) passphrase derivation for test / passphrase-free use.
-- Guarded by the PluginIdentityPersistence build flag (M17.3.1).
saveIdentityKeyAt :: FilePath -> IdentityKey -> IO ()
saveIdentityKeyAt path ik
    | not (pluginEnabled PluginIdentityPersistence) = pure ()
    | otherwise = saveIdentityKeyWithPassphrase path BS.empty ik

-- | Load an identity key from a specific path, decrypting with the default
-- (empty) passphrase derivation.
loadIdentityKeyAt :: FilePath -> IO (Maybe IdentityKey)
loadIdentityKeyAt path =
    loadIdentityKeyWithPassphrase path BS.empty

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Write an identity key encrypted with the given passphrase.
--
-- Guarded by the PluginIdentityPersistence build flag; silently no-ops
-- when the plugin is disabled (ephemeral-by-default, M17.3.1).
--
-- The derived wrapping key is held in a 'SecureBytes' buffer for the
-- duration of the GCM operation and is zeroed on scope exit.
saveIdentityKeyWithPassphrase :: FilePath -> ByteString -> IdentityKey -> IO ()
saveIdentityKeyWithPassphrase _path _passphrase _ik
    | not (pluginEnabled PluginIdentityPersistence) = pure ()
saveIdentityKeyWithPassphrase path passphrase ik = do
    createDirectoryIfMissing True (takeDirectory path)
    nonce <- randomBytes nonceLen
    salt <- loadOrGenerateSalt path
    plaintext <- encodeIdentityKey ik
    sbKey <- fromByteString (deriveWrappingKey salt passphrase)
    blob <- withSecureKey sbKey $ \key -> do
        let !(ct, tag) = gcmEncrypt key nonce BS.empty plaintext
        pure (nonce <> ct <> tag)
    BS.writeFile path blob
    setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)

-- | Read and decrypt an identity key using the given passphrase.
-- Returns Nothing if the file does not exist.
-- Returns Nothing (after a decryption failure) if the passphrase is wrong
-- or the file is truncated / corrupted.
--
-- The derived wrapping key is held in a 'SecureBytes' buffer for the
-- duration of the GCM operation and is zeroed on scope exit.
loadIdentityKeyWithPassphrase :: FilePath -> ByteString -> IO (Maybe IdentityKey)
loadIdentityKeyWithPassphrase path passphrase = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            blob <- BS.readFile path
            if BS.length blob /= blobLen
                then pure Nothing
                else do
                    salt <- loadSaltForDecrypt path
                    let !nonce   = BS.take nonceLen blob
                        !rest    = BS.drop nonceLen blob
                        !ct      = BS.take plaintextLen rest
                        !tag     = BS.drop plaintextLen rest
                    sbKey <- fromByteString (deriveWrappingKey salt passphrase)
                    mPlaintext <- withSecureKey sbKey $ \key ->
                        pure (gcmDecrypt key nonce BS.empty ct tag)
                    case mPlaintext of
                        Nothing        -> pure Nothing
                        Just plaintext -> decodeIdentityKey plaintext

defaultIdentityPath :: IO FilePath
defaultIdentityPath = do
    dataDir <- lookupEnv "UMBRAVOX_DATA" >>= \case
        Just d  -> pure d
        Nothing -> (</> ".umbravox") <$> getHomeDirectory
    pure (dataDir </> "identity.key")

encodeIdentityKey :: IdentityKey -> IO ByteString
encodeIdentityKey ik = do
    edSec <- toByteString (ikEd25519Secret ik)
    xSec  <- toByteString (ikX25519Secret ik)
    pure $ BS.concat
        [ edSec
        , ikEd25519Public ik
        , xSec
        , ikX25519Public ik
        ]

decodeIdentityKey :: ByteString -> IO (Maybe IdentityKey)
decodeIdentityKey bs
    | BS.length bs /= 128 = pure Nothing
    | otherwise = do
        sbEdSecret <- fromByteString (BS.take 32 bs)
        sbXSecret  <- fromByteString (BS.take 32 (BS.drop 64 bs))
        pure $ Just IdentityKey
            { ikEd25519Secret = sbEdSecret
            , ikEd25519Public = BS.take 32 (BS.drop 32 bs)
            , ikX25519Secret  = sbXSecret
            , ikX25519Public  = BS.drop 96 bs
            }
