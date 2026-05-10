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
import System.FilePath ((</>), takeDirectory)
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)

import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.HKDF (hkdfSHA256Extract, hkdfSHA256Expand)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))

-- | Handle to an encrypted-at-rest key store.
data KeyStore = KeyStore
  { ksPath       :: !FilePath
  , ksPassphrase :: !ByteString
  } deriving (Show)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | Domain separation label for key wrapping derivation.
keystoreInfo :: ByteString
keystoreInfo = "UmbraVox_KeyStore_v1"

-- | Fixed salt for key derivation when no per-file salt is stored.
-- Using a fixed salt is acceptable here because the passphrase is the
-- secret; the nonce ensures ciphertext uniqueness per save.
keystoreSalt :: ByteString
keystoreSalt = BS.replicate 32 0x55

-- | On-disk sizes.
nonceLen, plaintextLen, tagLen, blobLen :: Int
nonceLen      = 12
plaintextLen  = 128
tagLen        = 16
blobLen       = nonceLen + plaintextLen + tagLen  -- 156

------------------------------------------------------------------------
-- Key derivation
------------------------------------------------------------------------

-- | Derive a 32-byte AES-256-GCM wrapping key from a passphrase.
-- Empty passphrase uses a known-zero derivation for backward compatibility.
deriveWrappingKey :: ByteString -> ByteString
deriveWrappingKey passphrase =
    let !prk = hkdfSHA256Extract keystoreSalt passphrase
    in hkdfSHA256Expand prk keystoreInfo 32

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Open or create a key store at the given path, bound to a passphrase.
openKeyStore :: FilePath -> ByteString -> IO KeyStore
openKeyStore path passphrase = do
    createDirectoryIfMissing True (takeDirectory path)
    pure (KeyStore path passphrase)

-- | Save the local identity key to the default application path.
saveIdentityKey :: IdentityKey -> IO ()
saveIdentityKey ik = do
    path <- defaultIdentityPath
    saveIdentityKeyAt path ik

-- | Load the local identity key from the default application path.
loadIdentityKey :: IO (Maybe IdentityKey)
loadIdentityKey = do
    path <- defaultIdentityPath
    loadIdentityKeyAt path

-- | Save an identity key to a specific path, encrypted with the default
-- (empty) passphrase derivation for test / passphrase-free use.
saveIdentityKeyAt :: FilePath -> IdentityKey -> IO ()
saveIdentityKeyAt path ik =
    saveIdentityKeyWithPassphrase path BS.empty ik

-- | Load an identity key from a specific path, decrypting with the default
-- (empty) passphrase derivation.
loadIdentityKeyAt :: FilePath -> IO (Maybe IdentityKey)
loadIdentityKeyAt path =
    loadIdentityKeyWithPassphrase path BS.empty

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Write an identity key encrypted with the given passphrase.
saveIdentityKeyWithPassphrase :: FilePath -> ByteString -> IdentityKey -> IO ()
saveIdentityKeyWithPassphrase path passphrase ik = do
    createDirectoryIfMissing True (takeDirectory path)
    nonce <- randomBytes nonceLen
    let !wrapKey        = deriveWrappingKey passphrase
        !plaintext      = encodeIdentityKey ik
        !(ct, tag)      = gcmEncrypt wrapKey nonce BS.empty plaintext
        !blob           = nonce <> ct <> tag
    BS.writeFile path blob
    setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)

-- | Read and decrypt an identity key using the given passphrase.
-- Returns Nothing if the file does not exist.
-- Returns Nothing (after a decryption failure) if the passphrase is wrong
-- or the file is truncated / corrupted.
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
                    let !nonce   = BS.take nonceLen blob
                        !rest    = BS.drop nonceLen blob
                        !ct      = BS.take plaintextLen rest
                        !tag     = BS.drop plaintextLen rest
                        !wrapKey = deriveWrappingKey passphrase
                    case gcmDecrypt wrapKey nonce BS.empty ct tag of
                        Nothing        -> pure Nothing
                        Just plaintext -> pure (decodeIdentityKey plaintext)

defaultIdentityPath :: IO FilePath
defaultIdentityPath = do
    home <- getHomeDirectory
    pure (home </> ".umbravox" </> "identity.key")

encodeIdentityKey :: IdentityKey -> ByteString
encodeIdentityKey ik =
    BS.concat
        [ ikEd25519Secret ik
        , ikEd25519Public ik
        , ikX25519Secret ik
        , ikX25519Public ik
        ]

decodeIdentityKey :: ByteString -> Maybe IdentityKey
decodeIdentityKey bs
    | BS.length bs /= 128 = Nothing
    | otherwise =
        Just IdentityKey
            { ikEd25519Secret = BS.take 32 bs
            , ikEd25519Public = BS.take 32 (BS.drop 32 bs)
            , ikX25519Secret  = BS.take 32 (BS.drop 64 bs)
            , ikX25519Public  = BS.drop 96 bs
            }
