-- SPDX-License-Identifier: Apache-2.0
-- | Identity key persistence for the MVP.
--
-- See: doc/spec/crypto.md
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

import UmbraVox.Crypto.Signal.X3DH (IdentityKey(..))

-- | Handle to an encrypted-at-rest key store.
data KeyStore = KeyStore
  { ksPath :: !FilePath
  } deriving (Show)

-- | Open or create a key store at the given path.
openKeyStore :: FilePath -> ByteString -> IO KeyStore
openKeyStore path _passphrase = do
    createDirectoryIfMissing True (takeDirectory path)
    pure (KeyStore path)

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

-- | Save an identity key to a specific path.
saveIdentityKeyAt :: FilePath -> IdentityKey -> IO ()
saveIdentityKeyAt path ik = do
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path (encodeIdentityKey ik)

-- | Load an identity key from a specific path.
loadIdentityKeyAt :: FilePath -> IO (Maybe IdentityKey)
loadIdentityKeyAt path = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else decodeIdentityKey <$> BS.readFile path

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
