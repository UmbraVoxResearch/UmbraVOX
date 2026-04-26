-- | Encrypted conversation export with AES-256-GCM and HKDF key derivation.
--
-- Wire format: salt (32) || nonce (12) || ciphertext || tag (16)
module UmbraVox.Crypto.Export
    ( encryptExport
    , decryptExport
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.HKDF (hkdfSHA256Extract, hkdfSHA256Expand)
import UmbraVox.Crypto.Random (randomBytes)

-- | Info string for domain separation.
exportInfo :: ByteString
exportInfo = "UmbraVox_Export_v1"

-- | Header size: 32 (salt) + 12 (nonce).
headerLen :: Int
headerLen = 44

-- | Tag size for AES-256-GCM.
tagLen :: Int
tagLen = 16

-- | Derive a 32-byte encryption key from password and salt via HKDF-SHA256.
deriveKey :: ByteString -> ByteString -> ByteString
deriveKey salt password =
    let !prk = hkdfSHA256Extract salt password
    in hkdfSHA256Expand prk exportInfo 32

-- | Encrypt plaintext with a password for export.
--
-- @encryptExport password plaintext@ returns the encrypted blob:
-- salt (32) || nonce (12) || ciphertext || tag (16)
encryptExport :: ByteString -> ByteString -> IO ByteString
encryptExport password plaintext = do
    salt  <- randomBytes 32
    nonce <- randomBytes 12
    let !key        = deriveKey salt password
        !(ct, tag)  = gcmEncrypt key nonce exportInfo plaintext
    pure (salt <> nonce <> ct <> tag)

-- | Decrypt an exported blob with a password.
--
-- @decryptExport password blob@ returns the plaintext if the password
-- is correct and the data has not been tampered with.
decryptExport :: ByteString -> ByteString -> Maybe ByteString
decryptExport password blob
    | BS.length blob < headerLen + tagLen = Nothing
    | otherwise =
        let !salt  = BS.take 32 blob
            !nonce = BS.take 12 (BS.drop 32 blob)
            !rest  = BS.drop headerLen blob
            !ctLen = BS.length rest - tagLen
            !ct    = BS.take ctLen rest
            !tag   = BS.drop ctLen rest
            !key   = deriveKey salt password
        in gcmDecrypt key nonce exportInfo ct tag
