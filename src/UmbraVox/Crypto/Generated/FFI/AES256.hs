-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.AES256
    ( ffiLinked
    , aesEncrypt
    , aesDecrypt
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.AES as Reference

foreign import ccall "aes256_link_probe" c_aes256_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_aes256_link_probe

aesEncrypt :: ByteString -> ByteString -> IO ByteString
aesEncrypt key block = do
    _ <- c_aes256_link_probe
    pure (Reference.aesEncrypt key block)

aesDecrypt :: ByteString -> ByteString -> IO ByteString
aesDecrypt key block = do
    _ <- c_aes256_link_probe
    pure (Reference.aesDecrypt key block)
