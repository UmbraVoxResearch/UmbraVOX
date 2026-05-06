-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.ChaCha20
    ( ffiLinked
    , chacha20Block
    , chacha20Encrypt
    ) where

import Data.ByteString (ByteString)
import Data.Word (Word32)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.Random as Reference

foreign import ccall "chacha20_link_probe" c_chacha20_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_chacha20_link_probe

chacha20Block :: ByteString -> ByteString -> Word32 -> IO ByteString
chacha20Block key nonce counter = do
    _ <- c_chacha20_link_probe
    pure (Reference.chacha20Block key nonce counter)

chacha20Encrypt :: ByteString -> ByteString -> Word32 -> ByteString -> IO ByteString
chacha20Encrypt key nonce counter plaintext = do
    _ <- c_chacha20_link_probe
    pure (Reference.chacha20Encrypt key nonce counter plaintext)
