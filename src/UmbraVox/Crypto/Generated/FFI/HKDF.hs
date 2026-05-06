-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.HKDF
    ( ffiLinked
    , hkdf
    , hkdfSHA256
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.HKDF as Reference

foreign import ccall "hkdf_link_probe" c_hkdf_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_hkdf_link_probe

hkdf :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString
hkdf salt ikm info len = do
    _ <- c_hkdf_link_probe
    pure (Reference.hkdf salt ikm info len)

hkdfSHA256 :: ByteString -> ByteString -> ByteString -> Int -> IO ByteString
hkdfSHA256 salt ikm info len = do
    _ <- c_hkdf_link_probe
    pure (Reference.hkdfSHA256 salt ikm info len)
