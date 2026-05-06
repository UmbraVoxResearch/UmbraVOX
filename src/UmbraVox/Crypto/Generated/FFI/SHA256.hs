-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.SHA256
    ( ffiLinked
    , sha256
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.SHA256 as Reference

foreign import ccall "sha256_link_probe" c_sha256_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_sha256_link_probe

sha256 :: ByteString -> IO ByteString
sha256 message = do
    _ <- c_sha256_link_probe
    pure (Reference.sha256 message)
