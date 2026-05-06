-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.SHA512
    ( ffiLinked
    , sha512
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.SHA512 as Reference

foreign import ccall "sha512_link_probe" c_sha512_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_sha512_link_probe

sha512 :: ByteString -> IO ByteString
sha512 message = do
    _ <- c_sha512_link_probe
    pure (Reference.sha512 message)
