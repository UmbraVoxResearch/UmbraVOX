-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.Poly1305
    ( ffiLinked
    , poly1305
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.Poly1305 as Reference

foreign import ccall "poly1305_link_probe" c_poly1305_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_poly1305_link_probe

poly1305 :: ByteString -> ByteString -> IO ByteString
poly1305 key message = do
    _ <- c_poly1305_link_probe
    pure (Reference.poly1305 key message)
