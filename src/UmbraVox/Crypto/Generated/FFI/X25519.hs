-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.X25519
    ( ffiLinked
    , x25519
    , x25519Basepoint
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.Curve25519 as Reference

foreign import ccall "x25519_link_probe" c_x25519_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_x25519_link_probe

x25519Basepoint :: ByteString
x25519Basepoint = Reference.x25519Basepoint

x25519 :: ByteString -> ByteString -> IO ByteString
x25519 scalar point = do
    _ <- c_x25519_link_probe
    pure (Reference.x25519 scalar point)
