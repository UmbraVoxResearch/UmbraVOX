-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.Ed25519Extended
    ( ffiLinked
    , ed25519Sign
    , ed25519Verify
    , ed25519PublicKey
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.Ed25519 as Reference

foreign import ccall "ed25519extended_link_probe" c_ed25519extended_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_ed25519extended_link_probe

ed25519Sign :: ByteString -> ByteString -> IO ByteString
ed25519Sign sk msg = do
    _ <- c_ed25519extended_link_probe
    pure (Reference.ed25519Sign sk msg)

ed25519Verify :: ByteString -> ByteString -> ByteString -> IO Bool
ed25519Verify pk msg sig = do
    _ <- c_ed25519extended_link_probe
    pure (Reference.ed25519Verify pk msg sig)

ed25519PublicKey :: ByteString -> IO ByteString
ed25519PublicKey sk = do
    _ <- c_ed25519extended_link_probe
    pure (Reference.ed25519PublicKey sk)
