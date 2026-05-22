-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.VRF
    ( ffiLinked
    , vrfProve
    , vrfVerify
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.VRF as Reference

foreign import ccall "vrf_link_probe" c_vrf_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_vrf_link_probe

vrfProve :: ByteString -> ByteString -> IO ByteString
vrfProve sk alpha = do
    _ <- c_vrf_link_probe
    pure (Reference.vrfProve sk alpha)

vrfVerify :: ByteString -> ByteString -> ByteString -> IO (Maybe ByteString)
vrfVerify pk alpha pi = do
    _ <- c_vrf_link_probe
    pure (Reference.vrfVerify pk alpha pi)
