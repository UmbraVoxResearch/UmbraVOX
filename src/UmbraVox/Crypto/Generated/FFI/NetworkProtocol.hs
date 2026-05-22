-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.NetworkProtocol
    ( ffiLinked
    ) where

import Foreign.C.Types (CInt(..))

foreign import ccall "networkprotocol_link_probe" c_networkprotocol_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_networkprotocol_link_probe
