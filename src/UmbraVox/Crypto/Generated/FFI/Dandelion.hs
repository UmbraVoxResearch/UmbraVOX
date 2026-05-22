-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.Dandelion
    ( ffiLinked
    ) where

import Foreign.C.Types (CInt(..))

foreign import ccall "dandelion_link_probe" c_dandelion_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_dandelion_link_probe
