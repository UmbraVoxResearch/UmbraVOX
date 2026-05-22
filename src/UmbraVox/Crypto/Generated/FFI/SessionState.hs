-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.SessionState
    ( ffiLinked
    ) where

import Foreign.C.Types (CInt(..))

foreign import ccall "sessionstate_link_probe" c_sessionstate_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_sessionstate_link_probe
