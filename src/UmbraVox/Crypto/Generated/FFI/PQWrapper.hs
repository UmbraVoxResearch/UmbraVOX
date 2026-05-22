-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.PQWrapper
    ( ffiLinked
    , pqEncrypt
    , pqDecrypt
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.PQWrapper as Reference

foreign import ccall "pqwrapper_link_probe" c_pqwrapper_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_pqwrapper_link_probe

pqEncrypt :: ByteString -> ByteString -> IO ByteString
pqEncrypt ek pt = do
    _ <- c_pqwrapper_link_probe
    pure (Reference.pqEncrypt ek pt)

pqDecrypt :: ByteString -> ByteString -> IO (Maybe ByteString)
pqDecrypt dk ct = do
    _ <- c_pqwrapper_link_probe
    pure (Reference.pqDecrypt dk ct)
