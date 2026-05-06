-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.MLKEM768
    ( ffiLinked
    , mlkemKeyGen
    , mlkemEncaps
    , mlkemDecaps
    , MLKEMEncapKey(..)
    , MLKEMDecapKey(..)
    , MLKEMCiphertext(..)
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import UmbraVox.Crypto.MLKEM
    ( MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..) )
import qualified UmbraVox.Crypto.MLKEM as Reference

foreign import ccall "mlkem768_link_probe" c_mlkem768_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_mlkem768_link_probe

mlkemKeyGen :: ByteString -> ByteString -> IO (MLKEMEncapKey, MLKEMDecapKey)
mlkemKeyGen d z = do
    _ <- c_mlkem768_link_probe
    pure (Reference.mlkemKeyGen d z)

mlkemEncaps :: MLKEMEncapKey -> ByteString -> IO (MLKEMCiphertext, ByteString)
mlkemEncaps ek m = do
    _ <- c_mlkem768_link_probe
    pure (Reference.mlkemEncaps ek m)

mlkemDecaps :: MLKEMDecapKey -> MLKEMCiphertext -> IO ByteString
mlkemDecaps dk ct = do
    _ <- c_mlkem768_link_probe
    pure (Reference.mlkemDecaps dk ct)
