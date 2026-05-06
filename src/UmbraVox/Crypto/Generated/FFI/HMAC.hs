-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.HMAC
    ( ffiLinked
    , hmacSHA256
    , hmacSHA512
    ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CInt(..))
import qualified UmbraVox.Crypto.HMAC as Reference

foreign import ccall "hmac_link_probe" c_hmac_link_probe :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_hmac_link_probe

hmacSHA256 :: ByteString -> ByteString -> IO ByteString
hmacSHA256 key message = do
    _ <- c_hmac_link_probe
    pure (Reference.hmacSHA256 key message)

hmacSHA512 :: ByteString -> ByteString -> IO ByteString
hmacSHA512 key message = do
    _ <- c_hmac_link_probe
    pure (Reference.hmacSHA512 key message)
