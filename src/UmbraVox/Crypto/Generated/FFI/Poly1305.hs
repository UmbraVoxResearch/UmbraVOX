-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.Poly1305 where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "poly1305" c_poly1305 :: Ptr Word8 -> Ptr Word8 -> IO Word32

poly1305 :: ByteString -> ByteString -> IO Word32
poly1305 key  message  =
  BSU.unsafeUseAsCStringLen key  $ \(key _ptr, _) ->
  BSU.unsafeUseAsCStringLen message  $ \(message _ptr, _) ->
  c_poly1305 key _ptr message _ptr
