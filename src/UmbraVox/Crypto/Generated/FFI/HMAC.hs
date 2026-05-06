-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.HMAC where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "hmac" c_hmac :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO Word32

hmac :: ByteString -> ByteString -> ByteString -> ByteString -> IO Word32
hmac key         message     hash_fn     block_size  =
  BSU.unsafeUseAsCStringLen key         $ \(key        _ptr, _) ->
  BSU.unsafeUseAsCStringLen message     $ \(message    _ptr, _) ->
  BSU.unsafeUseAsCStringLen hash_fn     $ \(hash_fn    _ptr, _) ->
  BSU.unsafeUseAsCStringLen block_size  $ \(block_size _ptr, _) ->
  c_hmac key        _ptr message    _ptr hash_fn    _ptr block_size _ptr
