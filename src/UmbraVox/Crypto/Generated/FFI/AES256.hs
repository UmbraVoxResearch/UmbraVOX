-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.AES256 where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "aes256" c_aes256 :: Ptr Word8 -> Ptr Word8 -> IO Word32

aes256 :: ByteString -> ByteString -> IO Word32
aes256 key  block  =
  BSU.unsafeUseAsCStringLen key  $ \(key _ptr, _) ->
  BSU.unsafeUseAsCStringLen block  $ \(block _ptr, _) ->
  c_aes256 key _ptr block _ptr
