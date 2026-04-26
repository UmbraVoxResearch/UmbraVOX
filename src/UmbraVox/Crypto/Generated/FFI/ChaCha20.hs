-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.ChaCha20 where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "chacha20" c_chacha20 :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO Word32

chacha20 :: ByteString -> ByteString -> Word32 -> IO Word32
chacha20 key      nonce    counter  =
  BSU.unsafeUseAsCStringLen key      $ \(key     _ptr, _) ->
  BSU.unsafeUseAsCStringLen nonce    $ \(nonce   _ptr, _) ->
  c_chacha20 key     _ptr nonce   _ptr counter 
