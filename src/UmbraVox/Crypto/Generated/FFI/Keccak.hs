-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.Keccak where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "keccak" c_keccak :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO Word32

keccak :: ByteString -> ByteString -> ByteString -> ByteString -> IO Word32
keccak message     rate        suffix      output_len  =
  BSU.unsafeUseAsCStringLen message     $ \(message    _ptr, _) ->
  BSU.unsafeUseAsCStringLen rate        $ \(rate       _ptr, _) ->
  BSU.unsafeUseAsCStringLen suffix      $ \(suffix     _ptr, _) ->
  BSU.unsafeUseAsCStringLen output_len  $ \(output_len _ptr, _) ->
  c_keccak message    _ptr rate       _ptr suffix     _ptr output_len _ptr
