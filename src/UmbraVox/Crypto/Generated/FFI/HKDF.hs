-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.HKDF where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "hkdf" c_hkdf :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO Word32

hkdf :: ByteString -> ByteString -> ByteString -> ByteString -> IO Word32
hkdf salt    ikm     info    length  =
  BSU.unsafeUseAsCStringLen salt    $ \(salt   _ptr, _) ->
  BSU.unsafeUseAsCStringLen ikm     $ \(ikm    _ptr, _) ->
  BSU.unsafeUseAsCStringLen info    $ \(info   _ptr, _) ->
  BSU.unsafeUseAsCStringLen length  $ \(length _ptr, _) ->
  c_hkdf salt   _ptr ikm    _ptr info   _ptr length _ptr
