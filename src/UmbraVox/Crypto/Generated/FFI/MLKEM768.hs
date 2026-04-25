-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.MLKEM768 where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "mlkem768" c_mlkem768 :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO Word32

mlkem768 :: ByteString -> ByteString -> ByteString -> IO Word32
mlkem768 seed_d  seed_z  message  =
  BSU.unsafeUseAsCStringLen seed_d  $ \(seed_d _ptr, _) ->
  BSU.unsafeUseAsCStringLen seed_z  $ \(seed_z _ptr, _) ->
  BSU.unsafeUseAsCStringLen message  $ \(message _ptr, _) ->
  c_mlkem768 seed_d _ptr seed_z _ptr message _ptr
