-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.SHA512 where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "sha512" c_sha512 :: Ptr Word8 -> IO Word32

sha512 :: ByteString -> IO Word32
sha512 message  =
  BSU.unsafeUseAsCStringLen message  $ \(message _ptr, _) ->
  c_sha512 message _ptr
