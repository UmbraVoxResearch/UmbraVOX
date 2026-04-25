-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.SHA256 where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "sha256" c_sha256 :: Ptr Word8 -> IO Word32

sha256 :: ByteString -> IO Word32
sha256 message  =
  BSU.unsafeUseAsCStringLen message  $ \(message _ptr, _) ->
  c_sha256 message _ptr
