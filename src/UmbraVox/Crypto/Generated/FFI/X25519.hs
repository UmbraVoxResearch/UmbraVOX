-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.X25519 where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "x25519" c_x25519 :: Ptr Word8 -> Ptr Word8 -> IO Word32

x25519 :: ByteString -> ByteString -> IO Word32
x25519 scalar        u_coordinate  =
  BSU.unsafeUseAsCStringLen scalar        $ \(scalar       _ptr, _) ->
  BSU.unsafeUseAsCStringLen u_coordinate  $ \(u_coordinate _ptr, _) ->
  c_x25519 scalar       _ptr u_coordinate _ptr
