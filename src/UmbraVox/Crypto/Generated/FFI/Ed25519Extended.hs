-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.Ed25519Extended where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "ed25519extended" c_ed25519extended :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO Word32

ed25519extended :: ByteString -> ByteString -> ByteString -> ByteString -> IO Word32
ed25519extended scalar point point2 data =
  BSU.unsafeUseAsCStringLen scalar $ \(scalar_ptr, _) ->
  BSU.unsafeUseAsCStringLen point $ \(point_ptr, _) ->
  BSU.unsafeUseAsCStringLen point2 $ \(point2_ptr, _) ->
  BSU.unsafeUseAsCStringLen data $ \(data_ptr, _) ->
  c_ed25519extended scalar_ptr point_ptr point2_ptr data_ptr
