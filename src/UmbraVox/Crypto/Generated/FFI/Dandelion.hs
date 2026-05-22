-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.Dandelion where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "dandelion" c_dandelion :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO Word32

dandelion :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> IO Word32
dandelion csprng_byte fluff_prob current_mode epoch_start epoch_len now =
  BSU.unsafeUseAsCStringLen csprng_byte $ \(csprng_byte_ptr, _) ->
  BSU.unsafeUseAsCStringLen fluff_prob $ \(fluff_prob_ptr, _) ->
  BSU.unsafeUseAsCStringLen current_mode $ \(current_mode_ptr, _) ->
  BSU.unsafeUseAsCStringLen epoch_start $ \(epoch_start_ptr, _) ->
  BSU.unsafeUseAsCStringLen epoch_len $ \(epoch_len_ptr, _) ->
  BSU.unsafeUseAsCStringLen now $ \(now_ptr, _) ->
  c_dandelion csprng_byte_ptr fluff_prob_ptr current_mode_ptr epoch_start_ptr epoch_len_ptr now_ptr
