-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.SessionState where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "sessionstate" c_sessionstate :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO Word32

sessionstate :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> IO Word32
sessionstate dh_send_secret dh_send_public dh_recv_present dh_recv_public root_key send_chain recv_chain send_n recv_n prev_chain_n skip_seq nonce_counter skipped_keys mac_key blob =
  BSU.unsafeUseAsCStringLen dh_send_secret $ \(dh_send_secret_ptr, _) ->
  BSU.unsafeUseAsCStringLen dh_send_public $ \(dh_send_public_ptr, _) ->
  BSU.unsafeUseAsCStringLen dh_recv_present $ \(dh_recv_present_ptr, _) ->
  BSU.unsafeUseAsCStringLen dh_recv_public $ \(dh_recv_public_ptr, _) ->
  BSU.unsafeUseAsCStringLen root_key $ \(root_key_ptr, _) ->
  BSU.unsafeUseAsCStringLen send_chain $ \(send_chain_ptr, _) ->
  BSU.unsafeUseAsCStringLen recv_chain $ \(recv_chain_ptr, _) ->
  BSU.unsafeUseAsCStringLen send_n $ \(send_n_ptr, _) ->
  BSU.unsafeUseAsCStringLen recv_n $ \(recv_n_ptr, _) ->
  BSU.unsafeUseAsCStringLen prev_chain_n $ \(prev_chain_n_ptr, _) ->
  BSU.unsafeUseAsCStringLen skip_seq $ \(skip_seq_ptr, _) ->
  BSU.unsafeUseAsCStringLen nonce_counter $ \(nonce_counter_ptr, _) ->
  BSU.unsafeUseAsCStringLen skipped_keys $ \(skipped_keys_ptr, _) ->
  BSU.unsafeUseAsCStringLen mac_key $ \(mac_key_ptr, _) ->
  BSU.unsafeUseAsCStringLen blob $ \(blob_ptr, _) ->
  c_sessionstate dh_send_secret_ptr dh_send_public_ptr dh_recv_present_ptr dh_recv_public_ptr root_key_ptr send_chain_ptr recv_chain_ptr send_n_ptr recv_n_ptr prev_chain_n_ptr skip_seq_ptr nonce_counter_ptr skipped_keys_ptr mac_key_ptr blob_ptr
