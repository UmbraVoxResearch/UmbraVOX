-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.NetworkProtocol where

import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "networkprotocol" c_networkprotocol :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO Word32

networkprotocol :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> IO Word32
networkprotocol msg_type hs_version hs_public_key hs_capabilities data_sequence data_payload ack_sequence peer_list wire_bytes =
  BSU.unsafeUseAsCStringLen msg_type $ \(msg_type_ptr, _) ->
  BSU.unsafeUseAsCStringLen hs_version $ \(hs_version_ptr, _) ->
  BSU.unsafeUseAsCStringLen hs_public_key $ \(hs_public_key_ptr, _) ->
  BSU.unsafeUseAsCStringLen hs_capabilities $ \(hs_capabilities_ptr, _) ->
  BSU.unsafeUseAsCStringLen data_sequence $ \(data_sequence_ptr, _) ->
  BSU.unsafeUseAsCStringLen data_payload $ \(data_payload_ptr, _) ->
  BSU.unsafeUseAsCStringLen ack_sequence $ \(ack_sequence_ptr, _) ->
  BSU.unsafeUseAsCStringLen peer_list $ \(peer_list_ptr, _) ->
  BSU.unsafeUseAsCStringLen wire_bytes $ \(wire_bytes_ptr, _) ->
  c_networkprotocol msg_type_ptr hs_version_ptr hs_public_key_ptr hs_capabilities_ptr data_sequence_ptr data_payload_ptr ack_sequence_ptr peer_list_ptr wire_bytes_ptr
