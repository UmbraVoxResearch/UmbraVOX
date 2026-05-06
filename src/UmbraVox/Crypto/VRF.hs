-- | ECVRF-ED25519-SHA512 (RFC 9381)
--
-- See: doc/spec/crypto.md
module UmbraVox.Crypto.VRF
  ( vrfProve
  , vrfVerify
  ) where

import Data.ByteString (ByteString)

-- | Generate a VRF proof for the given secret key and input.
vrfProve :: ByteString -> ByteString -> ByteString
vrfProve = error "not implemented"

-- | Verify a VRF proof and return the output if valid.
vrfVerify :: ByteString -> ByteString -> ByteString -> Maybe ByteString
vrfVerify = error "not implemented"
