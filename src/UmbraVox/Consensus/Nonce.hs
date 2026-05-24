-- SPDX-License-Identifier: Apache-2.0
-- | Epoch nonce evolution
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.Nonce
  ( evolveNonce
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import UmbraVox.Crypto.SHA256 (sha256)

-- | Evolve the epoch nonce by mixing in the new VRF output.
--
-- @evolveNonce oldNonce vrfOutput = SHA-256(oldNonce || vrfOutput)@
--
-- This follows the Ouroboros Praos nonce evolution: the new epoch nonce
-- is the hash of the previous nonce concatenated with the VRF output
-- from the current slot leader.
evolveNonce :: ByteString -> ByteString -> ByteString
evolveNonce oldNonce vrfOutput = sha256 (oldNonce `BS.append` vrfOutput)
