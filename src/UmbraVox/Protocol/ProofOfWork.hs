-- SPDX-License-Identifier: Apache-2.0
-- | Proof-of-work challenge-response for DoS mitigation (M23.1.1h).
--
-- Peers may require a PoW solution before accepting messages from
-- unknown or rate-limited sources.  The difficulty is 16 leading zero
-- bits in SHA-256(challenge || nonce), requiring ~65K hashes on average
-- (<100ms on modern hardware).
--
-- See: doc/ENCRYPTED-ENVELOPE-DESIGN.md (DoS mitigations)
module UmbraVox.Protocol.ProofOfWork
    ( -- * Challenge generation
      generateChallenge
      -- * Solving
    , solveChallenge
      -- * Verification
    , verifyChallenge
      -- * Constants
    , challengeSize
    , nonceSize
    , difficultyBits
    ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64)

import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.SHA256 (sha256)
import UmbraVox.Protocol.Encoding (putWord64BE)

-- | Challenge size in bytes.
challengeSize :: Int
challengeSize = 32

-- | Nonce size in bytes.
nonceSize :: Int
nonceSize = 8

-- | Number of leading zero bits required in the hash.
-- 16 bits = ~65K expected SHA-256 hashes.
difficultyBits :: Int
difficultyBits = 16

-- | Generate a random 32-byte challenge.
generateChallenge :: IO ByteString
generateChallenge = randomBytes challengeSize

-- | Solve a challenge by finding a nonce such that
-- @SHA-256(challenge || nonce)@ has 'difficultyBits' leading zero bits.
--
-- Returns an 8-byte nonce (big-endian encoding of the solution counter).
solveChallenge :: ByteString -> ByteString
solveChallenge challenge = go 0
  where
    go :: Word64 -> ByteString
    go !n =
        let !nonceBS = putWord64BE (fromIntegral n)
            !hash    = sha256 (challenge <> nonceBS)
        in if hasLeadingZeroBits difficultyBits hash
               then nonceBS
               else go (n + 1)

-- | Verify that @SHA-256(challenge || nonce)@ has 'difficultyBits' leading
-- zero bits.
--
-- Returns 'False' if the challenge is not exactly 'challengeSize' bytes or
-- the nonce is not exactly 'nonceSize' bytes.
verifyChallenge :: ByteString -> ByteString -> Bool
verifyChallenge challenge nonce
    | BS.length challenge /= challengeSize = False
    | BS.length nonce /= nonceSize = False
    | otherwise =
        let !hash = sha256 (challenge <> nonce)
        in hasLeadingZeroBits difficultyBits hash

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Check whether a hash has at least @n@ leading zero bits.
--
-- Checks full bytes first (8 bits at a time), then the remaining
-- fractional byte with a mask.
hasLeadingZeroBits :: Int -> ByteString -> Bool
hasLeadingZeroBits n hash
    | n <= 0           = True
    | BS.length hash < fullBytes + extra = False
    | otherwise =
        let !fullOk = all (\i -> BS.index hash i == 0) [0 .. fullBytes - 1]
        in if extra == 0
               then fullOk
               else fullOk && (BS.index hash fullBytes `shiftR` (8 - extra)) == 0
  where
    !fullBytes = n `div` 8
    !extra     = n .&. 7
