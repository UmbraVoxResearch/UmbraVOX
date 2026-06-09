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
    , generateBoundChallenge
      -- * Solving
    , solveChallenge
    , solveBoundChallenge
      -- * Verification
    , verifyChallenge
    , verifyBoundChallenge
      -- * Constants
    , challengeSize
    , nonceSize
    , difficultyBits
    , challengeExpirySeconds
    , maxSolverIterations
    ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64)

import UmbraVox.Crypto.Random (randomBytes)
import qualified UmbraVox.Crypto.Generated.FFI.SHA256 as SHA256FFI
import qualified UmbraVox.Crypto.Generated.FFI.HMAC as HMACFFI
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

-- | M27.6.8: Challenge expiry timeout in seconds.
-- Challenges older than this are rejected to prevent pre-computation attacks.
challengeExpirySeconds :: Word64
challengeExpirySeconds = 30

-- | Maximum iterations for the PoW solver (M35A DoS mitigation).
--
-- At difficulty 16 (~65K expected hashes), this allows 16× margin before
-- giving up.  An unsolvable or deliberately broken challenge (e.g. all bits
-- set) cannot cause the solver to loop indefinitely.  Callers receive
-- 'Nothing' and should disconnect or report an error.
--
-- Finding    M35A.PoW — Prior solver looped unconditionally over all 2^64
--            Word64 values with no bound.  A crafted challenge with no
--            feasible solution would cause the client to spin indefinitely.
-- Vulnerability: Peer sends a challenge that can never satisfy the PoW
--            predicate (e.g. requires 64 leading zero bits); the solver
--            thread hangs, consuming CPU indefinitely — a DoS against the
--            connecting client.
-- Fix:       Abort after 'maxSolverIterations' and return 'Nothing'.
-- Verified:  Both 'solveChallenge' and 'solveBoundChallenge' now return
--            'IO (Maybe ByteString)'; callers treat 'Nothing' as a
--            connection error.
maxSolverIterations :: Word64
maxSolverIterations = 1048576  -- 2^20 ≈ 1M iterations, 16× margin at difficulty 16

-- | Generate a random 32-byte challenge.
generateChallenge :: IO ByteString
generateChallenge = randomBytes challengeSize

-- | M27.6.8: Generate a connection-bound challenge.
--
-- The challenge incorporates a server nonce (binding the challenge to
-- this specific connection) and a timestamp (for expiry checking).
-- Layout: HMAC-SHA-256(serverNonce, randomBytes || BE64(timestamp))
--
-- The server retains @serverNonce@ and @timestamp@ to verify solutions.
--
-- Adaptive difficulty: the current implementation uses a fixed
-- 'difficultyBits' (16 leading zero bits).  A future enhancement will
-- adjust difficulty dynamically based on server load / connection rate,
-- increasing bits under heavy traffic and decreasing during idle periods.
-- The solver already handles arbitrary difficulty via 'hasLeadingZeroBits',
-- so the only change needed is a difficulty parameter in the challenge
-- message format.
generateBoundChallenge :: ByteString  -- ^ 32-byte server nonce (unique per connection)
                       -> Word64      -- ^ current wall-clock timestamp (seconds)
                       -> IO ByteString
generateBoundChallenge serverNonce timestamp = do
    entropy <- randomBytes challengeSize
    HMACFFI.hmacSHA256 serverNonce (entropy <> putWord64BE timestamp)

-- | Solve a challenge by finding a nonce such that
-- @SHA-256(challenge || nonce)@ has 'difficultyBits' leading zero bits.
--
-- Returns @Just nonce@ (8-byte big-endian counter) on success, or
-- @Nothing@ if no solution is found within 'maxSolverIterations'.
solveChallenge :: ByteString -> IO (Maybe ByteString)
solveChallenge challenge = go 0
  where
    go :: Word64 -> IO (Maybe ByteString)
    go !n
        | n >= maxSolverIterations = pure Nothing
        | otherwise = do
            let !nonceBS = putWord64BE (fromIntegral n)
            !hash <- SHA256FFI.sha256 (challenge <> nonceBS)
            if hasLeadingZeroBits difficultyBits hash
                then pure (Just nonceBS)
                else go (n + 1)

-- | Verify that @SHA-256(challenge || nonce)@ has 'difficultyBits' leading
-- zero bits.
--
-- Returns 'False' if the challenge is not exactly 'challengeSize' bytes or
-- the nonce is not exactly 'nonceSize' bytes.
verifyChallenge :: ByteString -> ByteString -> IO Bool
verifyChallenge challenge nonce
    | BS.length challenge /= challengeSize = pure False
    | BS.length nonce /= nonceSize = pure False
    | otherwise = do
        !hash <- SHA256FFI.sha256 (challenge <> nonce)
        pure (hasLeadingZeroBits difficultyBits hash)

-- | M27.6.8: Solve a connection-bound challenge.
--
-- Identical to 'solveChallenge' but takes an explicit server nonce that is
-- prepended to the hash input, binding the solution to this connection.
-- Returns @Nothing@ if no solution found within 'maxSolverIterations'.
solveBoundChallenge :: ByteString  -- ^ challenge (from 'generateBoundChallenge')
                    -> ByteString  -- ^ server nonce
                    -> IO (Maybe ByteString)  -- ^ 8-byte solution nonce, or Nothing
solveBoundChallenge challenge serverNonce = go 0
  where
    go :: Word64 -> IO (Maybe ByteString)
    go !n
        | n >= maxSolverIterations = pure Nothing
        | otherwise = do
            let !nonceBS = putWord64BE (fromIntegral n)
            !hash <- SHA256FFI.sha256 (serverNonce <> challenge <> nonceBS)
            if hasLeadingZeroBits difficultyBits hash
                then pure (Just nonceBS)
                else go (n + 1)

-- | M27.6.8: Verify a connection-bound PoW solution with expiry.
--
-- Returns 'True' only if:
-- 1. The PoW hash has the required leading zero bits
-- 2. The challenge has not expired (timestamp within 'challengeExpirySeconds')
-- 3. The server nonce matches (connection binding)
verifyBoundChallenge :: ByteString  -- ^ challenge
                     -> ByteString  -- ^ server nonce
                     -> ByteString  -- ^ solution nonce (8 bytes)
                     -> Word64      -- ^ challenge creation timestamp (seconds)
                     -> Word64      -- ^ current timestamp (seconds)
                     -> IO Bool
verifyBoundChallenge challenge serverNonce nonce createdAt now
    | BS.length challenge /= challengeSize = pure False
    | BS.length nonce /= nonceSize = pure False
    | now > createdAt && (now - createdAt) > challengeExpirySeconds = pure False
    | otherwise = do
        !hash <- SHA256FFI.sha256 (serverNonce <> challenge <> nonce)
        pure (hasLeadingZeroBits difficultyBits hash)

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
