-- SPDX-License-Identifier: Apache-2.0
-- | Per-peer message rate limiting for DoS mitigation (M23.1.1h).
--
-- Provides a simple sliding-window rate limiter backed by an 'IORef'
-- containing a timestamp and message count.  When the count exceeds
-- the configured cap within the window, subsequent messages are
-- dropped until the window slides forward.
--
-- See: doc/ENCRYPTED-ENVELOPE-DESIGN.md (DoS mitigations)
module UmbraVox.Network.RateLimit
    ( -- * Rate limiter state
      RateLimiter
    , newRateLimiter
      -- * Operations
    , checkRate
      -- * Constants
    , defaultMessageCap
    , defaultRelayCap
    , windowSeconds
    ) where

import Data.IORef
import Data.Word (Word64)

-- | Mutable rate limiter state for a single peer session.
--
-- Tracks the count of messages received and the start of the current
-- one-second sliding window (POSIX seconds).  The window-start and
-- message-count are stored as a single pair in one 'IORef' so that
-- 'checkRate' can update both atomically via 'atomicModifyIORef'',
-- eliminating the TOCTOU race that separate IORefs would introduce.
data RateLimiter = RateLimiter
    { rlState :: !(IORef (Word64, Int))  -- ^ (window start, message count)
    , rlCap   :: !Int                    -- ^ Maximum messages per window
    }

-- | Create a new rate limiter with the given per-window cap.
newRateLimiter :: Int -> IO RateLimiter
newRateLimiter cap = do
    st <- newIORef (0, 0)
    pure RateLimiter
        { rlState = st
        , rlCap   = cap
        }

-- | Check whether a message should be accepted under the rate limit.
--
-- @checkRate limiter now@ returns 'True' if the message is accepted,
-- 'False' if it should be dropped.  @now@ is the current POSIX time
-- in seconds.
--
-- If the current window has expired (more than 'windowSeconds' since
-- the window start), the window and counter are reset.  Otherwise the
-- counter is incremented and compared against the cap.
--
-- The read-modify-write is performed atomically via 'atomicModifyIORef''
-- to avoid TOCTOU races when multiple threads share a limiter.
checkRate :: RateLimiter -> Word64 -> IO Bool
checkRate rl now =
    atomicModifyIORef' (rlState rl) $ \(start, count) ->
        if now >= start + windowSeconds
            then -- New window: reset and accept
                 ((now, 1), True)
            else if count >= rlCap rl
                then -- Rate exceeded: drop without incrementing
                     ((start, count), False)
                else -- Accept and increment
                     ((start, count + 1), True)

-- | Sliding window length in seconds.
windowSeconds :: Word64
windowSeconds = 1

-- | Default per-peer message rate cap (messages per second).
-- If a single peer sends more than 100 messages/sec, subsequent
-- messages are dropped for the remainder of the window.
defaultMessageCap :: Int
defaultMessageCap = 100

-- | Default per-session relay forwarding cap (forwards per second).
-- Prevents a single session from dominating relay bandwidth.
defaultRelayCap :: Int
defaultRelayCap = 50
