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
-- one-second sliding window (POSIX seconds).
data RateLimiter = RateLimiter
    { rlWindowStart :: !(IORef Word64)  -- ^ Window start (POSIX seconds)
    , rlCount       :: !(IORef Int)     -- ^ Messages in current window
    , rlCap         :: !Int             -- ^ Maximum messages per window
    }

-- | Create a new rate limiter with the given per-window cap.
newRateLimiter :: Int -> IO RateLimiter
newRateLimiter cap = do
    ws <- newIORef 0
    cnt <- newIORef 0
    pure RateLimiter
        { rlWindowStart = ws
        , rlCount       = cnt
        , rlCap         = cap
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
checkRate :: RateLimiter -> Word64 -> IO Bool
checkRate rl now = do
    start <- readIORef (rlWindowStart rl)
    count <- readIORef (rlCount rl)
    if now >= start + windowSeconds
        then do
            -- New window
            writeIORef (rlWindowStart rl) now
            writeIORef (rlCount rl) 1
            pure True
        else if count >= rlCap rl
            then pure False  -- Rate exceeded, drop
            else do
                writeIORef (rlCount rl) (count + 1)
                pure True

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
