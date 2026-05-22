-- Dandelion Specification — Dandelion++ Routing Decision (M21.3.4)
--
-- This .spec file encodes the Dandelion++ routing decision logic for
-- UmbraVOX P2P message propagation.
--
-- Dandelion++ provides sender anonymity by splitting message propagation
-- into two phases:
--
--   1. Stem phase: message forwarded along a single relay chain
--   2. Fluff phase: message broadcast to all peers (standard gossip)
--
-- Each relay independently decides to transition from stem to fluff with
-- probability p (default 0.1 per hop).  This prevents linking messages
-- to their origin IP address.
--
-- Epoch rotation selects a new stem peer every epoch_len seconds,
-- preventing long-lived stem paths from becoming traffic-analysis targets.
--
-- All routing decisions use constant-time comparisons to prevent timing
-- leaks of the stem/fluff decision.
--
-- References:
--   - Fanti et al., "Dandelion++: Lightweight Cryptocurrency Networking
--     with Formal Anonymity Guarantees" (ACM SIGMETRICS 2018)
--   - UmbraVox.Network.Dandelion module

algorithm Dandelion {

  params {
    -- route_decision inputs
    csprng_byte     : UInt8        -- Single random byte from CSPRNG
    fluff_prob      : Float64      -- Probability of stem→fluff transition [0,1]
    current_mode    : UInt8        -- 0x00 = Stem, 0x01 = Fluff

    -- epoch_check inputs
    epoch_start     : UInt64       -- Epoch start time (POSIX seconds)
    epoch_len       : UInt32       -- Epoch length in seconds
    now             : UInt64       -- Current time (POSIX seconds)
  }

  constants {
    -- Routing modes
    MODE_STEM  = 0x00
    MODE_FLUFF = 0x01

    -- Route decision results
    DECISION_STEM  = 0x00   -- Continue stem forwarding
    DECISION_FLUFF = 0x01   -- Transition to fluff broadcast

    -- Default epoch length (seconds)
    DEFAULT_EPOCH_LEN = 600

    -- Default fluff probability
    DEFAULT_FLUFF_PROB = 0.1

    -- Maximum byte value for threshold comparison
    BYTE_MAX = 256
  }

  steps {
    -- ==================================================================
    -- Route Decision — given CSPRNG byte + fluff probability → stem or fluff
    -- ==================================================================

    -- Step 1: Clamp fluff probability to [0, 1]
    -- Ensures well-defined behavior for out-of-range inputs.
    clamped_prob = CLAMP(fluff_prob, 0.0, 1.0)

    -- Step 2: Compute threshold for stem→fluff transition
    -- threshold = floor(fluff_prob * 256)
    -- A random byte < threshold means "transition to fluff"
    -- P(byte < threshold) = threshold / 256 ≈ fluff_prob
    threshold = FLOOR(clamped_prob * BYTE_MAX)

    -- Step 3: Compare CSPRNG byte against threshold (constant-time)
    -- Use arithmetic comparison rather than branching to avoid
    -- timing leak of the routing decision.
    --
    -- fluff_flag = 1 if csprng_byte < threshold, else 0
    -- This is computed as: (threshold - 1 - csprng_byte) >> 63
    -- inverted, or equivalently via constant-time less-than.
    fluff_flag = constantTimeLT(csprng_byte, threshold)

    -- Step 4: Determine route decision based on current mode
    -- If already in fluff mode, always fluff (no return to stem).
    -- If in stem mode, use the probabilistic fluff_flag.
    --
    -- decision = fluff_flag | current_mode
    -- (constant-time OR: if either is fluff, result is fluff)
    already_fluff = constantTimeEq(current_mode, MODE_FLUFF)
    decision = already_fluff | fluff_flag

    -- Step 5: Compute new mode
    -- Once transitioned to fluff, remain in fluff until epoch rotation.
    -- new_mode = MODE_FLUFF if decision == 1, else MODE_STEM
    new_mode = constantTimeSelect(decision, MODE_FLUFF, MODE_STEM)

    -- ==================================================================
    -- Epoch Check — compare timestamps for epoch expiry
    -- ==================================================================

    -- Step 6: Compute epoch deadline
    -- deadline = epoch_start + epoch_len
    -- Handle the initial case where epoch_start == 0 (first epoch).
    epoch_deadline = epoch_start + epoch_len

    -- Step 7: Check if epoch has expired (constant-time)
    -- expired = 1 if (epoch_start == 0) || (now >= epoch_deadline)
    -- First epoch (start == 0) always triggers rotation.
    first_epoch = constantTimeEqZero(epoch_start)
    time_expired = constantTimeGTE(now, epoch_deadline)
    expired = first_epoch | time_expired

    -- Step 8: Compute new epoch start
    -- If expired, new_epoch_start = now; otherwise unchanged.
    new_epoch_start = constantTimeSelect(expired, now, epoch_start)

    -- Step 9: Compute post-rotation mode
    -- On epoch expiry, reset to stem mode for new stem peer selection.
    -- post_rotation_mode = MODE_STEM if expired, else current_mode
    post_rotation_mode = constantTimeSelect(expired, MODE_STEM, current_mode)
  }
}
