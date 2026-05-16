{-# LANGUAGE CPP #-}
-- SPDX-License-Identifier: Apache-2.0
-- | M17.5.4 — Detect active swap on Linux and warn when ephemeral mode
-- is enabled.  Sensitive key material held in memory may be paged out to
-- unencrypted swap partitions, defeating the purpose of ephemeral mode.
module UmbraVox.App.SwapCheck
    ( swapIsActive
    ) where

import Control.Exception (SomeException, catch)

-- | Returns 'True' when the host has active, potentially unencrypted swap.
--
-- On Linux this reads @\/proc\/swaps@ and checks whether any data lines
-- exist beyond the header.  On all other platforms this conservatively
-- returns 'False' (swap detection is not implemented).
swapIsActive :: IO Bool
#if defined(linux_HOST_OS)
swapIsActive = checkProcSwaps `catch` \(_ :: SomeException) -> pure False

checkProcSwaps :: IO Bool
checkProcSwaps = do
    contents <- readFile "/proc/swaps"
    -- /proc/swaps always has a header line; any additional line means
    -- at least one swap device is active.
    let ls = lines contents
    -- Force the spine so the file handle is closed promptly.
    length ls `seq` pure (length ls > 1)
#else
swapIsActive = pure False
#endif
