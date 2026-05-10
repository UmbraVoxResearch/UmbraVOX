-- SPDX-License-Identifier: Apache-2.0
-- | WARNING: Pure Haskell crypto is NOT constant-time.
-- All timing-sensitive operations (AES S-box, scalar multiplication,
-- polynomial arithmetic) use variable-time Haskell code.
-- Production deployments MUST use FFI to constant-time C implementations.
-- Enable the 'pure-haskell-crypto' flag only for testing/development.
module UmbraVox.Crypto.Warning
    ( cryptoTimingWarning
    ) where

-- | Runtime string surfacing the compile-time safety constraint.
-- Embed this in log output or startup checks so operators see it clearly.
cryptoTimingWarning :: String
cryptoTimingWarning = "WARNING: Pure Haskell crypto - NOT constant-time"
