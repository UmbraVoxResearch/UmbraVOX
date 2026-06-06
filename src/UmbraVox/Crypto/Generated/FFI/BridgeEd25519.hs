-- SPDX-License-Identifier: Apache-2.0
-- | FFI bridge probe for fiat-crypto Ed25519 field operations (M13.15.4).
--
-- Wires the formally-verified fiat-crypto GF(2^255-19) field arithmetic
-- (csrc/fiat/fiat_25519_64.c) and the Ed25519 group-law bridge
-- (csrc/fiat/bridge_ed25519.c) into the Haskell build.
--
-- The bridge is compiled with -DFIAT_VENDORED so all concrete
-- umbravox_ed_* and umbravox_fe_* symbols are active.  The link probe
-- confirms successful linkage at runtime; callers that require the C-side
-- constant-time path should check ffiLinked before invoking operations.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.BridgeEd25519
    ( ffiLinked
    , bridgeEd25519LinkProbe
    ) where

import Foreign.C.Types (CInt(..))

-- | Link probe exported by csrc/fiat/bridge_ed25519.c.
-- Returns 2 when FIAT_VENDORED is defined (fully active),
-- or 1 in stub/fallback mode.
foreign import ccall "bridge_ed25519_link_probe"
    c_bridge_ed25519_link_probe :: IO CInt

-- | Raw probe result: 2 = fiat-crypto active, 1 = stub mode.
bridgeEd25519LinkProbe :: IO Int
bridgeEd25519LinkProbe = fromIntegral <$> c_bridge_ed25519_link_probe

-- | True when the fiat-crypto Ed25519 bridge is fully active
-- (FIAT_VENDORED compiled in, all umbravox_ed_* symbols linked).
ffiLinked :: IO Bool
ffiLinked = (== 2) <$> c_bridge_ed25519_link_probe
