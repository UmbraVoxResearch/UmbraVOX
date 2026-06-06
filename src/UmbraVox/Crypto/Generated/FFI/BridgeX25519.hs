-- SPDX-License-Identifier: Apache-2.0
-- | FFI bridge probe for fiat-crypto X25519 Montgomery ladder (M13.15.5).
--
-- Wires the formally-verified fiat-crypto GF(2^255-19) field arithmetic
-- (csrc/fiat/fiat_25519_64.c) and the X25519 Montgomery ladder bridge
-- (csrc/fiat/bridge_x25519.c) into the Haskell build.
--
-- The bridge is compiled with -DFIAT_VENDORED so the concrete
-- umbravox_x25519 and umbravox_x25519_base symbols are active.  The
-- link probe confirms successful linkage at runtime; callers that require
-- the C-side constant-time path should check ffiLinked before use.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.BridgeX25519
    ( ffiLinked
    , bridgeX25519LinkProbe
    ) where

import Foreign.C.Types (CInt(..))

-- | Link probe exported by csrc/fiat/bridge_x25519.c.
-- Returns 2 when FIAT_VENDORED is defined (fully active),
-- or 1 in stub/fallback mode.
foreign import ccall "bridge_x25519_link_probe"
    c_bridge_x25519_link_probe :: IO CInt

-- | Raw probe result: 2 = fiat-crypto active, 1 = stub mode.
bridgeX25519LinkProbe :: IO Int
bridgeX25519LinkProbe = fromIntegral <$> c_bridge_x25519_link_probe

-- | True when the fiat-crypto X25519 bridge is fully active
-- (FIAT_VENDORED compiled in, umbravox_x25519 and umbravox_x25519_base linked).
ffiLinked :: IO Bool
ffiLinked = (== 2) <$> c_bridge_x25519_link_probe
