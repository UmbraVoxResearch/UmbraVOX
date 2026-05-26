-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
-- | Constant-time comparison for ByteStrings.
--
-- When compiled with C FFI support (the default), delegates to a C
-- implementation that uses a volatile accumulator to prevent the
-- compiler from short-circuiting.  Falls back to a pure Haskell XOR
-- fold when @PURE_HASKELL_CRYPTO@ or @WASM_BUILD@ is defined.
module UmbraVox.Crypto.ConstantTime
    ( constantEq
    ) where

import Data.ByteString (ByteString)

#if defined(PURE_HASKELL_CRYPTO) || defined(WASM_BUILD)

import Data.Bits (xor, (.|.))
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word8)

-- | Constant-time comparison for ByteStrings (pure Haskell XOR fold).
-- Compares all bytes regardless of early mismatch to avoid timing
-- side-channels.  When the inputs differ in length the shorter one is
-- zero-padded to max(lenA,lenB) so that iteration count does not leak
-- length information.  Returns False whenever the inputs differ in
-- length or content.
constantEq :: ByteString -> ByteString -> Bool
constantEq a b =
    let !lenA     = BS.length a
        !lenB     = BS.length b
        !maxLen   = max lenA lenB
        -- Pad both sides to maxLen with 0x00 bytes.
        !a'       = a <> BS.replicate (maxLen - lenA) 0
        !b'       = b <> BS.replicate (maxLen - lenB) 0
        -- Seed the accumulator with 1 when lengths differ so that
        -- equal-content but different-length inputs still return False.
        !lenMatch = if lenA == lenB then 0 else 1 :: Word8
        !acc      = foldl' (\v (x, y) -> v .|. (x `xor` y)) lenMatch (BS.zip a' b')
    in acc == 0

#else

import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (Ptr, castPtr)
import Data.Word (Word8)
import System.IO.Unsafe (unsafeDupablePerformIO)

foreign import ccall unsafe "constant_time_eq"
    c_constant_time_eq :: Ptr Word8 -> Ptr Word8 -> CSize -> CInt

-- | Constant-time comparison for ByteStrings (C FFI, volatile accumulator).
-- M27.6.6: delegates to a C implementation that is guaranteed not to
-- short-circuit on early mismatch.
--
-- Finding    — Length mismatch caused an early return, creating a timing
--              oracle: callers could distinguish "wrong length" from "wrong
--              content" by measuring execution time.
-- Vulnerability: Timing side-channel that leaks whether two ByteStrings
--              share the same length, useful for oracle attacks on MACs or
--              password hashes stored with their length.
-- Fix:       Pad both inputs to max(lenA, lenB) with zero bytes and XOR a
--              length-mismatch sentinel (1) into the C accumulator result so
--              that the C path always runs over the full max-length buffer.
--              Equal-length inputs follow the existing fast path unchanged.
-- Verified:  The C function is always invoked with equal-length buffers;
--              the sentinel ensures different-length inputs still return False
--              even when their padded byte contents happen to be identical.
constantEq :: ByteString -> ByteString -> Bool
constantEq a b = unsafeDupablePerformIO $
    let !lenA    = BS.length a
        !lenB    = BS.length b
        !maxLen  = max lenA lenB
        -- Pad shorter side with zeros so C always sees equal-length buffers.
        !a'      = if lenA == maxLen then a
                   else a <> BS.replicate (maxLen - lenA) 0
        !b'      = if lenB == maxLen then b
                   else b <> BS.replicate (maxLen - lenB) 0
        -- Sentinel: 1 when lengths differ, 0 otherwise.  XOR'd into the C
        -- result so a length mismatch always yields a non-zero accumulator.
        !lenSentinel = if lenA == lenB then 0 else 1 :: CInt
    in if maxLen == 0
       then return (lenSentinel == 0)   -- both empty → equal
       else BSU.unsafeUseAsCStringLen a' $ \(ptrA, len) ->
            BSU.unsafeUseAsCStringLen b' $ \(ptrB, _)  ->
                let r = c_constant_time_eq (castPtr ptrA) (castPtr ptrB)
                                           (fromIntegral len)
                in return ((r `xor` lenSentinel) == 0)

#endif
