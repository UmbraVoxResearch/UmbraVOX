-- SPDX-License-Identifier: Apache-2.0
-- | Constant-time comparison for ByteStrings.
--
-- Uses XOR accumulation over all bytes to avoid timing side-channels.
-- Shared by GCM, ML-KEM, Noise, StealthAddress, and any other module
-- that must compare secrets without leaking information through timing.
module UmbraVox.Crypto.ConstantTime
    ( constantEq
    ) where

import Data.Bits (xor, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word8)

-- | Constant-time comparison for ByteStrings (XOR fold).
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
