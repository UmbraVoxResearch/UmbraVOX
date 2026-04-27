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
-- side-channels. Returns False for different-length inputs without
-- leaking which bytes differ.
constantEq :: ByteString -> ByteString -> Bool
constantEq a b =
    let !lenA = BS.length a
        !lenB = BS.length b
        !lenMatch = if lenA == lenB then 0 else 1 :: Word8
        !acc = foldl' (\v (x, y) -> v .|. (x `xor` y)) lenMatch (BS.zip a b)
    in acc == 0
