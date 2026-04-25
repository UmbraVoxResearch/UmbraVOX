-- | Safety number and fingerprint display for key verification.
--
-- Instead of a non-scannable QR code, we provide two verification methods:
--   1. Hex fingerprint — easy to read aloud over a phone call
--   2. Safety number   — 60 digits (12 groups of 5), like Signal
module UmbraVox.Protocol.QRCode
    ( generateSafetyNumber
    , renderSafetyNumber
    , renderFingerprint
    ) where

import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Word (Word8)
import UmbraVox.Crypto.SHA256 (sha256)

-- | Generate a 60-digit safety number from two public keys.
--
-- The keys are sorted (lexicographic on raw bytes) before hashing so
-- that both parties derive the same number regardless of role.
-- SHA-256 of the sorted concatenation is used to extract 60 decimal
-- digits (each from 0-9), displayed as 12 groups of 5.
generateSafetyNumber :: ByteString -> ByteString -> String
generateSafetyNumber ours theirs =
    let (lo, hi) = if ours <= theirs then (ours, theirs) else (theirs, ours)
        hash     = sha256 (lo <> hi)
        -- We need 60 digits. Each hash byte gives one digit (mod 10).
        -- 32 bytes per hash; hash twice to get 64 bytes, take 60.
        hash2    = sha256 hash
        allBytes = BS.unpack hash <> BS.unpack hash2  -- 64 bytes
        digits   = take 60 (map byteToDigit allBytes)
    in digits
  where
    byteToDigit :: Word8 -> Char
    byteToDigit w = toEnum (fromEnum '0' + fromIntegral w `mod` 10)

-- | Format a 60-digit safety number as 6 rows of 2 groups (5 digits each).
renderSafetyNumber :: String -> [String]
renderSafetyNumber digits = map formatRow (groups 10 digits)
  where
    formatRow row = let (a, b) = splitAt 5 row
                    in "  " ++ a ++ " " ++ b
    groups _ [] = []
    groups n xs = let (g, rest) = splitAt n xs in g : groups n rest

-- | Format a public key as hex groups for display.
-- Produces 2 rows of 4 groups (4 hex chars each), covering 16 bytes.
renderFingerprint :: ByteString -> [String]
renderFingerprint bs =
    let hexStr = concatMap byte2hex (BS.unpack (BS.take 16 bs))
        row1   = intercalate " " (hexGroups (take 16 hexStr))
        row2   = intercalate " " (hexGroups (drop 16 hexStr))
    in ["  " ++ row1, "  " ++ row2]
  where
    byte2hex :: Word8 -> String
    byte2hex w = [hexC (w `shiftR` 4), hexC (w .&. 0x0f)]
    hexC n | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
           | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)
    hexGroups [] = []
    hexGroups xs = let (g, rest) = splitAt 4 xs in g : hexGroups rest
