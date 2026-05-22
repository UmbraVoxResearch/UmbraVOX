-- SPDX-License-Identifier: Apache-2.0
-- | Fixed 1024-byte message blocks for traffic-analysis resistance.
--
-- Every message is packed into a uniform-size block so that an observer
-- cannot distinguish short messages from long ones on the wire.
--
-- Format: @[4-byte BE length][payload][PKCS7 padding to 1024]@
--
-- See: doc/spec/protocol.md
module UmbraVox.Protocol.MessageFormat
  ( MessageBlock (..)
  , blockSize
  , packBlock
  , unpackBlock
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)

-- | A fixed-size 1024-byte message block.
-- All messages are padded to uniform size to prevent traffic analysis.
newtype MessageBlock = MessageBlock { unMessageBlock :: ByteString }
    deriving stock (Show, Eq)

-- | The fixed block size in bytes.
blockSize :: Int
blockSize = 1024

-- | Maximum payload size: block minus the 4-byte length header.
maxPayload :: Int
maxPayload = blockSize - 4

-- | Pack a payload into a 1024-byte block.
--
-- Format: @[4-byte BE length][payload][PKCS7 padding to 1024]@
--
-- Fails if payload exceeds 1020 bytes (block size minus 4-byte header).
packBlock :: ByteString -> Either String MessageBlock
packBlock payload
    | payloadLen > maxPayload =
        Left $ "payload too large: " ++ show payloadLen
               ++ " bytes (max " ++ show maxPayload ++ ")"
    | otherwise =
        Right . MessageBlock $ BS.concat [header, payload, padding]
  where
    payloadLen :: Int
    payloadLen = BS.length payload

    header :: ByteString
    header = putWord32BE (fromIntegral payloadLen)

    -- PKCS7: padCount bytes each equal to padCount.
    -- When payload fills exactly, padCount would be 0 — but PKCS7
    -- requires at least 1 byte of padding, which is impossible here
    -- since 4 + 1020 = 1024.  However the maximum payload is 1020,
    -- so there is always at least 0 pad bytes... except PKCS7 mandates
    -- a full extra block when padCount = 0.  In our fixed-size scheme
    -- there is no room for that, but payloadLen <= 1020 guarantees
    -- padCount >= 0.  We treat padCount = 0 as a degenerate edge that
    -- cannot occur because maxPayload = 1020 and blockSize = 1024
    -- leaves exactly 0 spare bytes only when payloadLen = 1020, giving
    -- padCount = 0.  We handle this by allowing zero-pad (no PKCS7
    -- padding byte) for the maximal payload case — the 4-byte length
    -- header is sufficient to determine the payload boundary.
    padCount :: Int
    padCount = blockSize - 4 - payloadLen

    padByte :: Word8
    padByte = if padCount == 0 then 0 else fromIntegral padCount

    padding :: ByteString
    padding = BS.replicate padCount padByte

-- | Unpack a 1024-byte block to extract the payload.
--
-- Verifies the length header and PKCS7 padding.
unpackBlock :: MessageBlock -> Either String ByteString
unpackBlock (MessageBlock bs)
    | BS.length bs /= blockSize =
        Left $ "invalid block size: " ++ show (BS.length bs)
               ++ " (expected " ++ show blockSize ++ ")"
    | payloadLen > maxPayload =
        Left $ "length header too large: " ++ show payloadLen
    | payloadLen < 0 =
        Left $ "negative length header: " ++ show payloadLen
    | padCount > 0 && not paddingValid =
        Left "invalid PKCS7 padding"
    | otherwise =
        Right payload
  where
    payloadLen :: Int
    payloadLen = fromIntegral (getWord32BE (BS.take 4 bs))

    payload :: ByteString
    payload = BS.take payloadLen (BS.drop 4 bs)

    padCount :: Int
    padCount = blockSize - 4 - payloadLen

    expectedPadByte :: Word8
    expectedPadByte = fromIntegral padCount

    paddingBytes :: ByteString
    paddingBytes = BS.drop (4 + payloadLen) bs

    paddingValid :: Bool
    paddingValid = BS.all (== expectedPadByte) paddingBytes

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Encode a Word32 as 4 big-endian bytes.
putWord32BE :: Word32 -> ByteString
putWord32BE !w = BS.pack
    [ fromIntegral (shiftR w 24 .&. 0xff)
    , fromIntegral (shiftR w 16 .&. 0xff)
    , fromIntegral (shiftR w  8 .&. 0xff)
    , fromIntegral (        w   .&. 0xff)
    ]

-- | Decode 4 big-endian bytes to a Word32.
getWord32BE :: ByteString -> Word32
getWord32BE !bs =
    (fromIntegral (BS.index bs 0) `shiftL` 24) .|.
    (fromIntegral (BS.index bs 1) `shiftL` 16) .|.
    (fromIntegral (BS.index bs 2) `shiftL`  8) .|.
     fromIntegral (BS.index bs 3)
