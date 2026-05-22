-- SPDX-License-Identifier: Apache-2.0
-- | Network message envelope
--
-- Wire format v2 (byte offsets):
--   [version:1][type:1][seq:4][ephemeralR:32][viewTag:1][scanTag:2][payloadLen:4][payload:N][hmac:32]
--
-- The trailing 32-byte HMAC-SHA-256 tag covers the entire preceding content
-- (version through payload).  Verification uses constant-time comparison.
--
-- See: doc/ENCRYPTED-ENVELOPE-DESIGN.md
module UmbraVox.Protocol.WireFormat
  ( Envelope(..)
  , wrapEnvelope
  , encodeEnvelope
  , decodeEnvelope
  , unwrapEnvelope
    -- * Per-connection sequence tracking (M23.2.12)
  , SeqWindow
  , newSeqWindow
  , validateSequence
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.IORef
import Data.Word (Word8, Word16, Word32)
import qualified Data.Set as Set

import UmbraVox.Protocol.Handshake (putW32BE, getW32BE)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Crypto.ConstantTime (constantEq)

-- | A network message envelope with stealth addressing metadata.
--
-- The sender identity has been removed from the cleartext header and
-- moved inside the encrypted payload (see M23.1.1d).  The destination
-- is replaced by ephemeral stealth addressing fields.
data Envelope = Envelope
    { envVersion    :: !Word8          -- ^ protocol version (2)
    , envType       :: !Word8          -- ^ message type (0=data, 1=ack, 2=handshake, 3=peer)
    , envSequence   :: !Word32         -- ^ sequence number (BE)
    , envEphemeralR :: !ByteString     -- ^ 32-byte ephemeral X25519 public key
    , envViewTag    :: !Word8          -- ^ 1-byte view tag for fast scan
    , envScanTag    :: !Word16         -- ^ 2-byte extended scan filter (1/65536 false positive)
    , envPayload    :: !ByteString     -- ^ encrypted payload
    } deriving (Show, Eq)

-- | Header size: version(1) + type(1) + seq(4) + ephemeralR(32) + viewTag(1) + scanTag(2) + payloadLen(4) = 45
headerSize :: Int
headerSize = 45

-- | HMAC-SHA-256 tag size in bytes.
hmacSize :: Int
hmacSize = 32

-- | Encode a 'Word16' in big-endian as 2 bytes.
putW16BE :: Word16 -> ByteString
putW16BE w = BS.pack
    [ fromIntegral (w `shiftR` 8 .&. 0xff)
    , fromIntegral (w            .&. 0xff)
    ]

-- | Decode a big-endian 'Word16' from a 2-byte 'ByteString'.
getW16BE :: ByteString -> Word16
getW16BE bs = (fromIntegral (BS.index bs 0) `shiftL` 8)
            + fromIntegral (BS.index bs 1)

-- | Wrap a payload in a routing envelope.
wrapEnvelope :: Word8      -- ^ message type
             -> Word32     -- ^ sequence number
             -> ByteString -- ^ 32-byte ephemeral X25519 public key R
             -> Word8      -- ^ 1-byte view tag
             -> Word16     -- ^ 2-byte scan tag
             -> ByteString -- ^ payload
             -> Envelope
wrapEnvelope msgType seqNum ephR vTag sTag payload = Envelope
    { envVersion    = 2
    , envType       = msgType
    , envSequence   = seqNum
    , envEphemeralR = BS.take 32 ephR
    , envViewTag    = vTag
    , envScanTag    = sTag
    , envPayload    = payload
    }

-- | Serialize envelope to wire bytes with a trailing HMAC-SHA-256 tag.
--
-- The first argument is the HMAC key.  The tag covers the entire
-- serialized content (version through payload).
encodeEnvelope :: ByteString -> Envelope -> ByteString
encodeEnvelope key env =
    let !body = BS.concat
            [ BS.singleton (envVersion env)
            , BS.singleton (envType env)
            , putW32BE (envSequence env)
            , BS.take 32 (envEphemeralR env)
            , BS.singleton (envViewTag env)
            , putW16BE (envScanTag env)
            , putW32BE (fromIntegral (BS.length (envPayload env)))
            , envPayload env
            ]
        !tag = hmacSHA256 key body
    in body <> tag

-- | Deserialize wire bytes to envelope, verifying the HMAC-SHA-256 tag.
--
-- The first argument is the HMAC key.  Returns 'Nothing' if the input
-- is too short, the payload length is inconsistent, or the HMAC tag
-- does not match (checked via constant-time comparison).
decodeEnvelope :: ByteString -> ByteString -> Maybe Envelope
decodeEnvelope key bs
    | BS.length bs < headerSize + hmacSize = Nothing
    | BS.index bs 0 /= 2 = Nothing  -- reject non-v2 envelopes
    | otherwise =
        let !ver     = BS.index bs 0
            !typ     = BS.index bs 1
            !seqNum  = getW32BE (BS.take 4 (BS.drop 2 bs))
            !ephR    = BS.take 32 (BS.drop 6 bs)
            !vTag    = BS.index bs 38
            !sTag    = getW16BE (BS.take 2 (BS.drop 39 bs))
            !pLen    = fromIntegral (getW32BE (BS.take 4 (BS.drop 41 bs))) :: Int
            -- body = everything before the 32-byte trailing HMAC
            !bodyLen = headerSize + pLen
            !body    = BS.take bodyLen bs
            !tag     = BS.take hmacSize (BS.drop bodyLen bs)
            !expected = hmacSHA256 key body
        in if pLen < 0 || BS.length bs < bodyLen + hmacSize
           then Nothing
           else if not (constantEq tag expected)
                then Nothing
                else Just Envelope
                     { envVersion    = ver
                     , envType       = typ
                     , envSequence   = seqNum
                     , envEphemeralR = ephR
                     , envViewTag    = vTag
                     , envScanTag    = sTag
                     , envPayload    = BS.take pLen (BS.drop headerSize bs)
                     }

-- | Extract payload from envelope.
unwrapEnvelope :: Envelope -> ByteString
unwrapEnvelope = envPayload

------------------------------------------------------------------------
-- Per-connection sequence tracking (M23.2.12)
------------------------------------------------------------------------

-- | Sliding window of recently-seen sequence numbers.
-- Tracks a set of at most 'seqWindowSize' entries; rejects duplicates.
type SeqWindow = IORef (Set.Set Word32)

-- | Size of the sliding window for duplicate detection.
seqWindowSize :: Int
seqWindowSize = 64

-- | Create a new empty sequence window.
newSeqWindow :: IO SeqWindow
newSeqWindow = newIORef Set.empty

-- | Validate a sequence number against the sliding window.
-- Returns True if the sequence number is new (accepted) and records it.
-- Returns False if the sequence number is a duplicate (rejected).
-- When the window is full, the oldest (smallest) entry is evicted.
validateSequence :: SeqWindow -> Word32 -> IO Bool
validateSequence ref seqNum = atomicModifyIORef' ref $ \s ->
    if Set.member seqNum s
        then (s, False)  -- duplicate
        else let s' = Set.insert seqNum s
                 s'' = if Set.size s' > seqWindowSize
                       then Set.deleteMin s'
                       else s'
             in (s'', True)
