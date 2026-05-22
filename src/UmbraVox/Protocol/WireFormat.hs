-- SPDX-License-Identifier: Apache-2.0
-- | Network message envelope
--
-- Wire format (byte offsets):
--   [version:1][type:1][seq:4][srcId:32][dstId:32][payloadLen:4][payload:N][hmac:32]
--
-- The trailing 32-byte HMAC-SHA-256 tag covers the entire preceding content
-- (version through payload).  Verification uses constant-time comparison.
--
-- See: doc/spec/protocol.md
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
import Data.IORef
import Data.Word (Word8, Word32)
import qualified Data.Set as Set

import UmbraVox.Protocol.Handshake (putW32BE, getW32BE)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Crypto.ConstantTime (constantEq)

-- | A network message envelope with routing metadata.
data Envelope = Envelope
    { envVersion   :: !Word8          -- ^ protocol version (1)
    , envType      :: !Word8          -- ^ message type (0=data, 1=ack, 2=handshake, 3=peer)
    , envSequence  :: !Word32         -- ^ sequence number (BE)
    , envSourceId  :: !ByteString     -- ^ 32-byte sender identity hash
    , envDestId    :: !ByteString     -- ^ 32-byte recipient identity hash
    , envPayload   :: !ByteString     -- ^ encrypted payload
    } deriving (Show, Eq)

-- | Header size: version(1) + type(1) + seq(4) + srcId(32) + dstId(32) + payloadLen(4) = 74
headerSize :: Int
headerSize = 74

-- | HMAC-SHA-256 tag size in bytes.
hmacSize :: Int
hmacSize = 32

-- | Wrap a payload in a routing envelope.
wrapEnvelope :: Word8      -- ^ message type
             -> Word32     -- ^ sequence number
             -> ByteString -- ^ 32-byte source identity
             -> ByteString -- ^ 32-byte destination identity
             -> ByteString -- ^ payload
             -> Envelope
wrapEnvelope msgType seqNum srcId dstId payload = Envelope
    { envVersion  = 1
    , envType     = msgType
    , envSequence = seqNum
    , envSourceId = BS.take 32 srcId
    , envDestId   = BS.take 32 dstId
    , envPayload  = payload
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
            , BS.take 32 (envSourceId env)
            , BS.take 32 (envDestId env)
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
    | BS.index bs 0 /= 1 = Nothing  -- M23.3.9: reject unknown version early
    | otherwise =
        let !ver     = BS.index bs 0
            !typ     = BS.index bs 1
            !seqNum  = getW32BE (BS.take 4 (BS.drop 2 bs))
            !srcId   = BS.take 32 (BS.drop 6 bs)
            !dstId   = BS.take 32 (BS.drop 38 bs)
            !pLen    = fromIntegral (getW32BE (BS.take 4 (BS.drop 70 bs))) :: Int
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
                     { envVersion  = ver
                     , envType     = typ
                     , envSequence = seqNum
                     , envSourceId = srcId
                     , envDestId   = dstId
                     , envPayload  = BS.take pLen (BS.drop headerSize bs)
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
