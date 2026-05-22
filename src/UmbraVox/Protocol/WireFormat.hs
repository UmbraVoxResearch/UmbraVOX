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
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word (Word8, Word32)

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
