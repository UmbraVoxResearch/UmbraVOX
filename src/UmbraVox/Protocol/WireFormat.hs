-- SPDX-License-Identifier: Apache-2.0
-- | Network message envelope
--
-- Wire format (byte offsets):
--   [version:1][type:1][seq:4][srcId:32][dstId:32][payloadLen:4][payload:N]
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

-- | Serialize envelope to wire bytes.
encodeEnvelope :: Envelope -> ByteString
encodeEnvelope env = BS.concat
    [ BS.singleton (envVersion env)
    , BS.singleton (envType env)
    , putW32BE (envSequence env)
    , BS.take 32 (envSourceId env)
    , BS.take 32 (envDestId env)
    , putW32BE (fromIntegral (BS.length (envPayload env)))
    , envPayload env
    ]

-- | Deserialize wire bytes to envelope.
decodeEnvelope :: ByteString -> Maybe Envelope
decodeEnvelope bs
    | BS.length bs < headerSize = Nothing
    | otherwise =
        let !ver     = BS.index bs 0
            !typ     = BS.index bs 1
            !seqNum  = getW32BE (BS.take 4 (BS.drop 2 bs))
            !srcId   = BS.take 32 (BS.drop 6 bs)
            !dstId   = BS.take 32 (BS.drop 38 bs)
            !pLen    = fromIntegral (getW32BE (BS.take 4 (BS.drop 70 bs))) :: Int
        in if pLen < 0 || BS.length bs < headerSize + pLen
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
