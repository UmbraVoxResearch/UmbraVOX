-- SPDX-License-Identifier: Apache-2.0
-- | P2P wire protocol message types.
--
-- Binary format: @[type:1][length:4BE][payload:N]@
--
-- See: doc/spec/network.md
module UmbraVox.Network.Protocol
  ( P2PMessage(..)
  , HandshakePayload(..)
  , DataPayload(..)
  , AckPayload(..)
  , PeerPayload(..)
  , encode
  , decode
    -- * Application-layer message type prefix (M24.4.4)
  , AppMessageType(..)
  , appMessageTypeByte
  , parseAppMessageType
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import Data.Word (Word8, Word16, Word32)

-- | Application-layer message type prefix for Noise payloads.
--
-- DHT messages need to be distinguishable from chat messages within the
-- encrypted Noise channel.  The first byte of each decrypted payload
-- carries this tag so the receiver can dispatch to the correct handler.
data AppMessageType
    = AppChat     -- ^ 0x01: Chat/session message
    | AppDHT      -- ^ 0x02: DHT RPC message
    deriving stock (Show, Eq)

-- | Encode an 'AppMessageType' as its wire byte.
appMessageTypeByte :: AppMessageType -> Word8
appMessageTypeByte AppChat = 0x01
appMessageTypeByte AppDHT  = 0x02

-- | Parse a wire byte into an 'AppMessageType', if valid.
parseAppMessageType :: Word8 -> Maybe AppMessageType
parseAppMessageType 0x01 = Just AppChat
parseAppMessageType 0x02 = Just AppDHT
parseAppMessageType _    = Nothing

-- | P2P protocol message types.
data P2PMessage
    = MsgHandshake HandshakePayload  -- ^ type 0x01
    | MsgData DataPayload            -- ^ type 0x02
    | MsgAck AckPayload              -- ^ type 0x03
    | MsgPeer PeerPayload            -- ^ type 0x04
    | MsgPing                        -- ^ type 0x05
    | MsgPong                        -- ^ type 0x06
    deriving stock (Show, Eq)

data HandshakePayload = HandshakePayload
    { hpVersion      :: !Word8
    , hpPublicKey    :: !ByteString  -- ^ 32 bytes
    , hpCapabilities :: !Word16      -- ^ bitfield
    } deriving stock (Show, Eq)

data DataPayload = DataPayload
    { dpSequence :: !Word32
    , dpPayload  :: !ByteString
    } deriving stock (Show, Eq)

data AckPayload = AckPayload
    { apSequence :: !Word32
    } deriving stock (Show, Eq)

data PeerPayload = PeerPayload
    { ppPeers :: [(String, Word16)]  -- ^ (host, port)
    } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Encoding helpers
------------------------------------------------------------------------

putW16 :: Word16 -> ByteString
putW16 !w = BS.pack
    [ fromIntegral (shiftR w 8 .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

putW32 :: Word32 -> ByteString
putW32 !w = BS.pack
    [ fromIntegral (shiftR w 24 .&. 0xff)
    , fromIntegral (shiftR w 16 .&. 0xff)
    , fromIntegral (shiftR w  8 .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

getW16 :: ByteString -> Word16
getW16 !bs =
    (fromIntegral (BS.index bs 0) `shiftL` 8) .|.
     fromIntegral (BS.index bs 1)

getW32 :: ByteString -> Word32
getW32 !bs =
    (fromIntegral (BS.index bs 0) `shiftL` 24) .|.
    (fromIntegral (BS.index bs 1) `shiftL` 16) .|.
    (fromIntegral (BS.index bs 2) `shiftL`  8) .|.
     fromIntegral (BS.index bs 3)

-- | Encode a string to UTF-8 bytes (ASCII subset).
packStr :: String -> ByteString
packStr = BS.pack . map (fromIntegral . ord)

-- | Decode UTF-8 bytes to a string (ASCII subset).
unpackStr :: ByteString -> String
unpackStr = map (chr . fromIntegral) . BS.unpack

------------------------------------------------------------------------
-- Payload encoding
------------------------------------------------------------------------

encodePayload :: P2PMessage -> (Word8, ByteString)
encodePayload (MsgHandshake hp) =
    ( 0x01
    , BS.concat
        [ BS.singleton (hpVersion hp)
        , hpPublicKey hp
        , putW16 (hpCapabilities hp)
        ]
    )
encodePayload (MsgData dp) =
    ( 0x02
    , BS.append (putW32 (dpSequence dp)) (dpPayload dp)
    )
encodePayload (MsgAck ap') =
    ( 0x03
    , putW32 (apSequence ap')
    )
encodePayload (MsgPeer pp) =
    let encodePeer (host, caps) =
            let hostBs = packStr host
                hlen   = fromIntegral (BS.length hostBs) :: Word16
            in  BS.concat [putW16 hlen, hostBs, putW16 caps]
        peerCount = fromIntegral (length (ppPeers pp)) :: Word16
    in  ( 0x04
        , BS.concat (putW16 peerCount : map encodePeer (ppPeers pp))
        )
encodePayload MsgPing = (0x05, BS.empty)
encodePayload MsgPong = (0x06, BS.empty)

-- | Encode a P2P message to wire bytes.
--
-- Format: @[type:1][length:4BE][payload:N]@
encode :: P2PMessage -> ByteString
encode msg =
    let (tag, payload) = encodePayload msg
        len = fromIntegral (BS.length payload) :: Word32
    in  BS.concat [BS.singleton tag, putW32 len, payload]

------------------------------------------------------------------------
-- Payload decoding
------------------------------------------------------------------------

-- | Decode wire bytes to a P2P message.
decode :: ByteString -> Either String P2PMessage
decode bs
    | BS.length bs < 5 =
        Left "message too short: need at least 5 bytes (type + length)"
    | otherwise =
        let tag     = BS.index bs 0
            len     = getW32 (BS.drop 1 bs)
            payload = BS.drop 5 bs
        in  if fromIntegral (BS.length payload) /= len
            then Left $ "payload length mismatch: header says "
                      ++ show len ++ " but got "
                      ++ show (BS.length payload)
            else decodeTagged tag payload

decodeTagged :: Word8 -> ByteString -> Either String P2PMessage
decodeTagged 0x01 bs = decodeHandshake bs
decodeTagged 0x02 bs = decodeData bs
decodeTagged 0x03 bs = decodeAck bs
decodeTagged 0x04 bs = decodePeer bs
decodeTagged 0x05 bs
    | BS.null bs = Right MsgPing
    | otherwise  = Left "MsgPing must have empty payload"
decodeTagged 0x06 bs
    | BS.null bs = Right MsgPong
    | otherwise  = Left "MsgPong must have empty payload"
decodeTagged tag _   = Left $ "unknown message type: 0x"
                            ++ showHex2 tag

showHex2 :: Word8 -> String
showHex2 w =
    let hi = w `shiftR` 4
        lo = w .&. 0x0f
        hexChar n
            | n < 10    = chr (fromIntegral n + ord '0')
            | otherwise = chr (fromIntegral n - 10 + ord 'a')
    in  [hexChar hi, hexChar lo]

decodeHandshake :: ByteString -> Either String P2PMessage
decodeHandshake bs
    | BS.length bs /= 35 =
        Left $ "handshake payload must be 35 bytes (1+32+2), got "
            ++ show (BS.length bs)
    | otherwise =
        let ver  = BS.index bs 0
            pkey = BS.take 32 (BS.drop 1 bs)
            caps = getW16 (BS.drop 33 bs)
        in  Right $ MsgHandshake HandshakePayload
                { hpVersion      = ver
                , hpPublicKey    = pkey
                , hpCapabilities = caps
                }

decodeData :: ByteString -> Either String P2PMessage
decodeData bs
    | BS.length bs < 4 =
        Left "data payload must be at least 4 bytes (sequence number)"
    | otherwise =
        let seqn    = getW32 bs
            payload = BS.drop 4 bs
        in  Right $ MsgData DataPayload
                { dpSequence = seqn
                , dpPayload  = payload
                }

decodeAck :: ByteString -> Either String P2PMessage
decodeAck bs
    | BS.length bs /= 4 =
        Left $ "ack payload must be 4 bytes, got " ++ show (BS.length bs)
    | otherwise =
        Right $ MsgAck AckPayload { apSequence = getW32 bs }

decodePeer :: ByteString -> Either String P2PMessage
decodePeer bs
    | BS.length bs < 2 =
        Left "peer payload must be at least 2 bytes (peer count)"
    | otherwise =
        let count = fromIntegral (getW16 bs) :: Int
        in  decodePeers count (BS.drop 2 bs) []

decodePeers :: Int -> ByteString -> [(String, Word16)]
            -> Either String P2PMessage
decodePeers 0 bs acc
    | BS.null bs = Right $ MsgPeer PeerPayload { ppPeers = reverse acc }
    | otherwise  = Left "trailing bytes after peer list"
decodePeers n bs acc
    | BS.length bs < 2 = Left "truncated peer entry (missing host length)"
    | otherwise =
        let hlen = fromIntegral (getW16 bs) :: Int
            rest = BS.drop 2 bs
        in  if BS.length rest < hlen + 2
            then Left "truncated peer entry"
            else let host = unpackStr (BS.take hlen rest)
                     caps = getW16 (BS.drop hlen rest)
                     remaining = BS.drop (hlen + 2) rest
                 in  decodePeers (n - 1) remaining ((host, caps) : acc)
