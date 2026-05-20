-- SPDX-License-Identifier: Apache-2.0
-- | Signal-compatible protobuf wire format encoder/decoder.
--
-- Hand-rolled minimal protobuf implementation for the ~5 Signal message
-- types needed for wire compatibility. No proto-lens or protobuf-haskell
-- dependency — just varint + length-delimited encoding.
--
-- Signal wire format reference:
--   - SignalMessage: version(1) + ratchetKey(33) + counter(uint32) +
--     previousCounter(uint32) + ciphertext(bytes)
--   - PreKeySignalMessage: registrationId(uint32) + preKeyId(uint32) +
--     signedPreKeyId(uint32) + baseKey(bytes) + identityKey(bytes) +
--     message(bytes)
--   - Envelope: type(uint32) + sourceDevice(uint32) + timestamp(uint64) +
--     content(bytes)
module UmbraVox.Protocol.SignalWire
    ( -- * Signal message types
      SignalMessage(..)
    , PreKeySignalMessage(..)
    , SignalEnvelope(..)
    , EnvelopeType(..)
      -- * Encoding
    , encodeSignalMessage
    , encodePreKeySignalMessage
    , encodeSignalEnvelope
      -- * Decoding
    , decodeSignalMessage
    , decodePreKeySignalMessage
    , decodeSignalEnvelope
      -- * Low-level protobuf helpers (exported for testing)
    , encodeVarint
    , decodeVarint
    , encodeField
    , decodeFields
    , WireType(..)
    , ProtoField(..)
    ) where

import Data.Bits (Bits, shiftL, shiftR, (.&.), (.|.), testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32, Word64)

------------------------------------------------------------------------
-- Protobuf wire types
------------------------------------------------------------------------

-- | Protobuf wire types (subset needed for Signal messages).
data WireType
    = WireVarint          -- ^ 0: int32, int64, uint32, uint64, bool, enum
    | WireLengthDelimited -- ^ 2: string, bytes, embedded messages
    deriving (Eq, Show)

wireTypeToTag :: WireType -> Word8
wireTypeToTag WireVarint          = 0
wireTypeToTag WireLengthDelimited = 2

-- | A decoded protobuf field.
data ProtoField = ProtoField
    { pfFieldNumber :: !Word32
    , pfWireType    :: !WireType
    , pfValue       :: !ByteString  -- ^ Raw value (varint-encoded or raw bytes)
    } deriving (Show)

------------------------------------------------------------------------
-- Varint encoding/decoding (protobuf base-128)
------------------------------------------------------------------------

-- | Encode a Word64 as a protobuf varint.
encodeVarint :: Word64 -> ByteString
encodeVarint n
    | n < 128   = BS.singleton (fromIntegral n)
    | otherwise = BS.cons (fromIntegral (n .&. 0x7F) .|. 0x80)
                          (encodeVarint (shiftR n 7))

-- | Decode a protobuf varint from the front of a ByteString.
-- Returns (value, remaining bytes) or Nothing on parse error.
decodeVarint :: ByteString -> Maybe (Word64, ByteString)
decodeVarint = go 0 0
  where
    go !acc !shift !bs
        | BS.null bs = Nothing
        | shift >= 64 = Nothing  -- overflow protection
        | otherwise =
            let !b    = BS.index bs 0
                !rest = BS.drop 1 bs
                !val  = acc .|. (fromIntegral (b .&. 0x7F) `shiftL` shift)
            in if testBit b 7
               then go val (shift + 7) rest
               else Just (val, rest)

------------------------------------------------------------------------
-- Field encoding/decoding
------------------------------------------------------------------------

-- | Encode a single protobuf field.
encodeField :: Word32 -> WireType -> ByteString -> ByteString
encodeField fieldNum wt payload =
    let !tag = fromIntegral fieldNum `shiftL` 3 .|. fromIntegral (wireTypeToTag wt)
    in case wt of
        WireVarint          -> encodeVarint (fromIntegral tag) <> payload
        WireLengthDelimited -> encodeVarint (fromIntegral tag)
                            <> encodeVarint (fromIntegral (BS.length payload))
                            <> payload

-- | Encode a varint field.
encodeVarintField :: Word32 -> Word64 -> ByteString
encodeVarintField fieldNum val =
    encodeField fieldNum WireVarint (encodeVarint val)

-- | Encode a bytes field.
encodeBytesField :: Word32 -> ByteString -> ByteString
encodeBytesField fieldNum val =
    encodeField fieldNum WireLengthDelimited val

-- | Decode all fields from a protobuf message.
decodeFields :: ByteString -> Maybe [ProtoField]
decodeFields bs
    | BS.null bs = Just []
    | otherwise = do
        (tagVal, rest1) <- decodeVarint bs
        let !fieldNum = fromIntegral (tagVal `shiftR` 3) :: Word32
            !wireTag  = fromIntegral (tagVal .&. 0x07) :: Word8
        case wireTag of
            0 -> do  -- Varint
                (val, rest2) <- decodeVarint rest1
                fields <- decodeFields rest2
                Just (ProtoField fieldNum WireVarint (encodeVarint val) : fields)
            2 -> do  -- Length-delimited
                (len, rest2) <- decodeVarint rest1
                let !payloadLen = fromIntegral len
                if BS.length rest2 < payloadLen
                    then Nothing
                    else do
                        let !payload = BS.take payloadLen rest2
                            !rest3   = BS.drop payloadLen rest2
                        fields <- decodeFields rest3
                        Just (ProtoField fieldNum WireLengthDelimited payload : fields)
            _ -> Nothing  -- Unsupported wire type

-- | Look up a field by number, return its value.
lookupField :: Word32 -> [ProtoField] -> Maybe ByteString
lookupField n = go
  where
    go [] = Nothing
    go (f:fs)
        | pfFieldNumber f == n = Just (pfValue f)
        | otherwise = go fs

-- | Extract a varint value from a raw varint-encoded field value.
fieldVarint :: ByteString -> Maybe Word64
fieldVarint bs = fst <$> decodeVarint bs

-- | Extract a Word32 from a varint field.
fieldWord32 :: ByteString -> Maybe Word32
fieldWord32 bs = fromIntegral <$> fieldVarint bs

------------------------------------------------------------------------
-- Signal message types
------------------------------------------------------------------------

-- | Signal protocol message (encrypted payload with ratchet header).
-- Wire: field 1 = ratchetKey (bytes), field 2 = counter (uint32),
--        field 3 = previousCounter (uint32), field 4 = ciphertext (bytes)
data SignalMessage = SignalMessage
    { smRatchetKey     :: !ByteString  -- ^ 33 bytes (0x05 + 32-byte Curve25519 public)
    , smCounter        :: !Word32
    , smPreviousCounter :: !Word32
    , smCiphertext     :: !ByteString
    } deriving (Show, Eq)

-- | Pre-key Signal message (initial message with identity + prekey info).
-- Wire: field 1 = registrationId (uint32), field 2 = preKeyId (uint32),
--        field 3 = signedPreKeyId (uint32), field 5 = baseKey (bytes),
--        field 6 = identityKey (bytes), field 8 = message (bytes)
data PreKeySignalMessage = PreKeySignalMessage
    { pksmRegistrationId :: !Word32
    , pksmPreKeyId       :: !Word32
    , pksmSignedPreKeyId :: !Word32
    , pksmBaseKey        :: !ByteString  -- ^ 33 bytes
    , pksmIdentityKey    :: !ByteString  -- ^ 33 bytes
    , pksmMessage        :: !ByteString  -- ^ Serialized SignalMessage
    } deriving (Show, Eq)

-- | Envelope type (subset).
data EnvelopeType
    = EnvelopeUnknown
    | EnvelopeCiphertext       -- ^ 1: Normal message
    | EnvelopeKeyExchange      -- ^ 2: Key exchange (unused)
    | EnvelopePreKeyBundle     -- ^ 3: PreKey message
    | EnvelopeReceipt          -- ^ 5: Delivery receipt
    | EnvelopeUnidentifiedSender -- ^ 6: Sealed sender (future)
    deriving (Show, Eq)

envelopeTypeToWord :: EnvelopeType -> Word32
envelopeTypeToWord EnvelopeUnknown            = 0
envelopeTypeToWord EnvelopeCiphertext         = 1
envelopeTypeToWord EnvelopeKeyExchange        = 2
envelopeTypeToWord EnvelopePreKeyBundle       = 3
envelopeTypeToWord EnvelopeReceipt            = 5
envelopeTypeToWord EnvelopeUnidentifiedSender = 6

envelopeTypeFromWord :: Word32 -> EnvelopeType
envelopeTypeFromWord 1 = EnvelopeCiphertext
envelopeTypeFromWord 2 = EnvelopeKeyExchange
envelopeTypeFromWord 3 = EnvelopePreKeyBundle
envelopeTypeFromWord 5 = EnvelopeReceipt
envelopeTypeFromWord 6 = EnvelopeUnidentifiedSender
envelopeTypeFromWord _ = EnvelopeUnknown

-- | Signal envelope (server delivery wrapper).
-- Wire: field 1 = type (uint32), field 7 = sourceDevice (uint32),
--        field 5 = timestamp (uint64), field 8 = content (bytes)
data SignalEnvelope = SignalEnvelope
    { seType         :: !EnvelopeType
    , seSourceDevice :: !Word32
    , seTimestamp    :: !Word64
    , seContent      :: !ByteString  -- ^ Encrypted inner message
    } deriving (Show, Eq)

------------------------------------------------------------------------
-- Encoding
------------------------------------------------------------------------

-- | Encode a SignalMessage to protobuf bytes.
--
-- The on-wire format prepends a version byte (0x33 for version 3)
-- and appends an 8-byte MAC, but those are handled by the caller.
-- This function produces the inner protobuf body only.
encodeSignalMessage :: SignalMessage -> ByteString
encodeSignalMessage sm = BS.concat
    [ encodeBytesField  1 (smRatchetKey sm)
    , encodeVarintField 2 (fromIntegral (smCounter sm))
    , encodeVarintField 3 (fromIntegral (smPreviousCounter sm))
    , encodeBytesField  4 (smCiphertext sm)
    ]

-- | Encode a PreKeySignalMessage to protobuf bytes.
encodePreKeySignalMessage :: PreKeySignalMessage -> ByteString
encodePreKeySignalMessage pksm = BS.concat
    [ encodeVarintField 1 (fromIntegral (pksmRegistrationId pksm))
    , encodeVarintField 2 (fromIntegral (pksmPreKeyId pksm))
    , encodeVarintField 3 (fromIntegral (pksmSignedPreKeyId pksm))
    , encodeBytesField  5 (pksmBaseKey pksm)
    , encodeBytesField  6 (pksmIdentityKey pksm)
    , encodeBytesField  8 (pksmMessage pksm)
    ]

-- | Encode a Signal envelope to protobuf bytes.
encodeSignalEnvelope :: SignalEnvelope -> ByteString
encodeSignalEnvelope se = BS.concat
    [ encodeVarintField 1 (fromIntegral (envelopeTypeToWord (seType se)))
    , encodeVarintField 7 (fromIntegral (seSourceDevice se))
    , encodeVarintField 5 (seTimestamp se)
    , encodeBytesField  8 (seContent se)
    ]

------------------------------------------------------------------------
-- Decoding
------------------------------------------------------------------------

-- | Decode a SignalMessage from protobuf bytes.
decodeSignalMessage :: ByteString -> Maybe SignalMessage
decodeSignalMessage bs = do
    fields <- decodeFields bs
    ratchetKey     <- lookupField 1 fields
    counterRaw     <- lookupField 2 fields >>= fieldWord32
    prevCounterRaw <- lookupField 3 fields >>= fieldWord32
    ciphertext     <- lookupField 4 fields
    Just SignalMessage
        { smRatchetKey      = ratchetKey
        , smCounter         = counterRaw
        , smPreviousCounter = prevCounterRaw
        , smCiphertext      = ciphertext
        }

-- | Decode a PreKeySignalMessage from protobuf bytes.
decodePreKeySignalMessage :: ByteString -> Maybe PreKeySignalMessage
decodePreKeySignalMessage bs = do
    fields <- decodeFields bs
    regId       <- lookupField 1 fields >>= fieldWord32
    preKeyId    <- lookupField 2 fields >>= fieldWord32
    signedPKId  <- lookupField 3 fields >>= fieldWord32
    baseKey     <- lookupField 5 fields
    identityKey <- lookupField 6 fields
    message     <- lookupField 8 fields
    Just PreKeySignalMessage
        { pksmRegistrationId = regId
        , pksmPreKeyId       = preKeyId
        , pksmSignedPreKeyId = signedPKId
        , pksmBaseKey        = baseKey
        , pksmIdentityKey    = identityKey
        , pksmMessage        = message
        }

-- | Decode a Signal envelope from protobuf bytes.
decodeSignalEnvelope :: ByteString -> Maybe SignalEnvelope
decodeSignalEnvelope bs = do
    fields <- decodeFields bs
    typeRaw      <- lookupField 1 fields >>= fieldWord32
    sourceDevice <- lookupField 7 fields >>= fieldWord32
    timestamp    <- lookupField 5 fields >>= fieldVarint
    content      <- lookupField 8 fields
    Just SignalEnvelope
        { seType         = envelopeTypeFromWord typeRaw
        , seSourceDevice = sourceDevice
        , seTimestamp    = timestamp
        , seContent      = content
        }
