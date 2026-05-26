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
    -- * Message padding (M27.2.1)
  , padToBucket
  , nextBucket
    -- * ChaCha20-Poly1305 AEAD envelope encryption (M23.1.1c)
  , deriveEnvelopeKey
  , encodeEnvelopeAEAD
  , decodeEnvelopeAEAD
    -- * Per-connection sequence tracking (M23.2.12)
  , SeqWindow
  , newSeqWindow
  , validateSequence
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), testBit)
import Data.IORef
import Data.Word (Word8, Word16, Word32)
import qualified Data.Set as Set

import UmbraVox.Protocol.Handshake (putW32BE, getW32BE)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Crypto.HKDF (hkdfSHA256)
import UmbraVox.Crypto.ChaChaPoly (chachaPolyEncrypt, chachaPolyDecrypt)
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
-- Message padding to fixed size buckets (M27.2.1)
------------------------------------------------------------------------

-- | Pad payload to the next size bucket to prevent length correlation.
-- Buckets: 256, 1024, 4096, 16384 bytes (then multiples of 16384).
-- The original payload length is stored in the first 2 bytes of the
-- padded block so the receiver can strip the padding.
padToBucket :: ByteString -> ByteString
padToBucket payload =
    let len = BS.length payload
        -- 2-byte length prefix is included in the bucket
        totalNeeded = 2 + len
        bucket = nextBucket totalNeeded
        padLen = bucket - totalNeeded
    in putW16BE (fromIntegral len) <> payload <> BS.replicate padLen 0x00

-- | Determine the next bucket size for a given byte count.
nextBucket :: Int -> Int
nextBucket n
    | n <= 256   = 256
    | n <= 1024  = 1024
    | n <= 4096  = 4096
    | otherwise  = ((n + 16383) `div` 16384) * 16384

-- | Strip bucket padding from a padded payload.
-- Reads the 2-byte original length prefix and returns only the
-- original payload bytes, or 'Nothing' if the encoding is invalid.
unpadFromBucket :: ByteString -> Maybe ByteString
unpadFromBucket bs
    | BS.length bs < 2 = Nothing
    | otherwise =
        let origLen = fromIntegral (getW16BE (BS.take 2 bs)) :: Int
        in if origLen < 0 || origLen + 2 > BS.length bs
           then Nothing
           else Just (BS.take origLen (BS.drop 2 bs))

------------------------------------------------------------------------
-- ChaCha20-Poly1305 AEAD envelope encryption (M23.1.1c)
------------------------------------------------------------------------

-- | Poly1305 tag size in bytes.
poly1305TagSize :: Int
poly1305TagSize = 16

-- | Minimum size of an AEAD-encrypted wire message:
-- aad(1) + inner ciphertext(44) + poly1305 tag(16) = 61 bytes.
-- The inner plaintext is: type(1) + seq(4) + ephemeralR(32) + viewTag(1) + scanTag(2) + payloadLen(4).
--
-- M27.6.4: The message type byte has been moved inside the encrypted payload
-- so that observers cannot distinguish message types.  The AAD now contains
-- only the version byte.
minAEADSize :: Int
minAEADSize = 1 + 44 + poly1305TagSize

-- | Derive a 32-byte envelope encryption key from the transport key.
--
-- Uses HKDF-SHA-256 with a 32-byte zero salt and the info string
-- @"UmbraVox_EnvelopeKey_v1"@.
--
-- See: doc/ENCRYPTED-ENVELOPE-DESIGN.md Section 4.5
deriveEnvelopeKey :: ByteString -> ByteString
deriveEnvelopeKey transportKey =
    hkdfSHA256 (BS.replicate 32 0) transportKey "UmbraVox_EnvelopeKey_v1" 32

-- | Build a 12-byte nonce from a sequence number.
--
-- Layout: @0x00*4 || BE32(sequence) || 0x00*4@ (12 bytes total).
-- Unique per message within a session since sequence numbers are not reused.
--
-- SECURITY: The 32-bit sequence number limits each session key to at most
-- 2^32 messages.  'seqNonceSafe' enforces a 2^31 threshold to allow
-- re-keying margin before nonce space exhaustion.
seqNonce :: Word32 -> ByteString
seqNonce seqNum =
    BS.replicate 4 0 <> putW32BE seqNum <> BS.replicate 4 0

-- | Maximum sequence number before mandatory re-key (2^31, leaving margin).
seqNonceMaxSeq :: Word32
seqNonceMaxSeq = 2^(31 :: Int)

-- | Safe variant of 'seqNonce' that returns 'Nothing' when the sequence number
-- exceeds the re-keying threshold (2^31), preventing nonce space exhaustion.
seqNonceSafe :: Word32 -> Maybe ByteString
seqNonceSafe seqNum
    | seqNum >= seqNonceMaxSeq = Nothing
    | otherwise = Just (seqNonce seqNum)

-- | Flag bit in the version byte that distinguishes AEAD-encrypted messages
-- from HMAC-authenticated messages.
--
-- When this bit is set in byte 0, the message uses ChaCha20-Poly1305 AEAD
-- (type byte encrypted inside the ciphertext).  When clear, the message uses
-- the legacy HMAC-SHA-256 format with a full cleartext header (handshake path).
--
-- Bit 7 (0x80) is used; the lower bits still carry the protocol version (2).
aeadFlag :: Word8
aeadFlag = 0x80

-- | Encode an envelope with ChaCha20-Poly1305 AEAD encryption.
--
-- M27.6.4: The message type byte is encrypted inside the AEAD payload so
-- that observers cannot distinguish message types on the wire.
--
-- For data messages (all types except handshake type 2):
--   AAD: version byte with AEAD flag set (authenticated but not encrypted)
--   Plaintext: type(1) + seq(4) + ephemeralR(32) + viewTag(1) + scanTag(2) + payloadLen(4) + payload(N)
--   Output: [version|0x80:1][aead_ciphertext:N][poly1305_tag:16]
--
-- For handshake messages (type 2): delegates to 'encodeEnvelope' (HMAC-SHA-256),
-- since no session key exists yet during the handshake.  The HMAC path emits a
-- full cleartext header where byte 0 does NOT have 'aeadFlag' set.
--
-- Returns 'Left' with a description when the sequence number has exceeded the
-- re-keying threshold (2^31), allowing callers to handle the error without a
-- process crash.  The caller must initiate re-keying before encoding further.
encodeEnvelopeAEAD :: ByteString  -- ^ 32-byte envelope key (from 'deriveEnvelopeKey')
                   -> Word32      -- ^ sequence number (used to derive nonce)
                   -> Envelope    -- ^ envelope to encode
                   -> Either String ByteString
encodeEnvelopeAEAD envelopeKey seqNum env
    | envType env == 2 =
        -- Handshake messages use HMAC authentication (no session key yet).
        -- The HMAC path writes a full cleartext header; byte 0 = version (0x02),
        -- without the aeadFlag bit set.
        Right (encodeEnvelope envelopeKey env)
    | seqNum >= seqNonceMaxSeq =
        -- M27.3.4: Refuse to encrypt past 2^31 messages; caller must re-key
        Left "encodeEnvelopeAEAD: sequence number exceeds re-keying threshold (2^31)"
    | otherwise =
        let -- M27.6.4: AAD is the version byte with the AEAD flag set, so the
            -- decoder can reliably distinguish this format from the HMAC format
            -- without inspecting any ciphertext byte.
            !aad = BS.singleton (envVersion env .|. aeadFlag)
            -- M27.2.1: Pad payload to fixed size bucket before encryption
            -- to prevent length correlation attacks.
            !paddedPayload = padToBucket (envPayload env)
            -- Plaintext: type byte + inner fields (seq through padded payload)
            !plaintext = BS.concat
                [ BS.singleton (envType env)
                , putW32BE (envSequence env)
                , BS.take 32 (envEphemeralR env)
                , BS.singleton (envViewTag env)
                , putW16BE (envScanTag env)
                , putW32BE (fromIntegral (BS.length paddedPayload))
                , paddedPayload
                ]
            !nonce = seqNonce seqNum
            (!ciphertext, !tag) = chachaPolyEncrypt envelopeKey nonce aad plaintext
        in Right (aad <> ciphertext <> tag)

-- | Decode an AEAD-encrypted envelope.
--
-- The caller must supply the expected sequence number (from the transport
-- layer's SeqWindow) so that the correct nonce can be reconstructed.
--
-- Message format detection uses the 'aeadFlag' bit (0x80) in the version byte
-- (byte 0), NOT byte 1 (which is ciphertext in the AEAD format):
--
--   * Byte 0 has 'aeadFlag' set (0x82): AEAD format — type byte is inside
--     the ciphertext; decode with ChaCha20-Poly1305 and then 'parseInnerEnvelopeV2'.
--   * Byte 0 does NOT have 'aeadFlag' set (0x02): HMAC format — full cleartext
--     header used for handshake messages; delegate to 'decodeEnvelope'.
--
-- Returns 'Nothing' on authentication failure, truncation, or parse error.
--
-- Finding:      Byte index 1 of an AEAD message is ciphertext, yet the old
--               decoder compared it to the literal value 2 to detect handshake
--               messages.  This could misclassify a data message whose first
--               ciphertext byte happens to equal 2 (≈1/256 probability),
--               causing the decoder to attempt HMAC verification on an AEAD
--               message and silently drop it.
-- Vulnerability: AEAD messages with a first ciphertext byte of 0x02 would be
--               incorrectly routed to the HMAC path, causing spurious decryption
--               failures and potential message loss.
-- Fix:           Set 'aeadFlag' (bit 7 = 0x80) in the version/AAD byte during
--               AEAD encoding, and test that bit during decoding to distinguish
--               the two formats.  Byte 1 is never examined for format detection.
-- Verified:     The encode path sets bit 0x80; the decode path tests bit 0x80
--               before any other byte is read.
decodeEnvelopeAEAD :: ByteString  -- ^ 32-byte envelope key (from 'deriveEnvelopeKey')
                   -> Word32      -- ^ expected sequence number (for nonce derivation)
                   -> ByteString  -- ^ wire bytes
                   -> Maybe Envelope
decodeEnvelopeAEAD envelopeKey seqNum bs
    | BS.length bs < 2 = Nothing
    -- Reject if base version bits (ignoring the AEAD flag) are not 2.
    | (BS.index bs 0 .&. 0x7f) /= 2 = Nothing
    | not (testBit (BS.index bs 0) 7) =
        -- AEAD flag not set: this is a legacy HMAC-authenticated handshake
        -- message with a full cleartext header.  Byte 1 is the cleartext type.
        decodeEnvelope envelopeKey bs
    | seqNum >= seqNonceMaxSeq = Nothing  -- M27.3.4: reject past re-key threshold
    | BS.length bs < minAEADSize = Nothing
    | otherwise =
        -- AEAD flag is set: type byte is inside the encrypted payload (M27.6.4).
        -- Byte 1 is ciphertext; do not inspect it for message type detection.
        let !aad = BS.take 1 bs
            -- ciphertext is everything between the 1-byte AAD and the trailing
            -- 16-byte Poly1305 tag
            !totalLen = BS.length bs
            !ciphertextLen = totalLen - 1 - poly1305TagSize
            !ciphertext = BS.take ciphertextLen (BS.drop 1 bs)
            !tag = BS.drop (totalLen - poly1305TagSize) bs
            !nonce = seqNonce seqNum
            -- Strip AEAD flag from version byte before passing to envelope parser
            !ver = BS.index bs 0 .&. 0x7f
        in case chachaPolyDecrypt envelopeKey nonce aad ciphertext tag of
            Nothing -> Nothing
            Just plaintext -> parseInnerEnvelopeV2 ver plaintext

-- | Parse the decrypted inner plaintext into an 'Envelope' (legacy format).
--
-- Expected layout: seq(4) + ephemeralR(32) + viewTag(1) + scanTag(2) + payloadLen(4) + paddedPayload(N)
-- Minimum plaintext size: 43 bytes (with empty payload).
-- The payload is bucket-padded (M27.2.1); the first 2 bytes encode the
-- original length so padding can be stripped.
parseInnerEnvelope :: Word8 -> Word8 -> ByteString -> Maybe Envelope
parseInnerEnvelope ver typ plaintext
    | BS.length plaintext < 43 = Nothing
    | otherwise =
        let !seqNum  = getW32BE (BS.take 4 plaintext)
            !ephR    = BS.take 32 (BS.drop 4 plaintext)
            !vTag    = BS.index plaintext 36
            !sTag    = getW16BE (BS.take 2 (BS.drop 37 plaintext))
            !pLen    = fromIntegral (getW32BE (BS.take 4 (BS.drop 39 plaintext))) :: Int
        in if pLen < 0 || BS.length plaintext < 43 + pLen
           then Nothing
           else -- M27.2.1: Strip bucket padding from payload
                case unpadFromBucket (BS.take pLen (BS.drop 43 plaintext)) of
                    Nothing -> Nothing
                    Just origPayload -> Just Envelope
                        { envVersion    = ver
                        , envType       = typ
                        , envSequence   = seqNum
                        , envEphemeralR = ephR
                        , envViewTag    = vTag
                        , envScanTag    = sTag
                        , envPayload    = origPayload
                        }

-- | M27.6.4: Parse inner plaintext where the type byte is inside the
-- encrypted payload (not in the AAD).
--
-- Expected layout: type(1) + seq(4) + ephemeralR(32) + viewTag(1) + scanTag(2) + payloadLen(4) + paddedPayload(N)
-- Minimum plaintext size: 44 bytes (with empty payload).
parseInnerEnvelopeV2 :: Word8 -> ByteString -> Maybe Envelope
parseInnerEnvelopeV2 ver plaintext
    | BS.length plaintext < 44 = Nothing
    | otherwise =
        let !typ     = BS.index plaintext 0
            !seqNum  = getW32BE (BS.take 4 (BS.drop 1 plaintext))
            !ephR    = BS.take 32 (BS.drop 5 plaintext)
            !vTag    = BS.index plaintext 37
            !sTag    = getW16BE (BS.take 2 (BS.drop 38 plaintext))
            !pLen    = fromIntegral (getW32BE (BS.take 4 (BS.drop 40 plaintext))) :: Int
        in if pLen < 0 || BS.length plaintext < 44 + pLen
           then Nothing
           else case unpadFromBucket (BS.take pLen (BS.drop 44 plaintext)) of
                    Nothing -> Nothing
                    Just origPayload -> Just Envelope
                        { envVersion    = ver
                        , envType       = typ
                        , envSequence   = seqNum
                        , envEphemeralR = ephR
                        , envViewTag    = vTag
                        , envScanTag    = sTag
                        , envPayload    = origPayload
                        }

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
--
-- Finding:     M27.6.11 — The sequence window had no high-water mark,
--              allowing an attacker to inject a very large sequence number
--              and then replay all earlier sequence numbers (which would
--              have been evicted from the window).
-- Vulnerability: Without a floor, an attacker can advance the window far
--              ahead with a single crafted packet, then replay packets
--              with old sequence numbers that are no longer tracked.
-- Fix:         Maintain a high-water mark (maximum accepted sequence number)
--              and reject any sequence number below @highWaterMark - windowSize@.
-- Verified:    Sequence numbers below the high-water floor are rejected.
validateSequence :: SeqWindow -> Word32 -> IO Bool
validateSequence ref seqNum = atomicModifyIORef' ref $ \s ->
    if Set.member seqNum s
        then (s, False)  -- duplicate
        else
            -- High-water mark: the maximum sequence number seen so far.
            -- Reject anything more than seqWindowSize below it.
            let highWater = if Set.null s then 0 else Set.findMax s
                floor'    = if highWater > fromIntegral seqWindowSize
                            then highWater - fromIntegral seqWindowSize
                            else 0
            in if seqNum < floor'
               then (s, False)  -- below high-water floor
               else let s' = Set.insert seqNum s
                        s'' = if Set.size s' > seqWindowSize
                              then Set.deleteMin s'
                              else s'
                    in (s'', True)
