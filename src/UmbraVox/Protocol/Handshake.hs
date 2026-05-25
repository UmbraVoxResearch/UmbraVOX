-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.Protocol.Handshake
    ( handshakeInitiator, handshakeResponder
    , genIdentity, genSignedPreKey, genPQPreKey
    , serializeBundle, deserializeBundle
    , recvBundle, recvInitialMessage
    , bundleVersion
    , bsSlice, putW32BE, getW32BE, getW32BESafe, fingerprint, timestamp
    -- * MitM protection: key confirmation (M23.1.1j)
    , keyConfirmMAC
    , verifyKeyConfirmation
    ) where

import qualified Data.ByteString as BS
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Word (Word8, Word32)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad (unless)
import UmbraVox.Chat.Session (ChatSession, initChatSession, initChatSessionBob)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Crypto.MLKEM (mlkemKeyGen,
    MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..))
import UmbraVox.Crypto.Ed25519 (ed25519Sign)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.SHA256 (sha256)
import UmbraVox.Crypto.Signal.PQXDH
    (PQPreKeyBundle(..), PQXDHResult(..), pqxdhInitiate, pqxdhRespond)
import UmbraVox.Crypto.ChaChaPoly (chachaPolyEncrypt, chachaPolyDecrypt)
import UmbraVox.Crypto.Curve25519 (x25519)
import UmbraVox.Crypto.HKDF (hkdfSHA256)
import UmbraVox.Crypto.SecureBytes (toByteString)
import UmbraVox.Crypto.Signal.X3DH
    (KeyPair(..), IdentityKey(..), generateIdentityKey, generateKeyPair,
     signPreKey)
import UmbraVox.Network.TransportClass (AnyTransport, anySend, anyRecv)
import UmbraVox.Protocol.CBOR (encodeMessage)

-- Helpers -----------------------------------------------------------------
timestamp :: IO String
timestamp = formatTime defaultTimeLocale "%H:%M" <$> getCurrentTime

fingerprint :: BS.ByteString -> String
fingerprint bs = concatMap hex2 (BS.unpack (BS.take 8 bs)) where
    hex2 w = [hexC (w `shiftR` 4), hexC (w .&. 0x0f), ':']
    hexC n | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
           | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)

bsSlice :: Int -> Int -> BS.ByteString -> BS.ByteString
bsSlice off len = BS.take len . BS.drop off

putW32BE :: Word32 -> BS.ByteString
putW32BE w = BS.pack
    [ fromIntegral (w `shiftR` 24 .&. 0xff), fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR`  8 .&. 0xff), fromIntegral (w             .&. 0xff) ]

getW32BE :: BS.ByteString -> Word32
getW32BE bs = (fromIntegral (BS.index bs 0) `shiftL` 24)
    + (fromIntegral (BS.index bs 1) `shiftL` 16)
    + (fromIntegral (BS.index bs 2) `shiftL` 8)
    + fromIntegral (BS.index bs 3)

getW32BESafe :: BS.ByteString -> Maybe Word32
getW32BESafe bs
    | BS.length bs < 4 = Nothing
    | otherwise = Just (getW32BE bs)

-- PQXDH key generation ----------------------------------------------------
genIdentity :: IO IdentityKey
genIdentity = do
    edSec <- randomBytes 32; xSec <- randomBytes 32
    generateIdentityKey edSec xSec

genSignedPreKey :: IdentityKey -> IO (KeyPair, BS.ByteString)
genSignedPreKey ik = do
    spkSec <- randomBytes 32
    spk <- generateKeyPair spkSec
    sig <- signPreKey ik (kpPublic spk)
    pure (spk, sig)

genPQPreKey :: IO (MLKEMEncapKey, MLKEMDecapKey)
genPQPreKey = do
    d <- randomBytes 32; z <- randomBytes 32; pure $! mlkemKeyGen d z

-- Prekey bundle wire format -----------------------------------------------
-- Layout (byte offsets):
--   0       Version byte (0x01)                    (1 byte, M23.2.3)
--   1..32   IK_x25519 pub  (32 bytes)
--  33..64   IK_ed25519 pub (32 bytes)
--  65..96   SPK pub        (32 bytes)
--  97..160  SPK sig        (64 bytes)
-- 161..164  PQ encap key length (Word32 BE)
-- 165..164+pqLen  PQ encap key bytes
-- 165+pqLen..164+pqLen+64  PQ key Ed25519 sig (64 bytes, M10.2.1)
-- 229+pqLen  OPK flag+data

-- | Current bundle wire-format version (M23.2.3).
bundleVersion :: Word8
bundleVersion = 0x01

serializeBundle :: IdentityKey -> BS.ByteString -> BS.ByteString
                -> MLKEMEncapKey -> Maybe BS.ByteString -> IO BS.ByteString
serializeBundle ik spkPub spkSig (MLKEMEncapKey pqpk) mOpk = do
    edSec <- toByteString (ikEd25519Secret ik)
    let !pqSig = ed25519Sign edSec pqpk
    pure $ BS.concat
        [ BS.singleton bundleVersion
        , ikX25519Public ik, ikEd25519Public ik, spkPub, spkSig
        , putW32BE (fromIntegral (BS.length pqpk)), pqpk
        , pqSig
        , maybe (BS.singleton 0x00) (\k -> BS.singleton 0x01 <> k) mOpk ]

deserializeBundle :: BS.ByteString -> Maybe PQPreKeyBundle
deserializeBundle bs
    | BS.length bs < 166 = Nothing          -- 1 (version) + 165 minimum payload
    | BS.index bs 0 /= bundleVersion = Nothing  -- reject unknown version (M23.2.3)
    | otherwise =
        let !body     = BS.drop 1 bs        -- strip version byte
            !pqLen    = fromIntegral (getW32BE (bsSlice 160 4 body)) :: Int
        -- Validate pqLen before any arithmetic to prevent integer overflow.
        -- Required total: 164 (fixed header) + pqLen (PQ key) + 64 (PQ sig) + 1 (OPK flag)
        in if pqLen < 0 || pqLen > BS.length body - 164
              || BS.length body < 164 + pqLen + 64 + 1
           then Nothing
           else let !pqSigOff = 164 + pqLen
                    !opkOff   = pqSigOff + 64
                    !rest     = BS.drop opkOff body
                    -- M23.2.4: validate OPK length before extraction
                    decOpk r | BS.null r         = Nothing
                             | BS.index r 0 == 1 = if BS.length r >= 33
                                                   then Just (BS.take 32 (BS.drop 1 r))
                                                   else Nothing  -- truncated OPK
                             | otherwise         = Nothing
                in Just PQPreKeyBundle
               { pqpkbIdentityKey     = bsSlice 0   32 body
               , pqpkbIdentityEd25519 = bsSlice 32  32 body
               , pqpkbSignedPreKey    = bsSlice 64  32 body
               , pqpkbSPKSignature    = bsSlice 96  64 body
               , pqpkbPQPreKey        = MLKEMEncapKey (bsSlice 164 pqLen body)
               , pqpkbPQKeySignature  = bsSlice pqSigOff 64 body
               , pqpkbOneTimePreKey   = decOpk rest }

-- Maximum receive sizes (guard against allocation-bomb attacks) -----------
maxBundleSize :: Int
maxBundleSize = 2048  -- ML-KEM bundle is ~1200 bytes

maxInitialMessageSize :: Int
maxInitialMessageSize = 2048

-- PQXDH Handshake ---------------------------------------------------------
handshakeInitiator :: AnyTransport -> IdentityKey -> IO ChatSession
handshakeInitiator t aliceIK = do
    bundle <- recvBundle t
    ekRand <- randomBytes 32; mlkemRand <- randomBytes 32
    mResult <- pqxdhInitiate aliceIK bundle ekRand mlkemRand
    result <- case mResult of
        Nothing -> fail "PQXDH: SPK signature verification failed"
        Just r  -> pure r
    let MLKEMCiphertext ctBS = pqxdhPQCiphertext result
    -- M27.2.4: Wrap the initiator's identity key using the ephemeral DH
    -- shared secret so it is not visible in cleartext on the wire.
    --
    -- Privacy rationale: Without wrapping, a passive observer can read
    -- Alice's long-term identity key from the initial PQXDH message and
    -- correlate her across sessions.  By encrypting IK under DH(ek, SPK),
    -- only Bob (who holds the SPK secret) can recover Alice's identity.
    ek <- generateKeyPair ekRand
    ekSecretBS <- toByteString (kpSecret ek)
    encIK <- case x25519 ekSecretBS (pqpkbSignedPreKey bundle) of
        Nothing -> fail "PQXDH: ephemeral DH for IK wrapping returned all-zero"
        Just dhSecret -> do
            let !wrapKey = deriveIKWrapKey dhSecret
                !wrapNonce = BS.replicate 12 0  -- single-use key, fixed nonce is safe
                (!ikCt, !ikTag) = chachaPolyEncrypt wrapKey wrapNonce BS.empty
                                      (ikX25519Public aliceIK)
            pure (ikCt <> ikTag)
    let !initMsg = BS.concat
            [ encIK, pqxdhEphemeralKey result
            , putW32BE (fromIntegral (BS.length ctBS)), ctBS ]
    anySend t . encodeMessage $ initMsg
    -- M27.6.7: Key confirmation — exchange MACs derived from the shared
    -- secret to detect MitM relay attacks.
    let !transcriptHash = sha256 initMsg
        !ourMAC = keyConfirmMAC (pqxdhSharedSecret result) "initiator" transcriptHash
    anySend t . encodeMessage $ ourMAC
    peerMACMsg <- anyRecv t 36  -- 4-byte length + 32-byte MAC
    let !peerMAC = BS.drop 4 peerMACMsg
    unless (verifyKeyConfirmation (pqxdhSharedSecret result) "responder"
                                  transcriptHash peerMAC) $
        fail "PQXDH: key confirmation failed (possible MitM detected)"
    xSecretBS <- toByteString (ikX25519Secret aliceIK)
    mSession <- initChatSession (pqxdhSharedSecret result)
                                xSecretBS (pqpkbSignedPreKey bundle)
    case mSession of
        Nothing -> fail "PQXDH: ratchet init DH returned all-zero (low-order point rejected)"
        Just s  -> pure s

handshakeResponder :: AnyTransport -> IdentityKey -> (BS.ByteString -> IO Bool) -> IO ChatSession
handshakeResponder t bobIK trustCheck = do
    (spk, spkSig) <- genSignedPreKey bobIK
    (pqEK, pqDK) <- genPQPreKey
    bundleBS <- serializeBundle bobIK (kpPublic spk) spkSig pqEK Nothing
    anySend t . encodeMessage $ bundleBS
    (encAliceIK, aliceEKPub, pqCt) <- recvInitialMessage t
    -- M27.2.4: Unwrap the initiator's encrypted identity key using
    -- DH(spkSecret, aliceEphemeralPub).  The identity key was encrypted
    -- by the initiator to prevent passive observers from learning it.
    spkSecretBS <- toByteString (kpSecret spk)
    aliceIKPub <- case x25519 spkSecretBS aliceEKPub of
        Nothing -> fail "PQXDH: ephemeral DH for IK unwrapping returned all-zero"
        Just dhSecret -> do
            let !wrapKey = deriveIKWrapKey dhSecret
                !wrapNonce = BS.replicate 12 0
                -- encAliceIK is 32 bytes ciphertext + 16 bytes Poly1305 tag
                !ikCiphertext = BS.take 32 encAliceIK
                !ikTag = BS.drop 32 encAliceIK
            case chachaPolyDecrypt wrapKey wrapNonce BS.empty ikCiphertext ikTag of
                Nothing -> fail "PQXDH: identity key unwrapping failed (auth failure)"
                Just ik -> pure ik
    -- M23.3.5: check trust before expensive PQXDH computation
    trusted <- trustCheck aliceIKPub
    unless trusted $ fail "Connection rejected: peer not trusted"
    mShared <- pqxdhRespond bobIK spkSecretBS Nothing pqDK
                            aliceIKPub aliceEKPub pqCt
    shared <- case mShared of
                  Nothing -> fail "PQXDH: DH returned all-zero (low-order point rejected)"
                  Just s  -> pure s
    -- M27.6.7: Key confirmation — reconstruct the transcript from the
    -- initial message fields and verify the initiator's confirmation MAC,
    -- then send our own.
    -- M27.2.4: The transcript uses the encrypted identity key (as sent on
    -- the wire) so both sides agree on the hash.
    let !ctBS = let MLKEMCiphertext ct = pqCt in ct
        !initMsg = BS.concat
            [ encAliceIK, aliceEKPub
            , putW32BE (fromIntegral (BS.length ctBS)), ctBS ]
        !transcriptHash = sha256 initMsg
    peerMACMsg <- anyRecv t 36  -- 4-byte length + 32-byte MAC
    let !peerMAC = BS.drop 4 peerMACMsg
    unless (verifyKeyConfirmation shared "initiator" transcriptHash peerMAC) $
        fail "PQXDH: key confirmation failed (possible MitM detected)"
    let !ourMAC = keyConfirmMAC shared "responder" transcriptHash
    anySend t . encodeMessage $ ourMAC
    initChatSessionBob shared spkSecretBS

recvBundle :: AnyTransport -> IO PQPreKeyBundle
recvBundle t = do
    lenBs <- anyRecv t 4
    case getW32BESafe lenBs of
        Nothing -> fail "PQXDH: incomplete length header (connection closed)"
        Just len -> do
            let !n = fromIntegral len :: Int
            if n > maxBundleSize
                then fail $ "PQXDH: bundle size " ++ show n
                         ++ " exceeds limit " ++ show maxBundleSize
                else do
                    payload <- anyRecv t n
                    case deserializeBundle payload of
                        Nothing     -> fail "PQXDH: malformed prekey bundle"
                        Just bundle -> pure bundle

-- | Receive the PQXDH initial message from the initiator.
--
-- M27.2.4: The identity key is now encrypted (48 bytes: 32 ciphertext +
-- 16 Poly1305 tag) instead of sent as a 32-byte cleartext public key.
-- Layout: encIK(48) + ephemeralPub(32) + ctLen(4) + ct(N)
recvInitialMessage :: AnyTransport -> IO (BS.ByteString, BS.ByteString, MLKEMCiphertext)
recvInitialMessage t = do
    lenBs <- anyRecv t 4
    case getW32BESafe lenBs of
        Nothing -> fail "PQXDH: incomplete length header (connection closed)"
        Just len -> do
            let !n = fromIntegral len :: Int
            if n > maxInitialMessageSize
                then fail $ "PQXDH: initial message size " ++ show n
                         ++ " exceeds limit " ++ show maxInitialMessageSize
                else do
                    payload <- anyRecv t n
                    -- encIK(48) + ephemeralPub(32) = 80 bytes before ctLen
                    case getW32BESafe (bsSlice 80 4 payload) of
                        Nothing -> fail "PQXDH: incomplete initial message payload"
                        Just ctLen -> do
                            let !ct = fromIntegral ctLen :: Int
                            pure (bsSlice 0 48 payload, bsSlice 48 32 payload,
                                  MLKEMCiphertext (bsSlice 84 ct payload))

------------------------------------------------------------------------
-- Identity key wrapping (M27.2.4)
------------------------------------------------------------------------

-- | Derive a 32-byte wrapping key for encrypting the initiator's identity
-- key in the PQXDH initial message.
--
-- The key is derived via HKDF-SHA-256 from the ephemeral DH shared secret
-- DH(ephemeralKey, signedPreKey), which is known only to Alice (who holds
-- the ephemeral secret) and Bob (who holds the SPK secret).
deriveIKWrapKey :: BS.ByteString -> BS.ByteString
deriveIKWrapKey dhSecret =
    hkdfSHA256 (BS.replicate 32 0) dhSecret "UmbraVox_IKWrap_v1" 32

------------------------------------------------------------------------
-- MitM protection: key confirmation MAC (M23.1.1j)
------------------------------------------------------------------------

-- | Generate a key confirmation MAC after handshake completes.
--
-- Both sides exchange a confirmation MAC derived from the shared token
-- material, their role ("initiator" or "responder"), and the handshake
-- transcript hash.  A MitM who relays two independent handshakes will
-- produce different token material on each leg, so the confirmation
-- MACs will not match when compared by the legitimate peers.
--
-- @
--   confirmMAC = HMAC-SHA-256(tokenMaterial,
--       "UmbraVox_TokenConfirm_" <> role <> handshakeHash)
-- @
keyConfirmMAC :: BS.ByteString  -- ^ Token material (shared secret from handshake)
              -> BS.ByteString  -- ^ Role: "initiator" or "responder"
              -> BS.ByteString  -- ^ Handshake transcript hash
              -> BS.ByteString  -- ^ 32-byte confirmation MAC
keyConfirmMAC tokenMaterial role handshakeHash =
    hmacSHA256 tokenMaterial ("UmbraVox_TokenConfirm_" <> role <> handshakeHash)

-- | Verify the peer's key confirmation MAC.
--
-- After sending our own confirmation MAC, we receive the peer's MAC and
-- recompute what it should be using the peer's role.  The comparison is
-- constant-time to prevent timing side-channels.
--
-- Returns 'True' if the peer's MAC matches (no MitM detected).
-- Returns 'False' if mismatch (MitM detected -- session must be rejected).
verifyKeyConfirmation :: BS.ByteString  -- ^ Token material
                      -> BS.ByteString  -- ^ Peer's role ("initiator" or "responder")
                      -> BS.ByteString  -- ^ Handshake transcript hash
                      -> BS.ByteString  -- ^ Received MAC from peer
                      -> Bool
verifyKeyConfirmation tokenMaterial peerRole handshakeHash receivedMAC =
    let !expectedMAC = keyConfirmMAC tokenMaterial peerRole handshakeHash
    in constantEq expectedMAC receivedMAC
