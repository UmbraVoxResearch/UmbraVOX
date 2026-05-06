-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.Handshake
    ( handshakeInitiator, handshakeResponder
    , genIdentity, genSignedPreKey, genPQPreKey
    , serializeBundle, deserializeBundle
    , recvBundle, recvInitialMessage
    , bsSlice, putW32BE, getW32BE, fingerprint, timestamp
    ) where

import qualified Data.ByteString as BS
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Word (Word32)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad (unless)
import UmbraVox.Chat.Session (ChatSession, initChatSession, initChatSessionBob)
import UmbraVox.Crypto.MLKEM (mlkemKeyGen,
    MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..))
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.Signal.PQXDH
    (PQPreKeyBundle(..), PQXDHResult(..), pqxdhInitiate, pqxdhRespond)
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

-- PQXDH key generation ----------------------------------------------------
genIdentity :: IO IdentityKey
genIdentity = do
    edSec <- randomBytes 32; xSec <- randomBytes 32
    pure $! generateIdentityKey edSec xSec

genSignedPreKey :: IdentityKey -> IO (KeyPair, BS.ByteString)
genSignedPreKey ik = do
    spkSec <- randomBytes 32
    let !spk = generateKeyPair spkSec; !sig = signPreKey ik (kpPublic spk)
    pure (spk, sig)

genPQPreKey :: IO (MLKEMEncapKey, MLKEMDecapKey)
genPQPreKey = do
    d <- randomBytes 32; z <- randomBytes 32; pure $! mlkemKeyGen d z

-- Prekey bundle wire format -----------------------------------------------
serializeBundle :: IdentityKey -> BS.ByteString -> BS.ByteString
                -> MLKEMEncapKey -> Maybe BS.ByteString -> BS.ByteString
serializeBundle ik spkPub spkSig (MLKEMEncapKey pqpk) mOpk = BS.concat
    [ ikX25519Public ik, ikEd25519Public ik, spkPub, spkSig
    , putW32BE (fromIntegral (BS.length pqpk)), pqpk
    , maybe (BS.singleton 0x00) (\k -> BS.singleton 0x01 <> k) mOpk ]

deserializeBundle :: BS.ByteString -> Maybe PQPreKeyBundle
deserializeBundle bs
    | BS.length bs < 165 = Nothing
    | otherwise =
        let !pqLen = fromIntegral (getW32BE (bsSlice 160 4 bs)) :: Int
            !rest  = BS.drop (164 + pqLen) bs
            decOpk r | BS.null r         = Nothing
                     | BS.index r 0 == 1 = Just (BS.take 32 (BS.drop 1 r))
                     | otherwise         = Nothing
        in if BS.length bs < 164 + pqLen + 1 then Nothing
           else Just PQPreKeyBundle
               { pqpkbIdentityKey     = bsSlice 0  32 bs
               , pqpkbIdentityEd25519 = bsSlice 32 32 bs
               , pqpkbSignedPreKey    = bsSlice 64 32 bs
               , pqpkbSPKSignature    = bsSlice 96 64 bs
               , pqpkbPQPreKey        = MLKEMEncapKey (bsSlice 164 pqLen bs)
               , pqpkbOneTimePreKey   = decOpk rest }

-- PQXDH Handshake ---------------------------------------------------------
handshakeInitiator :: AnyTransport -> IdentityKey -> IO ChatSession
handshakeInitiator t aliceIK = do
    bundle <- recvBundle t
    ekRand <- randomBytes 32; mlkemRand <- randomBytes 32
    result <- case pqxdhInitiate aliceIK bundle ekRand mlkemRand of
        Nothing -> fail "PQXDH: SPK signature verification failed"
        Just r  -> pure r
    let MLKEMCiphertext ctBS = pqxdhPQCiphertext result
    anySend t . encodeMessage $ BS.concat
        [ ikX25519Public aliceIK, pqxdhEphemeralKey result
        , putW32BE (fromIntegral (BS.length ctBS)), ctBS ]
    initChatSession (pqxdhSharedSecret result)
                    (ikX25519Secret aliceIK) (pqpkbSignedPreKey bundle)

handshakeResponder :: AnyTransport -> IdentityKey -> (BS.ByteString -> IO Bool) -> IO ChatSession
handshakeResponder t bobIK trustCheck = do
    (spk, spkSig) <- genSignedPreKey bobIK
    (pqEK, pqDK) <- genPQPreKey
    anySend t . encodeMessage $ serializeBundle bobIK (kpPublic spk) spkSig pqEK Nothing
    (aliceIKPub, aliceEKPub, pqCt) <- recvInitialMessage t
    trusted <- trustCheck aliceIKPub
    unless trusted $ fail "Connection rejected: peer not trusted"
    let !shared = pqxdhRespond bobIK (kpSecret spk) Nothing pqDK
                               aliceIKPub aliceEKPub pqCt
    initChatSessionBob shared (kpSecret spk)

recvBundle :: AnyTransport -> IO PQPreKeyBundle
recvBundle t = do
    lenBs <- anyRecv t 4
    payload <- anyRecv t (fromIntegral (getW32BE lenBs))
    case deserializeBundle payload of
        Nothing     -> fail "PQXDH: malformed prekey bundle"
        Just bundle -> pure bundle

recvInitialMessage :: AnyTransport -> IO (BS.ByteString, BS.ByteString, MLKEMCiphertext)
recvInitialMessage t = do
    lenBs <- anyRecv t 4
    payload <- anyRecv t (fromIntegral (getW32BE lenBs))
    let !ctLen = fromIntegral (getW32BE (bsSlice 64 4 payload)) :: Int
    pure (bsSlice 0 32 payload, bsSlice 32 32 payload,
          MLKEMCiphertext (bsSlice 68 ctLen payload))
