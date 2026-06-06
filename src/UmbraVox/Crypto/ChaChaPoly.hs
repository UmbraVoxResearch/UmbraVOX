{-# LANGUAGE ForeignFunctionInterface #-}
-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "CRYPTO-012" #-} ChaCha20-Poly1305 AEAD (RFC 8439 §2.8)
--
-- Authenticated Encryption with Associated Data constructed from the
-- ChaCha20 stream cipher and the Poly1305 one-time authenticator.
--
-- Finding    M10.1.7 — The Noise transport layer used ChaCha20+HMAC-SHA256
--            with separate encryption and MAC keys.  HMAC-SHA256 is a
--            general-purpose PRF and does not provide the nonce-committing
--            or key-committing properties of Poly1305 in the RFC 8439
--            construction.  Additionally, the separate-key split doubled
--            the key material required in NoiseState, increasing the
--            serialised state size and complicating key derivation.
-- Vulnerability: Using separate HMAC keys departs from the standard
--            ChaCha20-Poly1305 AEAD defined in RFC 8439 and Noise spec
--            §11.1.  A non-standard AEAD is harder to audit, more likely
--            to contain composition errors, and incompatible with
--            reference Noise implementations.
-- Fix:       Implement RFC 8439 §2.8 ChaCha20-Poly1305 AEAD:
--            (1) derive the Poly1305 one-time key from chacha20Block(k,n,0);
--            (2) encrypt at counter=1; (3) compute the Poly1305 tag over
--            pad16(aad)||pad16(ct)||len64LE(aad)||len64LE(ct);
--            (4) decrypt only after constant-time tag verification.
--            Replace the HMAC-SHA256 transport MAC in Noise with this AEAD.
-- Verified:  chachaPolyEncrypt/chachaPolyDecrypt pass RFC 8439 §2.8.2
--            test vector.  chachaPolyDecrypt returns Nothing on any
--            single-bit flip in the ciphertext or tag.  Round-trip
--            fidelity is preserved for all message lengths 0–65535 bytes.
--
-- Finding    M38.4.1 — ChaChaPoly.hs called reference Haskell poly1305 and
--            chacha20Block/chacha20Encrypt internally (NOT constant-time).
--            This was missed during M38.4 because ChaChaPoly is a composition
--            layer and does not directly import Crypto.SHA256 etc.; the grep
--            for direct Crypto.* imports did not catch it.
-- Vulnerability: All Noise transport traffic encrypted or decrypted via this
--            module used variable-time Haskell implementations despite M38.4
--            closing 47 other constant-time gaps.
-- Fix:       Wire EverCrypt_Chacha20Poly1305 (HACL*, F*-proved, formally
--            constant-time by type system, pure-software — no hardware
--            requirement) via csrc/hacl/bridge_chacha20poly1305.c.  Use
--            unsafePerformIO to preserve the pure API (both operations are
--            deterministic functions of their inputs; no observable side
--            effects; NOINLINE prevents CSE/floating).  Remove all reference
--            Haskell crypto imports from this module.
-- Verified:  Module compiles and links against bridge_chacha20poly1305.c.
--            Pure API preserved; no cascade to the 5 production callers
--            (Network/Noise.hs, Network/Noise/Handshake.hs, Network/Relay.hs,
--            Protocol/Handshake.hs, Protocol/WireFormat.hs).
--
-- See: doc/spec/crypto.md
module UmbraVox.Crypto.ChaChaPoly
    ( chachaPolyEncrypt
    , chachaPolyDecrypt
    , chachaPolyEncryptSafe
    , chachaPolyDecryptSafe
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------
-- FFI: EverCrypt ChaCha20-Poly1305 (csrc/hacl/bridge_chacha20poly1305.c)
-- INTERIM PRODUCTION: superseded by csrc/extracted/chacha20poly1305.c when M36B.3+4 land.
------------------------------------------------------------------------

foreign import ccall "umbravox_chacha20poly1305_link_probe"
    c_chacha20poly1305_link_probe :: IO CInt

foreign import ccall safe "umbravox_chacha20poly1305_encrypt"
    c_chacha20poly1305_encrypt
        :: Ptr Word8            -- key (32 bytes)
        -> Ptr Word8            -- nonce (12 bytes)
        -> Ptr Word8 -> Word32  -- aad, aad_len
        -> Ptr Word8 -> Word32  -- plaintext, pt_len
        -> Ptr Word8            -- cipher_out (pt_len bytes, caller-allocated)
        -> Ptr Word8            -- tag_out (16 bytes, caller-allocated)
        -> IO ()

foreign import ccall safe "umbravox_chacha20poly1305_decrypt"
    c_chacha20poly1305_decrypt
        :: Ptr Word8            -- key (32 bytes)
        -> Ptr Word8            -- nonce (12 bytes)
        -> Ptr Word8 -> Word32  -- aad, aad_len
        -> Ptr Word8 -> Word32  -- ciphertext, ct_len
        -> Ptr Word8            -- tag (16 bytes)
        -> Ptr Word8            -- plain_out (ct_len bytes, caller-allocated)
        -> IO Word32            -- 0 = success, non-zero = auth failure

-- | Confirm the EverCrypt ChaCha20-Poly1305 bridge is linked.
_ffiLinked :: IO Bool
_ffiLinked = (/= 0) <$> c_chacha20poly1305_link_probe

------------------------------------------------------------------------
-- RFC 8439 §2.8 — ChaCha20-Poly1305 AEAD Construction
-- Implemented via EverCrypt_Chacha20Poly1305 (formally verified, CT by proof)
------------------------------------------------------------------------

-- | Encrypt @plaintext@ under @key@ and @nonce@ with optional
-- associated data @aad@.
--
-- Returns @(ciphertext, tag)@ where:
--
-- * @ciphertext@ has the same length as @plaintext@
-- * @tag@        is 16 bytes (Poly1305 output)
--
-- Parameters:
--
-- * @key@   — 32 bytes
-- * @nonce@ — 12 bytes (unique per (key, message) pair)
-- * @aad@   — arbitrary length (may be empty)
-- * @plaintext@ — arbitrary length
-- | Encrypt (safe variant).
-- Returns @Left msg@ on invalid input instead of calling 'error'.
--
-- Uses EverCrypt_Chacha20Poly1305 (HACL*, F*-proved, formally constant-time).
-- unsafePerformIO is safe here: encrypt is a deterministic pure function of
-- its inputs (key, nonce, aad, plaintext) → (ciphertext, tag) with no
-- observable side effects.  NOINLINE prevents GHC from CSE-ing calls with
-- different inputs.
chachaPolyEncryptSafe
    :: ByteString   -- ^ key (32 bytes)
    -> ByteString   -- ^ nonce (12 bytes)
    -> ByteString   -- ^ associated data (aad)
    -> ByteString   -- ^ plaintext
    -> Either String (ByteString, ByteString)  -- ^ (ciphertext, 16-byte tag)
chachaPolyEncryptSafe !key !nonce !aad !plaintext
    | BS.length key   /= 32 = Left "chachaPolyEncrypt: key must be 32 bytes"
    | BS.length nonce /= 12 = Left "chachaPolyEncrypt: nonce must be 12 bytes"
    | otherwise = Right $ unsafePerformIO $ do
        let !ptLen = BS.length plaintext
        allocaBytes (max 1 ptLen) $ \cipherPtr ->
          allocaBytes 16           $ \tagPtr    ->
          BSU.unsafeUseAsCStringLen key       $ \(keyPtr,   _)      ->
          BSU.unsafeUseAsCStringLen nonce     $ \(noncePtr, _)      ->
          BSU.unsafeUseAsCStringLen aad       $ \(aadPtr,   aadLen) ->
          BSU.unsafeUseAsCStringLen plaintext $ \(ptPtr,    _)      -> do
              c_chacha20poly1305_encrypt
                  (castPtr keyPtr)
                  (castPtr noncePtr)
                  (castPtr aadPtr) (fromIntegral aadLen)
                  (castPtr ptPtr)  (fromIntegral ptLen)
                  cipherPtr
                  tagPtr
              !ct  <- BS.packCStringLen (castPtr cipherPtr, ptLen)
              !tag <- BS.packCStringLen (castPtr tagPtr,    16)
              pure (ct, tag)
{-# NOINLINE chachaPolyEncryptSafe #-}

chachaPolyEncrypt
    :: ByteString   -- ^ key (32 bytes)
    -> ByteString   -- ^ nonce (12 bytes)
    -> ByteString   -- ^ associated data (aad)
    -> ByteString   -- ^ plaintext
    -> (ByteString, ByteString)  -- ^ (ciphertext, 16-byte tag)
chachaPolyEncrypt !key !nonce !aad !plaintext =
    case chachaPolyEncryptSafe key nonce aad plaintext of
        Right result -> result
        Left msg     -> error msg

-- | Decrypt @ciphertext@ and verify @tag@; returns @Just plaintext@ on
-- success, @Nothing@ on authentication failure.
--
-- Tag verification is performed inside EverCrypt_Chacha20Poly1305_aead_decrypt
-- in constant time (formally verified by HACL* team).
--
-- Parameters identical to 'chachaPolyEncrypt'.
-- | Decrypt (safe variant).
-- Returns @Left msg@ on invalid key/nonce, @Right Nothing@ on auth failure,
-- @Right (Just plaintext)@ on success.
--
-- unsafePerformIO is safe here: decrypt is a deterministic pure function of
-- its inputs → Maybe plaintext with no observable side effects.
-- NOINLINE prevents GHC from CSE-ing calls with different inputs.
chachaPolyDecryptSafe
    :: ByteString          -- ^ key (32 bytes)
    -> ByteString          -- ^ nonce (12 bytes)
    -> ByteString          -- ^ associated data (aad)
    -> ByteString          -- ^ ciphertext
    -> ByteString          -- ^ tag (16 bytes)
    -> Either String (Maybe ByteString)
chachaPolyDecryptSafe !key !nonce !aad !ciphertext !tag
    | BS.length key   /= 32 = Left "chachaPolyDecrypt: key must be 32 bytes"
    | BS.length nonce /= 12 = Left "chachaPolyDecrypt: nonce must be 12 bytes"
    | BS.length tag   /= 16 = Right Nothing
    | otherwise = Right $ unsafePerformIO $ do
        let !ctLen = BS.length ciphertext
        allocaBytes (max 1 ctLen) $ \plainPtr  ->
          BSU.unsafeUseAsCStringLen key        $ \(keyPtr,   _)      ->
          BSU.unsafeUseAsCStringLen nonce      $ \(noncePtr, _)      ->
          BSU.unsafeUseAsCStringLen aad        $ \(aadPtr,   aadLen) ->
          BSU.unsafeUseAsCStringLen ciphertext $ \(ctPtr,    _)      ->
          BSU.unsafeUseAsCStringLen tag        $ \(tagPtr,   _)      -> do
              rc <- c_chacha20poly1305_decrypt
                        (castPtr keyPtr)
                        (castPtr noncePtr)
                        (castPtr aadPtr) (fromIntegral aadLen)
                        (castPtr ctPtr)  (fromIntegral ctLen)
                        (castPtr tagPtr)
                        plainPtr
              if rc == 0
                  then do
                      !pt <- BS.packCStringLen (castPtr plainPtr, ctLen)
                      pure (Just pt)
                  else pure Nothing
{-# NOINLINE chachaPolyDecryptSafe #-}

chachaPolyDecrypt
    :: ByteString          -- ^ key (32 bytes)
    -> ByteString          -- ^ nonce (12 bytes)
    -> ByteString          -- ^ associated data (aad)
    -> ByteString          -- ^ ciphertext
    -> ByteString          -- ^ tag (16 bytes)
    -> Maybe ByteString    -- ^ plaintext, or Nothing on auth failure
chachaPolyDecrypt !key !nonce !aad !ciphertext !tag =
    case chachaPolyDecryptSafe key nonce aad ciphertext tag of
        Right result -> result
        Left msg     -> error msg
