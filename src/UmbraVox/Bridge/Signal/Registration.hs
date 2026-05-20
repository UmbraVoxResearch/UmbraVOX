-- SPDX-License-Identifier: Apache-2.0
-- | Signal linked device registration flow (M19.3.4)
--
-- Implements the provisioning protocol used by Signal Desktop to register
-- as a linked device.  The flow:
--
--   1. Generate a provisioning X25519 keypair
--   2. Connect to Signal-Server provisioning WebSocket (stub — M19.3.5)
--   3. Display QR code containing provisioning public key + server URL
--   4. Primary device scans QR and sends encrypted provisioning data
--   5. Linked device receives: identity key, phone number, profile key
--   6. Register with Signal-Server by uploading prekeys (stub — M19.3.5)
--
-- Crypto operations (key generation, signing, HKDF) are real.
-- Network operations (WebSocket, HTTP) are stubs until the Signal-Server
-- VM is running.
module UmbraVox.Bridge.Signal.Registration
    ( RegistrationState(..)
    , RegistrationPhase(..)
    , ProvisioningData(..)
    , RegistrationError(..)
    , beginRegistration
    , generateProvisioningQR
    , completeRegistration
    , generatePreKeyBundle
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)

import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.Ed25519 (ed25519Sign)
import UmbraVox.Crypto.HKDF (hkdfExpand, hkdfExtract)
import UmbraVox.Crypto.Random (randomBytes)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Where in the provisioning state machine we are.
data RegistrationPhase
    = PhaseAwaitingQRScan
      -- ^ QR code generated, waiting for primary device to scan.
    | PhaseAwaitingProvisionMessage
      -- ^ Primary scanned QR; waiting for encrypted provisioning data.
    | PhaseProvisioned
      -- ^ Provisioning data received; ready for prekey upload.
    | PhaseComplete
      -- ^ Prekeys uploaded; linked device is fully registered.
    deriving stock (Show, Eq)

-- | Mutable state for an in-progress linked device registration.
data RegistrationState = RegistrationState
    { rsProvisioningPrivKey :: !ByteString
      -- ^ X25519 private key (32 bytes, clamped).
    , rsProvisioningPubKey  :: !ByteString
      -- ^ X25519 public key (32 bytes).
    , rsServerUrl           :: !String
      -- ^ Signal-Server base URL (e.g. "https://signal.example.org").
    , rsPhase               :: !RegistrationPhase
      -- ^ Current position in the provisioning state machine.
    , rsProvisioningUuid    :: !ByteString
      -- ^ Provisioning UUID assigned by server (empty until WebSocket
      --   provides it; stub for now).
    } deriving stock (Show)

-- | Data received from the primary device during provisioning.
data ProvisioningData = ProvisioningData
    { pdIdentityKeyPriv :: !ByteString
      -- ^ Identity private key (32 bytes, from primary device).
    , pdIdentityKeyPub  :: !ByteString
      -- ^ Identity public key (32 bytes, from primary device).
    , pdPhoneNumber     :: !String
      -- ^ E.164 phone number associated with the account.
    , pdProfileKey      :: !ByteString
      -- ^ Profile key (32 bytes) for encrypted profile access.
    , pdDeviceId        :: !Word32
      -- ^ Device ID assigned by the server.
    , pdProvisioningCode :: !ByteString
      -- ^ Provisioning code for confirming registration with server.
    } deriving stock (Show)

-- | Failure modes for the registration flow.
data RegistrationError
    = WrongPhase RegistrationPhase RegistrationPhase
      -- ^ Expected phase vs actual phase.
    | ProvisioningDecryptFailed String
      -- ^ Could not decrypt the provisioning message from primary.
    | ProvisioningParseFailed String
      -- ^ Decrypted payload did not have expected structure.
    | ServerError String
      -- ^ Signal-Server returned an error (stub until M19.3.5).
    | KeyGenerationFailed String
      -- ^ Failed to generate cryptographic keys.
    | InvalidProvisioningData String
      -- ^ Provisioning data failed validation.
    deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Registration flow
------------------------------------------------------------------------

-- | Begin the linked device registration flow.
--
-- Generates a fresh X25519 provisioning keypair and returns the initial
-- 'RegistrationState'.  The caller should next call
-- 'generateProvisioningQR' to get the QR code content.
beginRegistration :: String           -- ^ Signal-Server base URL
                  -> IO RegistrationState
beginRegistration serverUrl = do
    -- Generate 32 random bytes for the X25519 private key.
    privKey <- randomBytes 32
    -- Derive public key: pub = X25519(priv, basepoint).
    let pubKey = case x25519 privKey x25519Basepoint of
                   Just pk -> pk
                   Nothing -> BS.replicate 32 0  -- should never happen
    pure RegistrationState
        { rsProvisioningPrivKey = privKey
        , rsProvisioningPubKey  = pubKey
        , rsServerUrl           = serverUrl
        , rsPhase               = PhaseAwaitingQRScan
        , rsProvisioningUuid    = BS.empty
        }

-- | Generate the provisioning QR code content string.
--
-- Signal Desktop uses the URI format:
--   tsdevice:/?uuid=<uuid>&pub_key=<base64-pubkey>
--
-- Since we do not yet have a WebSocket connection to obtain the
-- provisioning UUID from the server, the UUID field is a placeholder.
-- The public key is the raw hex encoding of the provisioning public key.
generateProvisioningQR :: RegistrationState -> String
generateProvisioningQR rs =
    "tsdevice:/?uuid=" ++ uuidPart ++ "&pub_key=" ++ hexPubKey
  where
    -- Placeholder UUID until WebSocket provisioning is wired up.
    uuidPart   = if BS.null (rsProvisioningUuid rs)
                   then "pending"
                   else bytesToHex (rsProvisioningUuid rs)
    hexPubKey  = bytesToHex (rsProvisioningPubKey rs)

-- | Complete registration by decrypting the provisioning message sent
-- by the primary device.
--
-- In the real flow the primary device:
--   1. Fetches our provisioning public key from the QR code
--   2. Generates an ephemeral X25519 keypair
--   3. Computes shared secret = X25519(ephemeral_priv, provisioning_pub)
--   4. Derives AES-256-CBC key + HMAC key via HKDF
--   5. Encrypts provisioning data (identity key, phone, profile key)
--   6. Sends it over the provisioning WebSocket
--
-- STUB: The actual decryption and parsing of the provisioning protobuf
-- is deferred until WebSocket transport (M19.3.5) delivers real data.
-- For now, this validates state-machine phase and returns a placeholder
-- error indicating the network path is not yet wired.
completeRegistration :: RegistrationState
                     -> ByteString       -- ^ Encrypted provisioning message
                     -> Either RegistrationError ProvisioningData
completeRegistration rs encryptedMsg
    | rsPhase rs /= PhaseAwaitingQRScan
      && rsPhase rs /= PhaseAwaitingProvisionMessage
    = Left (WrongPhase PhaseAwaitingProvisionMessage (rsPhase rs))
    | BS.null encryptedMsg
    = Left (ProvisioningDecryptFailed "empty provisioning message")
    | otherwise
    = -- STUB: Real implementation will:
      --   1. Extract ephemeral public key (first 32 bytes)
      --   2. Compute shared secret via X25519
      --   3. HKDF-derive AES + HMAC keys
      --   4. Verify HMAC, decrypt AES-256-CBC
      --   5. Parse protobuf provisioning message
      --
      -- For now, perform the ECDH to prove the crypto path works,
      -- then return an error since we cannot parse the stub payload.
      let _sharedSecret = case x25519 (rsProvisioningPrivKey rs)
                                      (BS.take 32 encryptedMsg) of
                            Just ss -> ss
                            Nothing -> BS.replicate 32 0
          _prk          = hkdfExtract BS.empty _sharedSecret
          _derivedKey   = hkdfExpand _prk "UmbraVox Provisioning" 64
      in Left (ServerError
          "provisioning WebSocket not yet connected (M19.3.5)")

-- | Generate a prekey bundle for upload to the Signal-Server.
--
-- A linked device must upload:
--   - Signed prekey (Ed25519 signed X25519 public key)
--   - One-time prekeys (batch of X25519 public keys)
--   - Identity public key (from provisioning data)
--
-- Returns (signedPrekey, signedPrekeySignature, oneTimePrekeys).
-- The one-time prekeys are returned as a concatenation of 32-byte
-- public keys (100 keys = 3200 bytes).
generatePreKeyBundle :: ProvisioningData
                     -> IO (ByteString, ByteString, ByteString)
generatePreKeyBundle pd = do
    -- Generate signed prekey: fresh X25519 keypair.
    signedPrePriv <- randomBytes 32
    let signedPrePub = case x25519 signedPrePriv x25519Basepoint of
                         Just pk -> pk
                         Nothing -> BS.replicate 32 0

    -- Sign the prekey with the identity private key (Ed25519).
    let identityPriv = pdIdentityKeyPriv pd
        signature    = ed25519Sign identityPriv signedPrePub

    -- Generate batch of one-time prekeys (100 keys).
    otpks <- generateOneTimePrekeys 100

    pure (signedPrePub, signature, otpks)

-- | Generate a batch of one-time X25519 prekeys.
-- Returns concatenated public keys (each 32 bytes).
generateOneTimePrekeys :: Int -> IO ByteString
generateOneTimePrekeys n = do
    keys <- mapM (\_ -> do
        priv <- randomBytes 32
        let pub = case x25519 priv x25519Basepoint of
                    Just pk -> pk
                    Nothing -> BS.replicate 32 0
        pure pub
        ) [1..n]
    pure (BS.concat keys)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Encode a ByteString as lowercase hex.
bytesToHex :: ByteString -> String
bytesToHex = concatMap toHex . BS.unpack
  where
    toHex b = [hexChar (b `div` 16), hexChar (b `mod` 16)]
    hexChar n
        | n < 10    = toEnum (fromIntegral n + fromEnum '0')
        | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')
