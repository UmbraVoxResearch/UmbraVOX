-- SPDX-License-Identifier: Apache-2.0
-- | Signal linked device registration flow (M19.3.4, M19.3.5)
--
-- Implements the provisioning protocol used by Signal Desktop to register
-- as a linked device.  The flow:
--
--   1. Generate a provisioning X25519 keypair
--   2. Connect to Signal-Server provisioning WebSocket
--   3. Receive server-assigned UUID; display QR code with real UUID + pubkey
--   4. Primary device scans QR and sends encrypted provisioning data
--   5. Decrypt: identity key, phone number, profile key
--   6. Register with Signal-Server by uploading prekeys
--
-- Crypto operations: X25519 ECDH, HKDF-SHA256, AES-256-CBC, HMAC-SHA256.
-- Network transport: WebSocket via 'UmbraVox.Bridge.Signal.WebSocket'.
module UmbraVox.Bridge.Signal.Registration
    ( RegistrationState(..)
    , RegistrationPhase(..)
    , ProvisioningData(..)
    , RegistrationError(..)
    , beginRegistration
    , generateProvisioningQR
    , completeRegistration
    , generatePreKeyBundle
    , runProvisioning
    ) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (stripPrefix)
import Data.Word (Word32, Word64)

import UmbraVox.Bridge.Signal.WebSocket
    ( SignalWSConnection
    , WSMessage(..), WSRequest(..), WSResponse(..)
    , connectSignalWSPath, disconnectSignalWS, recvWSMessage, sendWSMessage
    )
import UmbraVox.Crypto.AES (aesDecrypt)
import UmbraVox.Crypto.ConstantTime (constantEq)
import qualified UmbraVox.Crypto.Generated.FFI.Ed25519Extended as Ed25519FFI
import qualified UmbraVox.Crypto.Generated.FFI.HKDF as HKDFFFI
import qualified UmbraVox.Crypto.Generated.FFI.HMAC as HMACFFI
import qualified UmbraVox.Crypto.Generated.FFI.X25519 as X25519FFI
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Protocol.SignalWire (ProtoField(..), WireType(..), decodeFields, decodeVarint)

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
    }

-- | Redacted Show instance: the provisioning private key is replaced with
-- @"\<redacted\>"@ to prevent accidental exposure in logs or error messages.
--
-- Finding:     M27.6.1 — 'RegistrationState' derived a stock 'Show' instance
--              that printed the raw X25519 provisioning private key.
-- Vulnerability: Any code path that logs or displays a 'RegistrationState'
--              value would leak the provisioning secret key in cleartext.
-- Fix:         Hand-written 'Show' that redacts 'rsProvisioningPrivKey'.
-- Verified:    @show@ on a 'RegistrationState' no longer contains the private key.
instance Show RegistrationState where
    show rs = "RegistrationState {"
        ++ " rsProvisioningPrivKey = <redacted>"
        ++ ", rsProvisioningPubKey = " ++ show (rsProvisioningPubKey rs)
        ++ ", rsServerUrl = " ++ show (rsServerUrl rs)
        ++ ", rsPhase = " ++ show (rsPhase rs)
        ++ ", rsProvisioningUuid = " ++ show (rsProvisioningUuid rs)
        ++ " }"

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
    }

-- | Redacted Show instance: private key and profile key fields are replaced
-- with @"\<redacted\>"@ to prevent accidental exposure in logs or error messages.
--
-- Finding:     M27.6.1 — 'ProvisioningData' derived a stock 'Show' instance
--              that printed the identity private key and profile key.
-- Vulnerability: Any code path that logs or displays a 'ProvisioningData'
--              value would leak live identity secret key material.
-- Fix:         Hand-written 'Show' that redacts 'pdIdentityKeyPriv' and
--              'pdProfileKey'.
-- Verified:    @show@ on a 'ProvisioningData' no longer contains secret fields.
instance Show ProvisioningData where
    show pd = "ProvisioningData {"
        ++ " pdIdentityKeyPriv = <redacted>"
        ++ ", pdIdentityKeyPub = " ++ show (pdIdentityKeyPub pd)
        ++ ", pdPhoneNumber = " ++ show (pdPhoneNumber pd)
        ++ ", pdProfileKey = <redacted>"
        ++ ", pdDeviceId = " ++ show (pdDeviceId pd)
        ++ ", pdProvisioningCode = " ++ show (pdProvisioningCode pd)
        ++ " }"

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
    --
    -- Finding:       M27.6.2 — X25519 failure path silently substituted an
    --                all-zero public key (the low-order identity point on Curve25519).
    -- Vulnerability: A device registered with a zero public key is trivially
    --                compromised: ECDH always produces the predictable zero shared
    --                secret, leaking all derived session keys.
    -- Fix:           Fail loudly via ioError. X25519(random_key, basepoint) cannot
    --                return all-zero in practice (basepoint is high-order); Nothing
    --                indicates a library or hardware fault that must not be swallowed.
    -- Verified:      The Nothing branch now raises ioError; callers catch an
    --                IOException and cannot continue registration with a degenerate key.
    mPubKey <- X25519FFI.x25519 privKey X25519FFI.x25519Basepoint
    pubKey <- case mPubKey of
                Just pk -> pure pk
                Nothing -> ioError (userError
                    "Registration.beginRegistration: X25519(privKey, basepoint) \
                    \returned all-zero — library or hardware fault; aborting key generation")
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
                     -> IO (Either RegistrationError ProvisioningData)
completeRegistration rs encryptedMsg
    | rsPhase rs /= PhaseAwaitingQRScan
      && rsPhase rs /= PhaseAwaitingProvisionMessage
    = pure (Left (WrongPhase PhaseAwaitingProvisionMessage (rsPhase rs)))
    | BS.null encryptedMsg
    = pure (Left (ProvisioningDecryptFailed "empty provisioning message"))
    | otherwise
    = case decodeProvisioningEnvelope encryptedMsg of
        Left e -> pure (Left e)
        Right (ephPub, iv, ciphertext, mac) -> do
            mSharedSecret <- X25519FFI.x25519 (rsProvisioningPrivKey rs) ephPub
            case mSharedSecret of
                Nothing -> pure (Left (ProvisioningDecryptFailed "x25519 ECDH failed"))
                Just sharedSecret -> do
                    prk        <- HKDFFFI.hkdfExtract BS.empty sharedSecret
                    derived    <- HKDFFFI.hkdfExpand prk provisioningHKDFInfo 64
                    let aesKey = BS.take 32 derived
                        macKey = BS.drop 32 derived
                    expectedMac <- HMACFFI.hmacSHA256 macKey (ephPub <> iv <> ciphertext)
                    if not (constantEq mac expectedMac)
                        then pure (Left (ProvisioningDecryptFailed "HMAC verification failed"))
                        else case aes256CbcPkcs7Decrypt aesKey iv ciphertext of
                            Nothing -> pure (Left (ProvisioningDecryptFailed "AES-256-CBC decrypt failed"))
                            Just pt -> case decodeProvisioningDataProto pt of
                                Nothing -> pure (Left (ProvisioningParseFailed "failed to decode provisioning protobuf"))
                                Just pd -> pure (case validateProvisioningData pd of
                                    Left e  -> Left e
                                    Right _ -> Right pd)

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
    mSignedPrePub <- X25519FFI.x25519 signedPrePriv X25519FFI.x25519Basepoint
    signedPrePub <- case mSignedPrePub of
                      Just pk -> pure pk
                      Nothing -> ioError (userError
                          "Registration.generatePreKeyBundle: X25519(prekeyPriv, basepoint) \
                          \returned all-zero — library or hardware fault; aborting prekey generation")

    -- Sign the prekey with the identity private key (Ed25519).
    let identityPriv = pdIdentityKeyPriv pd
    signature <- Ed25519FFI.ed25519Sign identityPriv signedPrePub

    -- Generate batch of one-time prekeys (100 keys).
    otpks <- generateOneTimePrekeys 100

    pure (signedPrePub, signature, otpks)

-- | Generate a batch of one-time X25519 prekeys.
-- Returns concatenated public keys (each 32 bytes).
generateOneTimePrekeys :: Int -> IO ByteString
generateOneTimePrekeys n = do
    keys <- mapM (\_ -> do
        priv <- randomBytes 32
        mPub <- X25519FFI.x25519 priv X25519FFI.x25519Basepoint
        case mPub of
            Just pk -> pure pk
            Nothing -> ioError (userError
                "Registration.generateOneTimePrekeys: X25519(privKey, basepoint) \
                \returned all-zero — library or hardware fault; aborting prekey generation")
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

------------------------------------------------------------------------
-- Provisioning crypto envelope + protobuf parsing
------------------------------------------------------------------------

-- | HKDF info string for provisioning key derivation.
--
-- This is intentionally local to UmbraVOX; the Signal-compatible
-- WebSocket transport can deliver the ciphertext, but the key schedule
-- and message structure must be fixed and versioned on our side.
provisioningHKDFInfo :: ByteString
provisioningHKDFInfo = "UmbraVox Provisioning v1"

-- | Provisioning envelope wire format.
--
-- @
--   [ ephPub(32) | iv(16) | ciphertext(N, multiple of 16) | mac(32) ]
-- @
--
-- mac = HMAC-SHA256(macKey, ephPub || iv || ciphertext)
decodeProvisioningEnvelope
    :: ByteString
    -> Either RegistrationError (ByteString, ByteString, ByteString, ByteString)
decodeProvisioningEnvelope bs
    | BS.length bs < 32 + 16 + 32 =
        Left (ProvisioningDecryptFailed "provisioning message too short")
    | otherwise =
        let ephPub = BS.take 32 bs
            iv = BS.take 16 (BS.drop 32 bs)
            rest = BS.drop (32 + 16) bs
            ciphertext = BS.take (BS.length rest - 32) rest
            mac = BS.drop (BS.length rest - 32) rest
        in if BS.length ciphertext == 0 || (BS.length ciphertext `mod` 16 /= 0)
            then Left (ProvisioningDecryptFailed "ciphertext length invalid for AES-CBC")
            else Right (ephPub, iv, ciphertext, mac)

-- | AES-256-CBC decryption with PKCS7 unpadding.
aes256CbcPkcs7Decrypt :: ByteString -> ByteString -> ByteString -> Maybe ByteString
aes256CbcPkcs7Decrypt key iv ciphertext
    | BS.length key /= 32 = Nothing
    | BS.length iv /= 16 = Nothing
    | BS.null ciphertext = Nothing
    | BS.length ciphertext `mod` 16 /= 0 = Nothing
    | otherwise =
        let blocks = chunk16 ciphertext
            ptBlocks = cbcDecryptBlocks key iv blocks
            pt = BS.concat ptBlocks
        in pkcs7Unpad16 pt

cbcDecryptBlocks :: ByteString -> ByteString -> [ByteString] -> [ByteString]
cbcDecryptBlocks _key _iv [] = []
cbcDecryptBlocks key iv (c0:cs) =
    let p0 = xorBS (aesDecrypt key c0) iv
    in p0 : go c0 cs
  where
    go _prev [] = []
    go prev (c:rest) =
        let p = xorBS (aesDecrypt key c) prev
        in p : go c rest

chunk16 :: ByteString -> [ByteString]
chunk16 bs
    | BS.null bs = []
    | otherwise =
        let (a, b) = BS.splitAt 16 bs
        in a : chunk16 b

xorBS :: ByteString -> ByteString -> ByteString
xorBS a b
    | BS.length a /= BS.length b = BS.empty
    | otherwise = BS.pack (BS.zipWith xor a b)

pkcs7Unpad16 :: ByteString -> Maybe ByteString
pkcs7Unpad16 bs
    | BS.null bs = Nothing
    | BS.length bs `mod` 16 /= 0 = Nothing
    | otherwise =
        let padLen = fromIntegral (BS.last bs) :: Int
            len = BS.length bs
        in if padLen <= 0 || padLen > 16 || padLen > len
            then Nothing
            else
                let padding = BS.drop (len - padLen) bs
                in if BS.all (== fromIntegral padLen) padding
                    then Just (BS.take (len - padLen) bs)
                    else Nothing

-- | Decode the decrypted provisioning protobuf into 'ProvisioningData'.
--
-- This uses the same minimal protobuf decoder as SignalWire.
-- Required fields (numbers are UmbraVOX-local for now):
--   1: identity private key (bytes, 32)
--   2: identity public key (bytes, 32)
--   3: phone number (string bytes)
--   4: profile key (bytes, 32)
--   5: device id (varint uint32)
--   6: provisioning code (bytes)
decodeProvisioningDataProto :: ByteString -> Maybe ProvisioningData
decodeProvisioningDataProto bs = do
    fields <- decodeFields bs
    idPriv <- lookupBytes 1 fields
    idPub  <- lookupBytes 2 fields
    phoneB <- lookupBytes 3 fields
    prof   <- lookupBytes 4 fields
    devIdB <- lookupVarint 5 fields
    code   <- lookupBytes 6 fields
    devIdW <- fmap fromIntegral (fst <$> decodeVarint devIdB)
    pure ProvisioningData
        { pdIdentityKeyPriv = idPriv
        , pdIdentityKeyPub = idPub
        , pdPhoneNumber = bytesToString phoneB
        , pdProfileKey = prof
        , pdDeviceId = devIdW
        , pdProvisioningCode = code
        }
  where
    lookupBytes n = lookupField n WireLengthDelimited
    lookupVarint n = lookupField n WireVarint

lookupField :: Word32 -> WireType -> [ProtoField] -> Maybe ByteString
lookupField n wt = go
  where
    go [] = Nothing
    go (f:fs)
        | pfFieldNumber f == n && pfWireType f == wt = Just (pfValue f)
        | otherwise = go fs

bytesToString :: ByteString -> String
bytesToString = map (toEnum . fromIntegral) . BS.unpack

stringToBS :: String -> ByteString
stringToBS = BS.pack . map (fromIntegral . fromEnum)

validateProvisioningData :: ProvisioningData -> Either RegistrationError ()
validateProvisioningData pd
    | BS.length (pdIdentityKeyPriv pd) /= 32 = Left (InvalidProvisioningData "identity private key must be 32 bytes")
    | BS.length (pdIdentityKeyPub pd) /= 32  = Left (InvalidProvisioningData "identity public key must be 32 bytes")
    | BS.length (pdProfileKey pd) /= 32      = Left (InvalidProvisioningData "profile key must be 32 bytes")
    | pdPhoneNumber pd == ""                 = Left (InvalidProvisioningData "phone number empty")
    | BS.null (pdProvisioningCode pd)        = Left (InvalidProvisioningData "provisioning code empty")
    | otherwise                              = Right ()

------------------------------------------------------------------------
-- WebSocket provisioning transport (M26.2)
------------------------------------------------------------------------

-- | Run the full linked device provisioning flow over the Signal WebSocket.
--
-- Connects to the Signal-Server provisioning endpoint, receives the
-- server-assigned UUID (for use in the QR code), waits for the primary
-- device to scan and send encrypted provisioning data, then decrypts
-- and returns the 'ProvisioningData'.
--
-- The 'onUuidAssigned' callback fires after the server assigns a UUID,
-- allowing the caller to re-render the QR code with the real UUID
-- (via 'generateProvisioningQR') before the provisioning message arrives.
runProvisioning
    :: RegistrationState
    -> (RegistrationState -> IO ())
       -- ^ Invoked after UUID is assigned; updated state has real UUID.
    -> IO (Either RegistrationError ProvisioningData)
runProvisioning rs onUuidAssigned = do
    let (host, port) = extractHostPort (rsServerUrl rs)
    connResult <- connectSignalWSPath host port BS.empty "/v1/websocket/provisioning/"
    case connResult of
        Left wsErr -> pure (Left (ServerError (show wsErr)))
        Right conn -> do
            result <- provisioningSession rs conn onUuidAssigned
            disconnectSignalWS conn
            pure result

-- | Internal: run the provisioning exchange over an established connection.
provisioningSession
    :: RegistrationState
    -> SignalWSConnection
    -> (RegistrationState -> IO ())
    -> IO (Either RegistrationError ProvisioningData)
provisioningSession rs conn onUuidAssigned = do
    -- Phase 1: receive UUID assignment from server.
    msg1Result <- recvWSMessage conn
    case msg1Result of
        Left wsErr -> pure (Left (ServerError (show wsErr)))
        Right msg1 ->
            case expectRequest "PUT" "/v1/address" msg1 of
                Left e -> pure (Left e)
                Right (addrReqId, addrBody) ->
                    case parseUuidBody addrBody of
                        Left e -> pure (Left e)
                        Right uuid -> do
                            _ <- sendWSMessage conn (ackResponse addrReqId)
                            let rs' = rs { rsProvisioningUuid = uuid
                                         , rsPhase = PhaseAwaitingProvisionMessage }
                            onUuidAssigned rs'
                            -- Phase 2: receive encrypted provisioning message.
                            msg2Result <- recvWSMessage conn
                            case msg2Result of
                                Left wsErr -> pure (Left (ServerError (show wsErr)))
                                Right msg2 ->
                                    case expectRequest "PUT" "/v1/message" msg2 of
                                        Left e -> pure (Left e)
                                        Right (msgReqId, encMsg) -> do
                                            result <- completeRegistration rs' encMsg
                                            case result of
                                                Left e -> pure (Left e)
                                                Right pd -> do
                                                    _ <- sendWSMessage conn (ackResponse msgReqId)
                                                    pure (Right pd)

-- | Build a 200 OK response for a given request ID.
ackResponse :: Word64 -> WSMessage
ackResponse reqId = WSMsgResponse WSResponse
    { wsRspId     = reqId
    , wsRspStatus = 200
    , wsRspBody   = BS.empty
    }

-- | Validate that a 'WSMessage' is a request with the expected verb and path.
--
-- Returns '(requestId, body)' on success, or 'ServerError' on mismatch.
expectRequest
    :: ByteString
    -> ByteString
    -> WSMessage
    -> Either RegistrationError (Word64, ByteString)
expectRequest verb path msg = case msg of
    WSMsgRequest req
        | wsReqVerb req == verb && wsReqPath req == path
        -> Right (wsReqId req, wsReqBody req)
        | otherwise
        -> Left (ServerError
            ( "unexpected request: "
           ++ bytesToString (wsReqVerb req) ++ " "
           ++ bytesToString (wsReqPath req)
           ++ " (expected: "
           ++ bytesToString verb ++ " " ++ bytesToString path ++ ")" ))
    WSMsgResponse _ ->
        Left (ServerError "expected WebSocket request, got response")
    WSMsgEnvelopePush _ ->
        Left (ServerError "expected WebSocket request, got envelope push")

-- | Parse the JSON body of a server UUID-assignment message.
--
-- Expects the form @{\"uuid\":\"<value>\"}@ with optional whitespace.
-- Uses a hand-rolled parser consistent with the project's no-JSON-library policy.
parseUuidBody :: ByteString -> Either RegistrationError ByteString
parseUuidBody bs =
    case extractJsonStringField "uuid" (bytesToString bs) of
        Nothing -> Left (InvalidProvisioningData
            "cannot parse UUID from server address response")
        Just v  -> Right (stringToBS v)

-- | Extract the value of a JSON string field by key (hand-rolled, no aeson).
--
-- Matches @\"key\":\"value\"@ with optional surrounding whitespace.
-- Returns 'Nothing' if the key is absent or the value is empty.
extractJsonStringField :: String -> String -> Maybe String
extractJsonStringField key s =
    let needle = "\"" ++ key ++ "\":\""
    in case findSubstring needle s of
        Nothing  -> Nothing
        Just idx ->
            let afterOpen = drop (length needle + idx) s
                val       = takeWhile (/= '"') afterOpen
            in if null val then Nothing else Just val

-- | Find the first index of @needle@ within @haystack@, or 'Nothing'.
findSubstring :: String -> String -> Maybe Int
findSubstring needle = go 0
  where
    n = length needle
    go _ []  = Nothing
    go i str
        | take n str == needle = Just i
        | otherwise            = go (i + 1) (tail str)

-- | Extract hostname and port from a URL string.
--
-- Handles @https://host@, @http://host:port@, and bare-host forms.
-- Defaults to port 443 for HTTPS and 8080 for HTTP.
extractHostPort :: String -> (String, Int)
extractHostPort url =
    let (scheme, rest) = case stripPrefix "https://" url of
                           Just r  -> ("https", r)
                           Nothing -> case stripPrefix "http://" url of
                                        Just r  -> ("http", r)
                                        Nothing -> ("https", url)
        hostPortPath        = takeWhile (/= '/') rest
        (hostPart, portStr) = break (== ':') hostPortPath
        defaultPort         = if scheme == "https" then 443 else 8080
        port                = case portStr of
                                (':':p) -> case reads p of
                                             [(pn, "")] -> pn
                                             _          -> defaultPort
                                _       -> defaultPort
    in (hostPart, port)
