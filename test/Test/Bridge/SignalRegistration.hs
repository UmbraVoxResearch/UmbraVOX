-- SPDX-License-Identifier: Apache-2.0
-- | Signal bridge registration tests (M26.1).
--
-- Exercises the provisioning decryption + protobuf parsing path without
-- requiring a live Signal-Server. The WebSocket transport is tested
-- separately; this suite focuses on the crypto envelope and decode logic.
module Test.Bridge.SignalRegistration (runTests) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util (assertEq)
import UmbraVox.Bridge.Signal.Registration
    ( beginRegistration
    , completeRegistration
    , RegistrationState(..)
    , ProvisioningData(..)
    , RegistrationError(..)
    )
import UmbraVox.Crypto.AES (aesEncrypt)
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.HKDF (hkdfExpand, hkdfExtract)
import UmbraVox.Crypto.HMAC (hmacSHA256)
import UmbraVox.Protocol.SignalWire (WireType(..), encodeField, encodeVarint)

------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------

runTests :: IO Bool
runTests = do
    putStrLn "[SignalRegistration] Running Signal registration tests..."
    results <- sequence
        [ testCompleteRegistrationHappyPath
        , testCompleteRegistrationBadMac
        , testCompleteRegistrationBadCiphertextLen
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[SignalRegistration] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

testCompleteRegistrationHappyPath :: IO Bool
testCompleteRegistrationHappyPath = do
    rs <- beginRegistration "https://signal.example.invalid"
    let idPriv = BS.pack [0..31]
        idPub  = BS.pack (reverse [0..31])
        phoneB = strToBS "+15551234567"
        profileKey = BS.replicate 32 0x42
        deviceId = 7 :: Int
        provCode = strToBS "prov-code-123"
        expected = ProvisioningData
            { pdIdentityKeyPriv = idPriv
            , pdIdentityKeyPub = idPub
            , pdPhoneNumber = "+15551234567"
            , pdProfileKey = profileKey
            , pdDeviceId = fromIntegral deviceId
            , pdProvisioningCode = provCode
            }
    msg <- buildProvisioningEnvelope rs idPriv idPub phoneB profileKey deviceId provCode
    case completeRegistration rs msg of
        Left err -> assertEq "completeRegistration happy path" True False
            >>= \ok -> putStrLn ("  ERROR: " ++ show err) >> pure ok
        Right pd -> do
            ok1 <- assertEq "identityPriv" (pdIdentityKeyPriv expected) (pdIdentityKeyPriv pd)
            ok2 <- assertEq "identityPub" (pdIdentityKeyPub expected) (pdIdentityKeyPub pd)
            ok3 <- assertEq "phone" (pdPhoneNumber expected) (pdPhoneNumber pd)
            ok4 <- assertEq "profileKey" (pdProfileKey expected) (pdProfileKey pd)
            ok5 <- assertEq "deviceId" (pdDeviceId expected) (pdDeviceId pd)
            ok6 <- assertEq "provCode" (pdProvisioningCode expected) (pdProvisioningCode pd)
            pure (ok1 && ok2 && ok3 && ok4 && ok5 && ok6)

testCompleteRegistrationBadMac :: IO Bool
testCompleteRegistrationBadMac = do
    rs <- beginRegistration "https://signal.example.invalid"
    msg <- buildProvisioningEnvelope rs (BS.replicate 32 0x11) (BS.replicate 32 0x22)
        (strToBS "+1") (BS.replicate 32 0x33) 1 (strToBS "x")
    let msgBad = flipLastByte msg
    case completeRegistration rs msgBad of
        Left (ProvisioningDecryptFailed _) -> assertEq "bad MAC" True True
        other -> do
            putStrLn ("  unexpected: " ++ show other)
            assertEq "bad MAC unexpected" True False

testCompleteRegistrationBadCiphertextLen :: IO Bool
testCompleteRegistrationBadCiphertextLen = do
    rs <- beginRegistration "https://signal.example.invalid"
    -- Well-formed header sizes but ciphertext not a multiple of 16.
    let ephPub = BS.replicate 32 0x01
        iv = BS.replicate 16 0x02
        ct = BS.replicate 17 0x03
        mac = BS.replicate 32 0x04
        msg = ephPub <> iv <> ct <> mac
    case completeRegistration rs msg of
        Left (ProvisioningDecryptFailed _) -> assertEq "bad ciphertext len" True True
        other -> do
            putStrLn ("  unexpected: " ++ show other)
            assertEq "bad ciphertext len unexpected" True False

------------------------------------------------------------------------
-- Test-only helpers
------------------------------------------------------------------------

provisioningHKDFInfo :: ByteString
provisioningHKDFInfo = "UmbraVox Provisioning v1"

buildProvisioningEnvelope
    :: RegistrationState
    -> ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> Int
    -> ByteString
    -> IO ByteString
buildProvisioningEnvelope rs idPriv idPub phoneB profileKey deviceId provCode = do
    let ephPriv = BS.replicate 32 0x09
        ephPub  = maybe (BS.replicate 32 0) id (x25519 ephPriv x25519Basepoint)
        provPub = rsProvisioningPubKey rs
        shared  = maybe (BS.replicate 32 0) id (x25519 ephPriv provPub)
        prk     = hkdfExtract BS.empty shared
        derived = hkdfExpand prk provisioningHKDFInfo 64
        aesKey  = BS.take 32 derived
        macKey  = BS.drop 32 derived
        iv      = BS.replicate 16 0xAA
        pt      = encodeProvisioningProto idPriv idPub phoneB profileKey deviceId provCode
        ct      = aes256CbcPkcs7Encrypt aesKey iv pt
        mac     = hmacSHA256 macKey (ephPub <> iv <> ct)
    pure (ephPub <> iv <> ct <> mac)

encodeProvisioningProto :: ByteString -> ByteString -> ByteString -> ByteString -> Int -> ByteString -> ByteString
encodeProvisioningProto idPriv idPub phoneB profileKey deviceId provCode =
    BS.concat
        [ encodeBytes 1 idPriv
        , encodeBytes 2 idPub
        , encodeBytes 3 phoneB
        , encodeBytes 4 profileKey
        , encodeVar   5 deviceId
        , encodeBytes 6 provCode
        ]
  where
    encodeBytes n b = encodeField (fromIntegral n) WireLengthDelimited b
    encodeVar n v = encodeField (fromIntegral n) WireVarint (encodeVarint (fromIntegral v))

aes256CbcPkcs7Encrypt :: ByteString -> ByteString -> ByteString -> ByteString
aes256CbcPkcs7Encrypt key iv pt =
    let padded = pkcs7Pad16 pt
        blocks = chunk16 padded
    in BS.concat (cbcEncryptBlocks key iv blocks)

cbcEncryptBlocks :: ByteString -> ByteString -> [ByteString] -> [ByteString]
cbcEncryptBlocks _key _iv [] = []
cbcEncryptBlocks key iv (p0:ps) =
    let c0 = aesEncrypt key (xorBS p0 iv)
    in c0 : go c0 ps
  where
    go _prev [] = []
    go prev (p:rest) =
        let c = aesEncrypt key (xorBS p prev)
        in c : go c rest

chunk16 :: ByteString -> [ByteString]
chunk16 bs
    | BS.null bs = []
    | otherwise =
        let (a, b) = BS.splitAt 16 bs
        in a : chunk16 b

xorBS :: ByteString -> ByteString -> ByteString
xorBS a b = BS.pack (BS.zipWith xor a b)

pkcs7Pad16 :: ByteString -> ByteString
pkcs7Pad16 bs =
    let padLen = 16 - (BS.length bs `mod` 16)
    in bs <> BS.replicate padLen (fromIntegral padLen)

flipLastByte :: ByteString -> ByteString
flipLastByte bs
    | BS.null bs = bs
    | otherwise =
        let lastIx = BS.length bs - 1
            x = BS.index bs lastIx
        in BS.take lastIx bs <> BS.singleton (x `xor` 0xFF)

strToBS :: String -> ByteString
strToBS = BS.pack . map (fromIntegral . fromEnum)
