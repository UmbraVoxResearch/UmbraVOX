-- SPDX-License-Identifier: Apache-2.0
-- | Signal bridge registration tests (M26.1, M26.2).
--
-- Two test layers:
--   * Crypto path (M26.1): exercises 'completeRegistration' directly with
--     synthetic encrypted envelopes — no network required.
--   * WebSocket transport (M26.2): spins up a minimal loopback TCP server
--     that speaks the Signal provisioning sub-protocol, then runs
--     'runProvisioning' end-to-end against it.
module Test.Bridge.SignalRegistration (runTests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word16, Word64)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import Test.Util (assertEq)
import UmbraVox.Bridge.Signal.Registration
    ( beginRegistration
    , completeRegistration
    , runProvisioning
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
        , testRunProvisioningLoopback
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[SignalRegistration] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Crypto path tests (M26.1)
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
-- WebSocket transport test (M26.2)
------------------------------------------------------------------------

-- | End-to-end loopback test for 'runProvisioning'.
--
-- Spins up a raw TCP loopback server that implements the Signal
-- provisioning WebSocket sub-protocol:
--   1. HTTP upgrade handshake
--   2. PUT /v1/address  {"uuid":"<uuid>"}  → client ACKs
--   3. PUT /v1/message  <encrypted-envelope> → client ACKs, decrypts
--
-- Finding:     M26.2 — WebSocket transport path had no test coverage.
-- Vulnerability: network integration errors (framing, encoding, ACK
--                ordering) would only surface against a live server.
-- Fix:         loopback server exercises the full round-trip without
--              a live Signal-Server dependency.
-- Verified:    test passes when runProvisioning returns correct ProvisioningData.
testRunProvisioningLoopback :: IO Bool
testRunProvisioningLoopback = do
    -- Bind server on an OS-assigned port on loopback.
    serverSock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
    NS.setSocketOption serverSock NS.ReuseAddr 1
    let loopback = NS.tupleToHostAddress (127, 0, 0, 1)
    NS.bind serverSock (NS.SockAddrInet 0 loopback)
    NS.listen serverSock 1
    assignedAddr <- NS.getSocketName serverSock
    let port = case assignedAddr of
                 NS.SockAddrInet p _ -> fromIntegral p :: Int
                 _                   -> 0

    -- Build the encrypted provisioning envelope we will serve.
    rs0 <- beginRegistration ("http://127.0.0.1:" ++ show port)
    let testUuid  = "deadbeef-cafe-1234-5678-abcdef012345"
        idPriv    = BS.pack [0..31]
        idPub     = BS.pack (reverse [0..31])
        phoneB    = strToBS "+15559876543"
        profKey   = BS.replicate 32 0x77
        devId     = 3 :: Int
        provCode  = strToBS "loopback-test-code"
    encMsg <- buildProvisioningEnvelope rs0 idPriv idPub phoneB profKey devId provCode

    -- Server thread: handle one provisioning session.
    serverDone <- newEmptyMVar
    _ <- forkIO $ do
        (clientSock, _) <- NS.accept serverSock
        -- HTTP upgrade handshake.
        _ <- NSB.recv clientSock 4096
        NSB.sendAll clientSock (strToBS wsUpgradeResponse)
        -- Frame 1: UUID assignment.
        let uuidBody = strToBS ("{\"uuid\":\"" ++ testUuid ++ "\"}")
        NSB.sendAll clientSock (buildServerFrame (buildSignalRequest 1 "PUT" "/v1/address" uuidBody))
        -- Receive client ACK.
        _ <- recvClientFrame clientSock
        -- Frame 2: provisioning message.
        NSB.sendAll clientSock (buildServerFrame (buildSignalRequest 2 "PUT" "/v1/message" encMsg))
        -- Receive client ACK.
        _ <- recvClientFrame clientSock
        NS.close clientSock
        NS.close serverSock
        putMVar serverDone ()

    -- UUID callback records the state after UUID is assigned.
    uuidVar <- newEmptyMVar
    let onUuidAssigned rs' = putMVar uuidVar (rsProvisioningUuid rs')

    -- Run the full provisioning flow.
    result <- runProvisioning rs0 onUuidAssigned
    _ <- takeMVar serverDone

    -- Validate results.
    case result of
        Left err -> do
            putStrLn ("  [loopback] ERROR: " ++ show err)
            assertEq "runProvisioning loopback" True False
        Right pd -> do
            uuid <- takeMVar uuidVar
            ok1 <- assertEq "uuid assigned"  (strToBS testUuid)       uuid
            ok2 <- assertEq "identityPriv"   idPriv   (pdIdentityKeyPriv pd)
            ok3 <- assertEq "identityPub"    idPub    (pdIdentityKeyPub pd)
            ok4 <- assertEq "phone"          "+15559876543" (pdPhoneNumber pd)
            ok5 <- assertEq "profileKey"     profKey  (pdProfileKey pd)
            ok6 <- assertEq "devId"          (fromIntegral devId) (pdDeviceId pd)
            ok7 <- assertEq "provCode"       provCode (pdProvisioningCode pd)
            pure (ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7)

------------------------------------------------------------------------
-- Loopback server helpers
------------------------------------------------------------------------

-- | Minimal HTTP 101 Switching Protocols response.
wsUpgradeResponse :: String
wsUpgradeResponse =
    "HTTP/1.1 101 Switching Protocols\r\n"
    ++ "Upgrade: websocket\r\n"
    ++ "Connection: Upgrade\r\n"
    ++ "\r\n"

-- | Build an unmasked server-to-client WebSocket binary frame.
buildServerFrame :: ByteString -> ByteString
buildServerFrame payload =
    let len = BS.length payload
        hdr
            | len < 126   = BS.pack [0x82, fromIntegral len]
            | len < 65536 = BS.pack [ 0x82, 0x7E
                                    , fromIntegral (len `shiftR` 8)
                                    , fromIntegral (len .&. 0xFF)
                                    ]
            | otherwise   = BS.pack [ 0x82, 0x7F
                                    , fromIntegral (len `shiftR` 56)
                                    , fromIntegral (len `shiftR` 48 .&. 0xFF)
                                    , fromIntegral (len `shiftR` 40 .&. 0xFF)
                                    , fromIntegral (len `shiftR` 32 .&. 0xFF)
                                    , fromIntegral (len `shiftR` 24 .&. 0xFF)
                                    , fromIntegral (len `shiftR` 16 .&. 0xFF)
                                    , fromIntegral (len `shiftR`  8 .&. 0xFF)
                                    , fromIntegral (len .&. 0xFF)
                                    ]
    in hdr <> payload

-- | Encode a Signal sub-protocol type-1 request.
--
-- Format: 0x01 + reqId-BE(8) + verbLen-BE(2) + verb + pathLen-BE(2) + path + body
buildSignalRequest :: Word64 -> String -> String -> ByteString -> ByteString
buildSignalRequest reqId verb path body =
    let verbBS = strToBS verb
        pathBS = strToBS path
    in BS.singleton 0x01
    <> w64BE reqId
    <> w16BE (fromIntegral (BS.length verbBS))
    <> verbBS
    <> w16BE (fromIntegral (BS.length pathBS))
    <> pathBS
    <> body

-- | Read one masked WebSocket frame from the client socket and return its payload.
recvClientFrame :: NS.Socket -> IO ByteString
recvClientFrame sock = do
    hdr <- recvN sock 2
    let byte1      = BS.index hdr 1
        masked     = byte1 .&. 0x80 /= 0
        len7       = fromIntegral (byte1 .&. 0x7F) :: Int
    payloadLen <- if len7 < 126
                    then pure len7
                    else if len7 == 126
                           then do ext <- recvN sock 2
                                   pure (fromIntegral (BS.index ext 0) `shiftL` 8
                                        .|. fromIntegral (BS.index ext 1))
                           else do ext <- recvN sock 8
                                   pure (fromIntegral (decodeW64BE ext))
    maskKey <- if masked then recvN sock 4 else pure BS.empty
    raw     <- recvN sock payloadLen
    pure (if masked then applyMask maskKey raw else raw)
  where
    applyMask mk p = BS.pack
        [ BS.index p i `xor` BS.index mk (i `mod` 4)
        | i <- [0 .. BS.length p - 1]
        ]
    decodeW64BE b =
        fromIntegral (BS.index b 0) `shiftL` 56
        .|. fromIntegral (BS.index b 1) `shiftL` 48
        .|. fromIntegral (BS.index b 2) `shiftL` 40
        .|. fromIntegral (BS.index b 3) `shiftL` 32
        .|. fromIntegral (BS.index b 4) `shiftL` 24
        .|. fromIntegral (BS.index b 5) `shiftL` 16
        .|. fromIntegral (BS.index b 6) `shiftL`  8
        .|. fromIntegral (BS.index b 7) :: Word64

recvN :: NS.Socket -> Int -> IO ByteString
recvN _ 0    = pure BS.empty
recvN sock n = go n []
  where
    go 0   acc = pure (BS.concat (reverse acc))
    go rem acc = do
        chunk <- NSB.recv sock (min rem 4096)
        if BS.null chunk
            then pure (BS.concat (reverse acc))
            else go (rem - BS.length chunk) (chunk : acc)

-- | Big-endian Word64 encoding.
w64BE :: Word64 -> ByteString
w64BE w = BS.pack
    [ fromIntegral (w `shiftR` 56)
    , fromIntegral (w `shiftR` 48 .&. 0xFF)
    , fromIntegral (w `shiftR` 40 .&. 0xFF)
    , fromIntegral (w `shiftR` 32 .&. 0xFF)
    , fromIntegral (w `shiftR` 24 .&. 0xFF)
    , fromIntegral (w `shiftR` 16 .&. 0xFF)
    , fromIntegral (w `shiftR`  8 .&. 0xFF)
    , fromIntegral (w .&. 0xFF)
    ]

-- | Big-endian Word16 encoding.
w16BE :: Word16 -> ByteString
w16BE w = BS.pack
    [ fromIntegral (w `shiftR` 8 .&. 0xFF)
    , fromIntegral (w .&. 0xFF)
    ]

------------------------------------------------------------------------
-- Crypto test helpers
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
