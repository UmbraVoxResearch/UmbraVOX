-- | Noise_IK handshake test suite.
--
-- Tests handshake via loopback transport, encrypt/decrypt round-trip,
-- and wrong-key rejection.
module Test.Network.Noise (runTests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Test.Util
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Network.Noise
    ( NoiseState(..)
    , noiseHandshakeInitiator, noiseHandshakeResponder
    , noiseEncrypt, noiseDecrypt
    )
import UmbraVox.Network.Transport.Loopback (newLoopbackPair)
import UmbraVox.Network.TransportClass (AnyTransport(..))

runTests :: IO Bool
runTests = do
    putStrLn "[Noise] Running Noise_IK handshake tests..."
    results <- sequence
        [ testHandshakeRoundTrip
        , testEncryptDecryptRoundTrip
        , testWrongKeyFails
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Noise] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | Full handshake via loopback, then encrypt/decrypt a message.
testHandshakeRoundTrip :: IO Bool
testHandshakeRoundTrip = do
    (tA, tB) <- newLoopbackPair "noise-hs"
    let iSec = hexDecode "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
        iPub = x25519 iSec x25519Basepoint
        rSec = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
        rPub = x25519 rSec x25519Basepoint
    -- Run responder in a background thread
    resultVar <- newEmptyMVar
    _ <- forkIO $ do
        mSt <- noiseHandshakeResponder rSec rPub (AnyTransport tB)
        putMVar resultVar mSt
    -- Initiator handshake
    mISt <- noiseHandshakeInitiator iSec iPub rPub (AnyTransport tA)
    mRSt <- takeMVar resultVar
    case (mISt, mRSt) of
        (Nothing, _) -> do
            putStrLn "  FAIL: handshake round-trip (initiator returned Nothing)"
            pure False
        (_, Nothing) -> do
            putStrLn "  FAIL: handshake round-trip (responder returned Nothing)"
            pure False
        (Just iSt, Just rSt) -> do
            -- Encrypt with initiator, decrypt with responder
            let msg = strToBS "Hello Noise!"
                (_iSt', ct) = noiseEncrypt iSt msg
            case noiseDecrypt rSt ct of
                Nothing -> do
                    putStrLn "  FAIL: handshake round-trip (decrypt failed)"
                    pure False
                Just (_, pt) ->
                    assertEq "handshake round-trip" msg pt

-- | Encrypt then decrypt round-trip with manually constructed states.
testEncryptDecryptRoundTrip :: IO Bool
testEncryptDecryptRoundTrip = checkPropertyIO
    "encrypt/decrypt round-trip (50 iterations)" 50 $ \g -> do
        let (sendEncKey, g1) = nextBytes 32 g
            (sendMacKey, g2) = nextBytes 32 g1
            (recvEncKey, g3) = nextBytes 32 g2
            (recvMacKey, g4) = nextBytes 32 g3
            (payload, _)     = nextBytesRange 1 200 g4
            senderSt = NoiseState
                { nsSendEncKey = sendEncKey
                , nsSendMacKey = sendMacKey
                , nsRecvEncKey = recvEncKey
                , nsRecvMacKey = recvMacKey
                , nsSendN      = 0
                , nsRecvN      = 0
                }
            receiverSt = NoiseState
                { nsSendEncKey = recvEncKey
                , nsSendMacKey = recvMacKey
                , nsRecvEncKey = sendEncKey
                , nsRecvMacKey = sendMacKey
                , nsSendN      = 0
                , nsRecvN      = 0
                }
            (_, ct) = noiseEncrypt senderSt payload
        pure $ case noiseDecrypt receiverSt ct of
            Nothing     -> False
            Just (_, pt) -> pt == payload

-- | Decryption with wrong key returns Nothing.
testWrongKeyFails :: IO Bool
testWrongKeyFails = do
    let sendEncKey = hexDecode "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
        sendMacKey = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
        recvEncKey = hexDecode "b8b4e236805318e93f48bfbb365656ec1d068bf3d8cabb64dd1ba4523cec3a2a"
        recvMacKey = hexDecode "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a"
        wrongKey   = hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
        senderSt = NoiseState
            { nsSendEncKey = sendEncKey
            , nsSendMacKey = sendMacKey
            , nsRecvEncKey = recvEncKey
            , nsRecvMacKey = recvMacKey
            , nsSendN      = 0
            , nsRecvN      = 0
            }
        badReceiverSt = NoiseState
            { nsSendEncKey = recvEncKey
            , nsSendMacKey = recvMacKey
            , nsRecvEncKey = wrongKey
            , nsRecvMacKey = wrongKey
            , nsSendN      = 0
            , nsRecvN      = 0
            }
        msg = strToBS "secret message"
        (_, ct) = noiseEncrypt senderSt msg
        rejected = case noiseDecrypt badReceiverSt ct of
            Nothing -> True
            Just _  -> False
    assertEq "wrong key fails decryption" True rejected
