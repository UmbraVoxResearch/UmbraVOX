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
    iSt <- noiseHandshakeInitiator iSec iPub rPub (AnyTransport tA)
    mRSt <- takeMVar resultVar
    case mRSt of
        Nothing -> do
            putStrLn "  FAIL: handshake round-trip (responder returned Nothing)"
            pure False
        Just rSt -> do
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
        let (sendKey, g1) = nextBytes 32 g
            (recvKey, g2) = nextBytes 32 g1
            (payload, _)  = nextBytesRange 1 200 g2
            senderSt = NoiseState
                { nsSendKey = sendKey
                , nsRecvKey = recvKey
                , nsSendN   = 0
                , nsRecvN   = 0
                }
            receiverSt = NoiseState
                { nsSendKey = recvKey
                , nsRecvKey = sendKey
                , nsSendN   = 0
                , nsRecvN   = 0
                }
            (_, ct) = noiseEncrypt senderSt payload
        pure $ case noiseDecrypt receiverSt ct of
            Nothing     -> False
            Just (_, pt) -> pt == payload

-- | Decryption with wrong key returns Nothing.
testWrongKeyFails :: IO Bool
testWrongKeyFails = do
    let sendKey = hexDecode "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
        recvKey = hexDecode "4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d"
        wrongKey = hexDecode "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb"
        senderSt = NoiseState
            { nsSendKey = sendKey
            , nsRecvKey = recvKey
            , nsSendN   = 0
            , nsRecvN   = 0
            }
        badReceiverSt = NoiseState
            { nsSendKey = recvKey
            , nsRecvKey = wrongKey
            , nsSendN   = 0
            , nsRecvN   = 0
            }
        msg = strToBS "secret message"
        (_, ct) = noiseEncrypt senderSt msg
        rejected = case noiseDecrypt badReceiverSt ct of
            Nothing -> True
            Just _  -> False
    assertEq "wrong key fails decryption" True rejected
