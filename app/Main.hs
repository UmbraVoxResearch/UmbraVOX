module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Bits (shiftL, shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import System.Environment (getArgs)
import System.IO (hFlush, stdout, stdin, hSetBuffering, BufferMode(..), hIsEOF)

import UmbraVox.Chat.Session
    (ChatSession, initChatSession, sendChatMessage, recvChatMessage)
import UmbraVox.Crypto.MLKEM (mlkemKeyGen,
    MLKEMEncapKey(..), MLKEMDecapKey(..), MLKEMCiphertext(..))
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.Signal.PQXDH
    (PQPreKeyBundle(..), PQXDHResult(..), pqxdhInitiate, pqxdhRespond)
import UmbraVox.Crypto.Signal.X3DH
    (KeyPair(..), IdentityKey(..), generateIdentityKey, generateKeyPair,
     signPreKey)
import UmbraVox.Network.Transport (Transport, listen, connect, send, recv)
import UmbraVox.Protocol.CBOR (encodeMessage)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case args of
        ["listen", port]        -> runServer (read port)
        ["connect", host, port] -> runClient host (read port)
        _                       -> printUsage

printUsage :: IO ()
printUsage = putStrLn "Usage: umbravox listen <port> | umbravox connect <host> <port>"

------------------------------------------------------------------------
-- Server (responder / Bob)
------------------------------------------------------------------------

runServer :: Int -> IO ()
runServer port = do
    putStrLn $ "Listening on port " ++ show port ++ "..."
    t <- listen port
    putStrLn "Peer connected. Performing PQXDH handshake..."
    session <- handshakeResponder t
    putStrLn "Session established. Type messages below."
    messageLoop t session

------------------------------------------------------------------------
-- Client (initiator / Alice)
------------------------------------------------------------------------

runClient :: String -> Int -> IO ()
runClient host port = do
    putStrLn $ "Connecting to " ++ host ++ ":" ++ show port ++ "..."
    t <- connect host port
    putStrLn "Connected. Performing PQXDH handshake..."
    session <- handshakeInitiator t
    putStrLn "Session established. Type messages below."
    messageLoop t session

------------------------------------------------------------------------
-- PQXDH key material generation
------------------------------------------------------------------------

-- | Generate a full identity key (Ed25519 + X25519) from fresh randomness.
genIdentity :: IO IdentityKey
genIdentity = do
    edSec <- randomBytes 32
    xSec  <- randomBytes 32
    pure $! generateIdentityKey edSec xSec

-- | Generate an X25519 signed prekey and its Ed25519 signature.
genSignedPreKey :: IdentityKey -> IO (KeyPair, BS.ByteString)
genSignedPreKey ik = do
    spkSec <- randomBytes 32
    let !spk = generateKeyPair spkSec
    let !sig = signPreKey ik (kpPublic spk)
    pure (spk, sig)

-- | Generate an ML-KEM-768 keypair from fresh randomness.
genPQPreKey :: IO (MLKEMEncapKey, MLKEMDecapKey)
genPQPreKey = do
    d <- randomBytes 32
    z <- randomBytes 32
    pure $! mlkemKeyGen d z

------------------------------------------------------------------------
-- Prekey bundle wire format (length-prefixed fields)
------------------------------------------------------------------------

-- | Serialize a prekey bundle for transmission.
--
-- Format: IK_x25519 (32) | IK_ed25519 (32) | SPK_pub (32) | SPK_sig (64)
--       | len(PQPK) as Word32 BE | PQPK (variable) | OPK flag+data (1+0 or 1+32)
serializeBundle :: IdentityKey -> BS.ByteString -> BS.ByteString
                -> MLKEMEncapKey -> Maybe BS.ByteString -> BS.ByteString
serializeBundle ik spkPub spkSig (MLKEMEncapKey pqpk) mOpk =
    BS.concat
        [ ikX25519Public ik
        , ikEd25519Public ik
        , spkPub
        , spkSig
        , putW32BE (fromIntegral (BS.length pqpk))
        , pqpk
        , encodeOptionalKey mOpk
        ]

-- | Encode an optional one-time prekey: 0x01 + key, or 0x00.
encodeOptionalKey :: Maybe BS.ByteString -> BS.ByteString
encodeOptionalKey Nothing  = BS.singleton 0x00
encodeOptionalKey (Just k) = BS.singleton 0x01 <> k

-- | Deserialize a prekey bundle received from the wire.
-- Returns Nothing if the data is malformed.
deserializeBundle :: BS.ByteString -> Maybe PQPreKeyBundle
deserializeBundle bs
    | BS.length bs < 165 = Nothing  -- 32+32+32+64+4+1 minimum
    | otherwise = parseBundleFields bs

-- | Parse the individual fields of a serialized prekey bundle.
parseBundleFields :: BS.ByteString -> Maybe PQPreKeyBundle
parseBundleFields bs =
    let !ikX   = bsSlice 0  32 bs
        !ikEd  = bsSlice 32 32 bs
        !spkP  = bsSlice 64 32 bs
        !sig   = bsSlice 96 64 bs
        !pqLen = fromIntegral (getW32BE (bsSlice 160 4 bs)) :: Int
        !rest  = BS.drop (164 + pqLen) bs
    in if BS.length bs < 164 + pqLen + 1
       then Nothing
       else Just PQPreKeyBundle
           { pqpkbIdentityKey     = ikX
           , pqpkbIdentityEd25519 = ikEd
           , pqpkbSignedPreKey    = spkP
           , pqpkbSPKSignature    = sig
           , pqpkbPQPreKey        = MLKEMEncapKey (bsSlice 164 pqLen bs)
           , pqpkbOneTimePreKey   = decodeOptionalKey rest
           }

-- | Decode an optional key from the wire: 0x01 + 32 bytes, or 0x00.
decodeOptionalKey :: BS.ByteString -> Maybe BS.ByteString
decodeOptionalKey bs
    | BS.null bs           = Nothing
    | BS.index bs 0 == 0x01 = Just (BS.take 32 (BS.drop 1 bs))
    | otherwise            = Nothing

------------------------------------------------------------------------
-- PQXDH Handshake — Initiator (Alice)
------------------------------------------------------------------------

-- | Alice: generate keys, receive Bob's bundle, run PQXDH, send initial
-- message, and establish the Double Ratchet session.
handshakeInitiator :: Transport -> IO ChatSession
handshakeInitiator t = do
    -- Step 1-3: Generate Alice's key material
    aliceIK  <- genIdentity
    -- Step 4: Receive Bob's prekey bundle
    bundle   <- recvBundle t
    -- Step 5: Run PQXDH initiation
    result   <- initiatePQXDH aliceIK bundle
    -- Step 6: Send Alice's identity + ephemeral + PQ ciphertext to Bob
    sendInitialMessage t aliceIK result
    -- Step 7: Initialize Double Ratchet
    initChatSession (pqxdhSharedSecret result)
                    (ikX25519Secret aliceIK)
                    (pqpkbSignedPreKey bundle)

-- | Run pqxdhInitiate with fresh randomness; abort on SPK verify failure.
initiatePQXDH :: IdentityKey -> PQPreKeyBundle -> IO PQXDHResult
initiatePQXDH aliceIK bundle = do
    ekRand     <- randomBytes 32
    mlkemRand  <- randomBytes 32
    case pqxdhInitiate aliceIK bundle ekRand mlkemRand of
        Nothing     -> fail "PQXDH: SPK signature verification failed"
        Just result -> pure result

-- | Send Alice's initial message: her IK pub (X25519), ephemeral pub,
-- and ML-KEM ciphertext, all length-prefixed.
sendInitialMessage :: Transport -> IdentityKey -> PQXDHResult -> IO ()
sendInitialMessage t aliceIK result = do
    let MLKEMCiphertext ctBS = pqxdhPQCiphertext result
    let !msg = BS.concat
            [ ikX25519Public aliceIK
            , pqxdhEphemeralKey result
            , putW32BE (fromIntegral (BS.length ctBS))
            , ctBS
            ]
    send t (encodeMessage msg)

------------------------------------------------------------------------
-- PQXDH Handshake — Responder (Bob)
------------------------------------------------------------------------

-- | Bob: generate keys, send prekey bundle, receive Alice's initial
-- message, run PQXDH respond, and establish the Double Ratchet session.
handshakeResponder :: Transport -> IO ChatSession
handshakeResponder t = do
    -- Step 1-3: Generate Bob's key material
    bobIK           <- genIdentity
    (spk, spkSig)   <- genSignedPreKey bobIK
    (pqEK, pqDK)    <- genPQPreKey
    -- Step 4: Send prekey bundle to Alice
    sendBundle t bobIK (kpPublic spk) spkSig pqEK
    -- Step 5: Receive Alice's initial message
    (aliceIKPub, aliceEKPub, pqCt) <- recvInitialMessage t
    -- Step 6: Derive shared secret
    let !shared = pqxdhRespond bobIK (kpSecret spk) Nothing pqDK
                               aliceIKPub aliceEKPub pqCt
    -- Step 7: Initialize Double Ratchet
    initChatSession shared (kpSecret spk) aliceEKPub

------------------------------------------------------------------------
-- Bundle send/receive helpers
------------------------------------------------------------------------

-- | Send a serialized prekey bundle over the transport.
sendBundle :: Transport -> IdentityKey -> BS.ByteString -> BS.ByteString
           -> MLKEMEncapKey -> IO ()
sendBundle t ik spkPub spkSig pqEK =
    send t (encodeMessage (serializeBundle ik spkPub spkSig pqEK Nothing))

-- | Receive and deserialize a prekey bundle from the transport.
recvBundle :: Transport -> IO PQPreKeyBundle
recvBundle t = do
    lenBs <- recv t 4
    let !len = getW32BE lenBs
    payload <- recv t (fromIntegral len)
    case deserializeBundle payload of
        Nothing     -> fail "PQXDH: malformed prekey bundle"
        Just bundle -> pure bundle

-- | Receive Alice's initial message: IK pub + EK pub + ML-KEM ciphertext.
recvInitialMessage :: Transport
                   -> IO (BS.ByteString, BS.ByteString, MLKEMCiphertext)
recvInitialMessage t = do
    lenBs <- recv t 4
    let !len = getW32BE lenBs
    payload <- recv t (fromIntegral len)
    pure $! parseInitialMessage payload

-- | Parse the fields of Alice's initial message.
parseInitialMessage :: BS.ByteString
                    -> (BS.ByteString, BS.ByteString, MLKEMCiphertext)
parseInitialMessage bs =
    let !ikPub  = bsSlice 0  32 bs
        !ekPub  = bsSlice 32 32 bs
        !ctLen  = fromIntegral (getW32BE (bsSlice 64 4 bs)) :: Int
        !ct     = bsSlice 68 ctLen bs
    in (ikPub, ekPub, MLKEMCiphertext ct)

------------------------------------------------------------------------
-- ByteString / Word32 helpers
------------------------------------------------------------------------

-- | Slice a ByteString: take @len@ bytes starting at @off@.
bsSlice :: Int -> Int -> BS.ByteString -> BS.ByteString
bsSlice off len = BS.take len . BS.drop off

-- | Encode a Word32 as 4 big-endian bytes.
putW32BE :: Word32 -> BS.ByteString
putW32BE w = BS.pack
    [ fromIntegral (w `shiftR` 24 .&. 0xff)
    , fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR`  8 .&. 0xff)
    , fromIntegral (w             .&. 0xff)
    ]

-- | Decode a big-endian Word32 from 4 bytes.
getW32BE :: BS.ByteString -> Word32
getW32BE bs =
    (fromIntegral (BS.index bs 0) `shiftL` 24)
    + (fromIntegral (BS.index bs 1) `shiftL` 16)
    + (fromIntegral (BS.index bs 2) `shiftL` 8)
    + fromIntegral (BS.index bs 3)

------------------------------------------------------------------------
-- Bidirectional message loop
------------------------------------------------------------------------

-- | Run two threads: one for sending (stdin), one for receiving.
messageLoop :: Transport -> ChatSession -> IO ()
messageLoop t session0 = do
    ref <- newIORef session0
    -- Receive thread: read from transport, decrypt, print
    void $ forkIO (recvLoop t ref)
    -- Send thread (main): read stdin, encrypt, send
    sendLoop t ref

-- | Read lines from stdin, encrypt, and send over the transport.
sendLoop :: Transport -> IORef ChatSession -> IO ()
sendLoop t ref = do
    putStr "> "
    hFlush stdout
    eof <- hIsEOF stdin
    if eof
        then putStrLn "[EOF on stdin, exiting]"
        else do
            line <- BC.getLine
            if BS.null line
                then sendLoop t ref
                else do
                    session <- readIORef ref
                    (session', wire) <- sendChatMessage session line
                    writeIORef ref session'
                    send t (encodeMessage wire)
                    sendLoop t ref

-- | Receive messages from the transport, decrypt, and print.
recvLoop :: Transport -> IORef ChatSession -> IO ()
recvLoop t ref = do
    lenBs <- recv t 4
    if BS.length lenBs < 4
        then putStrLn "\n[Peer disconnected]"
        else do
            let !len = fromIntegral (getW32BE lenBs)
            payload <- recv t len
            session <- readIORef ref
            result <- recvChatMessage session payload
            case result of
                Nothing -> do
                    putStrLn "\n[Decryption failed]"
                    recvLoop t ref
                Just (session', plaintext) -> do
                    writeIORef ref session'
                    BC.putStrLn $ "\r< " <> plaintext
                    putStr "> "
                    hFlush stdout
                    recvLoop t ref
