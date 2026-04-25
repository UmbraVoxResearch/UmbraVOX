module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Environment (getArgs)
import System.IO (hFlush, stdout, stdin, hSetBuffering, BufferMode(..), hIsEOF)

import UmbraVox.Chat.Session
    (ChatSession, initChatSession, sendChatMessage, recvChatMessage)
import UmbraVox.Crypto.Curve25519 (x25519, x25519Basepoint)
import UmbraVox.Crypto.HKDF (hkdfExtract, hkdfExpand)
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Network.Transport (Transport, listen, connect, send, recv)
import UmbraVox.Protocol.CBOR (encodeMessage, decodeMessage)

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
-- Server (responder)
------------------------------------------------------------------------

runServer :: Int -> IO ()
runServer port = do
    sk <- randomBytes 32
    let !pk = x25519 sk x25519Basepoint
    putStrLn $ "Listening on port " ++ show port ++ "..."
    t <- listen port
    putStrLn "Peer connected. Performing key exchange..."
    session <- handshakeResponder t sk pk
    putStrLn "Session established. Type messages below."
    messageLoop t session

------------------------------------------------------------------------
-- Client (initiator)
------------------------------------------------------------------------

runClient :: String -> Int -> IO ()
runClient host port = do
    sk <- randomBytes 32
    let !pk = x25519 sk x25519Basepoint
    putStrLn $ "Connecting to " ++ host ++ ":" ++ show port ++ "..."
    t <- connect host port
    putStrLn "Connected. Performing key exchange..."
    session <- handshakeInitiator t sk pk
    putStrLn "Session established. Type messages below."
    messageLoop t session

------------------------------------------------------------------------
-- Ephemeral X25519 key exchange
------------------------------------------------------------------------

-- | Initiator sends their public key first, receives peer's public key,
-- computes shared secret via HKDF.
handshakeInitiator :: Transport -> BS.ByteString -> BS.ByteString
                   -> IO ChatSession
handshakeInitiator t sk pk = do
    send t (encodeMessage pk)
    peerPkRaw <- recv t 36  -- 4-byte length + 32-byte key
    let Just (peerPk, _) = decodeMessage peerPkRaw
    let !dh = x25519 sk peerPk
    let !shared = deriveShared dh pk peerPk
    initChatSession shared sk peerPk

-- | Responder receives peer's public key, sends own public key,
-- computes shared secret via HKDF.
handshakeResponder :: Transport -> BS.ByteString -> BS.ByteString
                   -> IO ChatSession
handshakeResponder t sk pk = do
    peerPkRaw <- recv t 36  -- 4-byte length + 32-byte key
    let Just (peerPk, _) = decodeMessage peerPkRaw
    send t (encodeMessage pk)
    let !dh = x25519 sk peerPk
    -- Responder uses ratchetInitBob-style ordering via initChatSession
    -- but as "Alice" with reversed key order for consistent shared secret
    let !shared = deriveShared dh peerPk pk
    initChatSession shared sk peerPk

-- | Derive a 32-byte shared secret from the DH output and both public keys.
deriveShared :: BS.ByteString -> BS.ByteString -> BS.ByteString
             -> BS.ByteString
deriveShared dhOutput pk1 pk2 =
    let !salt = pk1 <> pk2
        !prk  = hkdfExtract salt dhOutput
    in BS.take 32 (hkdfExpand prk "UmbraVox_Handshake_v1" 32)

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
            let !len = getWord32BE lenBs
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

-- | Read a big-endian Word32 from 4 bytes.
getWord32BE :: BS.ByteString -> Int
getWord32BE bs =
    fromIntegral (BS.index bs 0) * 16777216
    + fromIntegral (BS.index bs 1) * 65536
    + fromIntegral (BS.index bs 2) * 256
    + fromIntegral (BS.index bs 3)
