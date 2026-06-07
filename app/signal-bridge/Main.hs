-- SPDX-License-Identifier: Apache-2.0
-- | Signal bridge plugin entry point (M19.3.1, M19.3.7-M19.3.8)
--
-- IPC subprocess that speaks UmbraVOX's line-based bridge protocol.
-- The host side is implemented in UmbraVox.Network.ProviderRuntime.
--
-- Host -> Plugin:
--   AUTH <fd>         -- authenticate / attach to Signal session
--   SEND <hex-data>   -- send message through bridge
--   RECV              -- request next available message
--   CONTACTS          -- list bridge contacts
--   STATUS            -- query bridge status
--   PING              -- liveness check
--   CLOSE             -- shut down plugin
--
-- Plugin -> Host:
--   AUTH_OK            -- authentication succeeded
--   DATA <hex-data>    -- message data (response to RECV)
--   CONTACTS <hex>     -- contact list
--   STATUS <hex>       -- status payload
--   PONG               -- liveness reply
--   OK                 -- generic acknowledgement
--   ERR <message>      -- error
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, isHexDigit, ord, chr, intToDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Exit (exitSuccess)
import System.IO
    ( BufferMode(..)
    , hFlush
    , hSetBuffering
    , stdin
    , stdout
    , stderr
    , isEOF
    )

import UmbraVox.Bridge.Signal.Session
    ( SignalBridgeSession(..)
    , initBridgeSession
    , sendBridgeMessage
    , encodeBridgeEnvelope
    )
import UmbraVox.Crypto.Signal.Compat
    ( signalRatchetInitBob
    )

main :: IO ()
main = do
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    sessionRef <- newIORef Nothing
    bridgeLoop sessionRef

bridgeLoop :: IORef (Maybe SignalBridgeSession) -> IO ()
bridgeLoop sessionRef = do
    eof <- isEOF
    if eof
        then pure ()
        else do
            line <- getLine
            dispatch sessionRef line
            hFlush stdout
            bridgeLoop sessionRef

dispatch :: IORef (Maybe SignalBridgeSession) -> String -> IO ()
dispatch sessionRef line =
    case words line of
        ("AUTH"     : _rest) -> handleAuth sessionRef
        ("SEND"     : rest)  -> handleSend sessionRef (unwords rest)
        ("RECV"     : _rest) -> handleRecv sessionRef
        ("CONTACTS" : _rest) -> handleContacts sessionRef
        ("STATUS"   : _rest) -> handleStatus sessionRef
        ("PING"     : _rest) -> handlePing
        ("CLOSE"    : _rest) -> handleClose
        []                   -> pure ()  -- ignore blank lines
        (cmd        : _)     -> respond ("ERR unknown command: " ++ cmd)

-- | AUTH: create a dummy bridge session (placeholder shared secret).
-- Real X3DH key agreement will replace this in a later milestone.
handleAuth :: IORef (Maybe SignalBridgeSession) -> IO ()
handleAuth sessionRef = do
    let sharedSecret = BS.replicate 32 0x42  -- placeholder
        bobSPK       = BS.replicate 32 0x13  -- placeholder
    ratchetState <- signalRatchetInitBob sharedSecret bobSPK
    session <- initBridgeSession "wss://localhost:8080" ratchetState 1 1
    writeIORef sessionRef (Just session)
    respond "AUTH_OK"

-- | SEND <hex>: decode hex to JSON envelope, extract body, encrypt via
-- Signal ratchet, encode as protobuf wire bytes, respond with hex.
handleSend :: IORef (Maybe SignalBridgeSession) -> String -> IO ()
handleSend sessionRef hexData = do
    mSession <- readIORef sessionRef
    case mSession of
        Nothing -> respond "ERR signal session not established"
        Just session ->
            case hexToBytes hexData of
                Nothing -> respond "ERR invalid hex encoding"
                Just jsonBytes ->
                    let jsonStr = bytesToString jsonBytes
                    in case parseJsonBody jsonStr of
                        Nothing -> respond "ERR failed to parse envelope JSON"
                        Just body -> do
                            let plaintext = stringToBytes body
                            eEnc <- sendBridgeMessage session plaintext
                            case eEnc of
                                Left err -> respond ("ERR encrypt: " ++ show err)
                                Right wireMsg -> do
                                    eEnv <- encodeBridgeEnvelope session wireMsg
                                    case eEnv of
                                        Left err ->
                                            respond ("ERR envelope: " ++ show err)
                                        Right envBytes ->
                                            respond ("OK " ++ bytesToHex envBytes)

-- | RECV: no pending messages (real polling needs WebSocket, M19.3.5).
handleRecv :: IORef (Maybe SignalBridgeSession) -> IO ()
handleRecv sessionRef = do
    mSession <- readIORef sessionRef
    case mSession of
        Nothing -> respond "ERR signal session not established"
        Just _  -> respond "ERR no pending messages"

-- | CONTACTS: return empty array (real contact list needs server query).
handleContacts :: IORef (Maybe SignalBridgeSession) -> IO ()
handleContacts sessionRef = do
    mSession <- readIORef sessionRef
    case mSession of
        Nothing -> respond "ERR signal session not established"
        Just _  -> respond ("CONTACTS " ++ bytesToHex (stringToBytes "[]"))

-- | STATUS: return JSON with connection/session state.
handleStatus :: IORef (Maybe SignalBridgeSession) -> IO ()
handleStatus sessionRef = do
    mSession <- readIORef sessionRef
    let hasSession = case mSession of
            Nothing -> False
            Just _  -> True
        jsonStr = "{\"connected\":false,\"session\":"
               ++ (if hasSession then "true" else "false")
               ++ "}"
    respond ("STATUS " ++ bytesToHex (stringToBytes jsonStr))

handlePing :: IO ()
handlePing = respond "PONG"

handleClose :: IO ()
handleClose = do
    respond "OK"
    exitSuccess

respond :: String -> IO ()
respond = putStrLn

------------------------------------------------------------------------
-- Minimal hex encoding/decoding
------------------------------------------------------------------------

-- | Encode a ByteString as lowercase hex.
bytesToHex :: ByteString -> String
bytesToHex = concatMap toHex . BS.unpack
  where
    toHex b = [intToDigit (fromIntegral (b `div` 16))
              ,intToDigit (fromIntegral (b `mod` 16))]

-- | Decode a hex string to a ByteString. Returns Nothing on invalid input.
hexToBytes :: String -> Maybe ByteString
hexToBytes s
    | odd (length s) = Nothing
    | not (all isHexDigit s) = Nothing
    | otherwise = Just (BS.pack (go s))
  where
    go []       = []
    go [_]      = []  -- unreachable due to odd check
    go (a:b:cs) = fromIntegral (digitToInt a * 16 + digitToInt b) : go cs

------------------------------------------------------------------------
-- Minimal string <-> ByteString (UTF-8 ASCII subset)
------------------------------------------------------------------------

-- | Convert a ByteString to a String (assuming ASCII/Latin-1).
bytesToString :: ByteString -> String
bytesToString = map (chr . fromIntegral) . BS.unpack

-- | Convert a String to a ByteString (truncates to low byte).
stringToBytes :: String -> ByteString
stringToBytes = BS.pack . map (fromIntegral . ord)

------------------------------------------------------------------------
-- Minimal JSON parsing (controlled internal envelopes only)
--
-- Parses: {"to":"...", "body":"...", "timestamp":123}
-- Extracts the "body" field value.
------------------------------------------------------------------------

-- | Extract the value of the "body" key from a flat JSON object.
-- Handles string escapes for \\, \", \n, \t only.
parseJsonBody :: String -> Maybe String
parseJsonBody = extractField "body"

-- | Extract a string-valued field from a flat JSON object.
extractField :: String -> String -> Maybe String
extractField key json =
    let needle = "\"" ++ key ++ "\""
    in case findSubstring needle json of
        Nothing  -> Nothing
        Just rest ->
            -- rest starts just after the key; skip whitespace and colon
            case dropWhile (\c -> c == ' ' || c == '\t') rest of
                (':' : afterColon) ->
                    case dropWhile (\c -> c == ' ' || c == '\t') afterColon of
                        ('"' : valStart) -> Just (parseJsonString valStart)
                        _                -> Nothing
                _ -> Nothing

-- | Find a substring and return everything after it.
findSubstring :: String -> String -> Maybe String
findSubstring _needle [] = Nothing
findSubstring needle haystack@(_:hs)
    | take (length needle) haystack == needle
        = Just (drop (length needle) haystack)
    | otherwise = findSubstring needle hs

-- | Parse a JSON string value (after opening quote).
-- Stops at unescaped closing quote.
parseJsonString :: String -> String
parseJsonString []             = []
parseJsonString ('\\':'"':cs)  = '"' : parseJsonString cs
parseJsonString ('\\':'\\':cs) = '\\' : parseJsonString cs
parseJsonString ('\\':'n':cs)  = '\n' : parseJsonString cs
parseJsonString ('\\':'t':cs)  = '\t' : parseJsonString cs
parseJsonString ('"':_)        = []
parseJsonString (c:cs)         = c : parseJsonString cs
