-- SPDX-License-Identifier: Apache-2.0
-- | UmbraClaw bridge plugin IPC subprocess management (M25.1.2, M25.2.2)
--
-- IPC subprocess that speaks UmbraVOX's line-based bridge protocol.
-- The host side is implemented in UmbraVox.Network.ProviderRuntime.
--
-- Host -> Plugin:
--   AUTH <fd>         -- authenticate / attach to UmbraClaw session
--   SEND <hex-data>   -- send message through bridge
--   RECV              -- request next available message
--   CONTACTS          -- list bridge contacts
--   STATUS            -- query bridge status
--   PING              -- liveness check
--   CLOSE             -- shut down plugin
--
-- Plugin -> Host:
--   AUTH_OK            -- authentication succeeded
--   AUTH_FAIL <msg>    -- authentication failed
--   DATA <hex-data>    -- message data (response to RECV)
--   CONTACTS <hex>     -- contact list
--   STATUS <hex>       -- status payload
--   PONG               -- liveness reply
--   OK                 -- generic acknowledgement
--   ERR <message>      -- error
module UmbraVox.Bridge.UmbraClaw.Main
    ( umbraClawBridge
      -- * Exported for testing (M25.3)
    , dispatch
    , bytesToHex
    , hexToBytes
    , stringToBytes
    , bytesToString
    ) where

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

import UmbraVox.Bridge.UmbraClaw.Session
    ( UmbraClawSession(..)
    , SessionError(..)
    , AuthState(..)
    , UmbraClawContact(..)
    , initSession
    , authenticate
    , syncContacts
    , enqueueSend
    , dequeueRecv
    )

-- | Run the UmbraClaw bridge subprocess loop.
--
-- Sets up line-buffered stdio and enters the command dispatch loop.
umbraClawBridge :: IO ()
umbraClawBridge = do
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    sessionRef <- newIORef Nothing
    bridgeLoop sessionRef

bridgeLoop :: IORef (Maybe UmbraClawSession) -> IO ()
bridgeLoop sessionRef = do
    eof <- isEOF
    if eof
        then pure ()
        else do
            line <- getLine
            dispatch sessionRef line
            hFlush stdout
            bridgeLoop sessionRef

dispatch :: IORef (Maybe UmbraClawSession) -> String -> IO ()
dispatch sessionRef line =
    case words line of
        ("AUTH"     : rest)  -> handleAuth sessionRef (unwords rest)
        ("SEND"     : rest)  -> handleSend sessionRef (unwords rest)
        ("RECV"     : _rest) -> handleRecv sessionRef
        ("CONTACTS" : _rest) -> handleContacts sessionRef
        ("STATUS"   : _rest) -> handleStatus sessionRef
        ("PING"     : _rest) -> handlePing
        ("CLOSE"    : _rest) -> handleClose
        []                   -> pure ()  -- ignore blank lines
        (cmd        : _)     -> respond ("ERR unknown command: " ++ cmd)

-- | AUTH [hex-credentials]: create and authenticate an UmbraClaw session.
--
-- If hex credentials are provided, they are decoded as "username:token".
-- Otherwise a default session is created with stub credentials.
handleAuth :: IORef (Maybe UmbraClawSession) -> String -> IO ()
handleAuth sessionRef hexCreds = do
    session <- initSession "localhost" 9000
    let (user, tok) = parseCredentials hexCreds
    authResult <- authenticate session user tok
    case authResult of
        AuthComplete -> do
            writeIORef sessionRef (Just session)
            respond "AUTH_OK"
        AuthFailed reason ->
            respond ("AUTH_FAIL " ++ reason)
        _ ->
            respond "ERR unexpected auth state"

-- | Parse hex-encoded credentials in "username:token" format.
-- Returns default stub credentials if input is empty or invalid.
parseCredentials :: String -> (ByteString, ByteString)
parseCredentials hexStr =
    case hexToBytes hexStr of
        Nothing -> (stringToBytes "umbraclaw", stringToBytes "stub-token")
        Just bs ->
            let s = bytesToString bs
                (u, rest) = break (== ':') s
            in if null rest
                then (stringToBytes "umbraclaw", stringToBytes "stub-token")
                else (stringToBytes u, stringToBytes (drop 1 rest))

-- | SEND <hex>: encode message and enqueue for UmbraClaw transport.
handleSend :: IORef (Maybe UmbraClawSession) -> String -> IO ()
handleSend sessionRef hexData = do
    mSession <- readIORef sessionRef
    case mSession of
        Nothing -> respond "ERR umbraclaw session not established"
        Just session -> do
            case hexToBytes hexData of
                Nothing -> respond "ERR invalid hex data"
                Just msgBytes -> do
                    enqueueSend session msgBytes
                    respond "OK"

-- | RECV: dequeue next available inbound message.
handleRecv :: IORef (Maybe UmbraClawSession) -> IO ()
handleRecv sessionRef = do
    mSession <- readIORef sessionRef
    case mSession of
        Nothing -> respond "ERR umbraclaw session not established"
        Just session -> do
            mMsg <- dequeueRecv session
            case mMsg of
                Nothing  -> respond "ERR no pending messages"
                Just msg -> respond ("DATA " ++ bytesToHex msg)

-- | CONTACTS: synchronize and return the contact list as hex-encoded JSON.
handleContacts :: IORef (Maybe UmbraClawSession) -> IO ()
handleContacts sessionRef = do
    mSession <- readIORef sessionRef
    case mSession of
        Nothing -> respond "ERR umbraclaw session not established"
        Just session -> do
            contacts <- syncContacts session
            let jsonStr = contactsToJson contacts
            respond ("CONTACTS " ++ bytesToHex (stringToBytes jsonStr))

-- | Serialize contact list to a JSON array string.
contactsToJson :: [UmbraClawContact] -> String
contactsToJson [] = "[]"
contactsToJson cs = "[" ++ go cs ++ "]"
  where
    go []     = ""
    go [c]    = contactToJson c
    go (c:rest) = contactToJson c ++ "," ++ go rest
    contactToJson c =
        "{\"id\":\"" ++ bytesToHex (uccId c)
        ++ "\",\"name\":\"" ++ escapeJson (uccName c)
        ++ "\",\"status\":\"" ++ escapeJson (uccStatus c)
        ++ "\"}"
    escapeJson = concatMap esc
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc ch   = [ch]

-- | STATUS: return JSON with connection/session/auth state.
handleStatus :: IORef (Maybe UmbraClawSession) -> IO ()
handleStatus sessionRef = do
    mSession <- readIORef sessionRef
    case mSession of
        Nothing -> do
            let jsonStr = "{\"connected\":false,\"session\":false,\"auth\":\"none\"}"
            respond ("STATUS " ++ bytesToHex (stringToBytes jsonStr))
        Just session -> do
            ready <- readIORef (ucsReady session)
            authState <- readIORef (ucsAuth session)
            let authStr = case authState of
                    AuthNone       -> "none"
                    AuthPending    -> "pending"
                    AuthComplete   -> "complete"
                    AuthFailed msg -> "failed:" ++ msg
                jsonStr = "{\"connected\":" ++ showBool ready
                       ++ ",\"session\":true"
                       ++ ",\"auth\":\"" ++ authStr ++ "\"}"
            respond ("STATUS " ++ bytesToHex (stringToBytes jsonStr))
  where
    showBool True  = "true"
    showBool False = "false"

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
hexToBytes [] = Just BS.empty
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
