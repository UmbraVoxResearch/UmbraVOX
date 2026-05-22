-- SPDX-License-Identifier: Apache-2.0
-- | UmbraClaw bridge plugin IPC subprocess management (M25.1.2)
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
--   DATA <hex-data>    -- message data (response to RECV)
--   CONTACTS <hex>     -- contact list
--   STATUS <hex>       -- status payload
--   PONG               -- liveness reply
--   OK                 -- generic acknowledgement
--   ERR <message>      -- error
module UmbraVox.Bridge.UmbraClaw.Main
    ( umbraClawBridge
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
    , initSession
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
        ("AUTH"     : _rest) -> handleAuth sessionRef
        ("SEND"     : rest)  -> handleSend sessionRef (unwords rest)
        ("RECV"     : _rest) -> handleRecv sessionRef
        ("CONTACTS" : _rest) -> handleContacts sessionRef
        ("STATUS"   : _rest) -> handleStatus sessionRef
        ("PING"     : _rest) -> handlePing
        ("CLOSE"    : _rest) -> handleClose
        []                   -> pure ()  -- ignore blank lines
        (cmd        : _)     -> respond ("ERR unknown command: " ++ cmd)

-- | AUTH: create a stub UmbraClaw session.
-- Real key agreement will be implemented in a later milestone.
handleAuth :: IORef (Maybe UmbraClawSession) -> IO ()
handleAuth sessionRef = do
    session <- initSession "localhost" 9000
    writeIORef sessionRef (Just session)
    respond "AUTH_OK"

-- | SEND <hex>: stub -- UmbraClaw transport not yet implemented.
handleSend :: IORef (Maybe UmbraClawSession) -> String -> IO ()
handleSend sessionRef _hexData = do
    mSession <- readIORef sessionRef
    case mSession of
        Nothing -> respond "ERR umbraclaw session not established"
        Just _  -> respond "ERR send not implemented: UmbraClaw integration pending"

-- | RECV: stub -- no pending messages.
handleRecv :: IORef (Maybe UmbraClawSession) -> IO ()
handleRecv sessionRef = do
    mSession <- readIORef sessionRef
    case mSession of
        Nothing -> respond "ERR umbraclaw session not established"
        Just _  -> respond "ERR no pending messages"

-- | CONTACTS: return empty array (stub).
handleContacts :: IORef (Maybe UmbraClawSession) -> IO ()
handleContacts sessionRef = do
    mSession <- readIORef sessionRef
    case mSession of
        Nothing -> respond "ERR umbraclaw session not established"
        Just _  -> respond ("CONTACTS " ++ bytesToHex (stringToBytes "[]"))

-- | STATUS: return JSON with connection/session state.
handleStatus :: IORef (Maybe UmbraClawSession) -> IO ()
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

-- Suppress unused-import warning for hexToBytes (used in future milestones).
_suppressUnused :: String -> Maybe ByteString
_suppressUnused = hexToBytes

------------------------------------------------------------------------
-- Minimal string <-> ByteString (UTF-8 ASCII subset)
------------------------------------------------------------------------

-- | Convert a ByteString to a String (assuming ASCII/Latin-1).
bytesToString :: ByteString -> String
bytesToString = map (chr . fromIntegral) . BS.unpack

-- | Convert a String to a ByteString (truncates to low byte).
stringToBytes :: String -> ByteString
stringToBytes = BS.pack . map (fromIntegral . ord)
