-- SPDX-License-Identifier: Apache-2.0
-- | Template bridge plugin for UmbraVOX (M19.6)
--
-- Standalone Haskell IPC subprocess that handles all bridge protocol verbs
-- with stub responses.  Copy this file and replace the stub handlers with
-- real logic for your target messaging system.
--
-- Compile:  ghc -O2 Main.hs -o my-bridge-binary
--
-- Protocol overview (line-based, stdin/stdout):
--
-- Host -> Plugin:
--   AUTH <arg>       -- authenticate / attach to external session
--   SEND <hex-data>  -- send message (hex-encoded JSON envelope)
--   RECV             -- request next inbound message
--   CONTACTS         -- list contacts
--   STATUS           -- query connection state
--   PING             -- liveness check
--   CLOSE            -- graceful shutdown
--
-- Plugin -> Host:
--   AUTH_OK          -- authentication succeeded
--   OK [<hex>]       -- success (optional payload)
--   DATA <hex>       -- inbound message
--   CONTACTS <hex>   -- contact list (hex-encoded JSON)
--   STATUS <hex>     -- status payload (hex-encoded JSON)
--   PONG             -- liveness reply
--   ERR <message>    -- error
module Main (main) where

import Data.Char (chr, digitToInt, intToDigit, isHexDigit, ord)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Exit (exitSuccess)
import System.IO
    ( BufferMode(..)
    , hFlush
    , hSetBuffering
    , isEOF
    , stderr
    , stdin
    , stdout
    )

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    ref <- newIORef False  -- simple "authenticated" flag; replace with session state
    bridgeLoop ref

bridgeLoop :: IORef Bool -> IO ()
bridgeLoop ref = do
    eof <- isEOF
    if eof
        then pure ()
        else do
            line <- getLine
            dispatch ref line
            hFlush stdout
            bridgeLoop ref

dispatch :: IORef Bool -> String -> IO ()
dispatch ref line =
    case words line of
        ("AUTH"     : rest)  -> handleAuth ref (unwords rest)
        ("SEND"     : rest)  -> handleSend ref (unwords rest)
        ("RECV"     : _)     -> handleRecv ref
        ("CONTACTS" : _)     -> handleContacts ref
        ("STATUS"   : _)     -> handleStatus ref
        ("PING"     : _)     -> handlePing
        ("CLOSE"    : _)     -> handleClose
        []                   -> pure ()  -- ignore blank lines
        (cmd        : _)     -> respond ("ERR unknown command: " ++ cmd)

------------------------------------------------------------------------
-- Verb handlers (stubs -- replace with real implementation)
------------------------------------------------------------------------

-- | AUTH: establish a session with the external service.
-- Replace this stub with real authentication logic (OAuth, API key, etc.).
handleAuth :: IORef Bool -> String -> IO ()
handleAuth ref _arg = do
    -- TODO: connect to external service, validate credentials
    writeIORef ref True
    respond "AUTH_OK"

-- | SEND <hex>: decode the hex-encoded JSON envelope and deliver the
-- message to the external service.
handleSend :: IORef Bool -> String -> IO ()
handleSend ref hexData = do
    authed <- readIORef ref
    if not authed
        then respond "ERR not authenticated"
        else case hexToBytes hexData of
            Nothing -> respond "ERR invalid hex encoding"
            Just bytes ->
                let json = bytesToString bytes
                in case extractField "body" json of
                    Nothing   -> respond "ERR missing body in envelope"
                    Just body -> do
                        -- TODO: send 'body' to external service
                        --       encrypt if needed, get wire bytes back
                        let _ = body  -- placeholder usage
                        respond "OK"

-- | RECV: return the next inbound message, or ERR if none available.
handleRecv :: IORef Bool -> IO ()
handleRecv ref = do
    authed <- readIORef ref
    if not authed
        then respond "ERR not authenticated"
        else
            -- TODO: poll/dequeue from external service
            respond "ERR no pending messages"

-- | CONTACTS: return the contact list as a hex-encoded JSON array.
handleContacts :: IORef Bool -> IO ()
handleContacts ref = do
    authed <- readIORef ref
    if not authed
        then respond "ERR not authenticated"
        else
            -- TODO: query external service for contacts
            respond ("CONTACTS " ++ stringToHex "[]")

-- | STATUS: return bridge state as a hex-encoded JSON object.
handleStatus :: IORef Bool -> IO ()
handleStatus ref = do
    authed <- readIORef ref
    let json = "{\"connected\":false,\"authenticated\":"
             ++ (if authed then "true" else "false")
             ++ "}"
    respond ("STATUS " ++ stringToHex json)

handlePing :: IO ()
handlePing = respond "PONG"

handleClose :: IO ()
handleClose = do
    respond "OK"
    exitSuccess

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

respond :: String -> IO ()
respond = putStrLn

-- | Encode a String as hex.
stringToHex :: String -> String
stringToHex = concatMap (\c -> let b = ord c
    in [intToDigit (b `div` 16), intToDigit (b `mod` 16)])

-- | Decode hex to a list of bytes (as Chars).
hexToBytes :: String -> Maybe String
hexToBytes s
    | odd (length s)        = Nothing
    | not (all isHexDigit s) = Nothing
    | otherwise             = Just (go s)
  where
    go []       = []
    go (a:b:cs) = chr (digitToInt a * 16 + digitToInt b) : go cs
    go [_]      = []  -- unreachable

-- | Alias: decode hex to String.
bytesToString :: String -> String
bytesToString = id

-- | Extract a string-valued field from a flat JSON object.
-- Handles \\, \", \n, \t escapes only.
extractField :: String -> String -> Maybe String
extractField key json =
    let needle = "\"" ++ key ++ "\""
    in case findSubstring needle json of
        Nothing   -> Nothing
        Just rest ->
            case dropWhile (\c -> c == ' ' || c == '\t') rest of
                (':' : afterColon) ->
                    case dropWhile (\c -> c == ' ' || c == '\t') afterColon of
                        ('"' : valStart) -> Just (parseJsonString valStart)
                        _                -> Nothing
                _ -> Nothing

findSubstring :: String -> String -> Maybe String
findSubstring _needle [] = Nothing
findSubstring needle haystack@(_:hs)
    | take (length needle) haystack == needle
        = Just (drop (length needle) haystack)
    | otherwise = findSubstring needle hs

parseJsonString :: String -> String
parseJsonString []             = []
parseJsonString ('\\':'"':cs)  = '"' : parseJsonString cs
parseJsonString ('\\':'\\':cs) = '\\' : parseJsonString cs
parseJsonString ('\\':'n':cs)  = '\n' : parseJsonString cs
parseJsonString ('\\':'t':cs)  = '\t' : parseJsonString cs
parseJsonString ('"':_)        = []
parseJsonString (c:cs)         = c : parseJsonString cs
