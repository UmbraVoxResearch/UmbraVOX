-- SPDX-License-Identifier: Apache-2.0
-- | Signal bridge plugin entry point (M19.3.1)
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

-- Bridge session support (stubs wired up in Phase 4).
import UmbraVox.Bridge.Signal.Session ()

main :: IO ()
main = do
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    bridgeLoop

bridgeLoop :: IO ()
bridgeLoop = do
    eof <- isEOF
    if eof
        then pure ()
        else do
            line <- getLine
            dispatch line
            hFlush stdout
            bridgeLoop

dispatch :: String -> IO ()
dispatch line =
    case words line of
        ("AUTH"     : _rest) -> handleAuth
        ("SEND"     : _rest) -> handleSend
        ("RECV"     : _rest) -> handleRecv
        ("CONTACTS" : _rest) -> handleContacts
        ("STATUS"   : _rest) -> handleStatus
        ("PING"     : _rest) -> handlePing
        ("CLOSE"    : _rest) -> handleClose
        []                   -> pure ()  -- ignore blank lines
        (cmd        : _)     -> respond ("ERR unknown command: " ++ cmd)

handleAuth :: IO ()
handleAuth = respond "AUTH_OK"

handleSend :: IO ()
handleSend = respond "ERR signal session not established"

handleRecv :: IO ()
handleRecv = respond "ERR signal session not established"

handleContacts :: IO ()
handleContacts = respond "ERR not implemented"

handleStatus :: IO ()
handleStatus = respond "ERR not implemented"

handlePing :: IO ()
handlePing = respond "PONG"

handleClose :: IO ()
handleClose = do
    respond "OK"
    exitSuccess

respond :: String -> IO ()
respond = putStrLn
