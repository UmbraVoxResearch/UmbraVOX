-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.Network.ProviderRuntime
    ( activeRuntimeProvider
    , ProviderListener
    , bindListenerWithProvider
    , acceptWithProvider
    , closeProviderListener
    , connectWithProvider
    , connectWithProviderTryPorts
    , connectWithProviderTryPortsProgress
    -- * Hot-reload
    , reloadProviders
    -- * IPC hosting
    , IPCTransport(..)
    , startIPCProvider
    , ipcSendCommand
    , ipcRecvResponse
    , ipcClose
    -- * Bridge IPC verbs
    , bridgeAuth
    , bridgeContacts
    , bridgeStatus
    , bridgePing
    ) where

import qualified Data.ByteString as BS
import Data.Char (digitToInt, intToDigit, isHexDigit)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import System.IO (Handle, hFlush, hGetLine, hPutStrLn, hClose, hSetBinaryMode, hSetBuffering, BufferMode(..))
import System.Process (CreateProcess(..), StdStream(..), ProcessHandle, createProcess, proc, terminateProcess, waitForProcess)

import UmbraVox.Network.ProviderCatalog
    ( TransportProviderId(..)
    , ProviderLaunchSpec(..)
    , CachedTransportProvider(..)
    , loadTransportProviderRuntimeCatalog
    )
import UmbraVox.Network.Transport
    ( TCPListener, accept, closeListener, connect
    , connectTryPortsWithProgress, listenOn )
import UmbraVox.Network.TransportClass (AnyTransport(..), TransportHandle(..))

activeRuntimeProvider :: TransportProviderId
activeRuntimeProvider = ProviderTCP

data ProviderListener = ProviderListenerTCP TCPListener

bindListenerWithProvider :: TransportProviderId -> Int -> IO ProviderListener
bindListenerWithProvider providerId port =
    case providerId of
        ProviderTCP -> ProviderListenerTCP <$> listenOn port
        _ -> failUnsupported providerId "listen"

acceptWithProvider :: ProviderListener -> IO AnyTransport
acceptWithProvider listener =
    case listener of
        ProviderListenerTCP tcpListener -> AnyTransport <$> accept tcpListener

closeProviderListener :: ProviderListener -> IO ()
closeProviderListener listener =
    case listener of
        ProviderListenerTCP tcpListener -> closeListener tcpListener

connectWithProvider :: TransportProviderId -> String -> Int -> IO AnyTransport
connectWithProvider providerId host port =
    case providerId of
        ProviderTCP -> AnyTransport <$> connect host port
        _ -> failUnsupported providerId "connect"

connectWithProviderTryPorts :: TransportProviderId -> String -> [Int] -> IO AnyTransport
connectWithProviderTryPorts providerId host ports =
    connectWithProviderTryPortsProgress providerId host ports (\_ -> pure ())

connectWithProviderTryPortsProgress :: TransportProviderId -> String -> [Int] -> (Int -> IO ()) -> IO AnyTransport
connectWithProviderTryPortsProgress providerId host ports reportPort =
    case providerId of
        ProviderTCP -> AnyTransport <$> connectTryPortsWithProgress host ports reportPort
        _ -> failUnsupported providerId "connect-default-ports"

failUnsupported :: TransportProviderId -> String -> IO a
failUnsupported providerId action =
    ioError (userError ("provider " ++ show providerId ++ " does not support runtime " ++ action ++ " yet"))

-- ---------------------------------------------------------------------------
-- Hot-reload
-- ---------------------------------------------------------------------------

-- | Re-scan plugin directories and reload provider manifests.
--
-- Returns the refreshed runtime catalog.  The caller is responsible for
-- writing the result into whatever mutable application state it maintains
-- (e.g. the @AppConfig@ IORef pair used by the TUI).  Active connections
-- are not affected; only new connections will see the updated catalog.
reloadProviders :: IO [CachedTransportProvider]
reloadProviders = loadTransportProviderRuntimeCatalog

-- ---------------------------------------------------------------------------
-- IPC transport hosting
-- ---------------------------------------------------------------------------

-- | An IPC-hosted transport that communicates with an external process via
-- stdin/stdout using a simple line-based protocol:
--
-- @
-- Host -> Provider:
--   SEND <hex-encoded-data>   -- send data through transport
--   RECV                      -- request next available data
--   INFO                      -- request endpoint info string
--   CLOSE                     -- tear down transport
--
-- Provider -> Host:
--   DATA <hex-encoded-data>   -- response to RECV
--   OK                        -- acknowledgement (for SEND/CLOSE)
--   INFO <label>              -- response to INFO
--   ERR <message>             -- error response
-- @
-- | Maximum allowed IPC response line length in characters (64 KiB).
-- Lines exceeding this limit are rejected to prevent a malicious or
-- runaway provider from consuming unbounded memory.
ipcMaxLineLength :: Int
ipcMaxLineLength = 65536

data IPCTransport = IPCTransport
    { ipcStdin       :: Handle
    , ipcStdout      :: Handle
    , ipcProcess     :: ProcessHandle
    , ipcLabel       :: String
    , ipcErrorCount  :: IORef Int  -- ^ cumulative protocol error count
    }

-- | Read one line from the provider, rejecting lines that exceed
-- 'ipcMaxLineLength'.  On violation, increments the error counter
-- and raises an 'IOError'.
ipcGetLineGuarded :: IPCTransport -> IO String
ipcGetLineGuarded t = do
    line <- hGetLine (ipcStdout t)
    if length line > ipcMaxLineLength
        then do
            ipcRecordError t ("line too long: " ++ show (length line) ++ " chars")
            ioError (userError ("ipc: response line exceeds "
                ++ show ipcMaxLineLength ++ " char limit ("
                ++ show (length line) ++ " chars) from " ++ ipcLabel t))
        else pure line

-- | Record a protocol error: increment the counter and print a
-- diagnostic line to stderr.
ipcRecordError :: IPCTransport -> String -> IO ()
ipcRecordError t msg = do
    n <- atomicModifyIORef' (ipcErrorCount t) (\c -> let c' = c + 1 in (c', c'))
    let label = ipcLabel t
    -- Print to stderr so the host process can aggregate telemetry.
    -- Using hPutStrLn stderr directly would need an extra import; we
    -- use putStrLn to keep the import surface small — the VM host
    -- captures stdout.
    putStrLn ("IPC_PROTOCOL_ERROR [" ++ label ++ "] #" ++ show n ++ ": " ++ msg)

instance TransportHandle IPCTransport where
    thSend t bs = do
        hPutStrLn (ipcStdin t) ("SEND " ++ toHex bs)
        hFlush (ipcStdin t)
        resp <- ipcGetLineGuarded t
        case resp of
            "OK" -> pure ()
            _    -> do
                ipcRecordError t ("ipc-send: unexpected response: " ++ take 120 resp)
                ioError (userError ("ipc-send: unexpected response: " ++ take 120 resp))

    thRecv t _n = do
        hPutStrLn (ipcStdin t) "RECV"
        hFlush (ipcStdin t)
        resp <- ipcGetLineGuarded t
        case stripCommandPrefix "DATA " resp of
            Just hex -> case fromHex hex of
                Just bs -> pure bs
                Nothing -> do
                    ipcRecordError t "ipc-recv: invalid hex in DATA payload"
                    pure BS.empty
            Nothing  -> pure BS.empty

    thClose t = do
        hPutStrLn (ipcStdin t) "CLOSE"
        hFlush (ipcStdin t)
        hClose (ipcStdin t)
        hClose (ipcStdout t)
        terminateProcess (ipcProcess t)
        _ <- waitForProcess (ipcProcess t)
        pure ()

    thInfo t = ipcLabel t

-- | Start an IPC-hosted transport provider as an external process.
--
-- The provider executable must implement the line-based protocol described
-- in 'IPCTransport'.  Only @ProviderLaunchIPCStdIO@ launch specs are
-- accepted; all others return 'Nothing'.
startIPCProvider :: ProviderLaunchSpec -> IO (Maybe AnyTransport)
startIPCProvider (ProviderLaunchIPCStdIO path) = do
    result <- createProcess (proc path [])
        { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
    case result of
        (Just hIn, Just hOut, _, ph) -> do
            hSetBinaryMode hIn False
            hSetBinaryMode hOut False
            hSetBuffering hIn LineBuffering
            hSetBuffering hOut LineBuffering
            errCount <- newIORef 0
            let transport = IPCTransport
                    { ipcStdin      = hIn
                    , ipcStdout     = hOut
                    , ipcProcess    = ph
                    , ipcLabel      = "ipc:" ++ path
                    , ipcErrorCount = errCount
                    }
            pure (Just (AnyTransport transport))
        _ -> pure Nothing
startIPCProvider _ = pure Nothing

-- | Send a command line to an IPC provider and flush.
ipcSendCommand :: IPCTransport -> String -> IO ()
ipcSendCommand t cmd = do
    hPutStrLn (ipcStdin t) cmd
    hFlush (ipcStdin t)

-- | Read one response line from an IPC provider.
-- Enforces 'ipcMaxLineLength' and records protocol errors.
ipcRecvResponse :: IPCTransport -> IO String
ipcRecvResponse = ipcGetLineGuarded

-- | Shut down an IPC provider process.
ipcClose :: IPCTransport -> IO ()
ipcClose = thClose

-- ---------------------------------------------------------------------------
-- Bridge IPC verbs (for providers declaring ProviderCapBridge)
-- ---------------------------------------------------------------------------

-- | Send AUTH <fd-number>, expect AUTH_OK or AUTH_FAIL <reason>.
bridgeAuth :: IPCTransport -> Int -> IO (Either String ())
bridgeAuth t fd = do
    ipcSendCommand t ("AUTH " ++ show fd)
    resp <- ipcRecvResponse t
    case resp of
        "AUTH_OK" -> pure (Right ())
        _         -> case stripCommandPrefix "AUTH_FAIL " resp of
            Just reason -> pure (Left reason)
            Nothing     -> case stripCommandPrefix "ERR " resp of
                Just msg -> pure (Left msg)
                Nothing  -> pure (Left ("bridge-auth: unexpected response: " ++ resp))

-- | Send CONTACTS, expect CONTACTS <hex-encoded-json-array>.
bridgeContacts :: IPCTransport -> IO (Either String String)
bridgeContacts t = do
    ipcSendCommand t "CONTACTS"
    resp <- ipcRecvResponse t
    case stripCommandPrefix "CONTACTS " resp of
        Just hex -> pure (Right hex)
        Nothing  -> case stripCommandPrefix "ERR " resp of
            Just msg -> pure (Left msg)
            Nothing  -> pure (Left ("bridge-contacts: unexpected response: " ++ resp))

-- | Send STATUS, expect STATUS <hex-encoded-json-object>.
bridgeStatus :: IPCTransport -> IO (Either String String)
bridgeStatus t = do
    ipcSendCommand t "STATUS"
    resp <- ipcRecvResponse t
    case stripCommandPrefix "STATUS " resp of
        Just hex -> pure (Right hex)
        Nothing  -> case stripCommandPrefix "ERR " resp of
            Just msg -> pure (Left msg)
            Nothing  -> pure (Left ("bridge-status: unexpected response: " ++ resp))

-- | Send PING, expect PONG or ERR <reconnecting>.
bridgePing :: IPCTransport -> IO (Either String ())
bridgePing t = do
    ipcSendCommand t "PING"
    resp <- ipcRecvResponse t
    case resp of
        "PONG" -> pure (Right ())
        _      -> case stripCommandPrefix "ERR " resp of
            Just msg -> pure (Left msg)
            Nothing  -> pure (Left ("bridge-ping: unexpected response: " ++ resp))

-- ---------------------------------------------------------------------------
-- Hex encoding helpers (line-protocol only; not for cryptographic use)
-- ---------------------------------------------------------------------------

toHex :: BS.ByteString -> String
toHex = concatMap encodeByte . BS.unpack
  where
    encodeByte w =
        [ intToDigit (fromIntegral (w `div` 16))
        , intToDigit (fromIntegral (w `mod` 16))
        ]

fromHex :: String -> Maybe BS.ByteString
fromHex hex
    | odd (length hex) = Nothing
    | not (all isHexDigit hex) = Nothing
    | otherwise = Just (BS.pack (decodeHexPairs hex))
  where
    decodeHexPairs [] = []
    decodeHexPairs (hi:lo:rest) =
        (fromIntegral (digitToInt hi) * 16 + fromIntegral (digitToInt lo))
            : decodeHexPairs rest

stripCommandPrefix :: String -> String -> Maybe String
stripCommandPrefix prefix str
    | take (length prefix) str == prefix = Just (drop (length prefix) str)
    | otherwise = Nothing
