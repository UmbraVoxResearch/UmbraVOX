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
    ) where

import qualified Data.ByteString as BS
import Data.Char (digitToInt, intToDigit)
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
data IPCTransport = IPCTransport
    { ipcStdin   :: Handle
    , ipcStdout  :: Handle
    , ipcProcess :: ProcessHandle
    , ipcLabel   :: String
    }

instance TransportHandle IPCTransport where
    thSend t bs = do
        hPutStrLn (ipcStdin t) ("SEND " ++ toHex bs)
        hFlush (ipcStdin t)
        resp <- hGetLine (ipcStdout t)
        case resp of
            "OK" -> pure ()
            _    -> ioError (userError ("ipc-send: unexpected response: " ++ resp))

    thRecv t _n = do
        hPutStrLn (ipcStdin t) "RECV"
        hFlush (ipcStdin t)
        resp <- hGetLine (ipcStdout t)
        case stripCommandPrefix "DATA " resp of
            Just hex -> pure (fromHex hex)
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
            let transport = IPCTransport
                    { ipcStdin   = hIn
                    , ipcStdout  = hOut
                    , ipcProcess = ph
                    , ipcLabel   = "ipc:" ++ path
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
ipcRecvResponse :: IPCTransport -> IO String
ipcRecvResponse t = hGetLine (ipcStdout t)

-- | Shut down an IPC provider process.
ipcClose :: IPCTransport -> IO ()
ipcClose = thClose

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

fromHex :: String -> BS.ByteString
fromHex = BS.pack . decodeHexPairs
  where
    decodeHexPairs [] = []
    decodeHexPairs [_] = []  -- odd trailing nibble is dropped
    decodeHexPairs (hi:lo:rest) =
        (fromIntegral (digitToInt hi) * 16 + fromIntegral (digitToInt lo))
            : decodeHexPairs rest

stripCommandPrefix :: String -> String -> Maybe String
stripCommandPrefix prefix str
    | take (length prefix) str == prefix = Just (drop (length prefix) str)
    | otherwise = Nothing
