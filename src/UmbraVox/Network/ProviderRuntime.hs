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
    ) where

import UmbraVox.Network.ProviderCatalog (TransportProviderId(..))
import UmbraVox.Network.Transport
    ( TCPListener, accept, closeListener, connect
    , connectTryPortsWithProgress, listenOn )
import UmbraVox.Network.TransportClass (AnyTransport(..))

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
