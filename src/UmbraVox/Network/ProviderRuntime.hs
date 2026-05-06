-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.Network.ProviderRuntime
    ( activeRuntimeProvider
    , listenWithProvider
    , connectWithProvider
    , connectWithProviderTryPorts
    ) where

import UmbraVox.Network.ProviderCatalog (TransportProviderId(..))
import UmbraVox.Network.Transport (connect, connectTryPorts, listen)
import UmbraVox.Network.TransportClass (AnyTransport(..))

activeRuntimeProvider :: TransportProviderId
activeRuntimeProvider = ProviderTCP

listenWithProvider :: TransportProviderId -> Int -> IO AnyTransport
listenWithProvider providerId port =
    case providerId of
        ProviderTCP -> AnyTransport <$> listen port
        _ -> failUnsupported providerId "listen"

connectWithProvider :: TransportProviderId -> String -> Int -> IO AnyTransport
connectWithProvider providerId host port =
    case providerId of
        ProviderTCP -> AnyTransport <$> connect host port
        _ -> failUnsupported providerId "connect"

connectWithProviderTryPorts :: TransportProviderId -> String -> [Int] -> IO AnyTransport
connectWithProviderTryPorts providerId host ports =
    case providerId of
        ProviderTCP -> AnyTransport <$> connectTryPorts host ports
        _ -> failUnsupported providerId "connect-default-ports"

failUnsupported :: TransportProviderId -> String -> IO a
failUnsupported providerId action =
    ioError (userError ("provider " ++ show providerId ++ " does not support runtime " ++ action ++ " yet"))
