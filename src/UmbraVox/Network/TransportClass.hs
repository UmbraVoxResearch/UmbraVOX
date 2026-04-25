{-# LANGUAGE ExistentialQuantification #-}
-- | Abstract transport interface
--
-- Any communication channel (TCP, UDP, blockchain, Signal server,
-- XMPP, Discord, etc.) implements 'TransportHandle'.
--
-- See: doc/spec/network.md
module UmbraVox.Network.TransportClass
    ( TransportHandle(..)
    , AnyTransport(..)
    , anySend, anyRecv, anyClose, anyInfo
    ) where

import Data.ByteString (ByteString)

-- | Abstract transport interface.
class TransportHandle t where
    thSend  :: t -> ByteString -> IO ()
    thRecv  :: t -> Int -> IO ByteString
    thClose :: t -> IO ()
    thInfo  :: t -> String

-- | Existential wrapper for polymorphic transport usage.
data AnyTransport = forall t. TransportHandle t => AnyTransport t

anySend :: AnyTransport -> ByteString -> IO ()
anySend (AnyTransport t) = thSend t

anyRecv :: AnyTransport -> Int -> IO ByteString
anyRecv (AnyTransport t) = thRecv t

anyClose :: AnyTransport -> IO ()
anyClose (AnyTransport t) = thClose t

anyInfo :: AnyTransport -> String
anyInfo (AnyTransport t) = thInfo t
