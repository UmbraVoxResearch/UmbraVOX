-- SPDX-License-Identifier: Apache-2.0
-- | Intercepting transport middleware for integration testing.
--
-- Wraps any 'AnyTransport' and logs all sent traffic to a shared
-- 'IORef [TrafficEntry]' for later inspection (e.g. to verify that
-- no plaintext leaks onto the wire).
module UmbraVox.Network.Transport.Intercept
    ( InterceptTransport(..)
    , TrafficEntry(..)
    , wrapWithIntercept
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, readIORef, modifyIORef')
import UmbraVox.Network.TransportClass
    (TransportHandle(..), AnyTransport, anySend, anyRecv, anyClose, anyInfo)

-- | A single logged traffic entry.
data TrafficEntry = TrafficEntry
    { teTimestamp :: !Int        -- ^ Monotonic counter value at send time
    , teFromName  :: !String     -- ^ Logical name of the sender
    , teToName    :: !String     -- ^ Logical name of the receiver
    , teRawBytes  :: !ByteString -- ^ The raw bytes that were sent
    , teSize      :: !Int        -- ^ Length of 'teRawBytes'
    } deriving stock (Show)

-- | A transport wrapper that logs every 'thSend' call before delegating
-- to the underlying transport.
data InterceptTransport = InterceptTransport
    { itInner   :: !AnyTransport          -- ^ Wrapped transport
    , itLog     :: !(IORef [TrafficEntry]) -- ^ Shared traffic log
    , itCounter :: !(IORef Int)            -- ^ Shared monotonic counter
    , itName    :: !String                 -- ^ This endpoint's name
    , itPeer    :: !String                 -- ^ Peer endpoint's name
    }

instance TransportHandle InterceptTransport where
    thSend t bs = do
        ts <- readIORef (itCounter t)
        modifyIORef' (itCounter t) (+ 1)
        let entry = TrafficEntry
                { teTimestamp = ts
                , teFromName  = itName t
                , teToName    = itPeer t
                , teRawBytes  = bs
                , teSize      = BS.length bs
                }
        modifyIORef' (itLog t) (entry :)
        anySend (itInner t) bs

    thRecv t n = anyRecv (itInner t) n

    thClose t = anyClose (itInner t)

    thInfo t = "intercept:" ++ itName t

-- | Wrap an existing transport with interception logging.
wrapWithIntercept
    :: IORef [TrafficEntry]  -- ^ Shared traffic log
    -> IORef Int             -- ^ Shared monotonic counter
    -> String                -- ^ Name of this endpoint
    -> String                -- ^ Name of the peer endpoint
    -> AnyTransport          -- ^ Underlying transport to wrap
    -> IO InterceptTransport
wrapWithIntercept logRef counterRef name peer inner =
    pure InterceptTransport
        { itInner   = inner
        , itLog     = logRef
        , itCounter = counterRef
        , itName    = name
        , itPeer    = peer
        }
