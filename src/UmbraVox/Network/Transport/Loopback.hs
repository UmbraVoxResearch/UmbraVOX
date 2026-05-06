-- | In-process loopback transport for testing and secure notes
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport.Loopback
    ( LoopbackTransport(..)
    , newLoopbackPair
    ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import UmbraVox.Network.TransportClass (TransportHandle(..))

-- | A loopback transport backed by 'MVar' pipes.
-- One side's send buffer is the other side's receive buffer.
data LoopbackTransport = LoopbackTransport
    { lbSendBuf :: MVar ByteString
    , lbRecvBuf :: MVar ByteString
    , lbLabel   :: String
    }

instance TransportHandle LoopbackTransport where
    thSend t bs = putMVar (lbSendBuf t) bs
    thRecv t n  = do
        bs <- takeMVar (lbRecvBuf t)
        pure (BS.take n bs)
    thClose _   = pure ()
    thInfo  t   = "loopback:" ++ lbLabel t

-- | Create a pair of connected loopback transports.
-- Data sent on the first is received on the second, and vice versa.
newLoopbackPair :: String -> IO (LoopbackTransport, LoopbackTransport)
newLoopbackPair label = do
    aToB <- newEmptyMVar
    bToA <- newEmptyMVar
    let a = LoopbackTransport { lbSendBuf = aToB, lbRecvBuf = bToA, lbLabel = label ++ "/A" }
        b = LoopbackTransport { lbSendBuf = bToA, lbRecvBuf = aToB, lbLabel = label ++ "/B" }
    pure (a, b)
