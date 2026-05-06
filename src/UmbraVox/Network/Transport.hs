-- | TCP connection management
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport
  ( TCPTransport(..)
  , listen
  , connect
  , connectTryPorts
  , send
  , recv
  , close
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Exception (SomeException, onException, try)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import UmbraVox.Network.TransportClass (TransportHandle(..))

-- | A TCP transport handle.
data TCPTransport = TCPTransport
  { tSocket :: !NS.Socket
  , tAddr   :: !NS.SockAddr
  }

instance TransportHandle TCPTransport where
    thSend  = send
    thRecv  = recv
    thClose = close
    thInfo t = "tcp:" ++ show (tAddr t)

-- | Listen on the given port and accept one incoming connection.
listen :: Int -> IO TCPTransport
listen port = do
    let hints = NS.defaultHints
          { NS.addrFlags      = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          , NS.addrFamily     = NS.AF_INET
          }
    addr : _ <- NS.getAddrInfo (Just hints) (Just "0.0.0.0") (Just (show port))
    sock <- NS.openSocket addr
    flip onException (NS.close sock) $ do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.bind sock (NS.addrAddress addr)
        NS.listen sock 1
        (conn, peer) <- NS.accept sock
        NS.close sock
        pure TCPTransport { tSocket = conn, tAddr = peer }

-- | Establish a TCP connection to the given host and port.
connect :: String -> Int -> IO TCPTransport
connect host port = do
    let hints = NS.defaultHints
          { NS.addrSocketType = NS.Stream
          , NS.addrFamily     = NS.AF_INET
          }
    addr : _ <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
    sock <- NS.openSocket addr
    NS.connect sock (NS.addrAddress addr) `onException` NS.close sock
    pure TCPTransport { tSocket = sock, tAddr = NS.addrAddress addr }

-- | Try connecting to a host on a sequence of ports, returning the first success.
-- Throws the last error if all ports fail.
connectTryPorts :: String -> [Int] -> IO TCPTransport
connectTryPorts host [] = connect host 7853  -- fallback to primary default
connectTryPorts host [p] = connect host p
connectTryPorts host (p:ps) = do
    result <- try (connect host p) :: IO (Either SomeException TCPTransport)
    case result of
        Right t -> pure t
        Left _  -> connectTryPorts host ps

-- | Send all bytes over the transport.
send :: TCPTransport -> ByteString -> IO ()
send t bs = NSB.sendAll (tSocket t) bs

-- | Receive exactly @n@ bytes from the transport.
recv :: TCPTransport -> Int -> IO ByteString
recv t n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
        chunk <- NSB.recv (tSocket t) remaining
        if BS.null chunk
            then pure (BS.concat (reverse acc))
            else go (remaining - BS.length chunk) (chunk : acc)

-- | Close the transport connection.
close :: TCPTransport -> IO ()
close t = NS.gracefulClose (tSocket t) 5000
