-- | TCP connection management
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport
  ( Transport(..)
  , listen
  , connect
  , send
  , recv
  , close
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

-- | An opaque TCP transport handle.
data Transport = Transport
  { tSocket :: !NS.Socket
  , tAddr   :: !NS.SockAddr
  }

-- | Listen on the given port and accept one incoming connection.
listen :: Int -> IO Transport
listen port = do
    let hints = NS.defaultHints
          { NS.addrFlags      = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          , NS.addrFamily     = NS.AF_INET
          }
    addr : _ <- NS.getAddrInfo (Just hints) (Just "0.0.0.0") (Just (show port))
    sock <- NS.openSocket addr
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress addr)
    NS.listen sock 1
    (conn, peer) <- NS.accept sock
    NS.close sock
    pure Transport { tSocket = conn, tAddr = peer }

-- | Establish a TCP connection to the given host and port.
connect :: String -> Int -> IO Transport
connect host port = do
    let hints = NS.defaultHints
          { NS.addrSocketType = NS.Stream
          , NS.addrFamily     = NS.AF_INET
          }
    addr : _ <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
    sock <- NS.openSocket addr
    NS.connect sock (NS.addrAddress addr)
    pure Transport { tSocket = sock, tAddr = NS.addrAddress addr }

-- | Send all bytes over the transport.
send :: Transport -> ByteString -> IO ()
send t bs = NSB.sendAll (tSocket t) bs

-- | Receive exactly @n@ bytes from the transport.
recv :: Transport -> Int -> IO ByteString
recv t n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
        chunk <- NSB.recv (tSocket t) remaining
        if BS.null chunk
            then pure (BS.concat (reverse acc))
            else go (remaining - BS.length chunk) (chunk : acc)

-- | Close the transport connection.
close :: Transport -> IO ()
close t = NS.gracefulClose (tSocket t) 5000
