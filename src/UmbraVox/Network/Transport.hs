-- | TCP connection management
--
-- See: doc/spec/network.md
module UmbraVox.Network.Transport
  ( Transport
  , connect
  ) where

-- | An opaque TCP transport handle.
data Transport = Transport
  deriving (Show)

-- | Establish a TCP connection to the given host and port.
connect :: String -> Int -> IO Transport
connect = error "not implemented"
