-- | Dandelion++ stem/fluff routing
--
-- See: doc/spec/network.md
module UmbraVox.Network.Dandelion
  ( routeMessage
  ) where

import Data.ByteString (ByteString)

-- | Route a message through the Dandelion++ stem/fluff phases.
routeMessage :: ByteString -> IO ()
routeMessage = error "not implemented"
