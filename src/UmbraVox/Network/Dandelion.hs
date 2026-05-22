-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
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
