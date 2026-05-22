-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
-- | Signal session state management
--
-- See: doc/spec/signal-protocol.md
module UmbraVox.Crypto.Signal.Session
  ( SessionState
  , initSession
  ) where

import Data.ByteString (ByteString)

-- | Opaque session state for Signal protocol.
data SessionState = SessionState
  deriving (Show)

-- | Initialize a new Signal session.
initSession :: ByteString -> SessionState
initSession = error "not implemented"
