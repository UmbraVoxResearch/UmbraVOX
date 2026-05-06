-- SPDX-License-Identifier: Apache-2.0
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
