-- | Post-Quantum X3DH
--
-- See: doc/spec/signal-protocol.md
module UmbraVox.Crypto.Signal.PQXDH
  ( pqxdh
  ) where

import Data.ByteString (ByteString)

-- | Perform the post-quantum extended X3DH key agreement.
pqxdh :: ByteString -> ByteString -> ByteString
pqxdh = error "not implemented"
