-- SPDX-License-Identifier: Apache-2.0
-- STUB MODULE — not yet implemented.
-- All exported functions raise 'error'. See TODO.txt M20.3.1 for plans
-- to move stubs to a deferred build target.
-- | Ledger state: balances, nonces, stakes
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.Ledger
  ( LedgerState
  , emptyLedger
  ) where

-- | The full ledger state.
data LedgerState = LedgerState
  deriving (Show)

-- | An empty initial ledger.
emptyLedger :: LedgerState
emptyLedger = error "not implemented"
