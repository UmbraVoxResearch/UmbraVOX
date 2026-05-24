-- SPDX-License-Identifier: Apache-2.0
-- | Ledger state: balances, nonces, stakes
-- Note: LedgerState is a placeholder type; fields will be added when
-- the ledger integration milestone lands.
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
emptyLedger = LedgerState
