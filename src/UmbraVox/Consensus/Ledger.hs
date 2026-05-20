-- SPDX-License-Identifier: Apache-2.0
-- | Ledger state: balances, nonces, stakes
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.Ledger
  ( LedgerState
  , emptyLedger
  ) where

{-# WARNING LedgerState "UmbraVox.Consensus.Ledger is a stub -- not implemented" #-}
{-# WARNING emptyLedger "UmbraVox.Consensus.Ledger is a stub -- not implemented" #-}

-- | The full ledger state.
data LedgerState = LedgerState
  deriving (Show)

-- | An empty initial ledger.
emptyLedger :: LedgerState
emptyLedger = error "not implemented"
