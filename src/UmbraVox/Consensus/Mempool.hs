-- SPDX-License-Identifier: Apache-2.0
-- | Transaction pool (50K cap, fee-priority)
-- Note: Mempool is a placeholder type; fields (TVar queue, capacity,
-- fee ordering) will be added when the consensus integration lands.
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.Mempool
  ( Mempool
  , emptyMempool
  ) where

-- | A bounded, fee-priority transaction pool.
data Mempool = Mempool
  deriving (Show)

-- | Create an empty mempool.
emptyMempool :: Mempool
emptyMempool = Mempool
