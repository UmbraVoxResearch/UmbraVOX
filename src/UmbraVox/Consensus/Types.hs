-- SPDX-License-Identifier: Apache-2.0
-- | Core consensus types: SlotNo, EpochNo, CycleNo, BlockNo
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.Types
  ( SlotNo(..)
  , EpochNo(..)
  , CycleNo(..)
  , BlockNo(..)
  ) where

import Data.Word (Word64)

newtype SlotNo = SlotNo Word64 deriving (Show, Eq, Ord)
newtype EpochNo = EpochNo Word64 deriving (Show, Eq, Ord)
newtype CycleNo = CycleNo Word64 deriving (Show, Eq, Ord)
newtype BlockNo = BlockNo Word64 deriving (Show, Eq, Ord)
