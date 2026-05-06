-- | Block and BlockHeader types
--
-- See: doc/spec/consensus.md
module UmbraVox.Consensus.Block
  ( Block(..)
  , BlockHeader(..)
  ) where

import Data.ByteString (ByteString)
import Data.Word (Word64)

-- | A block header.
data BlockHeader = BlockHeader
  { bhSlot      :: !Word64
  , bhPrevHash  :: !ByteString
  , bhBodyHash  :: !ByteString
  } deriving (Show, Eq)

-- | A complete block.
data Block = Block
  { blockHeader :: !BlockHeader
  , blockBody   :: !ByteString
  } deriving (Show, Eq)
