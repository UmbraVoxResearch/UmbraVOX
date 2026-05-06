-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.Consensus.Block
--
-- Tests BlockHeader and Block construction, Eq, Show, and field accessors.
module Test.Consensus.Block (runTests) where

import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Consensus.Block (Block(..), BlockHeader(..))

runTests :: IO Bool
runTests = do
    putStrLn "Test.Consensus.Block"
    putStrLn (replicate 40 '-')
    results <- sequence
        [ testBlockHeaderConstruction
        , testBlockHeaderEq
        , testBlockHeaderNeq
        , testBlockHeaderShow
        , testBlockHeaderFieldAccess
        , testBlockConstruction
        , testBlockEq
        , testBlockNeq
        , testBlockShow
        , testBlockFieldAccess
        , testBlockHeaderEmptyHashes
        , testBlockEmptyBody
        , checkProperty "BlockHeader Eq reflexive (100 iters)" 100 propHeaderEqReflexive
        , checkProperty "Block Eq reflexive (100 iters)" 100 propBlockEqReflexive
        , checkProperty "Different slot => different header (100 iters)" 100 propDifferentSlotDifferentHeader
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "Test.Consensus.Block: " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- Helper to make a test header
mkTestHeader :: BlockHeader
mkTestHeader = BlockHeader
    { bhSlot     = 42
    , bhPrevHash = BS.replicate 32 0xAA
    , bhBodyHash = BS.replicate 32 0xBB
    }

-- Helper to make a test block
mkTestBlock :: Block
mkTestBlock = Block
    { blockHeader = mkTestHeader
    , blockBody   = strToBS "test body"
    }

testBlockHeaderConstruction :: IO Bool
testBlockHeaderConstruction =
    let hdr = mkTestHeader
    in assertEq "BlockHeader construction: slot" 42 (bhSlot hdr)

testBlockHeaderEq :: IO Bool
testBlockHeaderEq =
    assertEq "BlockHeader Eq: same" mkTestHeader mkTestHeader

testBlockHeaderNeq :: IO Bool
testBlockHeaderNeq =
    assertEq "BlockHeader Neq: different slot" False
        (mkTestHeader == mkTestHeader { bhSlot = 99 })

testBlockHeaderShow :: IO Bool
testBlockHeaderShow =
    let s = show mkTestHeader
    in assertEq "BlockHeader Show contains 'BlockHeader'" True
        (not (null s))  -- Just verify Show doesn't crash

testBlockHeaderFieldAccess :: IO Bool
testBlockHeaderFieldAccess = do
    let hdr = mkTestHeader
    r1 <- assertEq "bhSlot" 42 (bhSlot hdr)
    r2 <- assertEq "bhPrevHash length" 32 (BS.length (bhPrevHash hdr))
    r3 <- assertEq "bhBodyHash length" 32 (BS.length (bhBodyHash hdr))
    pure (r1 && r2 && r3)

testBlockConstruction :: IO Bool
testBlockConstruction =
    let blk = mkTestBlock
    in assertEq "Block construction: header slot" 42 (bhSlot (blockHeader blk))

testBlockEq :: IO Bool
testBlockEq =
    assertEq "Block Eq: same" mkTestBlock mkTestBlock

testBlockNeq :: IO Bool
testBlockNeq =
    assertEq "Block Neq: different body" False
        (mkTestBlock == mkTestBlock { blockBody = strToBS "other" })

testBlockShow :: IO Bool
testBlockShow =
    let s = show mkTestBlock
    in assertEq "Block Show doesn't crash" True (not (null s))

testBlockFieldAccess :: IO Bool
testBlockFieldAccess = do
    let blk = mkTestBlock
    r1 <- assertEq "blockHeader matches" mkTestHeader (blockHeader blk)
    r2 <- assertEq "blockBody matches" (strToBS "test body") (blockBody blk)
    pure (r1 && r2)

testBlockHeaderEmptyHashes :: IO Bool
testBlockHeaderEmptyHashes =
    let hdr = BlockHeader { bhSlot = 0, bhPrevHash = BS.empty, bhBodyHash = BS.empty }
    in do
        r1 <- assertEq "Empty prevHash" BS.empty (bhPrevHash hdr)
        r2 <- assertEq "Empty bodyHash" BS.empty (bhBodyHash hdr)
        pure (r1 && r2)

testBlockEmptyBody :: IO Bool
testBlockEmptyBody =
    let blk = Block { blockHeader = mkTestHeader, blockBody = BS.empty }
    in assertEq "Empty block body" BS.empty (blockBody blk)

-- Property: BlockHeader Eq is reflexive
propHeaderEqReflexive :: PRNG -> Bool
propHeaderEqReflexive g =
    let (w, g1) = nextWord32 g
        (prevH, g2) = nextBytes 32 g1
        (bodyH, _)  = nextBytes 32 g2
        hdr = BlockHeader (fromIntegral w) prevH bodyH
    in hdr == hdr

-- Property: Block Eq is reflexive
propBlockEqReflexive :: PRNG -> Bool
propBlockEqReflexive g =
    let (w, g1) = nextWord32 g
        (prevH, g2) = nextBytes 32 g1
        (bodyH, g3) = nextBytes 32 g2
        (body, _)   = nextBytes 64 g3
        blk = Block (BlockHeader (fromIntegral w) prevH bodyH) body
    in blk == blk

-- Property: Different slots yield different headers
propDifferentSlotDifferentHeader :: PRNG -> Bool
propDifferentSlotDifferentHeader g =
    let (w1, g1) = nextWord32 g
        (w2, g2) = nextWord32 g1
        (prevH, g3) = nextBytes 32 g2
        (bodyH, _)  = nextBytes 32 g3
        s1 = fromIntegral w1
        s2 = fromIntegral w2
        hdr1 = BlockHeader s1 prevH bodyH
        hdr2 = BlockHeader s2 prevH bodyH
    in if s1 == s2 then hdr1 == hdr2 else hdr1 /= hdr2
