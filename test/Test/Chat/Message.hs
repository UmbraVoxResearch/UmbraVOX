-- | Tests for UmbraVox.Chat.Message chunking and reassembly.
module Test.Chat.Message (runTests) where

import qualified Data.ByteString as BS
import Test.Util (assertEq, strToBS, checkProperty, PRNG, nextBytes)
import UmbraVox.Chat.Message (chunkMessage, reassemble)

runTests :: IO Bool
runTests = do
    putStrLn "Chat.Message"
    p1 <- testChunkSingle
    p2 <- testReassembleRecovers
    p3 <- testEmptyMessage
    p4 <- testRoundTripProperty
    pure (p1 && p2 && p3 && p4)

-- | chunkMessage produces a single-element list.
testChunkSingle :: IO Bool
testChunkSingle = do
    let msg = strToBS "Hello, UmbraVox!"
        chunks = chunkMessage msg
    assertEq "chunkMessage single element" 1 (length chunks)

-- | reassemble recovers the original message.
testReassembleRecovers :: IO Bool
testReassembleRecovers = do
    let msg = strToBS "Hello, UmbraVox!"
        chunks = chunkMessage msg
        recovered = reassemble chunks
    assertEq "reassemble recovers original" msg recovered

-- | Empty message round-trips correctly.
testEmptyMessage :: IO Bool
testEmptyMessage = do
    let chunks = chunkMessage BS.empty
        recovered = reassemble chunks
    a <- assertEq "empty chunkMessage length" 1 (length chunks)
    b <- assertEq "empty reassemble" BS.empty recovered
    pure (a && b)

-- | Property: chunk then reassemble is identity for random messages.
testRoundTripProperty :: IO Bool
testRoundTripProperty =
    checkProperty "chunk/reassemble round-trip" 100 $ \g ->
        let (msg, _) = nextBytes 256 g
            recovered = reassemble (chunkMessage msg)
        in recovered == msg
