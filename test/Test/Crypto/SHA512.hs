-- SPDX-License-Identifier: Apache-2.0
-- | SHA-512 test suite: NIST KAT vectors + edge cases + property/fuzz tests.
module Test.Crypto.SHA512 (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.SHA512 (sha512)

runTests :: IO Bool
runTests = do
    putStrLn "[SHA-512] Running NIST FIPS 180-4 KAT vectors..."
    katResults <- mapM runKAT katVectors
    putStrLn "[SHA-512] Running edge case tests..."
    edgeResults <- mapM runKAT edgeVectors
    putStrLn "[SHA-512] Running property/fuzz tests..."
    propResults <- sequence
        [ checkProperty "output always 64 bytes (1000 random)" 1000 propOutputLen
        , checkProperty "deterministic (1000 random)" 1000 propDeterminism
        ]
    let results = katResults ++ edgeResults ++ propResults
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[SHA-512] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

runKAT :: (String, ByteString, String) -> IO Bool
runKAT (name, input, expected) =
    assertEq name expected (hexEncode (sha512 input))

katVectors :: [(String, ByteString, String)]
katVectors =
    [ ("Empty string", BS.empty, "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e")
    , ("FIPS C.1: \"abc\"", strToBS "abc", "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f")
    , ("FIPS C.2: 896-bit", strToBS "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu", "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909")
    , ("Single 'a'", strToBS "a", "1f40fc92da241694750979ee6cf582f2d5d7d28e18335de05abc54d0560e0f5302860c652bf08d560252aa5e74210546f369fbbbce8c12cfc7957b2652fe9a75")
    ]

edgeVectors :: [(String, ByteString, String)]
edgeVectors =
    [ ("Edge: single 0x00", BS.singleton 0x00, "b8244d028981d693af7b456af8efa4cad63d282e19ff14942c246e50d9351d22704a802a71c3580b6370de4ceb293c324a8423342557d4e5c38438f0e36910ee")
    , ("Edge: single 0xFF", BS.singleton 0xff, "6700df6600b118ab0432715a7e8a68b0bf37cdf4adaf0fb9e2b3ebe04ad19c7032cbad55e932792af360bafaa09962e2e690652bc075b2dad0c30688ba2f31a3")
    , ("Edge: 128x 0x00", BS.replicate 128 0x00, "ab942f526272e456ed68a979f50202905ca903a141ed98443567b11ef0bf25a552d639051a01be58558122c58e3de07d749ee59ded36acf0c55cd91924d6ba11")
    , ("Edge: 128x 0xFF", BS.replicate 128 0xff, "5acaf06f5dd1d107b81b9b7516d454e304cf5699d01ffe66a01ad554cfb0db896bbc16e08bd4fbcab36364909edf50ecb6c8042722a4c59456d9048244cd57f0")
    , ("Edge: 128x 0xAA", BS.replicate 128 0xaa, "77fdca421e16ebe2a2bcd585621c11d2d4dcb72e9df5d5d48a75f0417f40d215569c25f00851ffed130cc540be281b6acdac96bd83d71631fce573ac6e01e9f0")
    , ("Edge: 128x 0x55", BS.replicate 128 0x55, "b35d79220d59457f0b2041f0f6d4f19290f2c7e950e52b9595e2618e00fc6957f24731e9c04a82991c1b7e8f6b3b75ba3b69880586d2b6d2ca0a41c3bef1ec27")
    , ("Edge: 111x 0x00", BS.replicate 111 0x00, "77ddd3a542e530fd047b8977c657ba6ce72f1492e360b2b2212cd264e75ec03882e4ff0525517ab4207d14c70c2259ba88d4d335ee0e7e20543d22102ab1788c")
    , ("Edge: 112x 0x00", BS.replicate 112 0x00, "2be2e788c8a8adeaa9c89a7f78904cacea6e39297d75e0573a73c756234534d6627ab4156b48a6657b29ab8beb73334040ad39ead81446bb09c70704ec707952")
    ]

propOutputLen :: PRNG -> Bool
propOutputLen g =
    let (msg, _) = nextBytesRange 0 4096 g
    in BS.length (sha512 msg) == 64

propDeterminism :: PRNG -> Bool
propDeterminism g =
    let (msg, _) = nextBytesRange 0 4096 g
    in sha512 msg == sha512 msg
