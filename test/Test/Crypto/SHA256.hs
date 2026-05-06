-- | SHA-256 test suite: NIST KAT vectors + edge cases + property/fuzz tests.
module Test.Crypto.SHA256 (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.SHA256 (sha256)

runTests :: IO Bool
runTests = do
    putStrLn "[SHA-256] Running NIST FIPS 180-4 KAT vectors..."
    katResults <- mapM runKAT katVectors
    putStrLn "[SHA-256] Running edge case tests..."
    edgeResults <- mapM runKAT edgeVectors
    putStrLn "[SHA-256] Running property/fuzz tests..."
    propResults <- sequence
        [ checkProperty "output always 32 bytes (1000 random inputs)" 1000 propOutputLen
        , checkProperty "deterministic (1000 random inputs)" 1000 propDeterminism
        ]
    let results = katResults ++ edgeResults ++ propResults
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[SHA-256] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

runKAT :: (String, ByteString, String) -> IO Bool
runKAT (name, input, expected) =
    assertEq name expected (hexEncode (sha256 input))

-- FIPS 180-4 official vectors
katVectors :: [(String, ByteString, String)]
katVectors =
    [ ("FIPS B.1: \"abc\"", strToBS "abc", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    , ("FIPS B.2: two-block", strToBS "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq", "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
    , ("Empty string", BS.empty, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    , ("CAVP: 0xbd", BS.singleton 0xbd, "68325720aabd7c82f30f554b313d0570c95accbb7dc4b5aae11204c08ffe732b")
    , ("Single 'a'", BS.singleton 0x61, "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb")
    , ("55 bytes 'a'", BS.replicate 55 0x61, "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318")
    , ("56 bytes 'a'", BS.replicate 56 0x61, "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a")
    , ("64 bytes 'a'", BS.replicate 64 0x61, "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb")
    , ("FIPS B.3: 896-bit", strToBS "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu", "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1")
    ]

-- Edge case vectors (reference values from Python hashlib)
edgeVectors :: [(String, ByteString, String)]
edgeVectors =
    [ ("Edge: single 0x00", BS.singleton 0x00, "6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d")
    , ("Edge: single 0xFF", BS.singleton 0xff, "a8100ae6aa1940d0b663bb31cd466142ebbdbd5187131b92d93818987832eb89")
    , ("Edge: 64x 0x00", BS.replicate 64 0x00, "f5a5fd42d16a20302798ef6ed309979b43003d2320d9f0e8ea9831a92759fb4b")
    , ("Edge: 64x 0xFF", BS.replicate 64 0xff, "8667e718294e9e0df1d30600ba3eeb201f764aad2dad72748643e4a285e1d1f7")
    , ("Edge: 64x 0xAA", BS.replicate 64 0xaa, "693e5f0f347a5d70acbb7baaab9beb988301b3e9588e32c73d7dcdfb7b2c4604")
    , ("Edge: 64x 0x55", BS.replicate 64 0x55, "3d9eae666b06b1a975071aca838b4bb5f27a8324eb2ddab0c8eccd71ceae6b50")
    , ("Edge: 55x 0x00", BS.replicate 55 0x00, "02779466cdec163811d078815c633f21901413081449002f24aa3e80f0b88ef7")
    , ("Edge: 56x 0x00", BS.replicate 56 0x00, "d4817aa5497628e7c77e6b606107042bbba3130888c5f47a375e6179be789fbb")
    ]

-- Property: output is always 32 bytes for any input length [0..4096]
propOutputLen :: PRNG -> Bool
propOutputLen g =
    let (msg, _) = nextBytesRange 0 4096 g
    in BS.length (sha256 msg) == 32

-- Property: sha256 is deterministic
propDeterminism :: PRNG -> Bool
propDeterminism g =
    let (msg, _) = nextBytesRange 0 4096 g
    in sha256 msg == sha256 msg
