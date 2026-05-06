-- | HMAC test suite: RFC 4231 KAT vectors + edge cases + property/fuzz tests.
module Test.Crypto.HMAC (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.HMAC (hmacSHA256, hmacSHA512)

runTests :: IO Bool
runTests = do
    putStrLn "[HMAC] Running RFC 4231 KAT vectors..."
    r256 <- mapM (runOne "HMAC-SHA-256" hmacSHA256) hmac256Vectors
    r512 <- mapM (runOne "HMAC-SHA-512" hmacSHA512) hmac512Vectors
    putStrLn "[HMAC] Running edge case tests..."
    edgeResults <- mapM (runOne "HMAC-SHA-256" hmacSHA256) edgeVectors256
    edge512 <- mapM (runOne "HMAC-SHA-512" hmacSHA512) edgeVectors512
    putStrLn "[HMAC] Running property/fuzz tests..."
    propResults <- sequence
        [ checkProperty "SHA-256 output=32B (1000 random)" 1000 propLen256
        , checkProperty "SHA-512 output=64B (1000 random)" 1000 propLen512
        , checkProperty "SHA-256 deterministic (1000 random)" 1000 propDet256
        ]
    let results = r256 ++ r512 ++ edgeResults ++ edge512 ++ propResults
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[HMAC] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

runOne :: String -> (ByteString -> ByteString -> ByteString) -> (String, ByteString, ByteString, String) -> IO Bool
runOne variant hmacFn (name, key, msg, expected) =
    assertEq (variant ++ " " ++ name) expected (hexEncode (hmacFn key msg))

hmac256Vectors :: [(String, ByteString, ByteString, String)]
hmac256Vectors =
    [ ("TC1", BS.replicate 20 0x0b, strToBS "Hi There", "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7")
    , ("TC2", strToBS "Jefe", strToBS "what do ya want for nothing?", "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")
    , ("TC3", BS.replicate 20 0xaa, BS.replicate 50 0xdd, "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe")
    , ("TC4", hexDecode "0102030405060708090a0b0c0d0e0f10111213141516171819", BS.replicate 50 0xcd, "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b")
    , ("TC6: long key", BS.replicate 131 0xaa, strToBS "Test Using Larger Than Block-Size Key - Hash Key First", "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54")
    , ("TC7: long key+data", BS.replicate 131 0xaa, strToBS "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm.", "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2")
    ]

hmac512Vectors :: [(String, ByteString, ByteString, String)]
hmac512Vectors =
    [ ("TC1", BS.replicate 20 0x0b, strToBS "Hi There", "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854")
    , ("TC2", strToBS "Jefe", strToBS "what do ya want for nothing?", "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737")
    , ("TC3", BS.replicate 20 0xaa, BS.replicate 50 0xdd, "fa73b0089d56a284efb0f0756c890be9b1b5dbdd8ee81a3655f83e33b2279d39bf3e848279a722c806b485a47e67c807b946a337bee8942674278859e13292fb")
    , ("TC4", hexDecode "0102030405060708090a0b0c0d0e0f10111213141516171819", BS.replicate 50 0xcd, "b0ba465637458c6990e5a8c5f61d4af7e576d97ff94b872de76f8050361ee3dba91ca5c11aa25eb4d679275cc5788063a5f19741120c4f2de2adebeb10a298dd")
    ]

edgeVectors256 :: [(String, ByteString, ByteString, String)]
edgeVectors256 =
    [ ("empty key+msg", BS.empty, BS.empty, "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad")
    , ("key=64B (block size)", BS.replicate 64 0x00, BS.empty, "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad")
    , ("key=65B (>block)", BS.replicate 65 0x00, BS.empty, "2dc19480eae3a02d634b585f777b82d13b92d3683016bb3266718cc5e8089417")
    , ("key=63B (<block)", BS.replicate 63 0x00, BS.empty, "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad")
    ]

edgeVectors512 :: [(String, ByteString, ByteString, String)]
edgeVectors512 =
    [ ("empty key+msg", BS.empty, BS.empty, "b936cee86c9f87aa5d3c6f2e84cb5a4239a5fe50480a6ec66b70ab5b1f4ac6730c6c515421b327ec1d69402e53dfb49ad7381eb067b338fd7b0cb22247225d47")
    , ("key=128B (block size)", BS.replicate 128 0x00, BS.empty, "b936cee86c9f87aa5d3c6f2e84cb5a4239a5fe50480a6ec66b70ab5b1f4ac6730c6c515421b327ec1d69402e53dfb49ad7381eb067b338fd7b0cb22247225d47")
    , ("key=129B (>block)", BS.replicate 129 0x00, BS.empty, "dffe04bc476bbaad4bf30a01f426e025fcab61d53766ed403c1eeb3d503bc152c75fcef757132328f4d9db5d020a90673ac1373c15632cfd5178221400f94be7")
    ]

propLen256 :: PRNG -> Bool
propLen256 g =
    let (g1, g2) = splitPRNG g
        (key, _) = nextBytesRange 0 256 g1
        (msg, _) = nextBytesRange 0 1024 g2
    in BS.length (hmacSHA256 key msg) == 32

propLen512 :: PRNG -> Bool
propLen512 g =
    let (g1, g2) = splitPRNG g
        (key, _) = nextBytesRange 0 256 g1
        (msg, _) = nextBytesRange 0 1024 g2
    in BS.length (hmacSHA512 key msg) == 64

propDet256 :: PRNG -> Bool
propDet256 g =
    let (g1, g2) = splitPRNG g
        (key, _) = nextBytesRange 0 256 g1
        (msg, _) = nextBytesRange 0 1024 g2
    in hmacSHA256 key msg == hmacSHA256 key msg
