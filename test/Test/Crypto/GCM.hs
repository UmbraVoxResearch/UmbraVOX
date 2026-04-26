-- | AES-256-GCM test suite: NIST KAT + edge cases + property/fuzz + tag tamper.
module Test.Crypto.GCM (runTests) where

import qualified Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)

runTests :: IO Bool
runTests = do
    putStrLn "[AES-256-GCM] Running NIST SP 800-38D test vectors..."
    katResults <- sequence [testTC16Enc, testTC16Dec, testTC14Empty]
    putStrLn "[AES-256-GCM] Running edge case tests..."
    edgeResults <- sequence
        [ testSingleBytePT, testExact16PT, test17BytePT, testEmptyPTWithAAD ]
    putStrLn "[AES-256-GCM] Running tag tamper tests..."
    tamperResults <- testTagTamperAll
    putStrLn "[AES-256-GCM] Running property/fuzz tests..."
    propResults <- sequence
        [ checkPropertyIO "round-trip (1000 random)" 1000 propRoundTrip
        , checkPropertyIO "ct length = pt length (1000 random)" 1000 propCTLen
        , checkPropertyIO "tag always 16 bytes (1000 random)" 1000 propTagLen
        ]
    let results = katResults ++ edgeResults ++ tamperResults ++ propResults
        passed = length (filter id results)
        total  = length results
    putStrLn $ "[AES-256-GCM] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- NIST TC16 encrypt
testTC16Enc :: IO Bool
testTC16Enc = do
    let key = hexDecode "feffe9928665731c6d6a8f9467308308feffe9928665731c6d6a8f9467308308"
        nonce = hexDecode "cafebabefacedbaddecaf888"
        pt = hexDecode "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b391aafd255"
        (ct, tag) = gcmEncrypt key nonce BS.empty pt
        ctOk = hexEncode ct == "522dc1f099567d07f47f37a32a84427d643a8cdcbfe5c0c97598a2bd2555d1aa8cb08e48590dbb3da7b08b1056828838c5f61e6393ba7a0abcc9f662898015ad"
        tagOk = hexEncode tag == "b094dac5d93471bdec1a502270e3cc6c"
    assertEq "NIST TC16 encrypt" True (ctOk && tagOk)

-- NIST TC16 decrypt
testTC16Dec :: IO Bool
testTC16Dec = do
    let key = hexDecode "feffe9928665731c6d6a8f9467308308feffe9928665731c6d6a8f9467308308"
        nonce = hexDecode "cafebabefacedbaddecaf888"
        ct = hexDecode "522dc1f099567d07f47f37a32a84427d643a8cdcbfe5c0c97598a2bd2555d1aa8cb08e48590dbb3da7b08b1056828838c5f61e6393ba7a0abcc9f662898015ad"
        tag = hexDecode "b094dac5d93471bdec1a502270e3cc6c"
        expected = "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b391aafd255"
    case gcmDecrypt key nonce BS.empty ct tag of
        Just pt -> assertEq "NIST TC16 decrypt" expected (hexEncode pt)
        Nothing -> putStrLn "  FAIL: NIST TC16 decrypt (tag rejected)" >> pure False

testTC14Empty :: IO Bool
testTC14Empty = do
    let key = BS.replicate 32 0
        nonce = BS.replicate 12 0
        (ct, tag) = gcmEncrypt key nonce BS.empty BS.empty
    assertEq "NIST TC14 empty" True (BS.null ct && hexEncode tag == "530f8afbc74536b9a963b4f1c4cb738b")

testSingleBytePT :: IO Bool
testSingleBytePT = do
    let key = BS.replicate 32 0x42
        nonce = BS.replicate 12 0x07
        (ct, tag) = gcmEncrypt key nonce BS.empty (BS.singleton 0xab)
    case gcmDecrypt key nonce BS.empty ct tag of
        Just pt -> assertEq "Edge: 1-byte PT roundtrip" (BS.singleton 0xab) pt
        Nothing -> putStrLn "  FAIL: Edge: 1-byte PT (tag rejected)" >> pure False

testExact16PT :: IO Bool
testExact16PT = do
    let key = BS.pack [0..31]
        nonce = BS.pack [0..11]
        pt = BS.pack [0..15]
        (ct, tag) = gcmEncrypt key nonce BS.empty pt
    case gcmDecrypt key nonce BS.empty ct tag of
        Just pt' -> assertEq "Edge: 16-byte PT roundtrip" pt pt'
        Nothing -> putStrLn "  FAIL: Edge: 16-byte PT (tag rejected)" >> pure False

test17BytePT :: IO Bool
test17BytePT = do
    let key = BS.pack [0..31]
        nonce = BS.pack [0..11]
        pt = BS.pack [0..16]
        (ct, tag) = gcmEncrypt key nonce BS.empty pt
    case gcmDecrypt key nonce BS.empty ct tag of
        Just pt' -> assertEq "Edge: 17-byte PT roundtrip" pt pt'
        Nothing -> putStrLn "  FAIL: Edge: 17-byte PT (tag rejected)" >> pure False

testEmptyPTWithAAD :: IO Bool
testEmptyPTWithAAD = do
    let key = BS.replicate 32 0xaa
        nonce = BS.replicate 12 0xbb
        aad = strToBS "authenticated but not encrypted"
        (ct, tag) = gcmEncrypt key nonce aad BS.empty
    case gcmDecrypt key nonce aad ct tag of
        Just pt -> assertEq "Edge: empty PT + AAD roundtrip" BS.empty pt
        Nothing -> putStrLn "  FAIL: Edge: empty PT + AAD (tag rejected)" >> pure False

-- Tag tamper: flip each byte position, verify decryption fails
testTagTamperAll :: IO [Bool]
testTagTamperAll = do
    let key = BS.replicate 32 0xcc
        nonce = BS.replicate 12 0xdd
        pt = BS.pack [1..32]
        (ct, tag) = gcmEncrypt key nonce BS.empty pt
    mapM (\i -> do
        let badTag = flipByte i tag
        case gcmDecrypt key nonce BS.empty ct badTag of
            Nothing -> putStrLn ("  PASS: Tag tamper byte " ++ show i ++ " rejected") >> pure True
            Just _  -> putStrLn ("  FAIL: Tag tamper byte " ++ show i ++ " accepted!") >> pure False
        ) [0..15]

flipByte :: Int -> ByteString -> ByteString
flipByte i bs = BS.concat [BS.take i bs, BS.singleton (Data.Bits.xor (BS.index bs i) 0xff), BS.drop (i+1) bs]

propRoundTrip :: PRNG -> IO Bool
propRoundTrip g = do
    let (g1, g2) = splitPRNG g
        (key, g3) = nextBytes 32 g1
        (nonce, g4) = nextBytes 12 g3
        (pt, _) = nextBytesRange 0 256 g2
        (ct, tag) = gcmEncrypt key nonce BS.empty pt
    case gcmDecrypt key nonce BS.empty ct tag of
        Just pt' -> pure (pt == pt')
        Nothing -> pure False

propCTLen :: PRNG -> IO Bool
propCTLen g = do
    let (g1, g2) = splitPRNG g
        (key, g3) = nextBytes 32 g1
        (nonce, _) = nextBytes 12 g3
        (pt, _) = nextBytesRange 0 256 g2
        (ct, _) = gcmEncrypt key nonce BS.empty pt
    pure (BS.length ct == BS.length pt)

propTagLen :: PRNG -> IO Bool
propTagLen g = do
    let (g1, g2) = splitPRNG g
        (key, g3) = nextBytes 32 g1
        (nonce, _) = nextBytes 12 g3
        (pt, _) = nextBytesRange 0 256 g2
        (_, tag) = gcmEncrypt key nonce BS.empty pt
    pure (BS.length tag == 16)
