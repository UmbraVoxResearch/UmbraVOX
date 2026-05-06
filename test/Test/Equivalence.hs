-- | Cross-validation tests: verify different implementations produce
-- consistent output, round-trip properties hold, and serialization
-- is lossless.
module Test.Equivalence (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)
import Data.Bits (shiftR, (.&.))

import Test.Util
import UmbraVox.Crypto.SHA256 (sha256)
import UmbraVox.Crypto.Keccak (sha3_256)
import UmbraVox.Crypto.MLKEM (mlkemKeyGen, MLKEMEncapKey(..))
import UmbraVox.Crypto.Signal.PQXDH (PQPreKeyBundle(..))
import UmbraVox.Crypto.Signal.X3DH
    (IdentityKey(..), generateIdentityKey, generateKeyPair,
     KeyPair(..), signPreKey)

import UmbraVox.Network.Noise (NoiseState(..), noiseEncrypt, noiseDecrypt)
import UmbraVox.Protocol.CBOR (encodeMessage, decodeMessage)

runTests :: IO Bool
runTests = do
    putStrLn "[Equivalence] Running SHA-256 vs SHA3-256 tests..."
    r1 <- testSHA256vsSHA3
    putStrLn "[Equivalence] Running ML-KEM consistency tests..."
    r2 <- testMLKEMConsistency
    putStrLn "[Equivalence] Running Noise encrypt/decrypt round-trip..."
    r3 <- testNoiseRoundTrip
    putStrLn "[Equivalence] Running CBOR round-trip tests..."
    r4 <- testCBORRoundTrip
    putStrLn "[Equivalence] Running serialization consistency tests..."
    r5 <- testSerializationConsistency
    let results = [r1, r2, r3, r4, r5]
        passed  = length (filter id results)
        total   = length results
    putStrLn $ "[Equivalence] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Test 1: SHA-256 vs SHA3-256 — both deterministic, both 32 bytes
------------------------------------------------------------------------

testSHA256vsSHA3 :: IO Bool
testSHA256vsSHA3 = do
    ok1 <- checkProperty "SHA-256 and SHA3-256 both produce 32 bytes" 100
        (\g -> let (input, _) = nextBytesRange 0 256 g
               in BS.length (sha256 input) == 32
                  && BS.length (sha3_256 input) == 32)
    ok2 <- checkProperty "SHA-256 and SHA3-256 differ for same input" 100
        (\g -> let (input, _) = nextBytesRange 1 256 g
               in sha256 input /= sha3_256 input)
    ok3 <- checkProperty "SHA-256 is deterministic" 50
        (\g -> let (input, _) = nextBytesRange 0 128 g
               in sha256 input == sha256 input)
    ok4 <- checkProperty "SHA3-256 is deterministic" 50
        (\g -> let (input, _) = nextBytesRange 0 128 g
               in sha3_256 input == sha3_256 input)
    pure (ok1 && ok2 && ok3 && ok4)

------------------------------------------------------------------------
-- Test 2: ML-KEM consistency — same seeds produce same keys
------------------------------------------------------------------------

testMLKEMConsistency :: IO Bool
testMLKEMConsistency = do
    ok1 <- checkProperty "ML-KEM keygen deterministic" 10
        (\g -> let (d, g1) = nextBytes 32 g
                   (z, _)  = nextBytes 32 g1
                   (ek1, dk1) = mlkemKeyGen d z
                   (ek2, dk2) = mlkemKeyGen d z
               in ek1 == ek2 && dk1 == dk2)
    ok2 <- checkProperty "ML-KEM different seeds -> different keys" 10
        (\g -> let (d1, g1) = nextBytes 32 g
                   (z1, g2) = nextBytes 32 g1
                   (d2, g3) = nextBytes 32 g2
                   (z2, _)  = nextBytes 32 g3
                   (ek1, _) = mlkemKeyGen d1 z1
                   (ek2, _) = mlkemKeyGen d2 z2
               in ek1 /= ek2)
    pure (ok1 && ok2)

------------------------------------------------------------------------
-- Test 3: Noise encrypt/decrypt round-trip
------------------------------------------------------------------------

testNoiseRoundTrip :: IO Bool
testNoiseRoundTrip = do
    ok1 <- checkProperty "Noise round-trip recovers plaintext" 50
        (\g -> let (key, g1)  = nextBytes 32 g
                   (msg, _)   = nextBytesRange 1 256 g1
                   sendState  = NoiseState key key 0 0
                   (st', ct)  = noiseEncrypt sendState msg
                   recvState  = NoiseState key key 0 0
               in case noiseDecrypt recvState ct of
                   Just (_, pt) -> pt == msg
                   Nothing      -> False)
    ok2 <- checkProperty "Noise wrong key rejects" 50
        (\g -> let (key1, g1) = nextBytes 32 g
                   (key2, g2) = nextBytes 32 g1
                   (msg, _)   = nextBytesRange 1 64 g2
                   sendSt = NoiseState key1 key1 0 0
                   (_, ct) = noiseEncrypt sendSt msg
                   recvSt = NoiseState key2 key2 0 0
               in case noiseDecrypt recvSt ct of
                   Nothing -> True
                   Just _  -> False)
    pure (ok1 && ok2)

------------------------------------------------------------------------
-- Test 4: CBOR round-trip — encodeMessage then decodeMessage
------------------------------------------------------------------------

testCBORRoundTrip :: IO Bool
testCBORRoundTrip = do
    ok1 <- checkProperty "CBOR round-trip recovers payload" 100
        (\g -> let (msg, _) = nextBytesRange 0 512 g
                   encoded  = encodeMessage msg
               in case decodeMessage encoded of
                   Just (payload, remainder) ->
                       payload == msg && BS.null remainder
                   Nothing -> False)
    ok2 <- assertEq "CBOR empty message round-trips" True
        (case decodeMessage (encodeMessage BS.empty) of
            Just (p, r) -> BS.null p && BS.null r
            Nothing     -> False)
    ok3 <- assertEq "CBOR rejects truncated input"
        Nothing (decodeMessage (BS.pack [0, 0, 0, 5, 1, 2]))
    pure (ok1 && ok2 && ok3)

------------------------------------------------------------------------
-- Test 5: Serialization consistency — serialize/deserialize bundle
------------------------------------------------------------------------

-- | Inline bundle serialization matching app/Main.hs format:
-- IK_x25519(32) | IK_ed25519(32) | SPK_pub(32) | SPK_sig(64)
-- | len(PQPK) as Word32 BE | PQPK | OPK flag+data
serializeBundle :: IdentityKey -> ByteString -> ByteString
               -> MLKEMEncapKey -> Maybe ByteString -> ByteString
serializeBundle ik spkPub spkSig (MLKEMEncapKey pqpk) mOpk =
    BS.concat
        [ ikX25519Public ik, ikEd25519Public ik
        , spkPub, spkSig
        , putW32BE (fromIntegral (BS.length pqpk))
        , pqpk
        , encodeOpt mOpk
        ]

deserializeBundle :: ByteString -> Maybe PQPreKeyBundle
deserializeBundle bs
    | BS.length bs < 165 = Nothing
    | otherwise =
        let ikX   = bsSlice 0  32 bs
            ikEd  = bsSlice 32 32 bs
            spkP  = bsSlice 64 32 bs
            sig   = bsSlice 96 64 bs
            pqLen = fromIntegral (getW32BE (bsSlice 160 4 bs)) :: Int
            rest  = BS.drop (164 + pqLen) bs
        in if BS.length bs < 164 + pqLen + 1
           then Nothing
           else Just PQPreKeyBundle
               { pqpkbIdentityKey     = ikX
               , pqpkbIdentityEd25519 = ikEd
               , pqpkbSignedPreKey    = spkP
               , pqpkbSPKSignature    = sig
               , pqpkbPQPreKey        = MLKEMEncapKey (bsSlice 164 pqLen bs)
               , pqpkbOneTimePreKey   = decodeOpt rest
               }

encodeOpt :: Maybe ByteString -> ByteString
encodeOpt Nothing  = BS.singleton 0x00
encodeOpt (Just k) = BS.singleton 0x01 <> k

decodeOpt :: ByteString -> Maybe ByteString
decodeOpt bs
    | BS.null bs            = Nothing
    | BS.index bs 0 == 0x01 = Just (BS.take 32 (BS.drop 1 bs))
    | otherwise             = Nothing

bsSlice :: Int -> Int -> ByteString -> ByteString
bsSlice off len = BS.take len . BS.drop off

putW32BE :: Word32 -> ByteString
putW32BE w = BS.pack
    [ fromIntegral (w `shiftR` 24 .&. 0xff)
    , fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR` 8  .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

getW32BE :: ByteString -> Word32
getW32BE bs =
    fromIntegral (BS.index bs 0) * 16777216
    + fromIntegral (BS.index bs 1) * 65536
    + fromIntegral (BS.index bs 2) * 256
    + fromIntegral (BS.index bs 3)

testSerializationConsistency :: IO Bool
testSerializationConsistency = do
    ok1 <- checkProperty "Bundle serialize/deserialize round-trip" 10
        (\g -> let (edSec, g1) = nextBytes 32 g
                   (xSec,  g2) = nextBytes 32 g1
                   (spkS,  g3) = nextBytes 32 g2
                   (mlD,   g4) = nextBytes 32 g3
                   (mlZ,   g5) = nextBytes 32 g4
                   ik = generateIdentityKey edSec xSec
                   spk = generateKeyPair spkS
                   sig = signPreKey ik (kpPublic spk)
                   (pqEK, _) = mlkemKeyGen mlD mlZ
                   blob = serializeBundle ik (kpPublic spk) sig pqEK Nothing
               in case deserializeBundle blob of
                   Nothing -> False
                   Just b  -> pqpkbIdentityKey b == ikX25519Public ik
                           && pqpkbSignedPreKey b == kpPublic spk
                           && pqpkbSPKSignature b == sig
                           && pqpkbOneTimePreKey b == Nothing)
    -- With OPK
    ok2 <- checkProperty "Bundle round-trip with OPK" 10
        (\g -> let (edSec, g1) = nextBytes 32 g
                   (xSec,  g2) = nextBytes 32 g1
                   (spkS,  g3) = nextBytes 32 g2
                   (opkS,  g4) = nextBytes 32 g3
                   (mlD,   g5) = nextBytes 32 g4
                   (mlZ,   _)  = nextBytes 32 g5
                   ik  = generateIdentityKey edSec xSec
                   spk = generateKeyPair spkS
                   opk = generateKeyPair opkS
                   sig = signPreKey ik (kpPublic spk)
                   (pqEK, _) = mlkemKeyGen mlD mlZ
                   blob = serializeBundle ik (kpPublic spk) sig pqEK
                              (Just (kpPublic opk))
               in case deserializeBundle blob of
                   Nothing -> False
                   Just b  -> pqpkbOneTimePreKey b == Just (kpPublic opk)
                           && pqpkbIdentityKey b == ikX25519Public ik)
    pure (ok1 && ok2)
