-- SPDX-License-Identifier: Apache-2.0
-- | Cross-validation tests: verify different implementations produce
-- consistent output, round-trip properties hold, and serialization
-- is lossless.
module Test.Equivalence (runTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)
import Data.Bits (shiftR, (.&.))

import Test.Util
import qualified UmbraVox.Crypto.AES as AES
import qualified UmbraVox.Crypto.Curve25519 as Curve25519
import qualified UmbraVox.Crypto.Generated.AES256 as GeneratedAES256
import qualified UmbraVox.Crypto.Generated.ChaCha20 as GeneratedChaCha20
import qualified UmbraVox.Crypto.Generated.FFI.AES256 as GeneratedFFIAES256
import qualified UmbraVox.Crypto.Generated.FFI.ChaCha20 as GeneratedFFIChaCha20
import qualified UmbraVox.Crypto.Generated.FFI.HKDF as GeneratedFFIHKDF
import qualified UmbraVox.Crypto.Generated.FFI.HMAC as GeneratedFFIHMAC
import qualified UmbraVox.Crypto.Generated.FFI.Keccak as GeneratedFFIKeccak
import qualified UmbraVox.Crypto.Generated.FFI.MLKEM768 as GeneratedFFIMLKEM768
import qualified UmbraVox.Crypto.Generated.FFI.Poly1305 as GeneratedFFIPoly1305
import qualified UmbraVox.Crypto.Generated.FFI.SHA256 as GeneratedFFISHA256
import qualified UmbraVox.Crypto.Generated.FFI.SHA512 as GeneratedFFISHA512
import qualified UmbraVox.Crypto.Generated.FFI.X25519 as GeneratedFFIX25519
import qualified UmbraVox.Crypto.Generated.HKDF as GeneratedHKDF
import qualified UmbraVox.Crypto.Generated.HMAC as GeneratedHMAC
import qualified UmbraVox.Crypto.Generated.Keccak as GeneratedKeccak
import qualified UmbraVox.Crypto.Generated.MLKEM768 as GeneratedMLKEM768
import qualified UmbraVox.Crypto.Generated.Poly1305 as GeneratedPoly1305
import qualified UmbraVox.Crypto.Generated.SHA256 as GeneratedSHA256
import qualified UmbraVox.Crypto.Generated.SHA512 as GeneratedSHA512
import qualified UmbraVox.Crypto.Generated.X25519 as GeneratedX25519
import qualified UmbraVox.Crypto.HKDF as HKDF
import qualified UmbraVox.Crypto.HMAC as HMAC
import qualified UmbraVox.Crypto.Keccak as Keccak
import qualified UmbraVox.Crypto.MLKEM as MLKEM
import UmbraVox.Crypto.MLKEM (mlkemKeyGen, MLKEMEncapKey(..))
import qualified UmbraVox.Crypto.Poly1305 as Poly1305
import qualified UmbraVox.Crypto.Random as Random
import qualified UmbraVox.Crypto.SHA256 as SHA256
import qualified UmbraVox.Crypto.SHA512 as SHA512
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
    putStrLn "[Equivalence] Running generated SHA-256 wrapper parity checks..."
    r6 <- testGeneratedSHA256WrapperParity
    putStrLn "[Equivalence] Running generated FFI bridge parity checks..."
    r7 <- testGeneratedFFIBridgeParity
    let results = [r1, r2, r3, r4, r5, r6, r7]
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
               in BS.length (SHA256.sha256 input) == 32
                  && BS.length (Keccak.sha3_256 input) == 32)
    ok2 <- checkProperty "SHA-256 and SHA3-256 differ for same input" 100
        (\g -> let (input, _) = nextBytesRange 1 256 g
               in SHA256.sha256 input /= Keccak.sha3_256 input)
    ok3 <- checkProperty "SHA-256 is deterministic" 50
        (\g -> let (input, _) = nextBytesRange 0 128 g
               in SHA256.sha256 input == SHA256.sha256 input)
    ok4 <- checkProperty "SHA3-256 is deterministic" 50
        (\g -> let (input, _) = nextBytesRange 0 128 g
               in Keccak.sha3_256 input == Keccak.sha3_256 input)
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
        (\g -> let (encKey, g1) = nextBytes 32 g
                   (macKey, g2) = nextBytes 32 g1
                   (msg, _)    = nextBytesRange 1 256 g2
                   sendState   = NoiseState encKey macKey encKey macKey 0 0
                   (st', ct)   = noiseEncrypt sendState msg
                   recvState   = NoiseState encKey macKey encKey macKey 0 0
               in case noiseDecrypt recvState ct of
                   Just (_, pt) -> pt == msg
                   Nothing      -> False)
    ok2 <- checkProperty "Noise wrong key rejects" 50
        (\g -> let (encKey1, g1) = nextBytes 32 g
                   (macKey1, g2) = nextBytes 32 g1
                   (encKey2, g3) = nextBytes 32 g2
                   (macKey2, g4) = nextBytes 32 g3
                   (msg, _)     = nextBytesRange 1 64 g4
                   sendSt = NoiseState encKey1 macKey1 encKey1 macKey1 0 0
                   (_, ct) = noiseEncrypt sendSt msg
                   recvSt = NoiseState encKey2 macKey2 encKey2 macKey2 0 0
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

-- Test 6: Generated wrapper parity
-- These checks verify that the preserved generated Haskell namespace is
-- actively wired and callable through the current harness.
------------------------------------------------------------------------

testGeneratedSHA256WrapperParity :: IO Bool
testGeneratedSHA256WrapperParity = do
    ok1 <- assertEq "Generated SHA-256 wrapper matches reference for abc"
        (SHA256.sha256 (BS.pack [97, 98, 99]))
        (GeneratedSHA256.sha256 (BS.pack [97, 98, 99]))
    ok2 <- checkProperty "Generated SHA-256 wrapper matches reference" 20
        (\g -> let (input, _) = nextBytesRange 0 128 g
               in SHA256.sha256 input == GeneratedSHA256.sha256 input)
    ok3 <- checkProperty "Generated SHA-512 wrapper matches reference" 20
        (\g -> let (input, _) = nextBytesRange 0 128 g
               in SHA512.sha512 input == GeneratedSHA512.sha512 input)
    ok4 <- checkProperty "Generated HMAC wrapper matches reference" 20
        (\g -> let (key, g1) = nextBytesRange 0 96 g
                   (msg, _) = nextBytesRange 0 128 g1
               in HMAC.hmacSHA256 key msg == GeneratedHMAC.hmacSHA256 key msg
                  && HMAC.hmacSHA512 key msg == GeneratedHMAC.hmacSHA512 key msg)
    ok5 <- checkProperty "Generated HKDF wrapper matches reference" 20
        (\g -> let (salt, g1) = nextBytesRange 0 32 g
                   (ikm, g2) = nextBytesRange 0 64 g1
                   (info, g3) = nextBytesRange 0 32 g2
                   (lenBs, _) = nextBytes 1 g3
                   len = fromIntegral (BS.head lenBs) `mod` 96
               in HKDF.hkdf salt ikm info len == GeneratedHKDF.hkdf salt ikm info len
                  && HKDF.hkdfSHA256 salt ikm info len == GeneratedHKDF.hkdfSHA256 salt ikm info len)
    ok6 <- checkProperty "Generated Poly1305 wrapper matches reference" 20
        (\g -> let (key, g1) = nextBytes 32 g
                   (msg, _) = nextBytesRange 0 128 g1
               in Poly1305.poly1305 key msg == GeneratedPoly1305.poly1305 key msg)
    ok7 <- checkProperty "Generated X25519 wrapper matches reference" 20
        (\g -> let (scalar, _) = nextBytes 32 g
               in Curve25519.x25519 scalar Curve25519.x25519Basepoint
                  == GeneratedX25519.x25519 scalar GeneratedX25519.x25519Basepoint)
    ok8 <- checkProperty "Generated AES wrapper matches reference" 20
        (\g -> let (key, g1) = nextBytes 32 g
                   (block, _) = nextBytes 16 g1
               in AES.aesEncrypt key block == GeneratedAES256.aesEncrypt key block
                  && AES.aesDecrypt key block == GeneratedAES256.aesDecrypt key block)
    ok9 <- checkProperty "Generated ChaCha20 wrapper matches reference" 20
        (\g -> let (key, g1) = nextBytes 32 g
                   (nonce, g2) = nextBytes 12 g1
                   (ctrBs, g3) = nextBytes 4 g2
                   (pt, _) = nextBytesRange 0 128 g3
                   ctr = word32FromBytes ctrBs
               in Random.chacha20Block key nonce ctr == GeneratedChaCha20.chacha20Block key nonce ctr
                  && Random.chacha20Encrypt key nonce ctr pt == GeneratedChaCha20.chacha20Encrypt key nonce ctr pt)
    ok10 <- checkProperty "Generated Keccak wrapper matches reference" 20
        (\g -> let (input, _) = nextBytesRange 0 128 g
               in Keccak.sha3_256 input == GeneratedKeccak.sha3_256 input
                  && Keccak.sha3_512 input == GeneratedKeccak.sha3_512 input)
    ok11 <- checkProperty "Generated MLKEM wrapper matches reference" 10
        (\g -> let (d, g1) = nextBytes 32 g
                   (z, g2) = nextBytes 32 g1
                   (m, _) = nextBytes 32 g2
                   (ek1, dk1) = MLKEM.mlkemKeyGen d z
                   (ek2, dk2) = GeneratedMLKEM768.mlkemKeyGen d z
                   (ct1, ss1) = MLKEM.mlkemEncaps ek1 m
                   (ct2, ss2) = GeneratedMLKEM768.mlkemEncaps ek2 m
               in ek1 == ek2
                  && dk1 == dk2
                  && ct1 == ct2
                  && ss1 == ss2
                  && MLKEM.mlkemDecaps dk1 ct1 == GeneratedMLKEM768.mlkemDecaps dk2 ct2)
    pure (and [ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9, ok10, ok11])

testGeneratedFFIBridgeParity :: IO Bool
testGeneratedFFIBridgeParity = do
    linked <- sequence
        [ GeneratedFFIAES256.ffiLinked
        , GeneratedFFIChaCha20.ffiLinked
        , GeneratedFFIHKDF.ffiLinked
        , GeneratedFFIHMAC.ffiLinked
        , GeneratedFFIKeccak.ffiLinked
        , GeneratedFFIMLKEM768.ffiLinked
        , GeneratedFFIPoly1305.ffiLinked
        , GeneratedFFISHA256.ffiLinked
        , GeneratedFFISHA512.ffiLinked
        , GeneratedFFIX25519.ffiLinked
        ]
    ok1 <- assertEq "Generated FFI bridges are linked" True (and linked)

    let shaInput = BS.pack [97, 98, 99]
    sha256Out <- GeneratedFFISHA256.sha256 shaInput
    sha512Out <- GeneratedFFISHA512.sha512 shaInput
    ok2 <- assertEq "Generated FFI SHA256 bridge matches reference"
        (SHA256.sha256 shaInput) sha256Out
    ok3 <- assertEq "Generated FFI SHA512 bridge matches reference"
        (SHA512.sha512 shaInput) sha512Out

    let hmacKey = BS.pack [1 .. 32]
        hmacMsg = BS.pack [33 .. 96]
    hmac256Out <- GeneratedFFIHMAC.hmacSHA256 hmacKey hmacMsg
    hmac512Out <- GeneratedFFIHMAC.hmacSHA512 hmacKey hmacMsg
    ok4 <- assertEq "Generated FFI HMAC-SHA256 bridge matches reference"
        (HMAC.hmacSHA256 hmacKey hmacMsg) hmac256Out
    ok5 <- assertEq "Generated FFI HMAC-SHA512 bridge matches reference"
        (HMAC.hmacSHA512 hmacKey hmacMsg) hmac512Out

    let hkdfSalt = BS.pack [5 .. 20]
        hkdfIkm = BS.pack [50 .. 90]
        hkdfInfo = BS.pack [9, 8, 7, 6]
        hkdfLen = 42
    hkdfOut <- GeneratedFFIHKDF.hkdf hkdfSalt hkdfIkm hkdfInfo hkdfLen
    hkdf256Out <- GeneratedFFIHKDF.hkdfSHA256 hkdfSalt hkdfIkm hkdfInfo hkdfLen
    ok6 <- assertEq "Generated FFI HKDF bridge matches reference"
        (HKDF.hkdf hkdfSalt hkdfIkm hkdfInfo hkdfLen) hkdfOut
    ok7 <- assertEq "Generated FFI HKDF-SHA256 bridge matches reference"
        (HKDF.hkdfSHA256 hkdfSalt hkdfIkm hkdfInfo hkdfLen) hkdf256Out

    let polyKey = BS.pack [0 .. 31]
        polyMsg = BS.pack [32 .. 96]
    polyOut <- GeneratedFFIPoly1305.poly1305 polyKey polyMsg
    ok8 <- assertEq "Generated FFI Poly1305 bridge matches reference"
        (Poly1305.poly1305 polyKey polyMsg) polyOut

    let scalar = BS.pack [1 .. 32]
    x25519Out <- GeneratedFFIX25519.x25519 scalar GeneratedFFIX25519.x25519Basepoint
    ok9 <- assertEq "Generated FFI X25519 bridge matches reference"
        (Curve25519.x25519 scalar Curve25519.x25519Basepoint) x25519Out

    let aesKey = BS.pack [0 .. 31]
        aesBlock = BS.pack [32 .. 47]
    aesEncOut <- GeneratedFFIAES256.aesEncrypt aesKey aesBlock
    aesDecOut <- GeneratedFFIAES256.aesDecrypt aesKey aesBlock
    ok10 <- assertEq "Generated FFI AES encrypt bridge matches reference"
        (AES.aesEncrypt aesKey aesBlock) aesEncOut
    ok11 <- assertEq "Generated FFI AES decrypt bridge matches reference"
        (AES.aesDecrypt aesKey aesBlock) aesDecOut

    let chachaKey = BS.pack [0 .. 31]
        chachaNonce = BS.pack [0 .. 11]
        chachaCtr = 7
        chachaPt = BS.pack [12 .. 75]
    chachaBlockOut <- GeneratedFFIChaCha20.chacha20Block chachaKey chachaNonce chachaCtr
    chachaEncOut <- GeneratedFFIChaCha20.chacha20Encrypt chachaKey chachaNonce chachaCtr chachaPt
    ok12 <- assertEq "Generated FFI ChaCha20 block bridge matches reference"
        (Random.chacha20Block chachaKey chachaNonce chachaCtr) chachaBlockOut
    ok13 <- assertEq "Generated FFI ChaCha20 encrypt bridge matches reference"
        (Random.chacha20Encrypt chachaKey chachaNonce chachaCtr chachaPt) chachaEncOut

    keccak256Out <- GeneratedFFIKeccak.sha3_256 shaInput
    keccak512Out <- GeneratedFFIKeccak.sha3_512 shaInput
    shake128Out <- GeneratedFFIKeccak.shake128 shaInput 48
    shake256Out <- GeneratedFFIKeccak.shake256 shaInput 48
    ok14 <- assertEq "Generated FFI Keccak-256 bridge matches reference"
        (Keccak.sha3_256 shaInput) keccak256Out
    ok15 <- assertEq "Generated FFI Keccak-512 bridge matches reference"
        (Keccak.sha3_512 shaInput) keccak512Out
    ok16 <- assertEq "Generated FFI SHAKE-128 bridge matches reference"
        (Keccak.shake128 shaInput 48) shake128Out
    ok17 <- assertEq "Generated FFI SHAKE-256 bridge matches reference"
        (Keccak.shake256 shaInput 48) shake256Out

    let d = BS.pack [10 .. 41]
        z = BS.pack [42 .. 73]
        m = BS.pack [74 .. 105]
    (ffiEk, ffiDk) <- GeneratedFFIMLKEM768.mlkemKeyGen d z
    let (refEk, refDk) = MLKEM.mlkemKeyGen d z
    (ffiCt, ffiSs) <- GeneratedFFIMLKEM768.mlkemEncaps ffiEk m
    let (refCt, refSs) = MLKEM.mlkemEncaps refEk m
    ffiDec <- GeneratedFFIMLKEM768.mlkemDecaps ffiDk ffiCt
    ok18 <- assertEq "Generated FFI MLKEM keygen bridge matches reference"
        (refEk, refDk) (ffiEk, ffiDk)
    ok19 <- assertEq "Generated FFI MLKEM encaps bridge matches reference"
        (refCt, refSs) (ffiCt, ffiSs)
    ok20 <- assertEq "Generated FFI MLKEM decaps bridge matches reference"
        (MLKEM.mlkemDecaps refDk refCt) ffiDec

    pure (and [ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9, ok10, ok11, ok12, ok13, ok14, ok15, ok16, ok17, ok18, ok19, ok20])

word32FromBytes :: ByteString -> Word32
word32FromBytes bs =
    fromIntegral (BS.index bs 0) * 16777216
    + fromIntegral (BS.index bs 1) * 65536
    + fromIntegral (BS.index bs 2) * 256
    + fromIntegral (BS.index bs 3)
