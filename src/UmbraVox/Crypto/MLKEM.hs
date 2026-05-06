-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "CRYPTO-010" #-} ML-KEM-768 (FIPS 203, formerly CRYSTALS-Kyber)
--
-- Pure Haskell reference implementation. NOT constant-time.
--
-- Uses the correct FIPS 202 hash functions as required by FIPS 203:
--   H = SHA3-256, G = SHA3-512, PRF/J = SHAKE-256, XOF = SHAKE-128.
module UmbraVox.Crypto.MLKEM
    ( MLKEMEncapKey(..)
    , MLKEMDecapKey(..)
    , MLKEMCiphertext(..)
    , mlkemKeyGen
    , mlkemEncaps
    , mlkemDecaps
    -- Internal (exported for testing/verification)
    , kpkeKeyGen
    , kpkeEncrypt
    , kpkeDecrypt
    , encodeEK
    , encodeDK
    , hashG
    , ntt
    , invNtt
    , Poly(..)
    ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, listArray, (!), elems, array)
import Data.Array.ST (STUArray, newListArray, readArray, writeArray, freeze)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16)
import Data.List (foldl')
import Data.Word (Word8)

import UmbraVox.Crypto.ConstantTime (constantEq)
import UmbraVox.Crypto.Keccak (sha3_256, sha3_512, shake128, shake256)

------------------------------------------------------------------------
-- ML-KEM-768 parameters (FIPS 203, Table 2)
------------------------------------------------------------------------

_N :: Int
_N = 256

_K :: Int
_K = 3

_Q :: Int
_Q = 3329

_ETA1, _ETA2 :: Int
_ETA1 = 2
_ETA2 = 2

_DU, _DV :: Int
_DU = 10
_DV = 4

------------------------------------------------------------------------
-- Public types
------------------------------------------------------------------------

-- | Encapsulation key (1184 bytes).
newtype MLKEMEncapKey = MLKEMEncapKey ByteString
    deriving newtype (Eq, Show)

-- | Decapsulation key (2400 bytes).
newtype MLKEMDecapKey = MLKEMDecapKey ByteString
    deriving newtype (Eq, Show)

-- | Ciphertext (1088 bytes).
newtype MLKEMCiphertext = MLKEMCiphertext ByteString
    deriving newtype (Eq, Show)

------------------------------------------------------------------------
-- Polynomial type and basic arithmetic
------------------------------------------------------------------------

-- | Polynomial with 256 coefficients in Z_q (q = 3329).
-- Coefficients stored as Int16 in range [0, q-1].
newtype Poly = Poly (Array Int Int16)
    deriving stock (Eq, Show)

-- | Reduce an Int to [0, q-1]
modQ :: Int -> Int16
modQ x = fromIntegral (((x `mod` _Q) + _Q) `mod` _Q)

-- | Polynomial addition mod q
polyAdd :: Poly -> Poly -> Poly
polyAdd (Poly a) (Poly b) =
    Poly (array (0, _N - 1)
        [(i, modQ (fromIntegral (a ! i) + fromIntegral (b ! i))) | i <- [0.._N-1]])

-- | Polynomial subtraction mod q
polySub :: Poly -> Poly -> Poly
polySub (Poly a) (Poly b) =
    Poly (array (0, _N - 1)
        [(i, modQ (fromIntegral (a ! i) - fromIntegral (b ! i))) | i <- [0.._N-1]])

------------------------------------------------------------------------
-- NTT (Number Theoretic Transform) — FIPS 203 Section 4.3
--
-- Uses mutable arrays (STUArray) for efficient in-place butterfly.
------------------------------------------------------------------------

-- | Primitive 256th root of unity: zeta = 17.
-- 17^128 ≡ -1 (mod 3329) and 17^256 ≡ 1 (mod 3329).

-- | Pre-computed zeta powers in bit-reversed order for NTT.
-- zetaTable[i] = 17^(bitrev7(i)) mod 3329, for i in [0..127].
zetaTable :: Array Int Int
zetaTable = listArray (0, 127) (map computeZeta [0..127])
  where
    computeZeta :: Int -> Int
    computeZeta i = powModInt 17 (bitRev7 i) _Q

    bitRev7 :: Int -> Int
    bitRev7 x = foldl' (\acc bit -> (acc `shiftL` 1) .|. ((x `shiftR` bit) .&. 1)) 0 [0..6]

powModInt :: Int -> Int -> Int -> Int
powModInt _ 0 _ = 1
powModInt base' expo m
    | even expo = let half = powModInt base' (expo `div` 2) m
                  in (half * half) `mod` m
    | otherwise = (base' * powModInt base' (expo - 1) m) `mod` m

-- | Forward NTT (Algorithm 9 in FIPS 203) using mutable array.
ntt :: Poly -> Poly
ntt (Poly f0) = Poly result
  where
    result :: Array Int Int16
    result = runST $ do
        arr <- newListArray (0, _N - 1)
                   (map fromIntegral (elems f0)) :: ST s (STUArray s Int Int)
        nttLoop arr
        out <- newListArray (0, _N - 1) (replicate _N 0) :: ST s (STUArray s Int Int16)
        forM_ [0.._N-1] $ \i -> do
            v <- readArray arr i
            writeArray out i (modQ v)
        freeze out

    nttLoop :: STUArray s Int Int -> ST s ()
    nttLoop arr = go 1 128
      where
        go !k !len
            | len < 2 = return ()
            | otherwise = do
                k' <- outerLoop arr k len 0
                go k' (len `div` 2)

    outerLoop :: STUArray s Int Int -> Int -> Int -> Int -> ST s Int
    outerLoop arr !k !len !start
        | start >= _N = return k
        | otherwise = do
            let z = zetaTable ! k
            forM_ [start .. start + len - 1] $ \j -> do
                fj    <- readArray arr j
                fjlen <- readArray arr (j + len)
                let t = (z * fjlen) `mod` _Q
                writeArray arr j       (((fj + t) `mod` _Q + _Q) `mod` _Q)
                writeArray arr (j+len) (((fj - t) `mod` _Q + _Q) `mod` _Q)
            outerLoop arr (k + 1) len (start + 2 * len)

-- | Inverse NTT (Algorithm 10 in FIPS 203) using mutable array.
invNtt :: Poly -> Poly
invNtt (Poly f0) = Poly result
  where
    nInv :: Int
    nInv = powModInt 128 (_Q - 2) _Q  -- 128^(-1) mod q

    result :: Array Int Int16
    result = runST $ do
        arr <- newListArray (0, _N - 1)
                   (map fromIntegral (elems f0)) :: ST s (STUArray s Int Int)
        invNttLoop arr
        -- Multiply by n^(-1) and reduce
        out <- newListArray (0, _N - 1) (replicate _N 0) :: ST s (STUArray s Int Int16)
        forM_ [0.._N-1] $ \i -> do
            v <- readArray arr i
            writeArray out i (modQ ((v * nInv) `mod` _Q))
        freeze out

    invNttLoop :: STUArray s Int Int -> ST s ()
    invNttLoop arr = go 127 2
      where
        go !k !len
            | len > 128 = return ()
            | otherwise = do
                k' <- outerLoop arr k len 0
                go k' (len * 2)

    outerLoop :: STUArray s Int Int -> Int -> Int -> Int -> ST s Int
    outerLoop arr !k !len !start
        | start >= _N = return k
        | otherwise = do
            let z = zetaTable ! k
            forM_ [start .. start + len - 1] $ \j -> do
                fj    <- readArray arr j
                fjlen <- readArray arr (j + len)
                -- FIPS 203 Algorithm 10: t = f[j], f[j] = t + f[j+len], f[j+len] = z*(f[j+len] - t)
                let t = ((fj + fjlen) `mod` _Q + _Q) `mod` _Q
                    u = (z * ((fjlen - fj + _Q) `mod` _Q)) `mod` _Q
                writeArray arr j       t
                writeArray arr (j+len) u
            outerLoop arr (k - 1) len (start + 2 * len)

-- | Pointwise multiplication in NTT domain.
-- Pairs of degree-1 polynomial multiplications (base-case multiply).
polyBaseMul :: Poly -> Poly -> Poly
polyBaseMul (Poly a) (Poly b) =
    Poly (listArray (0, _N - 1) (concatMap mulPair [0..63]))
  where
    mulPair :: Int -> [Int16]
    mulPair i =
        let idx = 4 * i
            a0 = fromIntegral (a ! idx) :: Int
            a1 = fromIntegral (a ! (idx + 1)) :: Int
            b0 = fromIntegral (b ! idx) :: Int
            b1 = fromIntegral (b ! (idx + 1)) :: Int
            gamma1 = zetaTable ! (64 + i)
            c0 = (a0 * b0 + a1 * b1 * gamma1) `mod` _Q
            c1 = (a0 * b1 + a1 * b0) `mod` _Q
            a2 = fromIntegral (a ! (idx + 2)) :: Int
            a3 = fromIntegral (a ! (idx + 3)) :: Int
            b2 = fromIntegral (b ! (idx + 2)) :: Int
            b3 = fromIntegral (b ! (idx + 3)) :: Int
            gamma2 = (_Q - gamma1) `mod` _Q
            c2 = (a2 * b2 + a3 * b3 * gamma2) `mod` _Q
            c3 = (a2 * b3 + a3 * b2) `mod` _Q
        in [modQ c0, modQ c1, modQ c2, modQ c3]

------------------------------------------------------------------------
-- Encoding / decoding helpers (FIPS 203 Algorithms 2-5)
------------------------------------------------------------------------

-- | BytesToBits (Algorithm 2): convert bytes to bit array (LSB first per byte).
bytesToBits :: ByteString -> [Int]
bytesToBits bs = concatMap byteBits (BS.unpack bs)
  where
    byteBits :: Word8 -> [Int]
    byteBits w = [if testBit w i then 1 else 0 | i <- [0..7]]

-- | BitsToBytes (Algorithm 3): convert bit array (LSB first) to bytes.
bitsToBytes :: [Int] -> ByteString
bitsToBytes bits = BS.pack (go bits)
  where
    go [] = []
    go xs =
        let (byte, rest) = splitAt 8 xs
            b = foldl' (\acc (bit, i) -> acc .|. (fromIntegral bit `shiftL` i))
                       (0 :: Word8) (zip byte [0..7])
        in b : go rest

-- | Compress_d (Algorithm 4): round(2^d / q * x) mod 2^d
compressD :: Int -> Int16 -> Int16
compressD d x =
    let x' = fromIntegral x :: Int
        num = (x' `shiftL` d) + (_Q `div` 2)
        result = (num `div` _Q) .&. ((1 `shiftL` d) - 1)
    in fromIntegral result

-- | Decompress_d (Algorithm 5): round(q / 2^d * y)
decompressD :: Int -> Int16 -> Int16
decompressD d y =
    let y' = fromIntegral y :: Int
        num = y' * _Q + (1 `shiftL` (d - 1))
        result = num `shiftR` d
    in fromIntegral result

------------------------------------------------------------------------
-- ByteEncode / ByteDecode (FIPS 203 Algorithms 6/7)
------------------------------------------------------------------------

-- | Encode polynomial with d bits per coefficient.
byteEncode :: Int -> Poly -> ByteString
byteEncode d (Poly coeffs) =
    bitsToBytes (concatMap (\i -> coeffToBits d (coeffs ! i)) [0.._N-1])
  where
    coeffToBits :: Int -> Int16 -> [Int]
    coeffToBits nbits c = [if testBit c j then 1 else 0 | j <- [0..nbits-1]]

-- | Decode bytes into polynomial with d bits per coefficient.
byteDecode :: Int -> ByteString -> Poly
byteDecode d bs =
    let bits = bytesToBits bs
        coeffs = [bitsToCoeff (take d (drop (i * d) bits)) | i <- [0.._N-1]]
    in Poly (listArray (0, _N - 1) coeffs)
  where
    bitsToCoeff :: [Int] -> Int16
    bitsToCoeff bitList =
        foldl' (\acc (b, j) -> acc .|. (fromIntegral b `shiftL` j)) 0 (zip bitList [0..])

-- | ByteDecode_12 with modular reduction mod q.
byteDecode12 :: ByteString -> Poly
byteDecode12 bs =
    let Poly coeffs = byteDecode 12 bs
    in Poly (array (0, _N - 1)
        [(i, modQ (fromIntegral (coeffs ! i))) | i <- [0.._N-1]])

------------------------------------------------------------------------
-- Sampling (FIPS 203 Algorithms 7-8)
------------------------------------------------------------------------

-- | CBD_eta (Algorithm 7): Centered Binomial Distribution.
-- Input: 64*eta bytes. Output: polynomial in [-eta, eta].
sampleCBD :: Int -> ByteString -> Poly
sampleCBD eta bs =
    let !bitArr = listArray (0, BS.length bs * 8 - 1) (bytesToBits bs)
        coeffs = [cbdCoeff eta bitArr i | i <- [0.._N-1]]
    in Poly (listArray (0, _N - 1) coeffs)
  where
    cbdCoeff :: Int -> Array Int Int -> Int -> Int16
    cbdCoeff e allBits i =
        let offset = 2 * e * i
            x = sum [allBits ! (offset + j) | j <- [0..e-1]]
            y = sum [allBits ! (offset + e + j) | j <- [0..e-1]]
        in modQ (x - y)

-- | SampleNTT (Algorithm 8): sample polynomial in NTT domain via rejection.
-- Uses SHAKE-128 as the XOF per FIPS 203.
sampleNTT :: ByteString -> Poly
sampleNTT seed =
    let coeffs = sampleLoop seed 0 [] 672
    in Poly (listArray (0, _N - 1) coeffs)
  where
    sampleLoop :: ByteString -> Int -> [Int16] -> Int -> [Int16]
    sampleLoop sd n acc streamLen
        | n >= 256 = take 256 (reverse acc)
        | otherwise =
            let stream = BS.unpack (shake128 sd streamLen)
                (n', acc') = rejection stream n acc
            in if n' >= 256 then take 256 (reverse acc')
               else sampleLoop sd n' acc' (streamLen * 2)
    rejection :: [Word8] -> Int -> [Int16] -> (Int, [Int16])
    rejection _ 256 acc = (256, acc)
    rejection (b0:b1:b2:rest) n acc =
        let d1 = fromIntegral b0 + 256 * (fromIntegral b1 .&. 0x0F) :: Int
            d2 = (fromIntegral b1 `shiftR` 4) + 16 * fromIntegral b2 :: Int
            (n', acc') = if d1 < _Q && n < 256
                         then (n + 1, fromIntegral d1 : acc)
                         else (n, acc)
            (n'', acc'') = if d2 < _Q && n' < 256
                           then (n' + 1, fromIntegral d2 : acc')
                           else (n', acc')
        in rejection rest n'' acc''
    rejection _ n acc = (n, acc)

------------------------------------------------------------------------
-- FIPS 203 hash functions (using correct FIPS 202 primitives)
------------------------------------------------------------------------

-- | H: SHA3-256 (FIPS 203 Section 4.1)
hashH :: ByteString -> ByteString
hashH = sha3_256

-- | G: SHA3-512 producing 64 bytes, split into two 32-byte halves (FIPS 203 Section 4.1)
hashG :: ByteString -> (ByteString, ByteString)
hashG input =
    let h = sha3_512 input
    in (BS.take 32 h, BS.drop 32 h)

-- | PRF_eta: SHAKE-256(s || b) truncated to len bytes (FIPS 203 Section 4.1)
prf :: ByteString -> Word8 -> Int -> ByteString
prf seed byte len =
    let input = seed `BS.append` BS.singleton byte
    in shake256 input len

-- | J: SHAKE-256(z || c) for implicit rejection (FIPS 203 Section 4.1)
hashJ :: ByteString -> ByteString -> ByteString
hashJ z ct = shake256 (z `BS.append` ct) 32

------------------------------------------------------------------------
-- XOF: SHAKE-128 (FIPS 203 Section 4.1)
------------------------------------------------------------------------

-- | XOF(rho, i, j): SHAKE-128 seed for matrix sampling
xof :: ByteString -> Word8 -> Word8 -> ByteString
xof seed i j = seed `BS.append` BS.pack [i, j]

------------------------------------------------------------------------
-- K-PKE (Internal PKE) — FIPS 203 Algorithms 12-14
------------------------------------------------------------------------

-- | Sample the public matrix A from rho (FIPS 203 Algorithm 12, line 6)
sampleMatrix :: ByteString -> [[Poly]]
sampleMatrix rho =
    [[sampleNTT (xof rho (fromIntegral i) (fromIntegral j))
     | j <- [0.._K-1]]
    | i <- [0.._K-1]]

-- | K-PKE.KeyGen (Algorithm 12)
kpkeKeyGen :: ByteString -> ([[Poly]], [Poly], [Poly])
kpkeKeyGen d =
    let (rho, sigma) = hashG d
        aHat = sampleMatrix rho
        sVec = [ntt (sampleCBD _ETA1 (prf sigma (fromIntegral i) (64 * _ETA1)))
               | i <- [0.._K-1]]
        eVec = [ntt (sampleCBD _ETA1 (prf sigma (fromIntegral (_K + i)) (64 * _ETA1)))
               | i <- [0.._K-1]]
        tHat = [foldl1 polyAdd
                    [polyBaseMul (aHat !! i !! j) (sVec !! j)
                    | j <- [0.._K-1]]
                `polyAdd` (eVec !! i)
               | i <- [0.._K-1]]
    in (aHat, tHat, sVec)

encodeEK :: [Poly] -> ByteString -> ByteString
encodeEK tHat rho =
    BS.concat (map (byteEncode 12) tHat) `BS.append` rho

decodeEK :: ByteString -> ([Poly], ByteString)
decodeEK ek =
    let polyBytes = 384  -- 256 * 12 / 8
        tHat = [byteDecode12 (bsSlice (i * polyBytes) polyBytes ek)
               | i <- [0.._K-1]]
        rho = BS.drop (_K * polyBytes) ek
    in (tHat, rho)

encodeDK :: [Poly] -> ByteString
encodeDK sVec = BS.concat (map (byteEncode 12) sVec)

decodeDK :: ByteString -> [Poly]
decodeDK dk =
    let polyBytes = 384
    in [byteDecode12 (bsSlice (i * polyBytes) polyBytes dk)
       | i <- [0.._K-1]]

-- | Sample error vectors for encryption (FIPS 203 Algorithm 13, lines 3-7)
sampleEncryptVectors :: ByteString -> ([Poly], [Poly], Poly)
sampleEncryptVectors r =
    let rVec = [ntt (sampleCBD _ETA1 (prf r (fromIntegral i) (64 * _ETA1)))
               | i <- [0.._K-1]]
        e1 = [sampleCBD _ETA2 (prf r (fromIntegral (_K + i)) (64 * _ETA2))
             | i <- [0.._K-1]]
        e2 = sampleCBD _ETA2 (prf r (fromIntegral (2 * _K)) (64 * _ETA2))
    in (rVec, e1, e2)

-- | Compute u = NTT^{-1}(A^T * r) + e1 (FIPS 203 Algorithm 13, line 9)
computeU :: [[Poly]] -> [Poly] -> [Poly] -> [Poly]
computeU aHat rVec e1 =
    [invNtt (foldl1 polyAdd
                [polyBaseMul (aHat !! j !! i) (rVec !! j)
                | j <- [0.._K-1]])
        `polyAdd` (e1 !! i)
    | i <- [0.._K-1]]

-- | K-PKE.Encrypt (Algorithm 13)
kpkeEncrypt :: ByteString -> ByteString -> ByteString -> ByteString
kpkeEncrypt ek m r =
    let (tHat, rho) = decodeEK ek
        aHat = sampleMatrix rho
        (rVec, e1, e2) = sampleEncryptVectors r
        uVec = computeU aHat rVec e1
        mu = decompressPoly 1 (byteDecode 1 m)
        v = foldl1 polyAdd
                [invNtt (polyBaseMul (tHat !! i) (rVec !! i))
                | i <- [0.._K-1]]
            `polyAdd` e2
            `polyAdd` mu
        c1 = BS.concat [byteEncode _DU (compressPoly _DU (uVec !! i))
                        | i <- [0.._K-1]]
        c2 = byteEncode _DV (compressPoly _DV v)
    in c1 `BS.append` c2

-- | K-PKE.Decrypt (Algorithm 14)
kpkeDecrypt :: ByteString -> ByteString -> ByteString
kpkeDecrypt dk ct =
    let sHat = decodeDK dk
        c1Bytes = _K * (_N * _DU `div` 8)
        c1 = BS.take c1Bytes ct
        c2 = BS.drop c1Bytes ct
        uVec = [decompressPoly _DU
                    (byteDecode _DU (bsSlice (i * (_N * _DU `div` 8))
                                             (_N * _DU `div` 8) c1))
               | i <- [0.._K-1]]
        v = decompressPoly _DV (byteDecode _DV c2)
        innerProd = foldl1 polyAdd
            [invNtt (polyBaseMul (sHat !! i) (ntt (uVec !! i)))
            | i <- [0.._K-1]]
        w = v `polySub` innerProd
        m = byteEncode 1 (compressPoly 1 w)
    in m

------------------------------------------------------------------------
-- Compress / Decompress for full polynomials
------------------------------------------------------------------------

compressPoly :: Int -> Poly -> Poly
compressPoly d (Poly coeffs) =
    Poly (array (0, _N - 1) [(i, compressD d (coeffs ! i)) | i <- [0.._N-1]])

decompressPoly :: Int -> Poly -> Poly
decompressPoly d (Poly coeffs) =
    Poly (array (0, _N - 1) [(i, decompressD d (coeffs ! i)) | i <- [0.._N-1]])

------------------------------------------------------------------------
-- ML-KEM (Algorithms 15-17)
------------------------------------------------------------------------

-- | ML-KEM.KeyGen (Algorithm 15): deterministic key generation.
mlkemKeyGen :: ByteString -> ByteString -> (MLKEMEncapKey, MLKEMDecapKey)
mlkemKeyGen d z =
    let (rho, _sigma) = hashG d
        (_aHat, tHat, sVec) = kpkeKeyGen d
        ek = encodeEK tHat rho
        dk = encodeDK sVec
        ekHash = hashH ek
        fullDK = dk `BS.append` ek `BS.append` ekHash `BS.append` z
    in (MLKEMEncapKey ek, MLKEMDecapKey fullDK)

-- | ML-KEM.Encaps (Algorithm 16): deterministic encapsulation.
mlkemEncaps :: MLKEMEncapKey -> ByteString -> (MLKEMCiphertext, ByteString)
mlkemEncaps (MLKEMEncapKey ek) m =
    let (sharedSecret, r) = hashG (m `BS.append` hashH ek)
        ct = kpkeEncrypt ek m r
    in (MLKEMCiphertext ct, sharedSecret)

-- | ML-KEM.Decaps (Algorithm 17): decapsulation with implicit rejection.
mlkemDecaps :: MLKEMDecapKey -> MLKEMCiphertext -> ByteString
mlkemDecaps (MLKEMDecapKey fullDK) (MLKEMCiphertext ct) =
    let dkPKELen = _K * 384
        ekLen = _K * 384 + 32
        dkPKE = BS.take dkPKELen fullDK
        ek = bsSlice dkPKELen ekLen fullDK
        ekHash = bsSlice (dkPKELen + ekLen) 32 fullDK
        z = BS.drop (dkPKELen + ekLen + 32) fullDK
        m' = kpkeDecrypt dkPKE ct
        (sharedSecret', r') = hashG (m' `BS.append` ekHash)
        ct' = kpkeEncrypt ek m' r'
        rejectionSecret = hashJ z ct
    in if constantEq ct' ct
       then sharedSecret'
       else rejectionSecret

------------------------------------------------------------------------
-- Utility
------------------------------------------------------------------------

bsSlice :: Int -> Int -> ByteString -> ByteString
bsSlice offset len bs
    | offset + len > BS.length bs = BS.empty
    | otherwise                   = BS.take len (BS.drop offset bs)

