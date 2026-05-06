-- | {-# REQ "CRYPTO-PQ-001" #-} SHA-3 (FIPS 202) — Keccak sponge construction
--
-- Pure Haskell reference implementation of Keccak-f[1600] permutation
-- and SHA-3 / SHAKE hash functions. Required as a dependency for
-- ML-KEM-768 (FIPS 203).
--
-- NOT constant-time. Production builds should use FFI to a
-- constant-time C implementation.
module UmbraVox.Crypto.Keccak
    ( sha3_256    -- SHA3-256: msg -> 32-byte digest
    , sha3_512    -- SHA3-512: msg -> 64-byte digest
    , shake128    -- SHAKE-128: msg -> output_len -> arbitrary-length output
    , shake256    -- SHAKE-256: msg -> output_len -> arbitrary-length output
    , keccakF1600 -- Raw permutation (for testing)
    ) where

import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Array.ST (STUArray, newArray, readArray, writeArray, freeze)
import Data.Bits ((.&.), (.|.), complement, rotateL, shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word64)
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)

------------------------------------------------------------------------
-- FIPS 202, Section 3.2 — Keccak-f[1600] round constants (iota step)
------------------------------------------------------------------------

roundConstants :: UArray Int Word64
roundConstants = listArray (0, 23)
    [ 0x0000000000000001, 0x0000000000008082
    , 0x800000000000808A, 0x8000000080008000
    , 0x000000000000808B, 0x0000000080000001
    , 0x8000000080008081, 0x8000000000008009
    , 0x000000000000008A, 0x0000000000000088
    , 0x0000000080008009, 0x000000008000000A
    , 0x000000008000808B, 0x800000000000008B
    , 0x8000000000008089, 0x8000000000008003
    , 0x8000000000008002, 0x8000000000000080
    , 0x000000000000800A, 0x800000008000000A
    , 0x8000000080008081, 0x8000000000008080
    , 0x0000000080000001, 0x8000000080008008
    ]

------------------------------------------------------------------------
-- FIPS 202, Section 3.2.2 — Rotation offsets (rho step)
-- Indexed by x + 5*y
------------------------------------------------------------------------

rotOffsets :: UArray Int Int
rotOffsets = listArray (0, 24)
    [  0,  1, 62, 28, 27   -- y=0: (0,0) (1,0) (2,0) (3,0) (4,0)
    , 36, 44,  6, 55, 20   -- y=1: (0,1) (1,1) (2,1) (3,1) (4,1)
    , 3,  10, 43, 25, 39   -- y=2: (0,2) (1,2) (2,2) (3,2) (4,2)
    , 41, 45, 15, 21,  8   -- y=3: (0,3) (1,3) (2,3) (3,3) (4,3)
    , 18,  2, 61, 56, 14   -- y=4: (0,4) (1,4) (2,4) (3,4) (4,4)
    ]

------------------------------------------------------------------------
-- Keccak-f[1600] permutation (mutable ST version for performance)
------------------------------------------------------------------------

-- | Apply the Keccak-f[1600] permutation to a 25-element state.
-- State is indexed as state[x + 5*y] where x,y in [0..4].
keccakF1600 :: UArray Int Word64 -> UArray Int Word64
keccakF1600 stateIn = runST $ do
    st <- newArray (0, 24) 0 :: ST s (STUArray s Int Word64)
    -- Copy input state
    forM_ [0..24] $ \i -> writeArray st i (stateIn ! i)
    -- 24 rounds
    forM_ [0..23] $ \roundIdx -> keccakRound st roundIdx
    freeze st
{-# INLINE keccakF1600 #-}

-- | One round of Keccak-f[1600]: theta, rho, pi, chi, iota
keccakRound :: STUArray s Int Word64 -> Int -> ST s ()
keccakRound st roundIdx = do
    -- Theta step
    -- Compute column parities
    c0 <- xor5 st 0 5 10 15 20
    c1 <- xor5 st 1 6 11 16 21
    c2 <- xor5 st 2 7 12 17 22
    c3 <- xor5 st 3 8 13 18 23
    c4 <- xor5 st 4 9 14 19 24

    let !d0 = c4 `xor` rotateL c1 1
        !d1 = c0 `xor` rotateL c2 1
        !d2 = c1 `xor` rotateL c3 1
        !d3 = c2 `xor` rotateL c4 1
        !d4 = c3 `xor` rotateL c0 1

    -- XOR d into each lane
    xorInto st  0 d0; xorInto st  5 d0; xorInto st 10 d0; xorInto st 15 d0; xorInto st 20 d0
    xorInto st  1 d1; xorInto st  6 d1; xorInto st 11 d1; xorInto st 16 d1; xorInto st 21 d1
    xorInto st  2 d2; xorInto st  7 d2; xorInto st 12 d2; xorInto st 17 d2; xorInto st 22 d2
    xorInto st  3 d3; xorInto st  8 d3; xorInto st 13 d3; xorInto st 18 d3; xorInto st 23 d3
    xorInto st  4 d4; xorInto st  9 d4; xorInto st 14 d4; xorInto st 19 d4; xorInto st 24 d4

    -- Rho and Pi steps (combined)
    -- Read all 25 lanes, rotate by rho offset, then write to pi-permuted position
    -- Pi: (x,y) -> (y, 2*x + 3*y mod 5), i.e. dst[y + 5*(2x+3y mod 5)] = rotateL(src[x+5*y], rot[x+5*y])
    s0  <- readArray st 0
    s1  <- readArray st 1
    s2  <- readArray st 2
    s3  <- readArray st 3
    s4  <- readArray st 4
    s5  <- readArray st 5
    s6  <- readArray st 6
    s7  <- readArray st 7
    s8  <- readArray st 8
    s9  <- readArray st 9
    s10 <- readArray st 10
    s11 <- readArray st 11
    s12 <- readArray st 12
    s13 <- readArray st 13
    s14 <- readArray st 14
    s15 <- readArray st 15
    s16 <- readArray st 16
    s17 <- readArray st 17
    s18 <- readArray st 18
    s19 <- readArray st 19
    s20 <- readArray st 20
    s21 <- readArray st 21
    s22 <- readArray st 22
    s23 <- readArray st 23
    s24 <- readArray st 24

    -- rho: rotate each lane; pi: permute positions
    -- pi maps (x,y) -> (y, 2x+3y mod 5)
    -- src index x+5y -> dst index y + 5*((2x+3y) mod 5)
    -- (0,0)->( 0,0)= 0  (1,0)->(0,2)=10  (2,0)->(0,4)=20  (3,0)->(0,1)= 5  (4,0)->(0,3)=15
    -- (0,1)->(1,3)=16  (1,1)->(1,0)= 1  (2,1)->(1,2)=11  (3,1)->(1,4)=21  (4,1)->(1,1)= 6
    -- (0,2)->(2,1)= 7  (1,2)->(2,3)=17  (2,2)->(2,0)= 2  (3,2)->(2,2)=12  (4,2)->(2,4)=22
    -- (0,3)->(3,4)=23  (1,3)->(3,1)= 8  (2,3)->(3,3)=18  (3,3)->(3,0)= 3  (4,3)->(3,2)=13
    -- (0,4)->(4,2)=14  (1,4)->(4,4)=24  (2,4)->(4,1)= 9  (3,4)->(4,3)=19  (4,4)->(4,0)= 4

    writeArray st  0 (rotateL s0  (rotOffsets !  0))  -- (0,0)->(0,0)
    writeArray st 10 (rotateL s1  (rotOffsets !  1))  -- (1,0)->(0,2)=10
    writeArray st 20 (rotateL s2  (rotOffsets !  2))  -- (2,0)->(0,4)=20
    writeArray st  5 (rotateL s3  (rotOffsets !  3))  -- (3,0)->(0,1)=5
    writeArray st 15 (rotateL s4  (rotOffsets !  4))  -- (4,0)->(0,3)=15
    writeArray st 16 (rotateL s5  (rotOffsets !  5))  -- (0,1)->(1,3)=16
    writeArray st  1 (rotateL s6  (rotOffsets !  6))  -- (1,1)->(1,0)=1
    writeArray st 11 (rotateL s7  (rotOffsets !  7))  -- (2,1)->(1,2)=11
    writeArray st 21 (rotateL s8  (rotOffsets !  8))  -- (3,1)->(1,4)=21
    writeArray st  6 (rotateL s9  (rotOffsets !  9))  -- (4,1)->(1,1)=6
    writeArray st  7 (rotateL s10 (rotOffsets ! 10))  -- (0,2)->(2,1)=7
    writeArray st 17 (rotateL s11 (rotOffsets ! 11))  -- (1,2)->(2,3)=17
    writeArray st  2 (rotateL s12 (rotOffsets ! 12))  -- (2,2)->(2,0)=2
    writeArray st 12 (rotateL s13 (rotOffsets ! 13))  -- (3,2)->(2,2)=12
    writeArray st 22 (rotateL s14 (rotOffsets ! 14))  -- (4,2)->(2,4)=22
    writeArray st 23 (rotateL s15 (rotOffsets ! 15))  -- (0,3)->(3,4)=23
    writeArray st  8 (rotateL s16 (rotOffsets ! 16))  -- (1,3)->(3,1)=8
    writeArray st 18 (rotateL s17 (rotOffsets ! 17))  -- (2,3)->(3,3)=18
    writeArray st  3 (rotateL s18 (rotOffsets ! 18))  -- (3,3)->(3,0)=3
    writeArray st 13 (rotateL s19 (rotOffsets ! 19))  -- (4,3)->(3,2)=13
    writeArray st 14 (rotateL s20 (rotOffsets ! 20))  -- (0,4)->(4,2)=14
    writeArray st 24 (rotateL s21 (rotOffsets ! 21))  -- (1,4)->(4,4)=24
    writeArray st  9 (rotateL s22 (rotOffsets ! 22))  -- (2,4)->(4,1)=9
    writeArray st 19 (rotateL s23 (rotOffsets ! 23))  -- (3,4)->(4,3)=19
    writeArray st  4 (rotateL s24 (rotOffsets ! 24))  -- (4,4)->(4,0)=4

    -- Chi step
    -- For each row y: a[x,y] = a[x,y] xor (not a[x+1,y] and a[x+2,y])
    forM_ [0..4] $ \y -> do
        let !base = 5 * y
        t0 <- readArray st (base + 0)
        t1 <- readArray st (base + 1)
        t2 <- readArray st (base + 2)
        t3 <- readArray st (base + 3)
        t4 <- readArray st (base + 4)
        writeArray st (base + 0) (t0 `xor` (complement t1 .&. t2))
        writeArray st (base + 1) (t1 `xor` (complement t2 .&. t3))
        writeArray st (base + 2) (t2 `xor` (complement t3 .&. t4))
        writeArray st (base + 3) (t3 `xor` (complement t4 .&. t0))
        writeArray st (base + 4) (t4 `xor` (complement t0 .&. t1))

    -- Iota step
    xorInto st 0 (roundConstants ! roundIdx)
{-# INLINE keccakRound #-}

-- | XOR five state elements together
xor5 :: STUArray s Int Word64 -> Int -> Int -> Int -> Int -> Int -> ST s Word64
xor5 st !a !b !c !d !e = do
    !va <- readArray st a
    !vb <- readArray st b
    !vc <- readArray st c
    !vd <- readArray st d
    !ve <- readArray st e
    return $! va `xor` vb `xor` vc `xor` vd `xor` ve
{-# INLINE xor5 #-}

-- | XOR a value into a state element
xorInto :: STUArray s Int Word64 -> Int -> Word64 -> ST s ()
xorInto st !i !v = do
    !old <- readArray st i
    writeArray st i (old `xor` v)
{-# INLINE xorInto #-}

------------------------------------------------------------------------
-- Byte <-> Word64 conversion (little-endian, as per FIPS 202)
------------------------------------------------------------------------

-- | Decode a Word64 from 8 little-endian bytes
bytesToWord64 :: ByteString -> Int -> Word64
bytesToWord64 bs offset =
    let b i = fromIntegral (BS.index bs (offset + i)) :: Word64
    in  b 0 .|. (b 1 `shiftL` 8) .|. (b 2 `shiftL` 16) .|. (b 3 `shiftL` 24)
        .|. (b 4 `shiftL` 32) .|. (b 5 `shiftL` 40) .|. (b 6 `shiftL` 48) .|. (b 7 `shiftL` 56)
{-# INLINE bytesToWord64 #-}

-- | Encode a Word64 as 8 little-endian bytes
word64ToBytes :: Word64 -> [Word8]
word64ToBytes w =
    [ fromIntegral (w)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 32)
    , fromIntegral (w `shiftR` 40)
    , fromIntegral (w `shiftR` 48)
    , fromIntegral (w `shiftR` 56)
    ]
{-# INLINE word64ToBytes #-}

------------------------------------------------------------------------
-- Sponge construction (FIPS 202, Section 4)
------------------------------------------------------------------------

-- | The core sponge function.
-- @rate@ is the rate in bytes, @suffix@ is the domain separation byte,
-- @outputLen@ is the desired output length in bytes.
sponge :: Int -> Word8 -> Int -> ByteString -> ByteString
sponge rate suffix outputLen msg = squeeze state0 outputLen
  where
    -- Pad the message: append suffix, pad with zeros, set last byte bit
    padded = pad rate suffix msg

    -- Absorb phase: process each rate-sized block
    state0 = absorbBlocks rate (emptyState) padded

    -- Empty state: 25 Word64s initialized to 0
    emptyState :: UArray Int Word64
    emptyState = listArray (0, 24) (replicate 25 0)

    -- Squeeze phase
    squeeze :: UArray Int Word64 -> Int -> ByteString
    squeeze st remaining
        | remaining <= 0 = BS.empty
        | remaining <= rate =
            BS.take remaining (stateToBytes st rate)
        | otherwise =
            let block = stateToBytes st rate
                st'   = keccakF1600 st
            in block `BS.append` squeeze st' (remaining - rate)

-- | FIPS 202 padding: append domain suffix, then pad10*1
pad :: Int -> Word8 -> ByteString -> ByteString
pad rate suffix msg =
    let msgLen   = BS.length msg
        -- How many bytes needed to fill current block
        padNeeded = rate - (msgLen `mod` rate)
    in if padNeeded == 1
       then -- suffix and 0x80 go in the same byte
           msg `BS.append` BS.singleton (suffix .|. 0x80)
       else
           msg `BS.append` BS.singleton suffix
               `BS.append` BS.replicate (padNeeded - 2) 0x00
               `BS.append` BS.singleton 0x80

-- | Absorb all rate-sized blocks into the state
absorbBlocks :: Int -> UArray Int Word64 -> ByteString -> UArray Int Word64
absorbBlocks rate st0 padded = go st0 0
  where
    totalLen = BS.length padded
    go !st !offset
        | offset >= totalLen = st
        | otherwise =
            let block = BS.take rate (BS.drop offset padded)
                st'   = absorbBlock rate st block
            in go st' (offset + rate)

-- | Absorb a single rate-sized block: XOR into state, then apply permutation
absorbBlock :: Int -> UArray Int Word64 -> ByteString -> UArray Int Word64
absorbBlock rate stateArr block = keccakF1600 xoredState
  where
    rateLanes = rate `div` 8
    xoredState = runST $ do
        st <- newArray (0, 24) 0 :: ST s (STUArray s Int Word64)
        forM_ [0..24] $ \i -> writeArray st i (stateArr ! i)
        -- XOR block lanes into state
        forM_ [0 .. rateLanes - 1] $ \i -> do
            let w = if (i * 8 + 7) < BS.length block
                    then bytesToWord64 block (i * 8)
                    else bytesToWord64 (block `BS.append` BS.replicate 8 0) (i * 8)
            xorInto st i w
        freeze st

-- | Extract rate bytes from the state
stateToBytes :: UArray Int Word64 -> Int -> ByteString
stateToBytes st rate =
    BS.take rate $ BS.pack $ concatMap (\i -> word64ToBytes (st ! i)) [0..24]

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | SHA3-256: produces a 32-byte (256-bit) digest.
-- Rate = 136 bytes (1088 bits), capacity = 512 bits, suffix = 0x06
sha3_256 :: ByteString -> ByteString
sha3_256 = sponge 136 0x06 32
{-# INLINE sha3_256 #-}

-- | SHA3-512: produces a 64-byte (512-bit) digest.
-- Rate = 72 bytes (576 bits), capacity = 1024 bits, suffix = 0x06
sha3_512 :: ByteString -> ByteString
sha3_512 = sponge 72 0x06 64
{-# INLINE sha3_512 #-}

-- | SHAKE-128: extendable output function.
-- Rate = 168 bytes (1344 bits), capacity = 256 bits, suffix = 0x1F
shake128 :: ByteString -> Int -> ByteString
shake128 msg outputLen = sponge 168 0x1F outputLen msg
{-# INLINE shake128 #-}

-- | SHAKE-256: extendable output function.
-- Rate = 136 bytes (1088 bits), capacity = 512 bits, suffix = 0x1F
shake256 :: ByteString -> Int -> ByteString
shake256 msg outputLen = sponge 136 0x1F outputLen msg
{-# INLINE shake256 #-}
