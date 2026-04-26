-- | {-# REQ "CRYPTO-001" #-} AES-256 block cipher (FIPS 197)
--
-- Pure Haskell reference implementation. NOT constant-time.
-- The S-box is implemented as a lookup table; the pure Haskell path
-- is for verification only. Production uses FFI to constant-time C.
module UmbraVox.Crypto.AES
    ( aesEncrypt
    , aesDecrypt
    , aesExpandKey
    , AESKey
    ) where

import Data.Array (Array, listArray, (!))
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word8, Word32)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Expanded AES-256 key schedule: 15 round keys (each 16 bytes).
-- Index 0 = initial key addition, index 14 = final round.
newtype AESKey = AESKey (Array Int Word32)

------------------------------------------------------------------------
-- FIPS 197 Section 5.2 — Key Expansion
------------------------------------------------------------------------

-- | Expand a 32-byte (256-bit) key into the AES-256 key schedule.
aesExpandKey :: ByteString -> AESKey
aesExpandKey !key
    | BS.length key /= 32 = error "AES-256 requires exactly 32-byte key"
    | otherwise = AESKey w
  where
    nk = 8   -- AES-256: 8 words in the key
    nr = 14  -- AES-256: 14 rounds
    w = listArray (0, 4 * (nr + 1) - 1) [wt i | i <- [0 .. 4 * (nr + 1) - 1]]
    wt i
        | i < nk    = getWord32 key (i * 4)
        | i `mod` nk == 0 =
            (w ! (i - nk)) `xor` subWord (rotWord (w ! (i - 1))) `xor` rcon (i `div` nk)
        | i `mod` nk == 4 =
            (w ! (i - nk)) `xor` subWord (w ! (i - 1))
        | otherwise =
            (w ! (i - nk)) `xor` (w ! (i - 1))

------------------------------------------------------------------------
-- FIPS 197 Section 5.1.1 — SubBytes (S-box)
------------------------------------------------------------------------

sbox :: Array Word8 Word8
sbox = listArray (0, 255)
    [ 0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76
    , 0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0
    , 0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15
    , 0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75
    , 0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84
    , 0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf
    , 0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8
    , 0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2
    , 0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73
    , 0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb
    , 0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79
    , 0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08
    , 0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a
    , 0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e
    , 0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf
    , 0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
    ]

invSbox :: Array Word8 Word8
invSbox = listArray (0, 255)
    [ 0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb
    , 0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb
    , 0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e
    , 0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25
    , 0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92
    , 0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84
    , 0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06
    , 0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b
    , 0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73
    , 0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e
    , 0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b
    , 0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4
    , 0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f
    , 0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef
    , 0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61
    , 0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d
    ]

subByte :: Word8 -> Word8
subByte b = sbox ! b
{-# INLINE subByte #-}

invSubByte :: Word8 -> Word8
invSubByte b = invSbox ! b
{-# INLINE invSubByte #-}

subWord :: Word32 -> Word32
subWord w =
    (fromIntegral (subByte (fromIntegral (w `shiftR` 24))) `shiftL` 24) .|.
    (fromIntegral (subByte (fromIntegral (w `shiftR` 16))) `shiftL` 16) .|.
    (fromIntegral (subByte (fromIntegral (w `shiftR` 8))) `shiftL` 8) .|.
    fromIntegral (subByte (fromIntegral w))

rotWord :: Word32 -> Word32
rotWord w = (w `shiftL` 8) .|. (w `shiftR` 24)

------------------------------------------------------------------------
-- Round constants
------------------------------------------------------------------------

rcon :: Int -> Word32
rcon 1  = 0x01000000
rcon 2  = 0x02000000
rcon 3  = 0x04000000
rcon 4  = 0x08000000
rcon 5  = 0x10000000
rcon 6  = 0x20000000
rcon 7  = 0x40000000
rcon _  = error "AES-256: rcon index out of range"

------------------------------------------------------------------------
-- State operations — state is 16 bytes in column-major order
-- Column c, row r = state[r + 4*c]
------------------------------------------------------------------------

type State = Array Int Word8

stateFromBS :: ByteString -> State
stateFromBS bs = listArray (0, 15) (BS.unpack bs)

stateToBS :: State -> ByteString
stateToBS st = BS.pack [st ! i | i <- [0..15]]

stateGet :: State -> Int -> Int -> Word8
stateGet st r c = st ! (r + 4 * c)
{-# INLINE stateGet #-}



------------------------------------------------------------------------
-- FIPS 197 Section 5.1.1 — SubBytes
------------------------------------------------------------------------

subBytes :: State -> State
subBytes st = listArray (0, 15) [subByte (st ! i) | i <- [0..15]]

invSubBytes :: State -> State
invSubBytes st = listArray (0, 15) [invSubByte (st ! i) | i <- [0..15]]

------------------------------------------------------------------------
-- FIPS 197 Section 5.1.2 — ShiftRows
------------------------------------------------------------------------

shiftRows :: State -> State
shiftRows st = listArray (0, 15)
    [ stateGet st 0 0, stateGet st 1 1, stateGet st 2 2, stateGet st 3 3  -- col 0
    , stateGet st 0 1, stateGet st 1 2, stateGet st 2 3, stateGet st 3 0  -- col 1
    , stateGet st 0 2, stateGet st 1 3, stateGet st 2 0, stateGet st 3 1  -- col 2
    , stateGet st 0 3, stateGet st 1 0, stateGet st 2 1, stateGet st 3 2  -- col 3
    ]

invShiftRows :: State -> State
invShiftRows st = listArray (0, 15)
    [ stateGet st 0 0, stateGet st 1 3, stateGet st 2 2, stateGet st 3 1  -- col 0
    , stateGet st 0 1, stateGet st 1 0, stateGet st 2 3, stateGet st 3 2  -- col 1
    , stateGet st 0 2, stateGet st 1 1, stateGet st 2 0, stateGet st 3 3  -- col 2
    , stateGet st 0 3, stateGet st 1 2, stateGet st 2 1, stateGet st 3 0  -- col 3
    ]

------------------------------------------------------------------------
-- FIPS 197 Section 5.1.3 — MixColumns
-- GF(2^8) multiplication with reduction polynomial x^8+x^4+x^3+x+1
------------------------------------------------------------------------

xtime :: Word8 -> Word8
xtime b = if b .&. 0x80 /= 0
    then (b `shiftL` 1) `xor` 0x1b
    else b `shiftL` 1
{-# INLINE xtime #-}

gmul :: Word8 -> Word8 -> Word8
gmul _ 0 = 0
gmul 0 _ = 0
gmul a 1 = a
gmul a 2 = xtime a
gmul a 3 = xtime a `xor` a
gmul a b = go a b 0
  where
    go _ 0 acc = acc
    go x y acc =
        let acc' = if y .&. 1 /= 0 then acc `xor` x else acc
            x'   = xtime x
            y'   = y `shiftR` 1
        in go x' y' acc'

mixColumns :: State -> State
mixColumns st = listArray (0, 15) (concatMap mixCol [0..3])
  where
    mixCol c =
        let s0 = stateGet st 0 c
            s1 = stateGet st 1 c
            s2 = stateGet st 2 c
            s3 = stateGet st 3 c
        in [ gmul 2 s0 `xor` gmul 3 s1 `xor` s2 `xor` s3
           , s0 `xor` gmul 2 s1 `xor` gmul 3 s2 `xor` s3
           , s0 `xor` s1 `xor` gmul 2 s2 `xor` gmul 3 s3
           , gmul 3 s0 `xor` s1 `xor` s2 `xor` gmul 2 s3
           ]

invMixColumns :: State -> State
invMixColumns st = listArray (0, 15) (concatMap mixCol [0..3])
  where
    mixCol c =
        let s0 = stateGet st 0 c
            s1 = stateGet st 1 c
            s2 = stateGet st 2 c
            s3 = stateGet st 3 c
        in [ gmul 0x0e s0 `xor` gmul 0x0b s1 `xor` gmul 0x0d s2 `xor` gmul 0x09 s3
           , gmul 0x09 s0 `xor` gmul 0x0e s1 `xor` gmul 0x0b s2 `xor` gmul 0x0d s3
           , gmul 0x0d s0 `xor` gmul 0x09 s1 `xor` gmul 0x0e s2 `xor` gmul 0x0b s3
           , gmul 0x0b s0 `xor` gmul 0x0d s1 `xor` gmul 0x09 s2 `xor` gmul 0x0e s3
           ]

------------------------------------------------------------------------
-- FIPS 197 Section 5.1.4 — AddRoundKey
------------------------------------------------------------------------

addRoundKey :: AESKey -> Int -> State -> State
addRoundKey (AESKey w) round st = listArray (0, 15) [xorByte c r | c <- [0..3], r <- [0..3]]
  where
    xorByte c r =
        let !wrd   = w ! (round * 4 + c)
            !kByte = fromIntegral (wrd `shiftR` (8 * (3 - r))) :: Word8
        in (st ! (r + 4 * c)) `xor` kByte

------------------------------------------------------------------------
-- FIPS 197 Section 5.1 — Cipher (Encryption)
------------------------------------------------------------------------

cipher :: AESKey -> State -> State
cipher key st =
    let nr = 14  -- AES-256
        -- Initial round key addition
        s0 = addRoundKey key 0 st
        -- Rounds 1 through nr-1
        sMid = foldl' doRound s0 [1..nr-1]
        -- Final round (no MixColumns)
        sFinal = addRoundKey key nr (shiftRows (subBytes sMid))
    in sFinal
  where
    doRound s r = addRoundKey key r (mixColumns (shiftRows (subBytes s)))

------------------------------------------------------------------------
-- FIPS 197 Section 5.3 — Inverse Cipher (Decryption)
------------------------------------------------------------------------

invCipher :: AESKey -> State -> State
invCipher key st =
    let nr = 14
        s0 = addRoundKey key nr st
        sMid = foldl' doRound s0 [nr-1, nr-2 .. 1]
        sFinal = addRoundKey key 0 (invSubBytes (invShiftRows sMid))
    in sFinal
  where
    doRound s r = invMixColumns (addRoundKey key r (invSubBytes (invShiftRows s)))

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

getWord32 :: ByteString -> Int -> Word32
getWord32 !bs !i =
    (fromIntegral (BS.index bs i) `shiftL` 24) .|.
    (fromIntegral (BS.index bs (i + 1)) `shiftL` 16) .|.
    (fromIntegral (BS.index bs (i + 2)) `shiftL` 8) .|.
    fromIntegral (BS.index bs (i + 3))

-- | AES-256 block encryption.
-- Key: 32 bytes. Plaintext: 16 bytes. Returns 16-byte ciphertext.
aesEncrypt :: ByteString  -- ^ 32-byte key
           -> ByteString  -- ^ 16-byte plaintext block
           -> ByteString  -- ^ 16-byte ciphertext block
aesEncrypt !key !plaintext
    | BS.length key /= 32       = error "AES-256: key must be 32 bytes"
    | BS.length plaintext /= 16 = error "AES-256: plaintext must be 16 bytes"
    | otherwise = stateToBS (cipher expandedKey (stateFromBS plaintext))
  where
    !expandedKey = aesExpandKey key

-- | AES-256 block decryption.
-- Key: 32 bytes. Ciphertext: 16 bytes. Returns 16-byte plaintext.
aesDecrypt :: ByteString  -- ^ 32-byte key
           -> ByteString  -- ^ 16-byte ciphertext block
           -> ByteString  -- ^ 16-byte plaintext block
aesDecrypt !key !ciphertext
    | BS.length key /= 32        = error "AES-256: key must be 32 bytes"
    | BS.length ciphertext /= 16 = error "AES-256: ciphertext must be 16 bytes"
    | otherwise = stateToBS (invCipher expandedKey (stateFromBS ciphertext))
  where
    !expandedKey = aesExpandKey key
