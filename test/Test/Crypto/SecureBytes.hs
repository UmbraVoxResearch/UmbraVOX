-- SPDX-License-Identifier: Apache-2.0
-- | Tests for 'UmbraVox.Crypto.SecureBytes' (M15.3.5, M15.3.6).
--
-- Finding: Key material persists in unzeroed malloc'd buffers after free.
-- Vulnerability: Long-lived secret keys may remain in RAM after deallocation.
-- Fix: SecureBytes uses pinned allocation + volatile-write zeroing + mlock.
-- Verified: zeroAndFree zeroes buffer contents via FFI; mlock does not crash.
module Test.Crypto.SecureBytes (runTests) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff, pokeByteOff)

import Test.Util (assertEq)
import UmbraVox.Crypto.SecureBytes
    ( newSecureBytes
    , fromByteString
    , toByteString
    , withSecurePtr
    , withSecureKey
    , zeroAndFree
    , secureBytesLength
    )

import qualified Data.ByteString as BS

runTests :: IO Bool
runTests = do
    putStrLn "[SecureBytes] Running secure memory tests..."
    results <- sequence
        [ testNewSecureBytesZeroInit
        , testSecureBytesLength
        , testFromByteStringRoundTrip
        , testZeroAndFreeZeroes
        , testWithSecureKeyExtractsCorrectly
        , testMlockDoesNotCrash
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[SecureBytes] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Test 1: newSecureBytes initialises to zero
------------------------------------------------------------------------

testNewSecureBytesZeroInit :: IO Bool
testNewSecureBytesZeroInit = do
    sb <- newSecureBytes 32
    bs <- toByteString sb
    assertEq "newSecureBytes 32 is all zeros" (BS.replicate 32 0) bs

------------------------------------------------------------------------
-- Test 2: secureBytesLength returns correct value
------------------------------------------------------------------------

testSecureBytesLength :: IO Bool
testSecureBytesLength = do
    sb <- newSecureBytes 64
    assertEq "secureBytesLength 64" 64 (secureBytesLength sb)

------------------------------------------------------------------------
-- Test 3: fromByteString round-trips correctly
------------------------------------------------------------------------

testFromByteStringRoundTrip :: IO Bool
testFromByteStringRoundTrip = do
    let original = BS.pack [0x01 .. 0x20]
    sb <- fromByteString original
    extracted <- toByteString sb
    assertEq "fromByteString round-trip" original extracted

------------------------------------------------------------------------
-- Test 4: zeroAndFree zeroes all bytes (M15.3.5)
--
-- Allocate a buffer, write a known pattern, call zeroAndFree, then
-- read back via withSecurePtr to verify all bytes are zero.
------------------------------------------------------------------------

testZeroAndFreeZeroes :: IO Bool
testZeroAndFreeZeroes = do
    sb <- newSecureBytes 32
    -- Write a known non-zero pattern
    withSecurePtr sb $ \p ->
        mapM_ (\i -> pokeByteOff p i (0xAA :: Word8)) [0..31]
    -- Verify pattern was written
    before <- toByteString sb
    let patternWritten = before == BS.replicate 32 0xAA
    -- zeroAndFree zeros the buffer in-place but does NOT free the memory.
    -- The ForeignPtr finalizer (which calls free) runs only when the value
    -- becomes unreachable. Since 'sb' is still in scope, the memory is valid.
    zeroAndFree sb
    -- Read back within withSecurePtr: buffer should be zeroed
    allZero <- withSecurePtr sb $ \p ->
        and <$> mapM (\i -> (== (0 :: Word8)) <$> peekByteOff p i) [0..31]
    a <- assertEq "pattern was written before zeroing" True patternWritten
    b <- assertEq "all bytes zero after zeroAndFree" True allZero
    pure (a && b)

------------------------------------------------------------------------
-- Test 5: withSecureKey extracts correct ByteString
------------------------------------------------------------------------

testWithSecureKeyExtractsCorrectly :: IO Bool
testWithSecureKeyExtractsCorrectly = do
    let original = BS.pack [0x41 .. 0x60]
    sb <- fromByteString original
    result <- withSecureKey sb $ \bs -> pure (bs == original)
    assertEq "withSecureKey extracts matching ByteString" True result

------------------------------------------------------------------------
-- Test 6: mlock integration — allocation does not crash (M15.3.6)
--
-- newSecureBytes calls mlock internally.  This test verifies that
-- mlock does not cause an exception, even if RLIMIT_MEMLOCK is low.
------------------------------------------------------------------------

testMlockDoesNotCrash :: IO Bool
testMlockDoesNotCrash = do
    -- Allocate several buffers of varying sizes to exercise mlock
    sb1 <- newSecureBytes 32
    sb2 <- newSecureBytes 256
    sb3 <- newSecureBytes 4096
    let ok = secureBytesLength sb1 == 32
          && secureBytesLength sb2 == 256
          && secureBytesLength sb3 == 4096
    assertEq "mlock integration: allocation succeeds" True ok
