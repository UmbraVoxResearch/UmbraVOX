-- SPDX-License-Identifier: Apache-2.0
-- | Pinned, zeroing byte buffer for sensitive key material.
--
-- 'SecureBytes' wraps a pinned 'ForeignPtr' 'Word8' whose associated
-- finalizer overwrites every byte with zero before the memory is freed.
-- This is a best-effort defence against key material persisting on the heap
-- after a value is no longer referenced.
--
-- == Limitation: not @explicit_bzero@
--
-- The zeroing finalizer uses 'pokeByteOff' in a strict loop.  GHC may
-- legally elide writes to memory it determines is dead after the last live
-- use of the 'ForeignPtr', so this approach does NOT provide the same
-- guarantee as @explicit_bzero(3)@ (Linux) or @SecureZeroMemory@ (Windows),
-- which are specifically designed to resist dead-store elimination.
-- Production deployments that require strong erasure guarantees must replace
-- the finalizer body with an FFI call to one of those platform functions.
-- That work is tracked as a known limitation in @doc/CRYPTO-SAFETY.md@ §4.
module UmbraVox.Crypto.SecureBytes
    ( SecureBytes
    , newSecureBytes
    , fromByteString
    , toByteString
    , withSecurePtr
    , zeroAndFree
    , secureBytesLength
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import qualified Foreign.Concurrent as FC
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeByteOff)

------------------------------------------------------------------------
-- Type
------------------------------------------------------------------------

-- | A pinned byte buffer that zeros its memory when finalised.
--
-- The buffer is allocated via 'mallocBytes', which returns memory that the
-- GC will never move (it lives outside the GC heap).
-- A Haskell IO finalizer registered via 'Foreign.Concurrent.newForeignPtr'
-- zeros every byte before freeing the allocation.
data SecureBytes = SecureBytes
    { _sbPtr :: !(ForeignPtr Word8)
    , _sbLen :: !Int
    }

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Allocate a new 'SecureBytes' of @n@ bytes, all initialised to zero.
--
-- Uses 'mallocBytes' for the allocation and 'Foreign.Concurrent.newForeignPtr'
-- to attach a zeroing finalizer.
newSecureBytes :: Int -> IO SecureBytes
newSecureBytes n = do
    p  <- mallocBytes n :: IO (Ptr Word8)
    -- Zero the freshly-allocated buffer.
    zeroPtr p n
    -- Attach a finalizer that zeros the buffer again and then frees it.
    -- Foreign.Concurrent.newForeignPtr takes a plain IO () action, unlike the
    -- Foreign.ForeignPtr variant which requires a C-level FunPtr.
    fp <- FC.newForeignPtr p (zeroPtr p n >> free p)
    pure (SecureBytes fp n)

-- | Wrap an existing 'ByteString' into a 'SecureBytes'.
--
-- Copies the bytes into a freshly allocated pinned buffer so that the
-- original 'ByteString' and the new 'SecureBytes' do not share memory.
fromByteString :: ByteString -> IO SecureBytes
fromByteString bs = do
    let len = BS.length bs
    sb <- newSecureBytes len
    withForeignPtr (_sbPtr sb) $ \p ->
        mapM_ (\i -> pokeByteOff p i (BS.index bs i)) [0 .. len - 1]
    pure sb

-- | Copy the contents of a 'SecureBytes' into a regular 'ByteString'.
--
-- The returned 'ByteString' is an ordinary heap value; it is NOT zeroed on
-- collection.  Callers should minimise the lifetime of the returned value.
toByteString :: SecureBytes -> IO ByteString
toByteString (SecureBytes fp len) =
    withForeignPtr fp $ \p ->
        BS.packCStringLen (castPtr p, len)

-- | Run an IO action with a raw pointer to the buffer contents.
--
-- The pointer is only valid for the duration of the action.  Do not store
-- it or allow it to escape.
withSecurePtr :: SecureBytes -> (Ptr Word8 -> IO a) -> IO a
withSecurePtr (SecureBytes fp _) = withForeignPtr fp

-- | Immediately zero the buffer contents.
--
-- Calling 'zeroAndFree' is optional; the registered finalizer will zero
-- and free the buffer when the 'SecureBytes' becomes unreachable.  Use this
-- when you want deterministic erasure at a known point rather than waiting
-- for GC.
--
-- After 'zeroAndFree' the 'SecureBytes' value must not be used.
zeroAndFree :: SecureBytes -> IO ()
zeroAndFree (SecureBytes fp len) =
    withForeignPtr fp $ \p -> zeroPtr p len

-- | Return the length of the buffer in bytes.
secureBytesLength :: SecureBytes -> Int
secureBytesLength = _sbLen

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Write zero to each byte in the range @[p, p+n)@.
--
-- Uses 'mapM_' over a strict list so each write is forced in order,
-- reducing (but not eliminating) the chance that the optimiser elides them.
-- See the module note on @explicit_bzero@ for the production upgrade path.
zeroPtr :: Ptr Word8 -> Int -> IO ()
zeroPtr p n = mapM_ (\i -> pokeByteOff p i (0 :: Word8)) [0 .. n - 1]
