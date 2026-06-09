-- SPDX-License-Identifier: Apache-2.0
-- | Pinned, zeroing byte buffer for sensitive key material.
--
-- 'SecureBytes' wraps a pinned 'ForeignPtr' 'Word8' whose associated
-- finalizer overwrites every byte with zero before the memory is freed.
-- This is a best-effort defence against key material persisting on the heap
-- after a value is no longer referenced.
--
-- == Memory protection
--
-- Buffers are locked into physical RAM via @mlock(2)@ to prevent swap-out,
-- and excluded from core dumps via @madvise(MADV_DONTDUMP)@ (Linux) or
-- @MADV_NOCORE@ (BSD).  Both calls are best-effort: failure (e.g. from
-- RLIMIT_MEMLOCK) is silently ignored.
--
-- == Secure zeroing via C FFI
--
-- The zeroing finalizer calls 'c_secure_zero', a C helper that writes zeros
-- through a @volatile@ pointer.  A compiler is not permitted to remove writes
-- through a @volatile@ pointer even when it proves the memory is dead after
-- the call, so this provides the same dead-store-elimination resistance as
-- @explicit_bzero(3)@ (Linux) or @SecureZeroMemory@ (Windows) on platforms
-- that do not expose those extensions.  See @csrc/secure_zero.c@.
--
-- == Limitations
--
-- * 'toByteString' and 'withSecureKey' create a GC-heap copy that is NOT
--   zeroed on collection.  GHC's copying GC may replicate it across
--   generations.
-- * Pure crypto functions ('gcmEncrypt', 'hmacSHA256', etc.) create
--   intermediate 'ByteString' values on the GC heap.  These are not zeroed.
-- * @mlock@ does not protect against hibernation (suspend-to-disk).
--
-- The accurate assurance claim is: long-lived key material at rest is stored
-- in pinned, zeroed-on-free, mlock'd buffers.  Short-lived copies during
-- crypto operations may persist on the GC heap until collection.
module UmbraVox.Crypto.SecureBytes
    ( SecureBytes
    , newSecureBytes
    , fromByteString
    , toByteString
    , withSecurePtr
    , withSecureKey
    , zeroAndFree
    , secureBytesLength
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import qualified Foreign.Concurrent as FC
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)

------------------------------------------------------------------------
-- C FFI
------------------------------------------------------------------------

-- | C helper that zeros @len@ bytes at @ptr@ via a volatile write loop.
-- Implemented in @csrc/secure_zero.c@.
foreign import ccall "umbravox_secure_zero"
    c_secure_zero :: Ptr Word8 -> CSize -> IO ()

-- | Lock memory pages against swap-out.  Best-effort; may fail with
-- ENOMEM (RLIMIT_MEMLOCK) or EPERM on unprivileged processes.
-- Implemented in @csrc/secure_mlock.c@.
foreign import ccall "umbravox_mlock"
    c_mlock :: Ptr Word8 -> CSize -> IO CInt

-- | Unlock memory pages previously locked with 'c_mlock'.
-- Implemented in @csrc/secure_mlock.c@.
foreign import ccall "umbravox_munlock"
    c_munlock :: Ptr Word8 -> CSize -> IO CInt

-- | Exclude memory from core dumps (MADV_DONTDUMP on Linux,
-- MADV_NOCORE on BSD, no-op elsewhere).
-- Implemented in @csrc/secure_mlock.c@.
foreign import ccall "umbravox_dontdump"
    c_dontdump :: Ptr Word8 -> CSize -> IO CInt

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
-- The buffer is mlock'd to prevent swap-out and excluded from core dumps
-- via @madvise(MADV_DONTDUMP)@.  Both calls are best-effort: on systems
-- with a low @RLIMIT_MEMLOCK@ (or when the process limit is exhausted)
-- @mlock@ may fail silently, meaning the buffer __is not__ actually locked
-- into RAM and may be swapped to disk.  Callers have no visibility into
-- which buffers are actually protected.
--
-- TODO(M35B.mlock): Log or return a warning when @mlock@/@madvise@ fails.
-- Recommended: verify at process startup that @RLIMIT_MEMLOCK@ is at least
-- @total_key_material_bytes@ and abort with a clear error if not.  This
-- gives operators early notice rather than silent degraded protection.
newSecureBytes :: Int -> IO SecureBytes
newSecureBytes n = do
    p  <- mallocBytes n :: IO (Ptr Word8)
    -- Zero the freshly-allocated buffer via the C volatile write loop.
    c_secure_zero p (fromIntegral n)
    -- Lock the page into RAM and exclude from core dumps (best-effort).
    _ <- c_mlock p (fromIntegral n)
    _ <- c_dontdump p (fromIntegral n)
    -- Attach a finalizer that zeros, unlocks, and frees the buffer.
    fp <- FC.newForeignPtr p $ do
        c_secure_zero p (fromIntegral n)
        _ <- c_munlock p (fromIntegral n)
        free p
    pure (SecureBytes fp n)

-- | Wrap an existing 'ByteString' into a 'SecureBytes'.
--
-- Copies the bytes into a freshly allocated pinned buffer so that the
-- original 'ByteString' and the new 'SecureBytes' do not share memory.
fromByteString :: ByteString -> IO SecureBytes
fromByteString bs = do
    let len = BS.length bs
    sb <- newSecureBytes len
    BS.useAsCStringLen bs $ \(src, _) ->
        withForeignPtr (_sbPtr sb) $ \dst ->
            copyBytes dst (castPtr src) len
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
    withForeignPtr fp $ \p -> c_secure_zero p (fromIntegral len)

-- | Temporarily extract the key as a 'ByteString' and run an action.
--
-- __Scoping only, not cryptographic erasure.__  The extracted 'ByteString'
-- is a GC-heap copy that is NOT zeroed when the callback returns.  GHC's
-- copying collector may replicate it across generations.  This pattern
-- limits how long the reference lives, but does not guarantee erasure.
--
-- For zero-copy access, prefer 'withSecurePtr' with C FFI functions that
-- accept a raw @Ptr Word8@.
withSecureKey :: SecureBytes -> (ByteString -> IO a) -> IO a
withSecureKey sb action = do
    bs <- toByteString sb
    action bs

-- | Return the length of the buffer in bytes.
secureBytesLength :: SecureBytes -> Int
secureBytesLength = _sbLen
