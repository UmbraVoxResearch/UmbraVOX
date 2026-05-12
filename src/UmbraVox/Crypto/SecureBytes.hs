-- SPDX-License-Identifier: Apache-2.0
-- | Pinned, zeroing byte buffer for sensitive key material.
--
-- 'SecureBytes' wraps a pinned 'ForeignPtr' 'Word8' whose associated
-- finalizer overwrites every byte with zero before the memory is freed.
-- This is a best-effort defence against key material persisting on the heap
-- after a value is no longer referenced.
--
-- == Secure zeroing via C FFI
--
-- The zeroing finalizer calls 'c_secure_zero', a C helper that writes zeros
-- through a @volatile@ pointer.  A compiler is not permitted to remove writes
-- through a @volatile@ pointer even when it proves the memory is dead after
-- the call, so this provides the same dead-store-elimination resistance as
-- @explicit_bzero(3)@ (Linux) or @SecureZeroMemory@ (Windows) on platforms
-- that do not expose those extensions.  See @csrc/secure_zero.c@.
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
import Foreign.C.Types (CSize(..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)

------------------------------------------------------------------------
-- C FFI
------------------------------------------------------------------------

-- | C helper that zeros @len@ bytes at @ptr@ via a volatile write loop.
-- The volatile writes cannot be optimised away by the C compiler, giving
-- the same dead-store-elimination resistance as @explicit_bzero(3)@.
-- Implemented in @csrc/secure_zero.c@.
foreign import ccall "umbravox_secure_zero"
    c_secure_zero :: Ptr Word8 -> CSize -> IO ()

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
    -- Zero the freshly-allocated buffer via the C volatile write loop.
    c_secure_zero p (fromIntegral n)
    -- Attach a finalizer that zeros the buffer again and then frees it.
    -- Foreign.Concurrent.newForeignPtr takes a plain IO () action, unlike the
    -- Foreign.ForeignPtr variant which requires a C-level FunPtr.
    fp <- FC.newForeignPtr p (c_secure_zero p (fromIntegral n) >> free p)
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

-- | Return the length of the buffer in bytes.
secureBytesLength :: SecureBytes -> Int
secureBytesLength = _sbLen

