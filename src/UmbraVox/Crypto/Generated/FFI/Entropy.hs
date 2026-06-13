-- SPDX-License-Identifier: Apache-2.0
-- | Auto-generated FFI bridge bindings by CryptoGen. DO NOT EDIT.
{-# LANGUAGE ForeignFunctionInterface #-}
module UmbraVox.Crypto.Generated.FFI.Entropy
    ( ffiLinked
    , entropyRead
    , entropyInit
    , entropyForked
    ) where

import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- Bridge: calls the OS CSPRNG via csrc/entropy/bridge_entropy.c
-- (Linux getrandom / BSD/illumos/macOS getentropy / Windows
-- BCryptGenRandom / /dev/urandom fallback).
--
-- Finding:     M40.7 / M40.35 / M40.36 — the CSPRNG read its key and
--              nonce from a non-blocking /dev/urandom handle and
--              detected fork() only lazily via getProcessID.
-- Vulnerability: early-boot low-entropy reads, and a fork/PID-reuse
--              window allowing (key,nonce) reuse -> keystream recovery.
-- Fix:         entropyRead obtains pool-ready (blocking) OS entropy;
--              entropyInit registers a pthread_atfork child handler and
--              entropyForked reports+clears the fork flag so the CSPRNG
--              can reseed immediately in a forked child.
-- Verified:    entropyRead returns Just bytes of the exact requested
--              length only when the C bridge reports success (rc == 0).
foreign import ccall "entropy_link_probe" c_entropy_link_probe :: IO CInt

foreign import ccall safe "umbravox_entropy"
    c_umbravox_entropy :: Ptr Word8 -> Word32 -> IO CInt

foreign import ccall safe "umbravox_entropy_init"
    c_umbravox_entropy_init :: IO ()

foreign import ccall unsafe "umbravox_entropy_forked"
    c_umbravox_entropy_forked :: IO CInt

ffiLinked :: IO Bool
ffiLinked = (/= 0) <$> c_entropy_link_probe

-- | Register the pthread_atfork child handler. Idempotent; call once
-- from application startup (and safe to call again).
entropyInit :: IO ()
entropyInit = c_umbravox_entropy_init

-- | True if a fork() has occurred since the last check. Reads AND
-- clears the C-side atomic flag, so a subsequent call returns False
-- unless another fork intervened.
entropyForked :: IO Bool
entropyForked = (/= 0) <$> c_umbravox_entropy_forked

-- | Read @n@ bytes from the OS CSPRNG with pool-ready (blocking)
-- semantics. Returns @Just@ exactly @n@ bytes on success, @Nothing@
-- on OS failure or when @n@ exceeds the C 'Word32' length bound.
entropyRead :: Int -> IO (Maybe ByteString)
entropyRead n
    | n <= 0 = pure (Just BS.empty)
    | toInteger n > toInteger (maxBound :: Word32) = pure Nothing
    | otherwise =
        allocaBytes n $ \buf -> do
            rc <- c_umbravox_entropy buf (fromIntegral n)
            if rc == 0
                then Just <$> BS.packCStringLen (castPtr buf, n)
                else pure Nothing
