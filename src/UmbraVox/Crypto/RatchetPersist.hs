-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "M15.1" #-} Ratchet counter persistence
--
-- Provides atomic counter persistence so that a crash after a failed
-- 'ratchetEncrypt' cannot cause GCM nonce reuse.  The contract is:
--
--   1. @persistRatchetCounter path (rsSendN st + 1)@ is called BEFORE
--      'ratchetEncrypt'.
--   2. On restart, @loadRatchetCounter@ returns the pre-incremented
--      counter.  The caller uses it to skip forward to the safe minimum
--      send counter before resuming encryption.
--
-- Atomicity is achieved via write-to-temp-file + @fileSynchronise@ + @rename@
-- (atomic on POSIX).  The counter is stored as a strict 4-byte
-- big-endian value so it is unambiguous and independent of host byte
-- order.
module UmbraVox.Crypto.RatchetPersist
    ( persistRatchetCounter
    , loadRatchetCounter
    , withPersistentEncrypt
    ) where

import Control.Exception (IOException, try)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)
import System.Directory (renameFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, hFlush, openTempFile)
import System.Posix.IO (handleToFd)
import System.Posix.Unistd (fileSynchronise)

import UmbraVox.Crypto.Signal.DoubleRatchet
    ( RatchetError(..)
    , RatchetHeader
    , RatchetState(..)
    , ratchetEncrypt
    )

------------------------------------------------------------------------
-- Encoding helpers
------------------------------------------------------------------------

-- | Encode a Word32 as 4-byte big-endian.
encodeWord32BE :: Word32 -> ByteString
encodeWord32BE w = BS.pack
    [ fromIntegral (w `shiftR` 24 .&. 0xff)
    , fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR`  8 .&. 0xff)
    , fromIntegral (w             .&. 0xff)
    ]

-- | Decode a 4-byte big-endian ByteString to Word32.
-- Returns Nothing if the input is not exactly 4 bytes.
decodeWord32BE :: ByteString -> Maybe Word32
decodeWord32BE bs
    | BS.length bs /= 4 = Nothing
    | otherwise =
        let b0 = fromIntegral (BS.index bs 0) :: Word32
            b1 = fromIntegral (BS.index bs 1) :: Word32
            b2 = fromIntegral (BS.index bs 2) :: Word32
            b3 = fromIntegral (BS.index bs 3) :: Word32
        in Just (b0 * 0x1000000 + b1 * 0x10000 + b2 * 0x100 + b3)

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Atomically persist @counter@ to @path@.
--
-- Algorithm:
--
--   1. Write the 4-byte big-endian counter to a temp file in the same
--      directory as @path@ (same filesystem, required for atomic rename).
--   2. @hFlush@ + @fileSynchronise@ (fsync) the temp file to flush to durable
--      storage before the rename.
--   3. @hClose@ the temp file handle.
--   4. @renameFile@ the temp file over @path@ (atomic on POSIX when
--      source and destination are on the same filesystem).
--
-- Throws 'IOException' on any failure.
persistRatchetCounter :: FilePath -> Word32 -> IO ()
persistRatchetCounter path counter = do
    let dir = takeDirectory path
    (tmpPath, h) <- openTempFile dir "ratchet-counter.tmp"
    -- Write, flush into the OS buffer
    BS.hPut h (encodeWord32BE counter)
    hFlush h
    -- fsync: ensure bytes reach durable storage before rename
    fd <- handleToFd h
    fileSynchronise fd
    -- handleToFd transfers ownership; the handle is now closed by the
    -- OS-level dup2 semantics.  We close the Haskell handle as well to
    -- be safe, ignoring "already closed" errors.
    _ <- try (hClose h) :: IO (Either IOException ())
    -- Atomic rename
    renameFile tmpPath path

-- | Load the persisted counter from @path@.
-- Returns @Nothing@ if the file does not exist or cannot be parsed.
loadRatchetCounter :: FilePath -> IO (Maybe Word32)
loadRatchetCounter path = do
    res <- try (BS.readFile path) :: IO (Either IOException ByteString)
    case res of
        Left _   -> pure Nothing
        Right bs -> pure (decodeWord32BE bs)

-- | Persist the pre-incremented send counter, then call 'ratchetEncrypt'.
--
-- If persistence fails the function returns
-- @Left (PersistenceError msg)@ and 'ratchetEncrypt' is never called,
-- so no GCM nonce is consumed and no nonce reuse can occur on crash.
--
-- On success the caller receives the updated 'RatchetState' together
-- with the header, ciphertext, and GCM tag, identical to calling
-- 'ratchetEncrypt' directly.
--
-- When @ephemeral@ is 'True' the counter write is skipped and
-- 'ratchetEncrypt' is called directly (no disk I/O).
withPersistentEncrypt
    :: Bool       -- ^ ephemeral: skip persistence when True
    -> FilePath
    -> RatchetState
    -> ByteString
    -> IO (Either RatchetError (RatchetState, RatchetHeader, ByteString, ByteString))
withPersistentEncrypt ephemeral path st plaintext
    | ephemeral = ratchetEncrypt st plaintext
    | otherwise = do
        -- Persist the counter that will be used AFTER this encryption.
        -- rsSendN st is the counter for the CURRENT message; ratchetEncrypt
        -- increments it to (rsSendN st + 1) in the returned state.
        -- We persist (rsSendN st + 1) so that on crash the receiver knows
        -- the next safe minimum counter.
        let nextCounter = rsSendN st + 1
        persistResult <- try (persistRatchetCounter path nextCounter)
                        :: IO (Either IOException ())
        case persistResult of
            Left ioErr -> pure (Left (PersistenceError (show ioErr)))
            Right ()   -> ratchetEncrypt st plaintext
