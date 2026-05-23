-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
-- | Secure credential passing to child processes via file descriptor inheritance.
--
-- == Security model
--
-- Bridge plugins need credentials (e.g. session keys) to operate, but passing
-- them over the IPC line protocol would leave credential bytes sitting in pipe
-- buffers visible via @\/proc\/pid\/fd@.  Instead, the parent process writes
-- credentials into an anonymous file (created via 'mkstemp' and immediately
-- unlinked), then passes the open file descriptor number to the child as a
-- command-line argument or environment variable.  The child reads the
-- credential bytes from the inherited fd and closes it.
--
-- Because the file is unlinked before the child is spawned, no path on the
-- filesystem ever points to the credential data.  The fd is only accessible
-- to the parent and any children that inherit it.
--
-- == Portable approach
--
-- On Linux, @memfd_create(2)@ would be ideal (purely in-memory, never touches
-- a filesystem).  For portability across illumos, the BSDs, and macOS, this
-- module uses the POSIX 'mkstemp' + immediate 'removeLink' pattern instead,
-- which works on every POSIX system and provides the same security properties
-- once the unlink completes (the data lives only in the page cache / buffer
-- cache and is reclaimed when all fds are closed).
--
-- == Limitations
--
-- * The credential bytes transit through a kernel-managed file page.  On swap-
--   enabled systems the page could theoretically be written to swap before the
--   fd is closed.  Locking the pages via @mlock@ is not possible on an fd we
--   do not @mmap@.  For long-lived credentials, consider 'SecureBytes' in the
--   parent process and pass only short-lived session tokens via this mechanism.
-- * The caller is responsible for ensuring the fd is not leaked to unrelated
--   child processes (set @CLOEXEC@ on fds that should not be inherited, which
--   is the default for Haskell-opened fds; the fd returned here intentionally
--   has @CLOEXEC@ cleared so that it survives @exec@).
module UmbraVox.Crypto.SecureFd
    ( -- * Secure credential passing
      createCredentialFd
    , readCredentialFd
    , closeCredentialFd
#ifndef mingw32_HOST_OS
    , CredentialFd  -- re-export the Fd type alias on POSIX
#endif
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.IO (hFlush, hSetBinaryMode, hSeek, SeekMode(AbsoluteSeek), hClose)

#ifdef mingw32_HOST_OS
import System.IO (Handle, openBinaryTempFile, hGetContents)
import System.Directory (removeFile, getTemporaryDirectory)
#else
import System.Posix.IO (handleToFd, fdToHandle, closeFd, setFdOption, FdOption(CloseOnExec))
import System.Posix.Temp (mkstemp)
import System.Posix.Types (Fd)
import System.Posix.Files (removeLink)
#endif

#ifdef mingw32_HOST_OS
-- | On Windows, credential passing uses a 'Handle' instead of a raw fd.
type CredentialFd = Handle

-- | Create a temporary file containing the given credential bytes.
--
-- On Windows, returns a 'Handle' to the temp file (seeked back to the start).
-- The backing file is removed from the directory after creation; the handle
-- keeps the data accessible until closed.
createCredentialFd :: ByteString -> IO CredentialFd
createCredentialFd cred = do
    tmpDir <- getTemporaryDirectory
    (path, handle) <- openBinaryTempFile tmpDir "uvx-cred-.tmp"
    removeFile path
    BS.hPut handle cred
    hFlush handle
    hSeek handle AbsoluteSeek 0
    return handle

-- | Read all credential bytes from the given handle, then close it.
readCredentialFd :: CredentialFd -> IO ByteString
readCredentialFd handle = do
    hSetBinaryMode handle True
    result <- BS.hGetContents handle  -- reads all + closes handle
    return result

-- | Close a credential handle, rendering it unreadable.
closeCredentialFd :: CredentialFd -> IO ()
closeCredentialFd = hClose

#else
-- | On POSIX, credential passing uses a raw file descriptor.
type CredentialFd = Fd

-- | Create an anonymous file descriptor containing the given credential bytes.
--
-- The returned 'Fd' is suitable for inheritance by a child process: it has
-- @CLOEXEC@ cleared so it survives @execve(2)@.  The caller should pass the
-- fd number (obtained via the 'Fd' constructor) to the child, e.g. as an
-- environment variable @UMBRAVOX_CRED_FD@.
--
-- The backing file is unlinked immediately after creation, so no filesystem
-- path ever points to the credential data.
createCredentialFd :: ByteString -> IO CredentialFd
createCredentialFd cred = do
    (path, handle) <- mkstemp "/tmp/uvx-cred-XXXXXX"
    removeLink path
    hSetBinaryMode handle True
    BS.hPut handle cred
    hFlush handle
    hSeek handle AbsoluteSeek 0
    fd <- handleToFd handle  -- converts Handle to Fd, closing the Handle
    -- Clear CLOEXEC so the fd is inherited across exec.
    setFdOption fd CloseOnExec False
    return fd

-- | Read all credential bytes from the given fd, then close it.
--
-- This is intended for use in the child process after @exec@.  The fd is
-- closed after reading to avoid leaking it to any further children.
readCredentialFd :: CredentialFd -> IO ByteString
readCredentialFd fd = do
    handle <- fdToHandle fd  -- converts Fd to Handle, takes ownership
    hSetBinaryMode handle True
    result <- BS.hGetContents handle  -- reads all + closes handle
    return result

-- | Close a credential fd, rendering it unreadable.
--
-- Unlike 'readCredentialFd', this does not read the contents first.  Use this
-- in the parent process after the child has been spawned and no longer needs
-- the fd to remain open in the parent.
closeCredentialFd :: CredentialFd -> IO ()
closeCredentialFd = closeFd
#endif

