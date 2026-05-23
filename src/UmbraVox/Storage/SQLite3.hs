-- SPDX-License-Identifier: Apache-2.0
-- | Minimal SQLite3 FFI bindings for prepared statements.
-- Zero external dependencies — direct FFI to libsqlite3.
--
-- This module eliminates SQL injection by construction: all user data
-- is bound via parameterised prepared statements, never interpolated
-- into SQL strings.
module UmbraVox.Storage.SQLite3
    ( Database
    , Statement
    , SQLiteError(..)
    , open
    , close
    , prepare
    , bindText
    , bindInt
    , bindBlob
    , bindNull
    , step
    , stepRow
    , StepResult(..)
    , columnText
    , columnInt
    , columnBlob
    , columnCount
    , finalize
    , exec
    , reset
    , withStatement
    ) where

import Prelude hiding (error)

import Control.Exception (Exception, bracket, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt(..), CLong(..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Storable (peek, poke)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

sqlite_OK :: CInt
sqlite_OK = 0

sqlite_ROW :: CInt
sqlite_ROW = 100

sqlite_DONE :: CInt
sqlite_DONE = 101

------------------------------------------------------------------------
-- Opaque types
------------------------------------------------------------------------

-- | Opaque handle to an open SQLite3 database connection.
newtype Database = Database (Ptr ())

-- | Opaque handle to a prepared statement.
newtype Statement = Statement (Ptr ())

------------------------------------------------------------------------
-- Error type
------------------------------------------------------------------------

-- | Error raised when a SQLite3 operation fails.
data SQLiteError = SQLiteError
    { sqliteErrorCode    :: !Int
    , sqliteErrorMessage :: !String
    } deriving stock (Show)

instance Exception SQLiteError

------------------------------------------------------------------------
-- FFI imports
------------------------------------------------------------------------

foreign import ccall unsafe "sqlite3_open"
    c_sqlite3_open :: CString -> Ptr (Ptr ()) -> IO CInt

foreign import ccall unsafe "sqlite3_close"
    c_sqlite3_close :: Ptr () -> IO CInt

foreign import ccall unsafe "sqlite3_prepare_v2"
    c_sqlite3_prepare_v2 :: Ptr ()       -- ^ db
                         -> CString      -- ^ sql
                         -> CInt         -- ^ nByte (-1 for NUL-terminated)
                         -> Ptr (Ptr ()) -- ^ out: ppStmt
                         -> Ptr (Ptr ()) -- ^ out: pzTail (unused)
                         -> IO CInt

foreign import ccall unsafe "sqlite3_finalize"
    c_sqlite3_finalize :: Ptr () -> IO CInt

foreign import ccall unsafe "sqlite3_step"
    c_sqlite3_step :: Ptr () -> IO CInt

foreign import ccall unsafe "sqlite3_reset"
    c_sqlite3_reset :: Ptr () -> IO CInt

foreign import ccall unsafe "sqlite3_bind_text"
    c_sqlite3_bind_text :: Ptr ()  -- ^ stmt
                        -> CInt    -- ^ index (1-based)
                        -> CString -- ^ value
                        -> CInt    -- ^ length (-1 for NUL-terminated)
                        -> Ptr ()  -- ^ destructor (SQLITE_TRANSIENT)
                        -> IO CInt

foreign import ccall unsafe "sqlite3_bind_int64"
    c_sqlite3_bind_int64 :: Ptr () -> CInt -> CLong -> IO CInt

foreign import ccall unsafe "sqlite3_bind_blob"
    c_sqlite3_bind_blob :: Ptr ()  -- ^ stmt
                        -> CInt    -- ^ index
                        -> Ptr ()  -- ^ value
                        -> CInt    -- ^ length
                        -> Ptr ()  -- ^ destructor
                        -> IO CInt

foreign import ccall unsafe "sqlite3_bind_null"
    c_sqlite3_bind_null :: Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "sqlite3_column_text"
    c_sqlite3_column_text :: Ptr () -> CInt -> IO CString

foreign import ccall unsafe "sqlite3_column_int64"
    c_sqlite3_column_int64 :: Ptr () -> CInt -> IO CLong

foreign import ccall unsafe "sqlite3_column_blob"
    c_sqlite3_column_blob :: Ptr () -> CInt -> IO (Ptr ())

foreign import ccall unsafe "sqlite3_column_bytes"
    c_sqlite3_column_bytes :: Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "sqlite3_column_count"
    c_sqlite3_column_count :: Ptr () -> IO CInt

foreign import ccall unsafe "sqlite3_exec"
    c_sqlite3_exec :: Ptr ()      -- ^ db
                   -> CString     -- ^ sql
                   -> Ptr ()      -- ^ callback (NULL)
                   -> Ptr ()      -- ^ callback arg (NULL)
                   -> Ptr CString -- ^ errmsg out
                   -> IO CInt

foreign import ccall unsafe "sqlite3_errmsg"
    c_sqlite3_errmsg :: Ptr () -> IO CString

-- SQLITE_TRANSIENT is the C macro ((sqlite3_destructor_type)-1), i.e.
-- the pointer value (void*)(-1).  It cannot be imported directly via
-- Haskell FFI, so csrc/sqlite3_shim.c exposes it as a global symbol.
foreign import ccall unsafe "&umbravox_sqlite_transient"
    c_sqlite_transient_addr :: Ptr (Ptr ())

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

-- | Read the SQLITE_TRANSIENT pointer value from the C shim.
sqliteTransient :: IO (Ptr ())
sqliteTransient = peek c_sqlite_transient_addr

-- | Open a SQLite3 database. Creates the file if it does not exist.
open :: FilePath -> IO Database
open path = alloca $ \ppDb -> do
    rc <- withCString path $ \cPath ->
        c_sqlite3_open cPath ppDb
    dbPtr <- peek ppDb
    if rc /= sqlite_OK
        then do
            msg <- if dbPtr /= nullPtr
                   then c_sqlite3_errmsg dbPtr >>= peekCString
                   else pure "sqlite3_open failed"
            _ <- if dbPtr /= nullPtr then c_sqlite3_close dbPtr else pure 0
            throwIO (SQLiteError (fromIntegral rc) msg)
        else do
            -- Enable WAL mode and busy timeout for concurrent access
            exec (Database dbPtr) "PRAGMA journal_mode=WAL"
            exec (Database dbPtr) "PRAGMA busy_timeout=5000"
            pure (Database dbPtr)

-- | Close a database connection.
close :: Database -> IO ()
close (Database dbPtr) = do
    rc <- c_sqlite3_close dbPtr
    if rc /= sqlite_OK
        then do
            msg <- c_sqlite3_errmsg dbPtr >>= peekCString
            throwIO (SQLiteError (fromIntegral rc) msg)
        else pure ()

-- | Prepare a SQL statement.
prepare :: Database -> String -> IO Statement
prepare (Database dbPtr) sql = alloca $ \ppStmt -> do
    poke ppStmt nullPtr
    rc <- withCString sql $ \cSql ->
        c_sqlite3_prepare_v2 dbPtr cSql (-1) ppStmt nullPtr
    if rc /= sqlite_OK
        then do
            msg <- c_sqlite3_errmsg dbPtr >>= peekCString
            throwIO (SQLiteError (fromIntegral rc)
                        ("prepare: " ++ msg ++ " [SQL: " ++ sql ++ "]"))
        else do
            stmtPtr <- peek ppStmt
            if stmtPtr == nullPtr
                then throwIO (SQLiteError 1
                        ("prepare: empty statement [SQL: " ++ sql ++ "]"))
                else pure (Statement stmtPtr)

-- | Bind a text value to a parameter (1-based index).
bindText :: Statement -> Int -> String -> IO ()
bindText (Statement stmtPtr) idx val = do
    transient <- sqliteTransient
    withCString val $ \cVal -> do
        rc <- c_sqlite3_bind_text stmtPtr (fromIntegral idx)
                                  cVal (-1) transient
        checkBind rc

-- | Bind an integer value to a parameter (1-based index).
bindInt :: Statement -> Int -> Int -> IO ()
bindInt (Statement stmtPtr) idx val = do
    rc <- c_sqlite3_bind_int64 stmtPtr (fromIntegral idx) (fromIntegral val)
    checkBind rc

-- | Bind a blob value to a parameter (1-based index).
bindBlob :: Statement -> Int -> ByteString -> IO ()
bindBlob (Statement stmtPtr) idx val = do
    transient <- sqliteTransient
    BU.unsafeUseAsCStringLen val $ \(ptr, len) -> do
        rc <- c_sqlite3_bind_blob stmtPtr (fromIntegral idx) (castPtr ptr)
                                  (fromIntegral len) transient
        checkBind rc

-- | Bind NULL to a parameter (1-based index).
bindNull :: Statement -> Int -> IO ()
bindNull (Statement stmtPtr) idx = do
    rc <- c_sqlite3_bind_null stmtPtr (fromIntegral idx)
    checkBind rc

-- | Result of stepping a statement.
data StepResult = Row | Done
    deriving stock (Eq, Show)

-- | Step a statement. Returns 'Row' if a row is available, 'Done' if finished.
step :: Statement -> IO StepResult
step (Statement stmtPtr) = do
    rc <- c_sqlite3_step stmtPtr
    case rc of
        sqlite_ROW  -> pure Row
        sqlite_DONE -> pure Done
        _           -> throwIO (SQLiteError (fromIntegral rc) "step failed")

-- | Step expecting a row. Returns 'True' if a row is available.
stepRow :: Statement -> IO Bool
stepRow stmt = do
    r <- step stmt
    pure (r == Row)

-- | Read a text column (0-based index).
columnText :: Statement -> Int -> IO String
columnText (Statement stmtPtr) idx = do
    cstr <- c_sqlite3_column_text stmtPtr (fromIntegral idx)
    if cstr == nullPtr
        then pure ""
        else peekCString cstr

-- | Read an integer column (0-based index).
columnInt :: Statement -> Int -> IO Int
columnInt (Statement stmtPtr) idx = do
    val <- c_sqlite3_column_int64 stmtPtr (fromIntegral idx)
    pure (fromIntegral val)

-- | Read a blob column (0-based index).
columnBlob :: Statement -> Int -> IO ByteString
columnBlob (Statement stmtPtr) idx = do
    ptr <- c_sqlite3_column_blob stmtPtr (fromIntegral idx)
    len <- c_sqlite3_column_bytes stmtPtr (fromIntegral idx)
    if ptr == nullPtr || len <= 0
        then pure BS.empty
        else BS.packCStringLen (castPtr ptr, fromIntegral len)

-- | Get the number of columns in the result set.
columnCount :: Statement -> IO Int
columnCount (Statement stmtPtr) = do
    n <- c_sqlite3_column_count stmtPtr
    pure (fromIntegral n)

-- | Finalize (destroy) a prepared statement.
finalize :: Statement -> IO ()
finalize (Statement stmtPtr) =
    c_sqlite3_finalize stmtPtr >> pure ()

-- | Reset a prepared statement for re-execution.
reset :: Statement -> IO ()
reset (Statement stmtPtr) =
    c_sqlite3_reset stmtPtr >> pure ()

-- | Execute a SQL string directly (for DDL, PRAGMA, etc.).
-- Not suitable for user data — use prepared statements instead.
exec :: Database -> String -> IO ()
exec (Database dbPtr) sql = alloca $ \ppErr -> do
    poke ppErr nullPtr
    rc <- withCString sql $ \cSql ->
        c_sqlite3_exec dbPtr cSql nullPtr nullPtr ppErr
    if rc /= sqlite_OK
        then do
            errPtr <- peek ppErr
            msg <- if errPtr /= nullPtr
                   then do
                       m <- peekCString errPtr
                       free errPtr
                       pure m
                   else pure "exec failed"
            throwIO (SQLiteError (fromIntegral rc) msg)
        else pure ()

-- | Prepare a statement, run an action, and finalize (bracket pattern).
-- The statement is always finalized, even if the action throws.
withStatement :: Database -> String -> (Statement -> IO a) -> IO a
withStatement db sql action =
    bracket (prepare db sql) finalize action

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

checkBind :: CInt -> IO ()
checkBind rc
    | rc == sqlite_OK = pure ()
    | otherwise       = throwIO (SQLiteError (fromIntegral rc) "bind failed")
