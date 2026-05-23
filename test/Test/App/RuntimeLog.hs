-- SPDX-License-Identifier: Apache-2.0
module Test.App.RuntimeLog (runTests) where

import Data.List (sort)
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.Posix.Files
    ( getFileStatus
    , fileMode
    , ownerReadMode
    , ownerWriteMode
    , ownerExecuteMode
    , groupReadMode
    , groupWriteMode
    , groupExecuteMode
    , otherReadMode
    , otherWriteMode
    , otherExecuteMode
    , unionFileModes
    , intersectFileModes
    )
import UmbraVox.App.RuntimeLog (redactedFieldKeys, ensureLogPermissions)
import Test.Util (assertEq, getProjectTmpDir)

runTests :: IO Bool
runTests = do
    putStrLn "  Runtime Logging Tests"
    putStrLn "  ====================="
    results <- sequence
        [ testRedactionListComplete
        , testRedactionListSorted
        , testEnsurePermissions
        , testSensitiveFieldsCovered
        ]
    pure (and results)

-- | All required sensitive fields must be present in the redaction list.
testRedactionListComplete :: IO Bool
testRedactionListComplete = do
    let required = [ "content", "host", "key", "password", "path", "peer"
                   , "port", "secret", "sender", "session_id", "token" ]
    let missing = filter (`notElem` redactedFieldKeys) required
    assertEq "all sensitive fields redacted" [] missing

-- | The redaction list must be alphabetically sorted for maintainability.
testRedactionListSorted :: IO Bool
testRedactionListSorted = do
    let sorted = redactedFieldKeys == sort redactedFieldKeys
    assertEq "redaction list sorted" True sorted

-- | ensureLogPermissions must set the file to 0600 (owner read/write only).
testEnsurePermissions :: IO Bool
testEnsurePermissions = do
    tmp <- getProjectTmpDir
    let path = tmp </> "umbravox-test-log-perms.log"
    writeFile path "test\n"
    ensureLogPermissions path
    stat <- getFileStatus path
    let allPerms = foldr1 unionFileModes
            [ ownerReadMode, ownerWriteMode, ownerExecuteMode
            , groupReadMode, groupWriteMode, groupExecuteMode
            , otherReadMode, otherWriteMode, otherExecuteMode
            ]
        permBits = fileMode stat `intersectFileModes` allPerms
        expected = ownerReadMode `unionFileModes` ownerWriteMode
    removeFile path
    assertEq "log permissions 0600" expected permBits

-- | Critical sensitive field names must never be accidentally removed.
testSensitiveFieldsCovered :: IO Bool
testSensitiveFieldsCovered = do
    let mustHave = ["content", "password", "secret", "key", "token"]
    let allPresent = all (`elem` redactedFieldKeys) mustHave
    assertEq "critical sensitive fields present" True allPresent
