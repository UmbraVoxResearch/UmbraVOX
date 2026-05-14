-- SPDX-License-Identifier: Apache-2.0
-- | Tests for 'UmbraVox.Plugin.Registry' (M17.6).
--
-- Finding: Plugin dependency resolution and ephemeral-mode defaults had no
--   dedicated test coverage, making it easy to regress dependency chains
--   silently.
-- Vulnerability: A faulty resolveEnable could enable a plugin without its
--   required dependencies (e.g. key-persistence without file-io), leading to
--   runtime failures or data corruption during persistence operations.
-- Fix: This test suite verifies dependency resolution, disable warnings, and
--   the all-disabled default state of the registry.
-- Verified: All assertions pass under the default persistence plugin registry.
module Test.Plugin.Registry (runTests) where

import Data.List (sort)
import Test.Util (assertEq)
import UmbraVox.Plugin.Registry
    ( defaultPersistencePlugins
    , resolveEnable
    , resolveDisable
    , pluginEnabled
    )

runTests :: IO Bool
runTests = do
    putStrLn "[Plugin.Registry] Running plugin registry tests..."
    results <- sequence
        [ testDefaultAllDisabled
        , testResolveEnableKeyPersistence
        , testResolveDisableFileIO
        , testResolveEnableUnknown
        , testResolveDisableUnknown
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[Plugin.Registry] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

-- | All plugins in the default registry must be disabled.
testDefaultAllDisabled :: IO Bool
testDefaultAllDisabled = do
    let reg = defaultPersistencePlugins
        ids = [ "sqlite-storage", "file-io", "atomic-write", "key-persistence"
              , "message-storage", "ratchet-persistence", "runtime-logging"
              , "full-persistence"
              ]
        allDisabled = all (\pid -> not (pluginEnabled pid reg)) ids
    assertEq "defaultRegistry: all plugins disabled" True allDisabled

-- | Enabling key-persistence must auto-include file-io, atomic-write,
-- and ratchet-persistence in the resolved set.
testResolveEnableKeyPersistence :: IO Bool
testResolveEnableKeyPersistence = do
    let reg = defaultPersistencePlugins
    case resolveEnable "key-persistence" reg of
        Left err -> do
            putStrLn $ "  FAIL: resolveEnable returned Left: " ++ err
            pure False
        Right ids -> do
            let sorted = sort ids
            a <- assertEq "resolveEnable key-persistence includes key-persistence"
                    True ("key-persistence" `elem` ids)
            b <- assertEq "resolveEnable key-persistence includes file-io"
                    True ("file-io" `elem` ids)
            c <- assertEq "resolveEnable key-persistence includes atomic-write"
                    True ("atomic-write" `elem` ids)
            d <- assertEq "resolveEnable key-persistence includes ratchet-persistence"
                    True ("ratchet-persistence" `elem` ids)
            _ <- sorted `seq` pure ()
            pure (a && b && c && d)

-- | Disabling file-io must report dependents that would break:
-- atomic-write, key-persistence, ratchet-persistence (via atomic-write),
-- runtime-logging, and full-persistence all depend transitively on file-io.
-- resolveDisable returns direct dependents only; we verify at least
-- key-persistence or atomic-write appear.
testResolveDisableFileIO :: IO Bool
testResolveDisableFileIO = do
    let reg = defaultPersistencePlugins
    case resolveDisable "file-io" reg of
        Left err -> do
            putStrLn $ "  FAIL: resolveDisable returned Left: " ++ err
            pure False
        Right deps -> do
            -- Direct dependents of file-io: atomic-write, key-persistence, runtime-logging, full-persistence
            let hasDirect = "atomic-write" `elem` deps || "key-persistence" `elem` deps
            assertEq "resolveDisable file-io warns about dependents" True hasDirect

-- | resolveEnable with an unknown plugin id returns Left.
testResolveEnableUnknown :: IO Bool
testResolveEnableUnknown = do
    let reg = defaultPersistencePlugins
    case resolveEnable "nonexistent-plugin" reg of
        Left _  -> assertEq "resolveEnable unknown: returns Left" True True
        Right _ -> assertEq "resolveEnable unknown: should not succeed" True False

-- | resolveDisable with an unknown plugin id returns Left.
testResolveDisableUnknown :: IO Bool
testResolveDisableUnknown = do
    let reg = defaultPersistencePlugins
    case resolveDisable "nonexistent-plugin" reg of
        Left _  -> assertEq "resolveDisable unknown: returns Left" True True
        Right _ -> assertEq "resolveDisable unknown: should not succeed" True False
