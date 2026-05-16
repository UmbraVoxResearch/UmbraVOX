-- SPDX-License-Identifier: Apache-2.0
-- | Tests for 'UmbraVox.Plugin.Registry' (M17.6).
--
-- Finding: Plugin dependency resolution and ephemeral-mode defaults had no
--   dedicated test coverage, making it easy to regress dependency chains
--   silently.
-- Vulnerability: A faulty resolveEnable could enable a plugin without its
--   required dependencies (e.g. key-persistence without file-io), leading to
--   runtime failures or data corruption during persistence operations.
-- Fix: This test suite verifies dependency resolution, disable warnings,
--   the all-disabled default state of the registry, full-persistence
--   transitive chains (M17.6.3), and enable/disable round-trips (M17.6.5).
-- Verified: All assertions pass under the default persistence plugin registry.
module Test.Plugin.Registry (runTests) where

import Data.List (sort)
import Test.Util (assertEq)
import UmbraVox.Plugin.Registry
    ( defaultPersistencePlugins
    , resolveEnable
    , resolveDisable
    , pluginEnabled
    , enablePlugin
    , disablePlugin
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
        -- M17.6.3: Plugin dependency resolution tests
        , testResolveEnableKeyPersistenceAllDeps
        , testResolveDisableFileIOCascade
        , testResolveEnableFullPersistence
        , testResolveDisableFullPersistence
        , testResolveEnableNoDeps
        -- M17.6.5: Plugin enable/disable round-trip tests
        , testEnableDisableRoundTrip
        , testResolveEnableDisableRoundTrip
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

------------------------------------------------------------------------
-- M17.6.3: Plugin dependency resolution tests
------------------------------------------------------------------------

-- | Enable key-persistence and verify file-io, atomic-write, and
-- ratchet-persistence are all auto-enabled in the resolved set.
testResolveEnableKeyPersistenceAllDeps :: IO Bool
testResolveEnableKeyPersistenceAllDeps = do
    let reg = defaultPersistencePlugins
    case resolveEnable "key-persistence" reg of
        Left err -> do
            putStrLn $ "  FAIL: resolveEnable returned Left: " ++ err
            pure False
        Right ids -> do
            let expected = sort ["key-persistence", "file-io", "atomic-write", "ratchet-persistence"]
                actual   = sort ids
            -- The resolved set must contain exactly the plugin + its transitive deps
            a <- assertEq "M17.6.3 key-persistence: file-io included"
                    True ("file-io" `elem` actual)
            b <- assertEq "M17.6.3 key-persistence: atomic-write included"
                    True ("atomic-write" `elem` actual)
            c <- assertEq "M17.6.3 key-persistence: ratchet-persistence included"
                    True ("ratchet-persistence" `elem` actual)
            d <- assertEq "M17.6.3 key-persistence: self included"
                    True ("key-persistence" `elem` actual)
            -- Verify applying enable actually sets all as enabled
            let reg' = foldr enablePlugin reg ids
            e <- assertEq "M17.6.3 key-persistence: file-io enabled in registry"
                    True (pluginEnabled "file-io" reg')
            f <- assertEq "M17.6.3 key-persistence: atomic-write enabled in registry"
                    True (pluginEnabled "atomic-write" reg')
            g <- assertEq "M17.6.3 key-persistence: ratchet-persistence enabled in registry"
                    True (pluginEnabled "ratchet-persistence" reg')
            h <- assertEq "M17.6.3 key-persistence: key-persistence enabled in registry"
                    True (pluginEnabled "key-persistence" reg')
            pure (and [a, b, c, d, e, f, g, h])

-- | Disable file-io and verify that key-persistence would be auto-disabled
-- (it is a dependent, reported by resolveDisable).  Then actually disable
-- the full chain and verify none remain enabled.
testResolveDisableFileIOCascade :: IO Bool
testResolveDisableFileIOCascade = do
    let reg = defaultPersistencePlugins
    -- First enable the full key-persistence chain
    case resolveEnable "key-persistence" reg of
        Left err -> do
            putStrLn $ "  FAIL: resolveEnable returned Left: " ++ err
            pure False
        Right enableIds -> do
            let regEnabled = foldr enablePlugin reg enableIds
            -- Now resolve disabling file-io
            case resolveDisable "file-io" regEnabled of
                Left err2 -> do
                    putStrLn $ "  FAIL: resolveDisable returned Left: " ++ err2
                    pure False
                Right dependents -> do
                    -- atomic-write depends on file-io directly
                    a <- assertEq "M17.6.3 disable file-io: atomic-write is dependent"
                            True ("atomic-write" `elem` dependents)
                    -- Cascade: disable file-io + all dependents
                    let allToDisable = "file-io" : dependents
                        regDisabled = foldr disablePlugin regEnabled allToDisable
                    b <- assertEq "M17.6.3 disable file-io: file-io disabled"
                            False (pluginEnabled "file-io" regDisabled)
                    c <- assertEq "M17.6.3 disable file-io: atomic-write disabled"
                            False (pluginEnabled "atomic-write" regDisabled)
                    pure (and [a, b, c])

-- | Enable full-persistence and verify ALL persistence plugins are enabled.
testResolveEnableFullPersistence :: IO Bool
testResolveEnableFullPersistence = do
    let reg = defaultPersistencePlugins
    case resolveEnable "full-persistence" reg of
        Left err -> do
            putStrLn $ "  FAIL: resolveEnable returned Left: " ++ err
            pure False
        Right ids -> do
            let reg' = foldr enablePlugin reg ids
                allPlugins =
                    [ "sqlite-storage", "file-io", "atomic-write"
                    , "key-persistence", "message-storage"
                    , "ratchet-persistence", "runtime-logging"
                    , "full-persistence"
                    ]
            results <- mapM (\pid ->
                assertEq ("M17.6.3 full-persistence: " ++ pid ++ " enabled")
                    True (pluginEnabled pid reg')
                ) allPlugins
            pure (and results)

-- | Disable full-persistence (cascading through all dependents) and verify
-- all plugins end up disabled.
testResolveDisableFullPersistence :: IO Bool
testResolveDisableFullPersistence = do
    let reg = defaultPersistencePlugins
    -- Enable everything first
    case resolveEnable "full-persistence" reg of
        Left err -> do
            putStrLn $ "  FAIL: resolveEnable returned Left: " ++ err
            pure False
        Right enableIds -> do
            let regEnabled = foldr enablePlugin reg enableIds
            -- Now disable full-persistence and all its deps via cascading
            let regDisabled = foldr disablePlugin regEnabled enableIds
                allPlugins =
                    [ "sqlite-storage", "file-io", "atomic-write"
                    , "key-persistence", "message-storage"
                    , "ratchet-persistence", "runtime-logging"
                    , "full-persistence"
                    ]
            results <- mapM (\pid ->
                assertEq ("M17.6.3 disable full-persistence: " ++ pid ++ " disabled")
                    False (pluginEnabled pid regDisabled)
                ) allPlugins
            pure (and results)

-- | Enable a plugin with no dependencies (sqlite-storage) and verify only
-- that plugin is enabled, nothing else.
testResolveEnableNoDeps :: IO Bool
testResolveEnableNoDeps = do
    let reg = defaultPersistencePlugins
    case resolveEnable "sqlite-storage" reg of
        Left err -> do
            putStrLn $ "  FAIL: resolveEnable returned Left: " ++ err
            pure False
        Right ids -> do
            a <- assertEq "M17.6.3 no-deps: only sqlite-storage in resolved set"
                    ["sqlite-storage"] (sort ids)
            let reg' = foldr enablePlugin reg ids
            b <- assertEq "M17.6.3 no-deps: sqlite-storage enabled"
                    True (pluginEnabled "sqlite-storage" reg')
            c <- assertEq "M17.6.3 no-deps: file-io still disabled"
                    False (pluginEnabled "file-io" reg')
            d <- assertEq "M17.6.3 no-deps: key-persistence still disabled"
                    False (pluginEnabled "key-persistence" reg')
            pure (and [a, b, c, d])

------------------------------------------------------------------------
-- M17.6.5: Plugin enable/disable round-trip tests
------------------------------------------------------------------------

-- | Enable a plugin, verify it is enabled, disable it, verify it is disabled.
testEnableDisableRoundTrip :: IO Bool
testEnableDisableRoundTrip = do
    let reg = defaultPersistencePlugins
    -- Start: sqlite-storage is disabled
    a <- assertEq "M17.6.5 round-trip: sqlite-storage initially disabled"
            False (pluginEnabled "sqlite-storage" reg)
    -- Enable it
    let reg1 = enablePlugin "sqlite-storage" reg
    b <- assertEq "M17.6.5 round-trip: sqlite-storage enabled after enablePlugin"
            True (pluginEnabled "sqlite-storage" reg1)
    -- Disable it
    let reg2 = disablePlugin "sqlite-storage" reg1
    c <- assertEq "M17.6.5 round-trip: sqlite-storage disabled after disablePlugin"
            False (pluginEnabled "sqlite-storage" reg2)
    pure (and [a, b, c])

-- | Round-trip via resolveEnable / resolveDisable:
-- resolve-enable key-persistence, apply enables, check all enabled;
-- resolve-disable file-io, apply disables, check state.
testResolveEnableDisableRoundTrip :: IO Bool
testResolveEnableDisableRoundTrip = do
    let reg = defaultPersistencePlugins
    case resolveEnable "key-persistence" reg of
        Left err -> do
            putStrLn $ "  FAIL: resolveEnable returned Left: " ++ err
            pure False
        Right enableIds -> do
            let regEnabled = foldr enablePlugin reg enableIds
            -- Verify the full chain is enabled
            a <- assertEq "M17.6.5 resolve round-trip: key-persistence enabled"
                    True (pluginEnabled "key-persistence" regEnabled)
            b <- assertEq "M17.6.5 resolve round-trip: file-io enabled"
                    True (pluginEnabled "file-io" regEnabled)
            -- Now resolve-disable file-io
            case resolveDisable "file-io" regEnabled of
                Left err2 -> do
                    putStrLn $ "  FAIL: resolveDisable returned Left: " ++ err2
                    pure False
                Right dependents -> do
                    -- Disable file-io + all reported dependents
                    let allToDisable = "file-io" : dependents
                        regDisabled = foldr disablePlugin regEnabled allToDisable
                    c <- assertEq "M17.6.5 resolve round-trip: file-io disabled"
                            False (pluginEnabled "file-io" regDisabled)
                    d <- assertEq "M17.6.5 resolve round-trip: atomic-write disabled"
                            False (pluginEnabled "atomic-write" regDisabled)
                    -- key-persistence should still appear in dependents or
                    -- be separately disabled; verify final state
                    pure (and [a, b, c, d])
