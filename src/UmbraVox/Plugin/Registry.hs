-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.Plugin.Registry
    ( defaultPersistencePlugins
    , resolveEnable
    , resolveDisable
    , pluginEnabled
    , enablePlugin
    , disablePlugin
    ) where

import qualified Data.Map.Strict as Map

import UmbraVox.Plugin.Types
    ( PluginDef(..)
    , PluginType(..)
    , PluginRegistry
    , registerPlugin
    , lookupPlugin
    , emptyRegistry
    )

-- | The default persistence plugin registry (all disabled).
defaultPersistencePlugins :: PluginRegistry
defaultPersistencePlugins =
    foldr registerPlugin emptyRegistry
        [ sqliteStorage
        , fileIO
        , atomicWrite
        , keyPersistence
        , messageStorage
        , ratchetPersistence
        , runtimeLogging
        , fullPersistence
        ]

sqliteStorage :: PluginDef
sqliteStorage = PluginDef
    { pdId           = "sqlite-storage"
    , pdType         = FunctionPlugin
    , pdDependencies = []
    , pdMessage      = "Disabled: ephemeral mode"
    , pdEnabled      = False
    }

fileIO :: PluginDef
fileIO = PluginDef
    { pdId           = "file-io"
    , pdType         = FunctionPlugin
    , pdDependencies = []
    , pdMessage      = "Disabled: ephemeral mode"
    , pdEnabled      = False
    }

atomicWrite :: PluginDef
atomicWrite = PluginDef
    { pdId           = "atomic-write"
    , pdType         = FunctionPlugin
    , pdDependencies = ["file-io"]
    , pdMessage      = "Disabled: ephemeral mode"
    , pdEnabled      = False
    }

-- | key-persistence requires both file-io, atomic-write, and ratchet-persistence (M17.1.4).
keyPersistence :: PluginDef
keyPersistence = PluginDef
    { pdId           = "key-persistence"
    , pdType         = TransactionPlugin
    , pdDependencies = ["file-io", "atomic-write", "ratchet-persistence"]
    , pdMessage      = "Disabled: ephemeral mode"
    , pdEnabled      = False
    }

messageStorage :: PluginDef
messageStorage = PluginDef
    { pdId           = "message-storage"
    , pdType         = TransactionPlugin
    , pdDependencies = ["sqlite-storage"]
    , pdMessage      = "Disabled: ephemeral mode"
    , pdEnabled      = False
    }

ratchetPersistence :: PluginDef
ratchetPersistence = PluginDef
    { pdId           = "ratchet-persistence"
    , pdType         = TransactionPlugin
    , pdDependencies = ["atomic-write"]
    , pdMessage      = "Disabled: ephemeral mode"
    , pdEnabled      = False
    }

runtimeLogging :: PluginDef
runtimeLogging = PluginDef
    { pdId           = "runtime-logging"
    , pdType         = TransactionPlugin
    , pdDependencies = ["file-io"]
    , pdMessage      = "Disabled: ephemeral mode"
    , pdEnabled      = False
    }

fullPersistence :: PluginDef
fullPersistence = PluginDef
    { pdId           = "full-persistence"
    , pdType         = MetaPlugin
    , pdDependencies =
        [ "sqlite-storage"
        , "file-io"
        , "atomic-write"
        , "key-persistence"
        , "message-storage"
        , "ratchet-persistence"
        , "runtime-logging"
        ]
    , pdMessage      = "Disabled: ephemeral mode"
    , pdEnabled      = False
    }

-- | Check whether a plugin is enabled in a registry.
pluginEnabled :: String -> PluginRegistry -> Bool
pluginEnabled pid reg =
    maybe False pdEnabled (lookupPlugin pid reg)

-- | Enable a plugin by id (does not resolve dependencies).
enablePlugin :: String -> PluginRegistry -> PluginRegistry
enablePlugin pid reg =
    case lookupPlugin pid reg of
        Nothing -> reg
        Just pd -> Map.insert pid (pd { pdEnabled = True }) reg

-- | Disable a plugin by id (does not cascade).
disablePlugin :: String -> PluginRegistry -> PluginRegistry
disablePlugin pid reg =
    case lookupPlugin pid reg of
        Nothing -> reg
        Just pd -> Map.insert pid (pd { pdEnabled = False }) reg

-- | Resolve dependencies for enabling a plugin.
-- Returns Right [ids to enable] (including the target) or Left error message.
resolveEnable :: String -> PluginRegistry -> Either String [String]
resolveEnable pid reg =
    case lookupPlugin pid reg of
        Nothing -> Left ("Unknown plugin: " ++ pid)
        Just pd -> case concatDeps (pdDependencies pd) of
            Left err -> Left err
            Right ds -> Right (pid : ds)
  where
    concatDeps [] = Right []
    concatDeps (dep:rest) =
        case resolveEnable dep reg of
            Left err -> Left ("dependency " ++ dep ++ ": " ++ err)
            Right ds -> case concatDeps rest of
                Left err  -> Left err
                Right ds' -> Right (ds ++ ds')

-- | Resolve dependents for disabling a plugin.
-- Returns Right [ids that depend on the target] or Left error message.
resolveDisable :: String -> PluginRegistry -> Either String [String]
resolveDisable pid reg =
    case lookupPlugin pid reg of
        Nothing -> Left ("Unknown plugin: " ++ pid)
        Just _  ->
            let dependents = Map.keys (Map.filter (elem pid . pdDependencies) reg)
            in Right dependents
