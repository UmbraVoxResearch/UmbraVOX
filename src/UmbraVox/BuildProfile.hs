{-# LANGUAGE CPP #-}
-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.BuildProfile
    ( BuildPluginId(..)
    , BuildPluginTier(..)
    , BuildPluginCategory(..)
    , BuildPluginSource(..)
    , PluginHost(..)
    , PluginEntrypoint(..)
    , PluginLaunchSpec(..)
    , PluginLoadStatus(..)
    , PackagedPluginRuntime(..)
    , PluginManifest(..)
    , BuildPlugin(..)
    , buildChastityOnly
    , buildProfileName
    , buildPluginRegistry
    , extensionPluginRegistry
    , allKnownPlugins
    , enabledPlugins
    , disabledPlugins
    , packagedPlugins
    , pluginEnabled
    , pluginName
    , pluginDescription
    , pluginManifestPath
    , loadPluginManifestFields
    , loadPackagedPluginManifestFields
    , loadPluginManifest
    , loadPackagedPluginManifests
    , loadPackagedPluginCatalog
    , loadPackagedPluginRuntimeCatalog
    , pluginManifestApiSupported
    , pluginManifestHost
    , pluginManifestEntrypoint
    , pluginManifestLaunchSpec
    , pluginManifestLoadStatus
    , pluginLaunchSpecLabel
    , pluginLoadStatusLabel
    , pluginSourceLabel
    , pluginUnavailableStatus
    , buildAllowsIdentityPersistence
    , buildSupportsPersistentStorage
    , buildSupportsDiscovery
    , buildSupportsPeerExchange
    , buildSupportsRuntimeLogging
    , buildSupportsChatTransfer
    , buildSupportsConnectionModeSelection
    , unavailableInBuildStatus
    ) where

import Data.Char (toLower)
import Data.List (isPrefixOf, sortOn)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), isAbsolute, takeDirectory)

data BuildPluginId
    = PluginIdentityPersistence
    | PluginPersistentStorage
    | PluginDiscovery
    | PluginPeerExchange
    | PluginRuntimeLogging
    | PluginChatTransfer
    | PluginConnectionModeSelection
    | PluginGroupChat
    | PluginLocationSharing
    | PluginImageSharing
    | PluginFileSharing
    deriving stock (Eq, Ord, Show, Enum, Bounded)

data BuildPluginTier = PluginCore | PluginOptional | PluginPremiumReady
    deriving stock (Eq, Ord, Show)

data BuildPluginCategory = PluginSecurity | PluginStorage | PluginNetworking | PluginDiagnostics | PluginUX
    deriving stock (Eq, Ord, Show)

data BuildPluginSource = PluginSourceBuiltIn | PluginSourcePackaged | PluginSourcePremiumPackaged
    deriving stock (Eq, Ord, Show)

data PluginHost
    = PluginHostManifestOnly
    | PluginHostIPCStdIO
    | PluginHostExecutableDirect
    | PluginHostInProcess
    deriving stock (Eq, Show)

data PluginEntrypoint
    = PluginEntrypointManifestOnly
    | PluginEntrypointIPC FilePath
    | PluginEntrypointExecutable FilePath
    | PluginEntrypointModule String
    deriving stock (Eq, Show)

data PluginLaunchSpec
    = PluginLaunchManifestOnly
    | PluginLaunchIPCStdIO FilePath
    | PluginLaunchExecutableDirect FilePath
    | PluginLaunchInProcess String
    | PluginLaunchInvalid PluginHost PluginEntrypoint
    deriving stock (Eq, Show)

data PluginLoadStatus
    = PluginLoadManifestOnly
    | PluginLoadUnsupportedApi String
    | PluginLoadMissingArtifact FilePath
    | PluginLoadReady FilePath
    | PluginLoadModuleTarget String
    | PluginLoadInvalidContract
    deriving stock (Eq, Show)

data PackagedPluginRuntime = PackagedPluginRuntime
    { pprPlugin :: BuildPlugin
    , pprManifest :: PluginManifest
    , pprLaunchSpec :: PluginLaunchSpec
    , pprLoadStatus :: PluginLoadStatus
    } deriving stock (Eq, Show)

data BuildPlugin = BuildPlugin
    { bpId :: BuildPluginId
    , bpStableId :: String
    , bpName :: String
    , bpDescription :: String
    , bpCategory :: BuildPluginCategory
    , bpTier :: BuildPluginTier
    , bpSource :: BuildPluginSource
    , bpManifestPath :: Maybe FilePath
    , bpEnabled :: Bool
    } deriving stock (Eq, Show)

data PluginManifest = PluginManifest
    { pmStableId :: String
    , pmApiVersion :: String
    , pmDisplayName :: String
    , pmSourceTag :: String
    , pmTierTag :: String
    , pmCategoryTag :: String
    , pmStatusTag :: String
    , pmHostTag :: String
    , pmEntrypointTag :: String
    , pmEncryptionTag :: String
    , pmNotes :: String
    } deriving stock (Eq, Show)

buildChastityOnly :: Bool
#ifdef CHASTITY_BUILD
buildChastityOnly = True
#else
buildChastityOnly = False
#endif

buildProfileName :: String
buildProfileName
    | buildChastityOnly = "chastity-only"
    | otherwise = "standard"

allPluginIds :: [BuildPluginId]
allPluginIds = [minBound .. maxBound]

buildPluginRegistry :: [BuildPlugin]
buildPluginRegistry = filter ((== PluginSourceBuiltIn) . bpSource) allKnownPlugins

extensionPluginRegistry :: [BuildPlugin]
extensionPluginRegistry = filter ((/= PluginSourceBuiltIn) . bpSource) allKnownPlugins

allKnownPlugins :: [BuildPlugin]
allKnownPlugins = map pluginDescriptor allPluginIds

enabledPlugins :: [BuildPlugin]
enabledPlugins = filter bpEnabled buildPluginRegistry

disabledPlugins :: [BuildPlugin]
disabledPlugins = filter (not . bpEnabled) buildPluginRegistry

packagedPlugins :: [BuildPlugin]
packagedPlugins = extensionPluginRegistry

pluginEnabled :: BuildPluginId -> Bool
pluginEnabled = bpEnabled . pluginDescriptor

pluginName :: BuildPluginId -> String
pluginName = bpName . pluginDescriptor

pluginDescription :: BuildPluginId -> String
pluginDescription = bpDescription . pluginDescriptor

pluginManifestPath :: BuildPluginId -> Maybe FilePath
pluginManifestPath = bpManifestPath . pluginDescriptor

pluginDescriptor :: BuildPluginId -> BuildPlugin
pluginDescriptor pid =
    case pid of
        PluginIdentityPersistence ->
            plugin pid Nothing "Identity Persistence"
                "Persist the local long-term identity across restarts."
                PluginSecurity PluginCore PluginSourceBuiltIn Nothing (not buildChastityOnly)
        PluginPersistentStorage ->
            plugin pid Nothing "Persistent Storage"
                "Store message history and related local state on disk."
                PluginStorage PluginOptional PluginSourceBuiltIn Nothing (not buildChastityOnly)
        PluginDiscovery ->
            plugin pid Nothing "Discovery"
                "Enable mDNS/LAN discovery and related browse UI."
                PluginNetworking PluginOptional PluginSourceBuiltIn Nothing (not buildChastityOnly)
        PluginPeerExchange ->
            plugin pid Nothing "Peer Exchange"
                "Advertise and consume peer referrals from connected peers."
                PluginNetworking PluginOptional PluginSourceBuiltIn Nothing (not buildChastityOnly)
        PluginRuntimeLogging ->
            plugin pid Nothing "Runtime Logging"
                "Write troubleshooting and operational metadata to local log files."
                PluginDiagnostics PluginOptional PluginSourceBuiltIn Nothing (not buildChastityOnly)
        PluginChatTransfer ->
            plugin pid Nothing "Chat Export/Import"
                "Encrypt chat histories for import and export via local files."
                PluginUX PluginPremiumReady PluginSourceBuiltIn Nothing (not buildChastityOnly)
        PluginConnectionModeSelection ->
            plugin pid Nothing "Connection Mode Selection"
                "Allow switching between Swing/Promiscuous/Selective/Chaste/Chastity at runtime."
                PluginSecurity PluginOptional PluginSourceBuiltIn Nothing (not buildChastityOnly)
        PluginGroupChat ->
            plugin pid (Just "group-chat") "Group Chat"
                "Future packaged module for sealed-sender group messaging."
                PluginUX PluginPremiumReady PluginSourcePackaged
                (Just "plugins/group-chat/manifest.uvx") False
        PluginLocationSharing ->
            plugin pid (Just "location-sharing") "Location Sharing"
                "Future packaged module for end-to-end encrypted live and pinned location exchange."
                PluginNetworking PluginPremiumReady PluginSourcePremiumPackaged
                (Just "plugins/location-sharing/manifest.uvx") False
        PluginImageSharing ->
            plugin pid (Just "image-sharing") "Image Sharing"
                "Future packaged module for end-to-end encrypted image transfer."
                PluginUX PluginPremiumReady PluginSourcePremiumPackaged
                (Just "plugins/image-sharing/manifest.uvx") False
        PluginFileSharing ->
            plugin pid (Just "file-sharing") "File Sharing"
                "Future packaged module for end-to-end encrypted file transfer."
                PluginUX PluginPremiumReady PluginSourcePremiumPackaged
                (Just "plugins/file-sharing/manifest.uvx") False
  where
    plugin pluginId stableId name desc category tier source manifestPath enabled =
        BuildPlugin
            { bpId = pluginId
            , bpStableId = normalizeTag (maybe (show pluginId) id stableId)
            , bpName = name
            , bpDescription = desc
            , bpCategory = category
            , bpTier = tier
            , bpSource = source
            , bpManifestPath = manifestPath
            , bpEnabled = enabled
            }

loadPluginManifestFields :: BuildPluginId -> IO [(String, String)]
loadPluginManifestFields pid =
    case pluginManifestPath pid of
        Nothing -> pure []
        Just path -> loadManifestFile path

loadPackagedPluginManifestFields :: IO [(BuildPluginId, [(String, String)])]
loadPackagedPluginManifestFields =
    map toFields <$> loadPackagedPluginCatalog
  where
    toFields (plugin, manifest) = (bpId plugin, manifestFields manifest)

loadPluginManifest :: BuildPluginId -> IO (Maybe PluginManifest)
loadPluginManifest pid = do
    fields <- loadPluginManifestFields pid
    pure (pluginManifestFromFields fields)

loadPackagedPluginManifests :: IO [(BuildPluginId, PluginManifest)]
loadPackagedPluginManifests =
    map toPair <$> loadPackagedPluginCatalog
  where
    toPair (plugin, manifest) = (bpId plugin, manifest)

loadPackagedPluginCatalog :: IO [(BuildPlugin, PluginManifest)]
loadPackagedPluginCatalog = do
    manifestPaths <- discoverPackagedPluginManifestPaths
    pairs <- mapM loadOne manifestPaths
    pure (sortOn (bpId . fst) (collectLoaded pairs))
  where
    loadOne path = do
        manifest <- loadManifestFile path
        pure (buildCatalogEntry path =<< pluginManifestFromFields manifest)

    collectLoaded [] = []
    collectLoaded (entry:rest) =
        case entry of
            Just pair -> pair : collectLoaded rest
            Nothing -> collectLoaded rest

loadPackagedPluginRuntimeCatalog :: IO [PackagedPluginRuntime]
loadPackagedPluginRuntimeCatalog = do
    catalog <- loadPackagedPluginCatalog
    mapM addLoadStatus catalog
  where
    addLoadStatus (plugin, manifest) =
        case bpManifestPath plugin of
            Nothing ->
                pure PackagedPluginRuntime
                    { pprPlugin = plugin
                    , pprManifest = manifest
                    , pprLaunchSpec = PluginLaunchManifestOnly
                    , pprLoadStatus = PluginLoadManifestOnly
                    }
            Just manifestPath -> do
                let launchSpec = pluginManifestLaunchSpec manifestPath manifest
                loadStatus <- pluginManifestLoadStatus manifestPath manifest
                pure PackagedPluginRuntime
                    { pprPlugin = plugin
                    , pprManifest = manifest
                    , pprLaunchSpec = launchSpec
                    , pprLoadStatus = loadStatus
                    }

discoverPackagedPluginManifestPaths :: IO [FilePath]
discoverPackagedPluginManifestPaths = do
    exists <- doesDirectoryExist "plugins"
    if not exists
        then pure []
        else do
            entries <- listDirectory "plugins"
            collectManifestPaths entries
  where
    collectManifestPaths [] = pure []
    collectManifestPaths (entry:rest) = do
        let path = "plugins" </> entry </> "manifest.uvx"
        present <- doesFileExist path
        remaining <- collectManifestPaths rest
        if present
            then pure (path : remaining)
            else pure remaining

buildCatalogEntry :: FilePath -> PluginManifest -> Maybe (BuildPlugin, PluginManifest)
buildCatalogEntry path manifest = do
    pid <- pluginIdFromStableId (pmStableId manifest)
    pure
        ( BuildPlugin
            { bpId = pid
            , bpStableId = normalizeTag (pmStableId manifest)
            , bpName = pmDisplayName manifest
            , bpDescription = pmNotes manifest
            , bpCategory = parseCategoryTag (pmCategoryTag manifest)
            , bpTier = parseTierTag (pmTierTag manifest)
            , bpSource = parseSourceTag (pmSourceTag manifest)
            , bpManifestPath = Just path
            , bpEnabled = False
            }
        , manifest
        )

pluginIdFromStableId :: String -> Maybe BuildPluginId
pluginIdFromStableId stableId =
    findMatching packagedPlugins
  where
    normalizedStableId = normalizeTag stableId

    findMatching [] = Nothing
    findMatching (plugin:rest)
        | pluginStableId (bpId plugin) == normalizedStableId = Just (bpId plugin)
        | otherwise = findMatching rest

pluginStableId :: BuildPluginId -> String
pluginStableId = bpStableId . pluginDescriptor

parseSourceTag :: String -> BuildPluginSource
parseSourceTag tag =
    case normalizeTag tag of
        "premium-packaged" -> PluginSourcePremiumPackaged
        "built-in" -> PluginSourceBuiltIn
        _ -> PluginSourcePackaged

parseTierTag :: String -> BuildPluginTier
parseTierTag tag =
    case normalizeTag tag of
        "core" -> PluginCore
        "optional" -> PluginOptional
        _ -> PluginPremiumReady

parseCategoryTag :: String -> BuildPluginCategory
parseCategoryTag tag =
    case normalizeTag tag of
        "security" -> PluginSecurity
        "storage" -> PluginStorage
        "networking" -> PluginNetworking
        "diagnostics" -> PluginDiagnostics
        _ -> PluginUX

normalizeTag :: String -> String
normalizeTag = map toLower

manifestFields :: PluginManifest -> [(String, String)]
manifestFields manifest =
    [ ("id", pmStableId manifest)
    , ("api", pmApiVersion manifest)
    , ("name", pmDisplayName manifest)
    , ("source", pmSourceTag manifest)
    , ("tier", pmTierTag manifest)
    , ("category", pmCategoryTag manifest)
    , ("status", pmStatusTag manifest)
    , ("host", pmHostTag manifest)
    , ("entrypoint", pmEntrypointTag manifest)
    , ("encryption", pmEncryptionTag manifest)
    , ("notes", pmNotes manifest)
    ]

loadManifestFile :: FilePath -> IO [(String, String)]
loadManifestFile path = do
    exists <- doesFileExist path
    if not exists
        then pure []
        else do
            raw <- readFile path
            pure (parseManifestFields raw)

parseManifestFields :: String -> [(String, String)]
parseManifestFields =
    mapMaybeField . lines
  where
    mapMaybeField [] = []
    mapMaybeField (line:rest) =
        case parseManifestLine line of
            Just field -> field : mapMaybeField rest
            Nothing -> mapMaybeField rest

parseManifestLine :: String -> Maybe (String, String)
parseManifestLine raw
    | null trimmed = Nothing
    | "#" `isPrefixOf` trimmed = Nothing
    | otherwise =
        case break (== '=') trimmed of
            ([], _) -> Nothing
            (_, []) -> Nothing
            (key, '=':value) -> Just (trim key, trim value)
            _ -> Nothing
  where
    trimmed = trim raw
    trim = dropWhile (== ' ') . dropTrailingSpaces
    dropTrailingSpaces = reverse . dropWhile (== ' ') . reverse

pluginManifestFromFields :: [(String, String)] -> Maybe PluginManifest
pluginManifestFromFields fields = do
    stableId <- lookupField "id"
    apiVersion <- lookupField "api"
    displayName <- lookupField "name"
    sourceTag <- lookupField "source"
    tierTag <- lookupField "tier"
    categoryTag <- lookupField "category"
    statusTag <- lookupField "status"
    hostTag <- lookupField "host"
    entrypointTag <- lookupField "entrypoint"
    encryptionTag <- lookupField "encryption"
    notes <- lookupField "notes"
    pure PluginManifest
        { pmStableId = stableId
        , pmApiVersion = apiVersion
        , pmDisplayName = displayName
        , pmSourceTag = sourceTag
        , pmTierTag = tierTag
        , pmCategoryTag = categoryTag
        , pmStatusTag = statusTag
        , pmHostTag = hostTag
        , pmEntrypointTag = entrypointTag
        , pmEncryptionTag = encryptionTag
        , pmNotes = notes
        }
  where
    lookupField key = lookup key fields

pluginManifestHost :: PluginManifest -> PluginHost
pluginManifestHost manifest =
    case normalizeTag (pmHostTag manifest) of
        "ipc-stdio" -> PluginHostIPCStdIO
        "exec-direct" -> PluginHostExecutableDirect
        "in-process" -> PluginHostInProcess
        _ -> PluginHostManifestOnly

pluginManifestApiSupported :: PluginManifest -> Bool
pluginManifestApiSupported manifest =
    normalizeTag (pmApiVersion manifest) == "uvx-plugin-v1"

pluginManifestEntrypoint :: PluginManifest -> PluginEntrypoint
pluginManifestEntrypoint manifest =
    case pmEntrypointTag manifest of
        "manifest-only" -> PluginEntrypointManifestOnly
        raw
            | Just path <- stripPrefixTag "ipc:" raw ->
                PluginEntrypointIPC path
            | Just path <- stripPrefixTag "exec:" raw ->
                PluginEntrypointExecutable path
            | Just modName <- stripPrefixTag "module:" raw ->
                PluginEntrypointModule modName
            | otherwise ->
                PluginEntrypointManifestOnly
  where
    stripPrefixTag prefix raw =
        if prefix `isPrefixOf` raw
            then Just (drop (length prefix) raw)
            else Nothing

pluginManifestLaunchSpec :: FilePath -> PluginManifest -> PluginLaunchSpec
pluginManifestLaunchSpec manifestPath manifest =
    case (pluginManifestHost manifest, pluginManifestEntrypoint manifest) of
        (PluginHostManifestOnly, _) ->
            PluginLaunchManifestOnly
        (PluginHostIPCStdIO, PluginEntrypointIPC artifactPath) ->
            PluginLaunchIPCStdIO (resolveArtifactPath manifestPath artifactPath)
        (PluginHostIPCStdIO, PluginEntrypointExecutable artifactPath) ->
            PluginLaunchIPCStdIO (resolveArtifactPath manifestPath artifactPath)
        (PluginHostExecutableDirect, PluginEntrypointExecutable artifactPath) ->
            PluginLaunchExecutableDirect (resolveArtifactPath manifestPath artifactPath)
        (PluginHostInProcess, PluginEntrypointModule modName) ->
            PluginLaunchInProcess modName
        (host, entrypoint) ->
            PluginLaunchInvalid host entrypoint

pluginManifestLoadStatus :: FilePath -> PluginManifest -> IO PluginLoadStatus
pluginManifestLoadStatus manifestPath manifest =
    if not (pluginManifestApiSupported manifest)
        then pure (PluginLoadUnsupportedApi (pmApiVersion manifest))
        else case pluginManifestLaunchSpec manifestPath manifest of
        PluginLaunchManifestOnly ->
            pure PluginLoadManifestOnly
        PluginLaunchInProcess modName ->
            pure (PluginLoadModuleTarget modName)
        PluginLaunchIPCStdIO resolvedPath ->
            resolveArtifactStatus resolvedPath
        PluginLaunchExecutableDirect resolvedPath ->
            resolveArtifactStatus resolvedPath
        PluginLaunchInvalid _ _ ->
            pure PluginLoadInvalidContract
  where
    resolveArtifactStatus resolvedPath = do
        exists <- doesFileExist resolvedPath
        pure $
            if exists
                then PluginLoadReady resolvedPath
                else PluginLoadMissingArtifact resolvedPath

pluginLaunchSpecLabel :: PluginLaunchSpec -> String
pluginLaunchSpecLabel launchSpec =
    case launchSpec of
        PluginLaunchManifestOnly -> "manifest-only"
        PluginLaunchIPCStdIO _ -> "ipc-stdio"
        PluginLaunchExecutableDirect _ -> "exec-direct"
        PluginLaunchInProcess _ -> "in-process"
        PluginLaunchInvalid _ _ -> "invalid-contract"

pluginLoadStatusLabel :: PluginLoadStatus -> String
pluginLoadStatusLabel loadStatus =
    case loadStatus of
        PluginLoadManifestOnly -> "manifest-only"
        PluginLoadUnsupportedApi _ -> "unsupported-api"
        PluginLoadMissingArtifact _ -> "artifact-missing"
        PluginLoadReady _ -> "load-ready"
        PluginLoadModuleTarget _ -> "module-target"
        PluginLoadInvalidContract -> "invalid-contract"

resolveArtifactPath :: FilePath -> FilePath -> FilePath
resolveArtifactPath manifestPath artifactPath
    | isAbsolute artifactPath = artifactPath
    | otherwise = takeDirectory manifestPath </> artifactPath

pluginSourceLabel :: BuildPluginSource -> String
pluginSourceLabel PluginSourceBuiltIn = "built-in"
pluginSourceLabel PluginSourcePackaged = "packaged"
pluginSourceLabel PluginSourcePremiumPackaged = "premium-packaged"

pluginUnavailableStatus :: BuildPluginId -> String
pluginUnavailableStatus pid = unavailableInBuildStatus (pluginName pid)

buildAllowsIdentityPersistence :: Bool
buildAllowsIdentityPersistence = pluginEnabled PluginIdentityPersistence

buildSupportsPersistentStorage :: Bool
buildSupportsPersistentStorage = pluginEnabled PluginPersistentStorage

buildSupportsDiscovery :: Bool
buildSupportsDiscovery = pluginEnabled PluginDiscovery

buildSupportsPeerExchange :: Bool
buildSupportsPeerExchange = pluginEnabled PluginPeerExchange

buildSupportsRuntimeLogging :: Bool
buildSupportsRuntimeLogging = pluginEnabled PluginRuntimeLogging

buildSupportsChatTransfer :: Bool
buildSupportsChatTransfer = pluginEnabled PluginChatTransfer

buildSupportsConnectionModeSelection :: Bool
buildSupportsConnectionModeSelection = pluginEnabled PluginConnectionModeSelection

unavailableInBuildStatus :: String -> String
unavailableInBuildStatus feature
    | buildChastityOnly = feature ++ " unavailable in chastity build"
    | otherwise = feature ++ " unavailable in this build"
