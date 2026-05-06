-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.Network.ProviderCatalog
    ( TransportProviderId(..)
    , ProviderClass(..)
    , ProviderCapability(..)
    , ProviderHost(..)
    , ProviderEntrypoint(..)
    , ProviderLaunchSpec(..)
    , ProviderLoadStatus(..)
    , ProviderManifest(..)
    , TransportProvider(..)
    , CachedTransportProvider(..)
    , transportProviderRegistry
    , loadTransportProviderManifestFields
    , loadTransportProviderManifest
    , loadTransportProviderManifests
    , loadTransportProviderCatalog
    , loadTransportProviderRuntimeCatalog
    , providerManifestApiSupported
    , providerManifestInherits
    , providerManifestCapabilities
    , providerManifestLaunchSpec
    , providerManifestLoadStatus
    , providerIdLabel
    , providerEndpointSchema
    , renderProviderEndpoint
    , providerClassLabel
    , providerCapabilityLabel
    , providerLaunchSpecLabel
    , providerLoadStatusLabel
    ) where

import Data.Char (toLower)
import Data.List (isPrefixOf, sortOn)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), isAbsolute, takeDirectory)

data TransportProviderId
    = ProviderTCP
    | ProviderUDP
    | ProviderTor
    | ProviderWireGuard
    | ProviderIRC
    | ProviderAIM
    | ProviderXMPP
    | ProviderMastodon
    | ProviderFacebook
    | ProviderInstagram
    | ProviderWhatsApp
    | ProviderSignal
    | ProviderSignalServer
    deriving stock (Eq, Ord, Show, Enum, Bounded)

data ProviderClass
    = ProviderDirectCarrier
    | ProviderOverlayCarrier
    | ProviderOpenBridge
    | ProviderClosedBridge
    deriving stock (Eq, Ord, Show)

data ProviderCapability
    = ProviderCapConnect
    | ProviderCapListen
    | ProviderCapDiscovery
    | ProviderCapGroupChat
    | ProviderCapMedia
    | ProviderCapBridge
    | ProviderCapTunnel
    deriving stock (Eq, Ord, Show)

data ProviderHost
    = ProviderHostManifestOnly
    | ProviderHostIPCStdIO
    | ProviderHostExecutableDirect
    | ProviderHostInProcess
    deriving stock (Eq, Show)

data ProviderEntrypoint
    = ProviderEntrypointManifestOnly
    | ProviderEntrypointIPC FilePath
    | ProviderEntrypointExecutable FilePath
    | ProviderEntrypointModule String
    deriving stock (Eq, Show)

data ProviderLaunchSpec
    = ProviderLaunchManifestOnly
    | ProviderLaunchIPCStdIO FilePath
    | ProviderLaunchExecutableDirect FilePath
    | ProviderLaunchInProcess String
    | ProviderLaunchInvalid ProviderHost ProviderEntrypoint
    deriving stock (Eq, Show)

data ProviderLoadStatus
    = ProviderLoadManifestOnly
    | ProviderLoadUnsupportedApi String
    | ProviderLoadMissingArtifact FilePath
    | ProviderLoadReady FilePath
    | ProviderLoadModuleTarget String
    | ProviderLoadInvalidContract
    deriving stock (Eq, Show)

data ProviderManifest = ProviderManifest
    { pmfStableId :: String
    , pmfApiVersion :: String
    , pmfDisplayName :: String
    , pmfClassTag :: String
    , pmfStatusTag :: String
    , pmfEndpointTag :: String
    , pmfInheritsTag :: String
    , pmfCapabilitiesTag :: String
    , pmfHostTag :: String
    , pmfEntrypointTag :: String
    , pmfEncryptionTag :: String
    , pmfNotes :: String
    } deriving stock (Eq, Show)

data TransportProvider = TransportProvider
    { tpId :: TransportProviderId
    , tpStableId :: String
    , tpName :: String
    , tpDescription :: String
    , tpClass :: ProviderClass
    , tpManifestPath :: FilePath
    } deriving stock (Eq, Show)

data CachedTransportProvider = CachedTransportProvider
    { ctpProvider :: TransportProvider
    , ctpManifest :: ProviderManifest
    , ctpLaunchSpec :: ProviderLaunchSpec
    , ctpLoadStatus :: ProviderLoadStatus
    , ctpInherits :: [TransportProviderId]
    , ctpCapabilities :: [ProviderCapability]
    } deriving stock (Eq, Show)

transportProviderRegistry :: [TransportProvider]
transportProviderRegistry = map providerDescriptor [minBound .. maxBound]

providerDescriptor :: TransportProviderId -> TransportProvider
providerDescriptor pid =
    case pid of
        ProviderTCP ->
            provider pid "tcp" "TCP"
                "Built-in direct stream carrier used by the active MVP."
                ProviderDirectCarrier
        ProviderUDP ->
            provider pid "udp" "UDP"
                "Datagram carrier stub reserved for future provider-backed sessions."
                ProviderDirectCarrier
        ProviderTor ->
            provider pid "tor" "Tor"
                "Planned onion-routing carrier wrapper below the secure session core."
                ProviderOverlayCarrier
        ProviderWireGuard ->
            provider pid "wireguard" "WireGuard"
                "Planned WireGuard-backed carrier wrapper below the secure session core."
                ProviderOverlayCarrier
        ProviderIRC ->
            provider pid "irc" "IRC"
                "Planned bridge provider adapting chat flows onto IRC-style endpoints."
                ProviderOpenBridge
        ProviderAIM ->
            provider pid "aim" "AIM"
                "Planned legacy closed-service bridge stub."
                ProviderClosedBridge
        ProviderXMPP ->
            provider pid "xmpp" "XMPP"
                "Planned federated bridge provider with typed registry metadata only."
                ProviderOpenBridge
        ProviderMastodon ->
            provider pid "mastodon" "Mastodon"
                "Planned federated social bridge provider stub."
                ProviderOpenBridge
        ProviderFacebook ->
            provider pid "facebook" "Facebook"
                "Planned closed-platform bridge provider stub."
                ProviderClosedBridge
        ProviderInstagram ->
            provider pid "instagram" "Instagram"
                "Planned closed-platform bridge provider inheriting Facebook bridge traits."
                ProviderClosedBridge
        ProviderWhatsApp ->
            provider pid "whatsapp" "WhatsApp"
                "Planned closed-platform bridge provider stub."
                ProviderClosedBridge
        ProviderSignal ->
            provider pid "signal" "Signal"
                "Planned Signal bridge provider layered over a Signal Server adapter."
                ProviderClosedBridge
        ProviderSignalServer ->
            provider pid "signal-server" "Signal Server"
                "Planned Signal Server bridge substrate for service-backed messaging."
                ProviderClosedBridge
  where
    provider providerId stableId name desc providerClass =
        TransportProvider
            { tpId = providerId
            , tpStableId = stableId
            , tpName = name
            , tpDescription = desc
            , tpClass = providerClass
            , tpManifestPath = "providers" </> stableId </> "manifest.uvx"
            }

loadTransportProviderManifestFields :: TransportProviderId -> IO [(String, String)]
loadTransportProviderManifestFields = loadManifestFile . tpManifestPath . providerDescriptor

loadTransportProviderManifest :: TransportProviderId -> IO (Maybe ProviderManifest)
loadTransportProviderManifest pid =
    providerManifestFromFields <$> loadTransportProviderManifestFields pid

loadTransportProviderManifests :: IO [(TransportProviderId, ProviderManifest)]
loadTransportProviderManifests =
    map toPair <$> loadTransportProviderCatalog
  where
    toPair (provider, manifest) = (tpId provider, manifest)

loadTransportProviderCatalog :: IO [(TransportProvider, ProviderManifest)]
loadTransportProviderCatalog = do
    manifestPaths <- discoverTransportProviderManifestPaths
    pairs <- mapM loadOne manifestPaths
    pure (sortOn (tpId . fst) (collectLoaded pairs))
  where
    loadOne path = do
        manifest <- loadManifestFile path
        pure (buildCatalogEntry path =<< providerManifestFromFields manifest)

    collectLoaded [] = []
    collectLoaded (entry:rest) =
        case entry of
            Just pair -> pair : collectLoaded rest
            Nothing -> collectLoaded rest

loadTransportProviderRuntimeCatalog :: IO [CachedTransportProvider]
loadTransportProviderRuntimeCatalog = do
    catalog <- loadTransportProviderCatalog
    mapM addRuntime catalog
  where
    addRuntime (provider, manifest) = do
        let launchSpec = providerManifestLaunchSpec (tpManifestPath provider) manifest
        loadStatus <- providerManifestLoadStatus (tpManifestPath provider) manifest
        pure CachedTransportProvider
            { ctpProvider = provider
            , ctpManifest = manifest
            , ctpLaunchSpec = launchSpec
            , ctpLoadStatus = loadStatus
            , ctpInherits = resolveProviderInherits manifest
            , ctpCapabilities = providerManifestCapabilities manifest
            }

discoverTransportProviderManifestPaths :: IO [FilePath]
discoverTransportProviderManifestPaths = do
    exists <- doesDirectoryExist "providers"
    if not exists
        then pure []
        else do
            entries <- listDirectory "providers"
            collect entries
  where
    collect [] = pure []
    collect (entry:rest) = do
        let path = "providers" </> entry </> "manifest.uvx"
        present <- doesFileExist path
        remaining <- collect rest
        if present
            then pure (path : remaining)
            else pure remaining

buildCatalogEntry :: FilePath -> ProviderManifest -> Maybe (TransportProvider, ProviderManifest)
buildCatalogEntry path manifest = do
    pid <- providerIdFromStableId (pmfStableId manifest)
    pure
        ( (providerDescriptor pid)
            { tpStableId = normalizeTag (pmfStableId manifest)
            , tpName = pmfDisplayName manifest
            , tpDescription = pmfNotes manifest
            , tpClass = parseClassTag (pmfClassTag manifest)
            , tpManifestPath = path
            }
        , manifest
        )

providerIdFromStableId :: String -> Maybe TransportProviderId
providerIdFromStableId stableId =
    findMatching transportProviderRegistry
  where
    normalizedStableId = normalizeTag stableId

    findMatching [] = Nothing
    findMatching (provider:rest)
        | tpStableId provider == normalizedStableId = Just (tpId provider)
        | otherwise = findMatching rest

providerManifestApiSupported :: ProviderManifest -> Bool
providerManifestApiSupported manifest =
    normalizeTag (pmfApiVersion manifest) == "uvx-provider-v1"

providerManifestInherits :: ProviderManifest -> [String]
providerManifestInherits =
    splitCommaTags . pmfInheritsTag

resolveProviderInherits :: ProviderManifest -> [TransportProviderId]
resolveProviderInherits manifest =
    resolve (providerManifestInherits manifest)
  where
    resolve [] = []
    resolve (stableId:rest) =
        case providerIdFromStableId stableId of
            Just providerId -> providerId : resolve rest
            Nothing -> resolve rest

providerManifestCapabilities :: ProviderManifest -> [ProviderCapability]
providerManifestCapabilities =
    mapMaybeCapability . splitCommaTags . pmfCapabilitiesTag
  where
    mapMaybeCapability [] = []
    mapMaybeCapability (raw:rest) =
        case parseCapabilityTag raw of
            Just capability -> capability : mapMaybeCapability rest
            Nothing -> mapMaybeCapability rest

providerManifestHost :: ProviderManifest -> ProviderHost
providerManifestHost manifest =
    case normalizeTag (pmfHostTag manifest) of
        "ipc-stdio" -> ProviderHostIPCStdIO
        "exec-direct" -> ProviderHostExecutableDirect
        "in-process" -> ProviderHostInProcess
        _ -> ProviderHostManifestOnly

providerManifestEntrypoint :: ProviderManifest -> ProviderEntrypoint
providerManifestEntrypoint manifest =
    case pmfEntrypointTag manifest of
        "manifest-only" -> ProviderEntrypointManifestOnly
        raw
            | Just path <- stripPrefixTag "ipc:" raw ->
                ProviderEntrypointIPC path
            | Just path <- stripPrefixTag "exec:" raw ->
                ProviderEntrypointExecutable path
            | Just modName <- stripPrefixTag "module:" raw ->
                ProviderEntrypointModule modName
            | otherwise ->
                ProviderEntrypointManifestOnly
  where
    stripPrefixTag prefix raw =
        if prefix `isPrefixOf` raw
            then Just (drop (length prefix) raw)
            else Nothing

providerManifestLaunchSpec :: FilePath -> ProviderManifest -> ProviderLaunchSpec
providerManifestLaunchSpec manifestPath manifest =
    case (providerManifestHost manifest, providerManifestEntrypoint manifest) of
        (ProviderHostManifestOnly, _) ->
            ProviderLaunchManifestOnly
        (ProviderHostIPCStdIO, ProviderEntrypointIPC artifactPath) ->
            ProviderLaunchIPCStdIO (resolveArtifactPath manifestPath artifactPath)
        (ProviderHostIPCStdIO, ProviderEntrypointExecutable artifactPath) ->
            ProviderLaunchIPCStdIO (resolveArtifactPath manifestPath artifactPath)
        (ProviderHostExecutableDirect, ProviderEntrypointExecutable artifactPath) ->
            ProviderLaunchExecutableDirect (resolveArtifactPath manifestPath artifactPath)
        (ProviderHostInProcess, ProviderEntrypointModule modName) ->
            ProviderLaunchInProcess modName
        (host, entrypoint) ->
            ProviderLaunchInvalid host entrypoint

providerManifestLoadStatus :: FilePath -> ProviderManifest -> IO ProviderLoadStatus
providerManifestLoadStatus manifestPath manifest =
    if not (providerManifestApiSupported manifest)
        then pure (ProviderLoadUnsupportedApi (pmfApiVersion manifest))
        else
            case providerManifestLaunchSpec manifestPath manifest of
                ProviderLaunchManifestOnly ->
                    pure ProviderLoadManifestOnly
                ProviderLaunchIPCStdIO artifactPath ->
                    artifactStatus artifactPath
                ProviderLaunchExecutableDirect artifactPath ->
                    artifactStatus artifactPath
                ProviderLaunchInProcess modName ->
                    pure (ProviderLoadModuleTarget modName)
                ProviderLaunchInvalid _ _ ->
                    pure ProviderLoadInvalidContract
  where
    artifactStatus artifactPath = do
        exists <- doesFileExist artifactPath
        pure $
            if exists
                then ProviderLoadReady artifactPath
                else ProviderLoadMissingArtifact artifactPath

providerClassLabel :: ProviderClass -> String
providerClassLabel providerClass =
    case providerClass of
        ProviderDirectCarrier -> "direct-carrier"
        ProviderOverlayCarrier -> "overlay-carrier"
        ProviderOpenBridge -> "open-bridge"
        ProviderClosedBridge -> "closed-bridge"

providerCapabilityLabel :: ProviderCapability -> String
providerCapabilityLabel capability =
    case capability of
        ProviderCapConnect -> "connect"
        ProviderCapListen -> "listen"
        ProviderCapDiscovery -> "discovery"
        ProviderCapGroupChat -> "group"
        ProviderCapMedia -> "media"
        ProviderCapBridge -> "bridge"
        ProviderCapTunnel -> "tunnel"

providerLaunchSpecLabel :: ProviderLaunchSpec -> String
providerLaunchSpecLabel launchSpec =
    case launchSpec of
        ProviderLaunchManifestOnly -> "manifest-only"
        ProviderLaunchIPCStdIO _ -> "ipc-stdio"
        ProviderLaunchExecutableDirect _ -> "exec-direct"
        ProviderLaunchInProcess _ -> "in-process"
        ProviderLaunchInvalid _ _ -> "invalid-contract"

providerLoadStatusLabel :: ProviderLoadStatus -> String
providerLoadStatusLabel loadStatus =
    case loadStatus of
        ProviderLoadManifestOnly -> "manifest-only"
        ProviderLoadUnsupportedApi _ -> "unsupported-api"
        ProviderLoadMissingArtifact _ -> "artifact-missing"
        ProviderLoadReady _ -> "load-ready"
        ProviderLoadModuleTarget _ -> "module-target"
        ProviderLoadInvalidContract -> "invalid-contract"

providerIdLabel :: TransportProviderId -> String
providerIdLabel providerId =
    tpStableId (providerDescriptor providerId)

providerEndpointSchema :: TransportProviderId -> String
providerEndpointSchema providerId =
    case providerId of
        ProviderTCP -> "host:port"
        ProviderUDP -> "host:port"
        ProviderTor -> "onion:port"
        ProviderWireGuard -> "peer@host:port"
        ProviderIRC -> "nick@server:port/#channel"
        ProviderAIM -> "screenname@server"
        ProviderXMPP -> "jid/resource"
        ProviderMastodon -> "@user@instance"
        ProviderFacebook -> "account-id"
        ProviderInstagram -> "account-id"
        ProviderWhatsApp -> "e164-or-handle"
        ProviderSignal -> "e164-or-username"
        ProviderSignalServer -> "server:port/account"

renderProviderEndpoint :: TransportProviderId -> String -> Int -> String
renderProviderEndpoint providerId host port =
    case providerId of
        ProviderTCP -> host ++ ":" ++ show port
        ProviderUDP -> "udp://" ++ host ++ ":" ++ show port
        ProviderTor -> host ++ ":" ++ show port ++ " via tor"
        ProviderWireGuard -> "wg://" ++ host ++ ":" ++ show port
        ProviderIRC -> "irc://" ++ host ++ ":" ++ show port
        ProviderAIM -> host ++ " via aim"
        ProviderXMPP -> host ++ " via xmpp"
        ProviderMastodon -> host ++ " via mastodon"
        ProviderFacebook -> host ++ " via facebook"
        ProviderInstagram -> host ++ " via instagram"
        ProviderWhatsApp -> host ++ ":" ++ show port ++ " via whatsapp"
        ProviderSignal -> host ++ ":" ++ show port ++ " via signal"
        ProviderSignalServer -> host ++ ":" ++ show port ++ " via signal-server"

resolveArtifactPath :: FilePath -> FilePath -> FilePath
resolveArtifactPath manifestPath artifactPath
    | isAbsolute artifactPath = artifactPath
    | otherwise = takeDirectory manifestPath </> artifactPath

parseClassTag :: String -> ProviderClass
parseClassTag tag =
    case normalizeTag tag of
        "overlay-carrier" -> ProviderOverlayCarrier
        "open-bridge" -> ProviderOpenBridge
        "closed-bridge" -> ProviderClosedBridge
        _ -> ProviderDirectCarrier

parseCapabilityTag :: String -> Maybe ProviderCapability
parseCapabilityTag tag =
    case normalizeTag tag of
        "connect" -> Just ProviderCapConnect
        "listen" -> Just ProviderCapListen
        "discovery" -> Just ProviderCapDiscovery
        "group" -> Just ProviderCapGroupChat
        "media" -> Just ProviderCapMedia
        "bridge" -> Just ProviderCapBridge
        "tunnel" -> Just ProviderCapTunnel
        _ -> Nothing

splitCommaTags :: String -> [String]
splitCommaTags raw =
    collect (splitCommas raw)
  where
    splitCommas [] = [""]
    splitCommas (',':rest) = "" : splitCommas rest
    splitCommas (ch:rest) =
        case splitCommas rest of
            [] -> [[ch]]
            (part:parts) -> (ch : part) : parts

    collect [] = []
    collect (part:rest) =
        let tag = trim part
        in if null tag
            then collect rest
            else tag : collect rest

trim :: String -> String
trim = dropWhile (== ' ') . dropTrailingSpaces
  where
    dropTrailingSpaces = reverse . dropWhile (== ' ') . reverse

normalizeTag :: String -> String
normalizeTag = map toLower . trim

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

providerManifestFromFields :: [(String, String)] -> Maybe ProviderManifest
providerManifestFromFields fields = do
    stableId <- lookupField "id"
    apiVersion <- lookupField "api"
    displayName <- lookupField "name"
    classTag <- lookupField "class"
    statusTag <- lookupField "status"
    endpointTag <- lookupField "endpoint"
    inheritsTag <- lookupField "inherits"
    capabilitiesTag <- lookupField "capabilities"
    hostTag <- lookupField "host"
    entrypointTag <- lookupField "entrypoint"
    encryptionTag <- lookupField "encryption"
    notes <- lookupField "notes"
    pure ProviderManifest
        { pmfStableId = stableId
        , pmfApiVersion = apiVersion
        , pmfDisplayName = displayName
        , pmfClassTag = classTag
        , pmfStatusTag = statusTag
        , pmfEndpointTag = endpointTag
        , pmfInheritsTag = inheritsTag
        , pmfCapabilitiesTag = capabilitiesTag
        , pmfHostTag = hostTag
        , pmfEntrypointTag = entrypointTag
        , pmfEncryptionTag = encryptionTag
        , pmfNotes = notes
        }
  where
    lookupField key = lookup key fields
