-- SPDX-License-Identifier: Apache-2.0
-- | Tests for UmbraVox.TUI.Types and related layout logic.
module Test.TUI.Types (runTests) where

import System.Directory (doesFileExist)
import Test.Util (assertEq)
import UmbraVox.BuildProfile
    ( BuildPluginId(..), PluginEntrypoint(..), PluginHost(..), PluginLaunchSpec(..), PluginLoadStatus(..), PluginManifest(..)
    , buildPluginRegistry, bpManifestPath, bpName, bpSource, bpStableId, disabledPlugins
    , enabledPlugins, extensionPluginRegistry, packagedPlugins
    , loadPackagedPluginCatalog
    , loadPackagedPluginManifestFields, loadPackagedPluginManifests
    , loadPluginManifest, loadPluginManifestFields
    , pluginLaunchSpecLabel, pluginLoadStatusLabel, pluginManifestApiSupported, pluginManifestEntrypoint, pluginManifestHost
    , pluginManifestLaunchSpec, pluginManifestLoadStatus
    , pluginEnabled, pluginManifestPath, pluginName, pluginSourceLabel
    , pmApiVersion, pmEncryptionTag, pmHostTag, pmStableId, pmStatusTag
    )
import UmbraVox.Network.ProviderCatalog
    ( ProviderLoadStatus(..), TransportProviderId(..), ctpLoadStatus, ctpProvider
    , loadTransportProviderCatalog, loadTransportProviderManifest
    , loadTransportProviderManifestFields, loadTransportProviderManifests
    , loadTransportProviderRuntimeCatalog, providerLoadStatusLabel
    , providerManifestApiSupported, providerManifestInherits, tpId, tpStableId
    )
import UmbraVox.TUI.Types (ContactStatus(..), statusTag, Layout(..), MenuTab(..),
                           menuTabLabel, menuTabItems, menuTabUnderlineIndex)
import UmbraVox.TUI.Layout (dropdownCol)
import UmbraVox.TUI.Render (calcLayout, sizeValid)

runTests :: IO Bool
runTests = do
    putStrLn "TUI.Types"
    p1 <- testStatusTagOnline
    p2 <- testStatusTagOffline
    p3 <- testStatusTagLocal
    p4 <- testStatusTagGroup
    p5 <- testStatusTagLAN
    p6 <- testStatusTagPEX
    p7 <- testCalcLayoutDimensions
    p8 <- testCalcLayoutMinSize
    p9 <- testSizeValid
    p10 <- testMenuTabEnum
    p11 <- testMenuTabItems
    p12 <- testMenuTabLabel
    p13 <- testMenuQuitUnderline
    p14 <- testDropdownAnchorTracksRightJustifiedTabs
    p15 <- testBuildPluginRegistry
    p16 <- testTransportProviderRegistry
    pure (p1 && p2 && p3 && p4 && p5 && p6 && p7 && p8 && p9 && p10 && p11 && p12 && p13 && p14 && p15 && p16)

testStatusTagOnline :: IO Bool
testStatusTagOnline = assertEq "statusTag Online"  " \x25CF"   (statusTag Online)

testStatusTagOffline :: IO Bool
testStatusTagOffline = assertEq "statusTag Offline" " \x25CB"   (statusTag Offline)

testStatusTagLocal :: IO Bool
testStatusTagLocal = assertEq "statusTag Local"  " \x1F512" (statusTag Local)

testStatusTagGroup :: IO Bool
testStatusTagGroup = assertEq "statusTag Group"  " \x1F465"   (statusTag Group)

testStatusTagLAN :: IO Bool
testStatusTagLAN = assertEq "statusTag LAN"    " \x1F5A7"   (statusTag LAN)

testStatusTagPEX :: IO Bool
testStatusTagPEX = assertEq "statusTag PEX"    " \x1F517"   (statusTag PEX)

-- | calcLayout at 80x24 should produce valid positive dimensions.
testCalcLayoutDimensions :: IO Bool
testCalcLayoutDimensions = do
    let lay = calcLayout 24 80
    a <- assertEq "layout lCols > 0"   True (lCols lay > 0)
    b <- assertEq "layout lRows > 0"   True (lRows lay > 0)
    c <- assertEq "layout lLeftW > 0"  True (lLeftW lay > 0)
    d <- assertEq "layout lRightW > 0" True (lRightW lay > 0)
    e <- assertEq "layout lChatH > 0"  True (lChatH lay > 0)
    -- lLeftW + lRightW should equal lCols
    f <- assertEq "layout width sum" (lCols lay) (lLeftW lay + lRightW lay)
    pure (a && b && c && d && e && f)

-- | calcLayout at larger terminal should produce larger usable area.
testCalcLayoutMinSize :: IO Bool
testCalcLayoutMinSize = do
    let small = calcLayout 24 80
        large = calcLayout 50 160
    a <- assertEq "larger terminal wider"  True (lCols large > lCols small)
    b <- assertEq "larger terminal taller" True (lRows large > lRows small)
    pure (a && b)

-- | sizeValid accepts normal sizes and rejects extremes.
testSizeValid :: IO Bool
testSizeValid = do
    a <- assertEq "sizeValid 24 80"  True  (sizeValid 24 80)
    b <- assertEq "sizeValid 10 40"  False (sizeValid 10 40)
    c <- assertEq "sizeValid 50 200" True  (sizeValid 50 200)
    pure (a && b && c)

-- | MenuTab enum covers all 5 tabs.
testMenuTabEnum :: IO Bool
testMenuTabEnum = do
    let tabs = [minBound .. maxBound] :: [MenuTab]
    assertEq "MenuTab has 5 variants" 5 (length tabs)

-- | Each MenuTab has at least one item.
testMenuTabItems :: IO Bool
testMenuTabItems = do
    let tabs = [MenuHelp, MenuContacts, MenuChat, MenuPrefs, MenuQuit]
        allNonEmpty = all (\t -> not (null (menuTabItems t))) tabs
    assertEq "all MenuTab items non-empty" True allNonEmpty

-- | Each MenuTab has a non-empty label.
testMenuTabLabel :: IO Bool
testMenuTabLabel = do
    let tabs = [MenuHelp, MenuContacts, MenuChat, MenuPrefs, MenuQuit]
        allNonEmpty = all (\t -> not (null (menuTabLabel t))) tabs
    a <- assertEq "all MenuTab labels non-empty" True allNonEmpty
    -- Verify specific labels contain expected F-key references
    b <- assertEq "MenuHelp label contains F1" True ("F1" `isIn` menuTabLabel MenuHelp)
    c <- assertEq "MenuPrefs label contains F4" True ("F4" `isIn` menuTabLabel MenuPrefs)
    d <- assertEq "MenuQuit label contains Q" True ("Q" `isIn` menuTabLabel MenuQuit)
    pure (a && b && c && d)
  where
    isIn needle haystack = any (\i -> take (length needle) (drop i haystack) == needle)
                               [0..length haystack - length needle]

testMenuQuitUnderline :: IO Bool
testMenuQuitUnderline =
    assertEq "MenuQuit underline index" (Just 1) (menuTabUnderlineIndex MenuQuit)

testDropdownAnchorTracksRightJustifiedTabs :: IO Bool
testDropdownAnchorTracksRightJustifiedTabs = do
    let help80 = dropdownCol 80 MenuHelp
        help120 = dropdownCol 120 MenuHelp
        contacts120 = dropdownCol 120 MenuContacts
        chat120 = dropdownCol 120 MenuChat
        prefs120 = dropdownCol 120 MenuPrefs
        quit120 = dropdownCol 120 MenuQuit
    a <- assertEq "dropdown anchor shifts right on wider terminals" True (help120 > help80)
    b <- assertEq "dropdown anchors remain ordered by tab" True
        (help120 < contacts120 && contacts120 < chat120 && chat120 < prefs120 && prefs120 < quit120)
    pure (a && b)

testBuildPluginRegistry :: IO Bool
testBuildPluginRegistry = do
    let names = map bpName buildPluginRegistry
        prefsItems = menuTabItems MenuPrefs
    a <- assertEq "build plugin registry size" 7 (length buildPluginRegistry)
    b <- assertEq "persistent storage plugin label" "Persistent Storage" (pluginName PluginPersistentStorage)
    c <- assertEq "persistent storage plugin enabled in standard test build" True (pluginEnabled PluginPersistentStorage)
    d <- assertEq "chat transfer plugin present" True ("Chat Export/Import" `elem` names)
    e <- assertEq "all plugins enabled in standard build" 7 (length enabledPlugins)
    f <- assertEq "no disabled plugins in standard build" 0 (length disabledPlugins)
    g <- assertEq "prefs menu shows discovery toggle from registry" True ("mDNS Toggle" `elem` prefsItems)
    h <- assertEq "prefs menu shows export from registry" True ("Export Chat" `elem` prefsItems)
    i <- assertEq "prefs menu shows import from registry" True ("Import Chat" `elem` prefsItems)
    j <- assertEq "extension registry size" 4 (length extensionPluginRegistry)
    k <- assertEq "packaged plugin slice size" 4 (length packagedPlugins)
    l <- assertEq "first extension source label" "packaged" (pluginSourceLabel (bpSource (head extensionPluginRegistry)))
    m <- assertEq "group chat manifest path" (Just "plugins/group-chat/manifest.uvx") (pluginManifestPath PluginGroupChat)
    n <- assertEq "all packaged plugins have manifest paths" True (all hasManifest packagedPlugins)
    o <- allManifestFilesExist
    p <- testManifestLoader
    q <- testPackagedManifestLoader
    r <- testTypedManifestLoader
    s <- testTypedManifestBatchLoader
    t <- testPackagedCatalogDiscovery
    u <- testPackagedCatalogOrder
    v <- testInvalidLaunchContract
    w <- testUnsupportedApiContract
    pure (a && b && c && d && e && f && g && h && i && j && k && l && m && n && o && p && q && r && s && t && u && v && w)
  where
    hasManifest plugin = case bpManifestPath plugin of
        Just _ -> True
        Nothing -> False
    allManifestFilesExist = do
        exists <- mapM doesFileExist [ path | Just path <- map bpManifestPath packagedPlugins ]
        assertEq "all packaged plugin manifest files exist" True (and exists)
    testManifestLoader = do
        fields <- loadPluginManifestFields PluginGroupChat
        a1 <- assertEq "group chat manifest id" (Just "group-chat") (lookup "id" fields)
        a2 <- assertEq "group chat manifest api" (Just "uvx-plugin-v1") (lookup "api" fields)
        a3 <- assertEq "group chat manifest host" (Just "ipc-stdio") (lookup "host" fields)
        a4 <- assertEq "group chat manifest encryption" (Just "sealed-sender") (lookup "encryption" fields)
        pure (a1 && a2 && a3 && a4)
    testPackagedManifestLoader = do
        manifests <- loadPackagedPluginManifestFields
        b1 <- assertEq "packaged manifest loader count" 4 (length manifests)
        b2 <- assertEq "location manifest status" (Just "planned")
            (lookup "status" =<< lookup PluginLocationSharing manifests)
        pure (b1 && b2)
    testTypedManifestLoader = do
        manifest <- loadPluginManifest PluginGroupChat
        c1 <- assertEq "typed group manifest present" True (maybe False (const True) manifest)
        c2 <- assertEq "typed group manifest api" (Just "uvx-plugin-v1") (pmApiVersion <$> manifest)
        c3 <- assertEq "typed group manifest status" (Just "planned") (pmStatusTag <$> manifest)
        c4 <- assertEq "typed group manifest host tag" (Just "ipc-stdio") (pmHostTag <$> manifest)
        c5 <- assertEq "typed group manifest host" (Just PluginHostIPCStdIO) (pluginManifestHost <$> manifest)
        c6 <- assertEq "typed group manifest encryption" (Just "sealed-sender") (pmEncryptionTag <$> manifest)
        c7 <- assertEq "typed group manifest entrypoint" (Just (PluginEntrypointExecutable "generated/group-chat-plugin"))
            (pluginManifestEntrypoint <$> manifest)
        c8 <- assertEq "typed group manifest launch spec"
            (Just (PluginLaunchIPCStdIO "plugins/group-chat/generated/group-chat-plugin"))
            (pluginManifestLaunchSpec "plugins/group-chat/manifest.uvx" <$> manifest)
        c9 <- case manifest of
            Nothing -> assertEq "typed group manifest load status" True False
            Just m -> do
                loadStatus <- pluginManifestLoadStatus "plugins/group-chat/manifest.uvx" m
                assertEq "typed group manifest load status"
                    (PluginLoadMissingArtifact "plugins/group-chat/generated/group-chat-plugin")
                    loadStatus
        c10 <- assertEq "typed group manifest launch label"
            (Just "ipc-stdio")
            ((pluginLaunchSpecLabel . pluginManifestLaunchSpec "plugins/group-chat/manifest.uvx") <$> manifest)
        c11 <- case manifest of
            Nothing -> assertEq "typed group manifest load label" True False
            Just m -> do
                loadStatus <- pluginManifestLoadStatus "plugins/group-chat/manifest.uvx" m
                assertEq "typed group manifest load label" "artifact-missing" (pluginLoadStatusLabel loadStatus)
        pure (c1 && c2 && c3 && c4 && c5 && c6 && c7 && c8 && c9 && c10 && c11)
    testTypedManifestBatchLoader = do
        manifests <- loadPackagedPluginManifests
        d1 <- assertEq "typed packaged manifest batch count" 4 (length manifests)
        d2 <- assertEq "typed file sharing id" (Just "file-sharing")
            (pmStableId <$> lookup PluginFileSharing manifests)
        pure (d1 && d2)
    testPackagedCatalogDiscovery = do
        catalog <- loadPackagedPluginCatalog
        e1 <- assertEq "discovered packaged catalog count" 4 (length catalog)
        e2 <- assertEq "first discovered catalog has manifest path" True
            (all (maybe False (const True) . bpManifestPath . fst) catalog)
        e3 <- assertEq "discovered group chat present" True
            (any (\(plugin, manifest) -> bpName plugin == "Group Chat" && pmStableId manifest == "group-chat") catalog)
        e4 <- assertEq "discovered catalog stable ids match descriptor ids" True
            (all (\(plugin, manifest) -> bpStableId plugin == pmStableId manifest) catalog)
        pure (e1 && e2 && e3 && e4)
    testPackagedCatalogOrder = do
        catalog <- loadPackagedPluginCatalog
        let stableIds = map (pmStableId . snd) catalog
        assertEq "discovered packaged catalog order"
            ["group-chat", "location-sharing", "image-sharing", "file-sharing"]
            stableIds
    testInvalidLaunchContract = do
        let invalidManifest = PluginManifest
                { pmStableId = "group-chat"
                , pmApiVersion = "uvx-plugin-v1"
                , pmDisplayName = "Group Chat"
                , pmSourceTag = "packaged"
                , pmTierTag = "premium-ready"
                , pmCategoryTag = "ux"
                , pmStatusTag = "planned"
                , pmHostTag = "ipc-stdio"
                , pmEntrypointTag = "module:UmbraVox.Plugin.GroupChat"
                , pmEncryptionTag = "sealed-sender"
                , pmNotes = "invalid test fixture"
                }
        e1 <- assertEq "invalid launch spec detected"
            (PluginLaunchInvalid PluginHostIPCStdIO (PluginEntrypointModule "UmbraVox.Plugin.GroupChat"))
            (pluginManifestLaunchSpec "plugins/group-chat/manifest.uvx" invalidManifest)
        loadStatus <- pluginManifestLoadStatus "plugins/group-chat/manifest.uvx" invalidManifest
        e2 <- assertEq "invalid launch status detected" PluginLoadInvalidContract loadStatus
        pure (e1 && e2)
    testUnsupportedApiContract = do
        let invalidManifest = PluginManifest
                { pmStableId = "group-chat"
                , pmApiVersion = "uvx-plugin-v2"
                , pmDisplayName = "Group Chat"
                , pmSourceTag = "packaged"
                , pmTierTag = "premium-ready"
                , pmCategoryTag = "ux"
                , pmStatusTag = "planned"
                , pmHostTag = "ipc-stdio"
                , pmEntrypointTag = "exec:generated/group-chat-plugin"
                , pmEncryptionTag = "sealed-sender"
                , pmNotes = "invalid api test fixture"
                }
        e1 <- assertEq "unsupported api support check" False (pluginManifestApiSupported invalidManifest)
        loadStatus <- pluginManifestLoadStatus "plugins/group-chat/manifest.uvx" invalidManifest
        e2 <- assertEq "unsupported api status detected" (PluginLoadUnsupportedApi "uvx-plugin-v2") loadStatus
        e3 <- assertEq "unsupported api label detected" "unsupported-api" (pluginLoadStatusLabel loadStatus)
        pure (e1 && e2 && e3)

testTransportProviderRegistry :: IO Bool
testTransportProviderRegistry = do
    tcpFields <- loadTransportProviderManifestFields ProviderTCP
    signalManifest <- loadTransportProviderManifest ProviderSignal
    providers <- loadTransportProviderManifests
    catalog <- loadTransportProviderCatalog
    runtimeCatalog <- loadTransportProviderRuntimeCatalog
    a <- assertEq "transport provider count" 13 (length runtimeCatalog)
    b <- assertEq "tcp provider manifest api" (Just "uvx-provider-v1") (lookup "api" tcpFields)
    c <- assertEq "tcp provider manifest host" (Just "in-process") (lookup "host" tcpFields)
    d <- assertEq "signal provider typed manifest present" True (maybe False (const True) signalManifest)
    e <- assertEq "signal provider inherits signal-server" (Just ["signal-server"])
        (providerManifestInherits <$> signalManifest)
    f <- assertEq "signal provider api supported" (Just True)
        (providerManifestApiSupported <$> signalManifest)
    g <- assertEq "provider typed manifest batch count" 13 (length providers)
    h <- assertEq "provider catalog count" 13 (length catalog)
    i <- assertEq "transport catalog first stable id" "tcp" (tpStableId (fst (head catalog)))
    j <- assertEq "tcp provider load label" "module-target"
        (providerLoadStatusLabel (ctpLoadStatus (head runtimeCatalog)))
    k <- assertEq "runtime catalog contains signal artifact gap" True
        (any isSignalArtifactMissing runtimeCatalog)
    pure (a && b && c && d && e && f && g && h && i && j && k)
  where
    isSignalArtifactMissing entry =
        tpId (ctpProvider entry) == ProviderSignal
            && ctpLoadStatus entry == ProviderLoadMissingArtifact "providers/signal/generated/signal-provider"
