-- SPDX-License-Identifier: Apache-2.0
-- | Domain configuration for UmbraVOX — independent of any TUI layer.
module UmbraVox.App.Config
    ( SessionId
    , ConnectionMode(..)
    , AppConfig(..)
    ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import UmbraVox.BuildProfile
    ( BuildPlugin, PluginManifest, PackagedPluginRuntime )
import UmbraVox.App.Types (SessionInfo)
import UmbraVox.Crypto.Signal.X3DH (IdentityKey)
import UmbraVox.Network.ProviderCatalog
    ( CachedTransportProvider, ProviderManifest, TransportProvider )
import UmbraVox.Network.MDNS (MDNSPeer)
import UmbraVox.Plugin.Types (PluginRegistry)
import UmbraVox.Storage.Anthony (AnthonyDB)
import UmbraVox.Storage.Class (StorageHandle)

type SessionId = Int

-- | Connection trust modes, ordered most open to most locked down.
-- Swing:       Accept all + auto PEX peer exchange + mDNS on
-- Promiscuous: Accept all connections automatically + mDNS on
-- Selective:   Accept with fingerprint confirmation + mDNS on
-- Chaste:      Trusted keys only, silent reject, mDNS off
-- Chastity:    Trusted keys only + ephemeral (no DB, no persistence)
data ConnectionMode = Swing | Promiscuous | Selective | Chaste | Chastity
    deriving stock (Eq, Show, Enum, Bounded)

data AppConfig = AppConfig
    { cfgListenPort  :: IORef Int
    , cfgDisplayName :: IORef String
    , cfgIdentity    :: IORef (Maybe IdentityKey)
    , cfgSessions    :: IORef (Map SessionId SessionInfo)
    , cfgNextId      :: IORef SessionId
    , cfgDebugLogging :: IORef Bool
    , cfgDebugLogPath :: IORef FilePath
    -- Discovery settings
    , cfgMDNSEnabled :: IORef Bool
    , cfgPEXEnabled  :: IORef Bool
    , cfgDBEnabled   :: IORef Bool
    , cfgDBPath      :: IORef String
    , cfgPersistencePreference :: IORef (Maybe Bool)
    , cfgPackagedPluginCatalog :: IORef [(BuildPlugin, PluginManifest)]
    , cfgPackagedPluginRuntimeCatalog :: IORef [PackagedPluginRuntime]
    , cfgTransportProviderCatalog :: IORef [(TransportProvider, ProviderManifest)]
    , cfgTransportProviderRuntimeCatalog :: IORef [CachedTransportProvider]
    -- Discovery state
    , cfgListenerThread :: IORef (Maybe ThreadId)
    , cfgMDNSThread  :: IORef (Maybe ThreadId)
    , cfgMDNSPeers   :: IORef [MDNSPeer]
    -- Retention settings
    , cfgRetentionDays    :: IORef Int              -- days to keep messages (0 = forever)
    , cfgAutoSaveMessages :: IORef Bool             -- auto-save messages to DB
    , cfgAnthonyDB        :: IORef (Maybe AnthonyDB) -- DB handle
    -- Security
    , cfgConnectionMode   :: IORef ConnectionMode
    , cfgTrustedKeys      :: IORef [ByteString]
    -- Finding: M10.2.13 — Selective mode previously accepted every peer without
    -- remembering whether a given key had been seen before, making it equivalent
    -- to Promiscuous.  There was no mechanism to detect a key-change attack where
    -- a second peer claims the same identity with a different key.
    --
    -- Vulnerability: Without a per-session TOFU set an attacker could present an
    -- arbitrary key and be accepted silently on every connection.  A key-change
    -- (man-in-the-middle substitution) would also go undetected.
    --
    -- Fix: 'cfgTofoKeys' is an IORef holding the set of peer public keys that
    -- have been accepted under Selective mode in this session.  On first
    -- connection a key is added; on repeat connection the same key is accepted.
    -- A different key claiming a previously-seen slot is rejected (see
    -- 'RuntimeNetwork.hs' trustCheck).
    --
    -- Verified: 'newDefaultAppConfig' initialises the set to empty; the trust
    -- check in 'acceptLoopBoundTUI' inserts on first encounter and rejects
    -- conflicting keys on subsequent encounters.
    , cfgTofoKeys         :: IORef (Set.Set ByteString)
    -- M17.3: ephemeral override — when True all disk writes are skipped
    , cfgEphemeral        :: IORef Bool
    -- M17.2: abstract storage backend (in-memory or Anthony-backed)
    , cfgStorage          :: IORef StorageHandle
    -- M17.5: runtime plugin registry (persistence plugins, all disabled by default)
    , cfgPluginRegistry   :: IORef PluginRegistry
    -- M20.1.1: runtime log lock and PID tracker (replaces unsafePerformIO globals)
    , cfgLogLock          :: MVar ()
    , cfgLogWriterPID     :: IORef Int
    }
