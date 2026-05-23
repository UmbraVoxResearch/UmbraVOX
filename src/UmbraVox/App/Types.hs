-- SPDX-License-Identifier: Apache-2.0
-- | Shared session and domain types — independent of any TUI layer.
module UmbraVox.App.Types
    ( ContactStatus(..)
    , statusTag
    , SessionInfo(..)
    , SessionCrypto(..)
    , BridgeState(..)
    ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef)
import UmbraVox.Chat.OutboundQueue (OutboundQueue)
import UmbraVox.Chat.Session (ChatSession)
import UmbraVox.Network.ProviderCatalog (TransportProviderId)
import UmbraVox.Network.TransportClass (AnyTransport)

data ContactStatus = Online | Offline | Local | Group | LAN | PEX | Bridge
    deriving stock (Eq)

statusTag :: ContactStatus -> String
statusTag Online  = " \x25CF"   -- ● filled circle
statusTag Offline = " \x25CB"   -- ○ empty circle
statusTag Local   = " \x1F512"  -- 🔒 lock
statusTag Group   = " \x1F465"  -- 👥 people
statusTag LAN     = " \x1F5A7"  -- 🖧 network
statusTag PEX     = " \x1F517"  -- 🔗 link
statusTag Bridge  = " \x21C4"   -- ⇄ bridge arrows

-- | Crypto backend for a session: either the local double-ratchet or
-- an external bridge plugin.
data SessionCrypto
    = RatchetCrypto (IORef ChatSession)
    | BridgeCrypto BridgeState

data BridgeState = BridgeState
    { bsProviderId :: TransportProviderId
    , bsRemoteId   :: String
    }

data SessionInfo = SessionInfo
    { siTransport :: Maybe AnyTransport, siCrypto :: SessionCrypto
    , siSessionLock :: MVar ()
    , siRecvTid :: Maybe ThreadId, siPeerName :: String
    , siHistory :: IORef [String], siStatus :: IORef ContactStatus
    , siOutboundQueue :: OutboundQueue }
