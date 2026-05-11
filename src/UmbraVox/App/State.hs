-- SPDX-License-Identifier: Apache-2.0
-- | Domain state shared between the TUI and headless runtimes.
--
-- 'CoreState' holds all state required by the network, crypto, and session
-- layers.  The TUI wraps it inside 'AppState' (in "UmbraVox.TUI.Types") and
-- adds presentation-only fields such as scroll positions and dialog mode.
-- Headless nodes operate on 'CoreState' directly, with no TUI dependency.
module UmbraVox.App.State
    ( CoreState(..)
    , newCoreState
    ) where

import Data.IORef (IORef, newIORef)

import UmbraVox.App.Config (AppConfig)

-- | Shared domain state for both the TUI and headless runtimes.
--
-- All fields that are needed by the network, session, or crypto layers live
-- here.  TUI-only concerns (scroll positions, dialog mode, render tokens,
-- etc.) are kept in 'UmbraVox.TUI.Types.AppState'.
data CoreState = CoreState
    { csConfig  :: !AppConfig
      -- ^ Full application configuration and domain-level mutable state
      --   (sessions, identity, DB handle, network threads, settings).
    , csRunning :: !(IORef Bool)
      -- ^ Lifecycle flag: 'True' while the node should continue accepting
      --   connections.  Set to 'False' to trigger a clean shutdown.
    }

-- | Construct a 'CoreState' from an existing 'AppConfig'.
-- The node starts in the running state (@csRunning = True@).
newCoreState :: AppConfig -> IO CoreState
newCoreState cfg = CoreState cfg <$> newIORef True
