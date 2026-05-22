-- SPDX-License-Identifier: Apache-2.0
-- | Build version information.
--
-- The version string is derived from the cabal package version.
-- No runtime IO or unsafePerformIO — pure constants only.
module UmbraVox.Version
    ( version
    , versionFull
    , versionShort
    ) where

-- | Version string, e.g. "v0.3.1".
-- Kept in sync with the @version@ field in @UmbraVox.cabal@.
version :: String
version = "v0.3.1"

-- | Full version with app name, e.g. "UmbraVOX v0.3.1"
versionFull :: String
versionFull = "UmbraVOX " ++ version

-- | Short version for status bar, e.g. "v0.3.1"
versionShort :: String
versionShort = version
