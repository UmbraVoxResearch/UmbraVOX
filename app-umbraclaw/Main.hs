-- SPDX-License-Identifier: Apache-2.0
-- | UmbraClaw bridge executable entry point (M25.1.4)
module Main (main) where

import UmbraVox.Bridge.UmbraClaw.Main (umbraClawBridge)

main :: IO ()
main = umbraClawBridge
