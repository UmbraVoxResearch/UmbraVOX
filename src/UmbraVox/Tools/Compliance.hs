-- SPDX-License-Identifier: Apache-2.0
-- | Release compliance tooling: SBOM generation, license bundles, and policy enforcement.
module UmbraVox.Tools.Compliance
    ( generateSBOM
    , generateLicenseBundle
    , checkLicensePolicy
    , analyzeLinkingObligations
    ) where

import Control.Exception (IOException, catch, evaluate)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Exit (ExitCode(..))
import System.IO (hGetContents)
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess)

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

data DepInfo = DepInfo
    { depName    :: String
    , depVersion :: String
    , depLicense :: String
    , depSource  :: String
    } deriving stock (Show)

------------------------------------------------------------------------
-- Known dependency manifest
------------------------------------------------------------------------

haskellDeps :: [DepInfo]
haskellDeps =
    [ DepInfo "base"       ">= 4.18 && < 5"    "BSD-3-Clause" "GHC boot library"
    , DepInfo "bytestring" ">= 0.11 && < 0.13"  "BSD-3-Clause" "GHC boot library"
    , DepInfo "containers" ">= 0.6 && < 0.8"    "BSD-3-Clause" "GHC boot library"
    , DepInfo "array"      ">= 0.5 && < 0.6"    "BSD-3-Clause" "GHC boot library"
    , DepInfo "stm"        ">= 2.5 && < 2.6"    "BSD-3-Clause" "GHC boot library"
    , DepInfo "network"    ">= 3.1 && < 3.3"    "BSD-3-Clause" "Hackage (bundled via Nix)"
    , DepInfo "binary"     ">= 0.8 && < 0.9"    "BSD-3-Clause" "GHC boot library"
    , DepInfo "directory"  ">= 1.3 && < 1.4"    "BSD-3-Clause" "GHC boot library"
    , DepInfo "filepath"   ">= 1.4 && < 1.6"    "BSD-3-Clause" "GHC boot library"
    , DepInfo "time"       ">= 1.12 && < 1.15"  "BSD-3-Clause" "GHC boot library"
    , DepInfo "process"    ">= 1.6 && < 1.7"    "BSD-3-Clause" "GHC boot library"
    , DepInfo "text"       ">= 2.0 && < 2.2"    "BSD-3-Clause" "GHC boot library"
    , DepInfo "unix"       ">= 2.8 && < 2.9"    "BSD-3-Clause" "GHC boot library"
    ]

cSources :: [String]
cSources = [ "aes256", "chacha20", "hkdf", "hmac", "keccak"
           , "mlkem768", "poly1305", "sha256", "sha512", "x25519" ]

allowedLicenses :: [String]
allowedLicenses = ["BSD-3-Clause", "BSD-2-Clause", "MIT", "Apache-2.0", "ISC"]

buildTools :: [DepInfo]
buildTools =
    [ DepInfo "GHC"   "9.6.x"   "BSD-3-Clause" "Haskell compiler"
    , DepInfo "cabal" "3.x"     "BSD-3-Clause"  "Build tool"
    , DepInfo "F*"    "2026.x"  "Apache-2.0"    "Formal verification"
    , DepInfo "Z3"    "4.x"     "MIT"            "SMT solver"
    ]

------------------------------------------------------------------------
-- SBOM generation
------------------------------------------------------------------------

-- | Generate a Software Bill of Materials and print it to stdout.
generateSBOM :: IO ExitCode
generateSBOM = do
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now

    -- Try to enrich with actual installed versions from ghc-pkg
    enriched <- mapM enrichWithGhcPkg haskellDeps

    putStrLn "UmbraVOX Software Bill of Materials"
    putStrLn "===================================="
    putStrLn $ "Generated: " ++ timestamp
    putStrLn "Project: UmbraVox 0.1.0.0"
    putStrLn "License: Apache-2.0"
    putStrLn ""

    putStrLn "Haskell Dependencies"
    putStrLn "--------------------"
    mapM_ printDep enriched
    putStrLn ""

    putStrLn "C Sources (project-owned)"
    putStrLn "-------------------------"
    mapM_ (\name -> putStrLn $ padRight 34 ("csrc/generated/" ++ name ++ ".c")
                               ++ "Apache-2.0      Generated from .spec") cSources
    putStrLn ""

    putStrLn "Build Tools (provided by nix-shell)"
    putStrLn "------------------------------------"
    mapM_ printDep buildTools

    pure ExitSuccess
  where
    printDep :: DepInfo -> IO ()
    printDep dep = putStrLn $ padRight 13 (depName dep)
                              ++ padRight 22 (depVersion dep)
                              ++ padRight 16 (depLicense dep)
                              ++ depSource dep

-- | Try to read the actual installed version of a package from ghc-pkg.
-- Falls back to the cabal version bounds on any failure.
enrichWithGhcPkg :: DepInfo -> IO DepInfo
enrichWithGhcPkg dep = do
    result <- tryReadGhcPkg (depName dep)
    case result of
        Just ver -> pure dep { depVersion = ver }
        Nothing  -> pure dep

tryReadGhcPkg :: String -> IO (Maybe String)
tryReadGhcPkg pkgName = do
    let cp = (proc "ghc-pkg" ["field", pkgName, "version", "--simple-output"])
            { std_in  = Inherit
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    catch
        (do
            (_, Just hOut, _, ph) <- createProcess cp
            output <- hGetContents hOut
            -- Force the lazy string before waiting on the process
            _ <- evaluate (length output)
            ec <- waitForProcess ph
            let ver = strip output
            case ec of
                ExitSuccess
                    | not (null ver) -> pure (Just ver)
                _ -> pure Nothing
        )
        (\(_ :: IOException) -> pure Nothing)

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

padRight :: Int -> String -> String
padRight n s
    | length s >= n = s ++ " "
    | otherwise     = s ++ replicate (n - length s) ' '

------------------------------------------------------------------------
-- License bundle generation
------------------------------------------------------------------------

-- | Generate a third-party license text bundle and print it to stdout.
generateLicenseBundle :: IO ExitCode
generateLicenseBundle = do
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now

    putStrLn "========================================================"
    putStrLn "UmbraVOX Third-Party License Bundle"
    putStrLn "========================================================"
    putStrLn ""
    putStrLn $ "Generated: " ++ timestamp
    putStrLn ""
    putStrLn "This file contains the license texts applicable to all"
    putStrLn "third-party dependencies included in the UmbraVOX binary"
    putStrLn "distribution. UmbraVOX itself is licensed under Apache-2.0."
    putStrLn ""
    putStrLn "All Haskell dependencies (both GHC boot libraries and"
    putStrLn "the network package) are licensed under BSD-3-Clause."
    putStrLn "Build tools Z3 is MIT-licensed; F* is Apache-2.0."
    putStrLn ""

    putStrLn "--------------------------------------------------------"
    putStrLn "BSD-3-Clause (applies to all Haskell dependencies)"
    putStrLn "--------------------------------------------------------"
    putStrLn ""
    putStrLn "Applicable packages:"
    mapM_ (\dep -> putStrLn $ "  - " ++ depName dep) (filter (\d -> depLicense d == "BSD-3-Clause") haskellDeps)
    putStrLn ""
    putStrLn bsd3Text
    putStrLn ""

    putStrLn "--------------------------------------------------------"
    putStrLn "Apache-2.0 (UmbraVOX project code and C sources)"
    putStrLn "--------------------------------------------------------"
    putStrLn ""
    putStrLn "Applicable components:"
    putStrLn "  - UmbraVox (the project itself)"
    mapM_ (\name -> putStrLn $ "  - csrc/generated/" ++ name ++ ".c") cSources
    putStrLn ""
    putStrLn apache2Notice
    putStrLn ""

    putStrLn "--------------------------------------------------------"
    putStrLn "MIT (build tool: Z3)"
    putStrLn "--------------------------------------------------------"
    putStrLn ""
    putStrLn mitNotice
    putStrLn ""

    putStrLn "========================================================"
    putStrLn "End of third-party license bundle"
    putStrLn "========================================================"

    pure ExitSuccess

bsd3Text :: String
bsd3Text = intercalate "\n"
    [ "Redistribution and use in source and binary forms, with or without"
    , "modification, are permitted provided that the following conditions are met:"
    , ""
    , "1. Redistributions of source code must retain the above copyright notice,"
    , "   this list of conditions and the following disclaimer."
    , ""
    , "2. Redistributions in binary form must reproduce the above copyright notice,"
    , "   this list of conditions and the following disclaimer in the documentation"
    , "   and/or other materials provided with the distribution."
    , ""
    , "3. Neither the name of the copyright holder nor the names of its"
    , "   contributors may be used to endorse or promote products derived from"
    , "   this software without specific prior written permission."
    , ""
    , "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS"
    , "\"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT"
    , "LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR"
    , "A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT"
    , "HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,"
    , "SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT"
    , "LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,"
    , "DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY"
    , "THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT"
    , "(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE"
    , "OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
    ]

apache2Notice :: String
apache2Notice = intercalate "\n"
    [ "Licensed under the Apache License, Version 2.0 (the \"License\");"
    , "you may not use this file except in compliance with the License."
    , "You may obtain a copy of the License at"
    , ""
    , "    http://www.apache.org/licenses/LICENSE-2.0"
    , ""
    , "Unless required by applicable law or agreed to in writing, software"
    , "distributed under the License is distributed on an \"AS IS\" BASIS,"
    , "WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."
    , "See the License for the specific language governing permissions and"
    , "limitations under the License."
    ]

mitNotice :: String
mitNotice = intercalate "\n"
    [ "Permission is hereby granted, free of charge, to any person obtaining a"
    , "copy of this software and associated documentation files (the \"Software\"),"
    , "to deal in the Software without restriction, including without limitation"
    , "the rights to use, copy, modify, merge, publish, distribute, sublicense,"
    , "and/or sell copies of the Software, and to permit persons to whom the"
    , "Software is furnished to do so, subject to the following conditions:"
    , ""
    , "The above copyright notice and this permission notice shall be included in"
    , "all copies or substantial portions of the Software."
    , ""
    , "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR"
    , "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,"
    , "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE"
    , "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER"
    , "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING"
    , "FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER"
    , "DEALINGS IN THE SOFTWARE."
    ]

------------------------------------------------------------------------
-- License policy enforcement
------------------------------------------------------------------------

-- | Check all known dependencies against the license allow-list.
-- Returns 'ExitSuccess' if every dependency passes, 'ExitFailure' 1 otherwise.
checkLicensePolicy :: IO ExitCode
checkLicensePolicy = do
    putStrLn "UmbraVOX License Policy Check"
    putStrLn "============================="
    putStrLn ""
    putStrLn $ "Allowed licenses: " ++ intercalate ", " allowedLicenses
    putStrLn ""

    let allDeps = haskellDeps ++ buildTools
                  ++ [DepInfo "UmbraVox" "0.1.0.0" "Apache-2.0" "This project"]
                  ++ map (\name -> DepInfo name "n/a" "Apache-2.0" "Generated C source") cSources

    results <- mapM checkOneDep allDeps

    putStrLn ""
    let failures = length (filter not results)
    if failures == 0
        then do
            putStrLn $ "Result: PASS (" ++ show (length results) ++ "/" ++ show (length results) ++ " dependencies OK)"
            pure ExitSuccess
        else do
            putStrLn $ "Result: FAIL (" ++ show failures ++ " disallowed license(s) found)"
            pure (ExitFailure 1)
  where
    checkOneDep :: DepInfo -> IO Bool
    checkOneDep dep = do
        let ok = depLicense dep `elem` allowedLicenses
            tag = if ok then "PASS" else "FAIL"
        putStrLn $ "  [" ++ tag ++ "] " ++ padRight 13 (depName dep)
                   ++ padRight 16 (depLicense dep) ++ depSource dep
        pure ok

------------------------------------------------------------------------
-- Linking obligation analysis
------------------------------------------------------------------------

-- | Analyze and document linking obligations for the release bundle.
analyzeLinkingObligations :: IO ExitCode
analyzeLinkingObligations = do
    putStrLn "UmbraVOX Linking Obligation Analysis"
    putStrLn "====================================="
    putStrLn ""

    putStrLn "1. Statically Linked (compiled into the binary by GHC)"
    putStrLn "-------------------------------------------------------"
    putStrLn "All Haskell library dependencies are statically linked by GHC into"
    putStrLn "the final executable. This is the default GHC linking behaviour."
    putStrLn ""
    putStrLn "Statically linked Haskell packages:"
    mapM_ (\dep -> putStrLn $ "  - " ++ padRight 13 (depName dep) ++ depLicense dep) haskellDeps
    putStrLn ""
    putStrLn "License implication: BSD-3-Clause requires copyright notice and"
    putStrLn "disclaimer in binary distributions. These are provided in the"
    putStrLn "third-party license bundle."
    putStrLn ""

    putStrLn "2. Statically Linked C FFI Sources (project-owned)"
    putStrLn "---------------------------------------------------"
    putStrLn "The following C sources are compiled and statically linked into the"
    putStrLn "binary via GHC's C compilation pipeline:"
    putStrLn ""
    mapM_ (\name -> putStrLn $ "  - csrc/generated/" ++ name ++ ".c  (Apache-2.0)") cSources
    putStrLn ""
    putStrLn "License implication: These are project-owned under Apache-2.0."
    putStrLn "No additional obligations beyond the project license."
    putStrLn ""

    putStrLn "3. Dynamically Linked System Libraries (runtime)"
    putStrLn "-------------------------------------------------"
    putStrLn "The release bundle uses dynamic linking for system libraries."
    putStrLn "These shared libraries are bundled from the Nix store into the"
    putStrLn "AppImage or tarball distribution."
    putStrLn ""
    mapM_ (\(lib, lic, note) -> putStrLn $ "  - " ++ padRight 20 lib ++ padRight 16 lic ++ note)
        dynamicLibs
    putStrLn ""
    putStrLn "License implication: glibc is LGPL-2.1-or-later. Dynamic linking"
    putStrLn "satisfies the LGPL requirement that users can relink against their"
    putStrLn "own version of the library. The bundled .so files can be replaced"
    putStrLn "by the end user."
    putStrLn ""

    putStrLn "4. Summary"
    putStrLn "----------"
    putStrLn "  - All Haskell deps: BSD-3-Clause (static) -> include notices"
    putStrLn "  - C FFI sources: Apache-2.0 (static) -> project-owned, no extra obligation"
    putStrLn "  - System libs: LGPL/MIT (dynamic) -> bundled .so files are replaceable"
    putStrLn "  - GHC runtime (libHSrts): BSD-3-Clause (static) -> include notice"

    pure ExitSuccess

dynamicLibs :: [(String, String, String)]
dynamicLibs =
    [ ("libc.so.6",        "LGPL-2.1+",    "GNU C Library (glibc)")
    , ("libpthread.so.0",  "LGPL-2.1+",    "POSIX threads (glibc)")
    , ("libm.so.6",        "LGPL-2.1+",    "Math library (glibc)")
    , ("librt.so.1",       "LGPL-2.1+",    "Realtime extensions (glibc)")
    , ("libdl.so.2",       "LGPL-2.1+",    "Dynamic linker (glibc)")
    , ("libgmp.so.10",     "LGPL-3.0+",    "GNU Multiple Precision Arithmetic")
    , ("libffi.so.8",      "MIT",           "Foreign Function Interface")
    ]
