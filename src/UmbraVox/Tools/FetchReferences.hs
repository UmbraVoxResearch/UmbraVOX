-- SPDX-License-Identifier: Apache-2.0
-- | Reference PDF downloader for UmbraVOX documentation.
--
-- Downloads freely available reference PDFs (NIST standards, IETF RFCs,
-- IACR ePrints) from their official sources.
--
-- Replaces the former fetch-open-access.sh bash script with pure Haskell.
module UmbraVox.Tools.FetchReferences
    ( -- * Types
      Reference(..)
    , Category(..)
    , FetchResult(..)
    , FetchSummary(..)
      -- * Reference catalog
    , allReferences
    , nistReferences
    , rfcReferences
    , openAccessReferences
    , referencesByCategory
      -- * Downloading
    , fetchReference
    , fetchAll
    , fetchSummary
    ) where

import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

-- | Category of a reference document.
data Category
    = NIST        -- ^ NIST FIPS and Special Publications
    | IETF        -- ^ IETF RFC documents
    | OpenAccess  -- ^ IACR ePrint and open access papers
    deriving stock (Show, Eq, Ord)

-- | A reference document with its download source.
data Reference = Reference
    { refFile     :: FilePath   -- ^ Target filename (e.g. "FIPS-180-4.pdf")
    , refUrl      :: String     -- ^ Download URL
    , refCategory :: Category   -- ^ Document category
    } deriving stock (Show, Eq)

-- | Result of attempting to fetch a single reference.
data FetchResult
    = Skipped   FilePath   -- ^ Already exists
    | Fetched   FilePath   -- ^ Successfully downloaded
    | FetchFail FilePath String  -- ^ Failed with error
    deriving stock (Show, Eq)

-- | Summary of a fetch run.
data FetchSummary = FetchSummary
    { fsTotal    :: Int
    , fsSkipped  :: Int
    , fsFetched  :: Int
    , fsFailed   :: Int
    , fsResults  :: [FetchResult]
    } deriving stock (Show, Eq)

-- | NIST FIPS and SP references.
nistReferences :: [Reference]
nistReferences =
    [ Reference "FIPS-180-4.pdf" "https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf" NIST
    , Reference "FIPS-197.pdf" "https://csrc.nist.gov/files/pubs/fips/197/final/docs/fips-197.pdf" NIST
    , Reference "FIPS-203.pdf" "https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.203.pdf" NIST
    , Reference "FIPS-204.pdf" "https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.204.pdf" NIST
    , Reference "FIPS-205.pdf" "https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.205.pdf" NIST
    , Reference "SP-800-38D.pdf" "https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-38d.pdf" NIST
    , Reference "SP-800-90A.pdf" "https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-90Ar1.pdf" NIST
    ]

-- | IETF RFC references.
rfcReferences :: [Reference]
rfcReferences =
    [ Reference "RFC-1951.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc1951.txt.pdf" IETF
    , Reference "RFC-2104.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc2104.txt.pdf" IETF
    , Reference "RFC-4033.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc4033.txt.pdf" IETF
    , Reference "RFC-4034.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc4034.txt.pdf" IETF
    , Reference "RFC-4035.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc4035.txt.pdf" IETF
    , Reference "RFC-5869.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc5869.txt.pdf" IETF
    , Reference "RFC-6177.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc6177.txt.pdf" IETF
    , Reference "RFC-7748.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc7748.txt.pdf" IETF
    , Reference "RFC-8032.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc8032.txt.pdf" IETF
    , Reference "RFC-8439.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc8439.txt.pdf" IETF
    , Reference "RFC-8446.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc8446.txt.pdf" IETF
    , Reference "RFC-8452.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc8452.txt.pdf" IETF
    , Reference "RFC-8949.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc8949.txt.pdf" IETF
    , Reference "RFC-9106.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc9106.txt.pdf" IETF
    , Reference "RFC-9381.pdf" "https://www.rfc-editor.org/rfc/pdfrfc/rfc9381.txt.pdf" IETF
    ]

-- | Open access papers.
openAccessReferences :: [Reference]
openAccessReferences =
    [ Reference "Bernstein-2005-CacheTimingAES.pdf" "https://cr.yp.to/antiforgery/cachetiming-20050414.pdf" OpenAccess
    , Reference "Bernstein-2006-Curve25519.pdf" "https://cr.yp.to/ecdh/curve25519-20060209.pdf" OpenAccess
    , Reference "Bernstein-2008-ChaCha.pdf" "https://cr.yp.to/chacha/chacha-20080128.pdf" OpenAccess
    , Reference "Perrin-2018-Noise.pdf" "https://noiseprotocol.org/noise.pdf" OpenAccess
    ]

-- | All references combined.
allReferences :: [Reference]
allReferences = nistReferences ++ rfcReferences ++ openAccessReferences

-- | Filter references by category.
referencesByCategory :: Category -> [Reference]
referencesByCategory cat = filter (\r -> refCategory r == cat) allReferences

-- | Fetch a single reference using curl. Skips if file already exists.
fetchReference :: FilePath -> Reference -> IO FetchResult
fetchReference dir ref = do
    let target = dir </> refFile ref
    exists <- doesFileExist target
    if exists
        then pure (Skipped (refFile ref))
        else do
            (exitCode, _stdout, stderr) <- readProcessWithExitCode
                "curl"
                ["-sSfL", "-o", target, refUrl ref]
                ""
            case exitCode of
                ExitSuccess   -> pure (Fetched (refFile ref))
                ExitFailure _ -> pure (FetchFail (refFile ref) stderr)

-- | Fetch all references into the given directory.
fetchAll :: FilePath -> [Reference] -> IO [FetchResult]
fetchAll dir = mapM (fetchReference dir)

-- | Build a summary from fetch results.
fetchSummary :: [FetchResult] -> FetchSummary
fetchSummary results = FetchSummary
    { fsTotal   = length results
    , fsSkipped = length [() | Skipped _   <- results]
    , fsFetched = length [() | Fetched _   <- results]
    , fsFailed  = length [() | FetchFail _ _ <- results]
    , fsResults = results
    }
