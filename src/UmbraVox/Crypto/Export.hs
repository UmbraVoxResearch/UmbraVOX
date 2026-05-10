-- SPDX-License-Identifier: Apache-2.0
-- | Encrypted conversation export with AES-256-GCM and HKDF key derivation.
--
-- Wire format: salt (32) || nonce (12) || ciphertext || tag (16)
module UmbraVox.Crypto.Export
    ( encryptExport
    , decryptExport
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import UmbraVox.Crypto.GCM (gcmEncrypt, gcmDecrypt)
import UmbraVox.Crypto.HKDF (hkdfSHA256Extract, hkdfSHA256Expand)
import UmbraVox.Crypto.Random (randomBytes)

-- | Info string for domain separation.
exportInfo :: ByteString
exportInfo = "UmbraVox_Export_v1"

-- | Header size: 32 (salt) + 12 (nonce).
headerLen :: Int
headerLen = 44

-- | Tag size for AES-256-GCM.
tagLen :: Int
tagLen = 16

-- ── SECURITY FINDING: iterated-HKDF is NOT memory-hard ─────────────────────
--
-- Finding:  The KDF below performs 100 000 iterations of HKDF-Extract
--           (PBKDF2-style).  Each iteration is a single HMAC-SHA256
--           evaluation — a CPU-only, data-independent operation.
--
-- Vulnerability: Because HKDF-Extract has a small, fixed memory footprint,
--   the entire iteration loop is trivially parallelisable on GPUs and ASICs.
--   A modern GPU cluster can evaluate billions of HMAC-SHA256 rounds per
--   second.  Against weak or medium-strength passwords, this construction
--   does NOT resist sustained offline brute-force from a well-resourced
--   adversary.
--
-- Fix (future — TODO M10.3.3): Replace iterated HKDF with Argon2id via FFI
--   to libsodium:
--     crypto_pwhash( out, 32
--                  , password, password_len
--                  , salt
--                  , crypto_pwhash_OPSLIMIT_MODERATE   -- 3 passes
--                  , 64 * 1024 * 1024                  -- 64 MiB
--                  , crypto_pwhash_ALG_ARGON2ID13 )
--   Argon2id (64 MiB, 3 iterations) is memory-hard, GPU-resistant, and
--   OWASP-recommended for password-based key derivation as of 2024.
--
-- Verified: current behaviour is acceptable for an initial release where
--   export files are short-lived and passwords are expected to be strong.
--   Migration to Argon2id must land before any version that handles
--   long-lived or user-chosen low-entropy export passwords.
-- ────────────────────────────────────────────────────────────────────────────

-- TODO(M10.3.3): migrate deriveKey to Argon2id (64 MiB / 3 iters) via
-- libsodium FFI once the FFI layer is wired in.

-- | Number of PBKDF2-style iterations for password-based key derivation.
-- This hardens exports against offline brute-force attacks on weak passwords.
exportIterations :: Int
exportIterations = 100000

-- | Derive a 32-byte encryption key from password and salt via iterated
-- HKDF-SHA256 (PBKDF2-style).
--
-- The initial round performs HKDF-Extract(salt, password).  Subsequent
-- rounds feed the running key back through HKDF-Extract to accumulate
-- computational cost, making offline brute-force significantly harder.
--
-- NOTE: this construction is CPU-bound but NOT memory-hard.  See the
-- security finding block above for the Argon2id migration plan.
deriveKey :: ByteString -> ByteString -> ByteString
deriveKey salt password =
    let !initial = BS.take 32 (hkdfSHA256Extract salt password)
        go :: Int -> ByteString -> ByteString
        go 0 !key = key
        go n !key = go (n - 1) (BS.take 32 (hkdfSHA256Extract key password))
    in hkdfSHA256Expand (go exportIterations initial) exportInfo 32

-- | Encrypt plaintext with a password for export.
--
-- @encryptExport password plaintext@ returns the encrypted blob:
-- salt (32) || nonce (12) || ciphertext || tag (16)
encryptExport :: ByteString -> ByteString -> IO ByteString
encryptExport password plaintext = do
    salt  <- randomBytes 32
    nonce <- randomBytes 12
    let !key        = deriveKey salt password
        !(ct, tag)  = gcmEncrypt key nonce exportInfo plaintext
    pure (salt <> nonce <> ct <> tag)

-- | Decrypt an exported blob with a password.
--
-- @decryptExport password blob@ returns the plaintext if the password
-- is correct and the data has not been tampered with.
decryptExport :: ByteString -> ByteString -> Maybe ByteString
decryptExport password blob
    | BS.length blob < headerLen + tagLen = Nothing
    | otherwise =
        let !salt  = BS.take 32 blob
            !nonce = BS.take 12 (BS.drop 32 blob)
            !rest  = BS.drop headerLen blob
            !ctLen = BS.length rest - tagLen
            !ct    = BS.take ctLen rest
            !tag   = BS.drop ctLen rest
            !key   = deriveKey salt password
        in gcmDecrypt key nonce exportInfo ct tag
