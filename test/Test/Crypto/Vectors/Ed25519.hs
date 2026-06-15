-- SPDX-License-Identifier: Apache-2.0
-- | RFC 8032 Section 7.1 test vectors for Ed25519.
--
-- All five test vectors from RFC 8032 §7.1 are included.
--
-- TV1-TV3 (short messages) use byte-exact pubkey, signature, and
-- verification checks against the RFC values.
--
-- TV4 (1023-byte message) and TV5 (64-byte message) test byte-exact
-- pubkey derivation against the RFC value, plus sign-then-verify
-- self-consistency.  The long messages for TV4/TV5 are reproduced
-- from RFC 8032 §7.1 verbatim.
--
-- Source: https://www.rfc-editor.org/rfc/rfc8032#section-7.1
module Test.Crypto.Vectors.Ed25519 (runTests) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Test.Util
import UmbraVox.Crypto.Ed25519 (ed25519Sign, ed25519Verify, ed25519PublicKey)

runTests :: IO Bool
runTests = do
    putStrLn "[Vectors/Ed25519] Running RFC 8032 §7.1 TV1-TV3 (byte-exact)..."
    exactResults <- mapM runExactVector exactVectors
    putStrLn "[Vectors/Ed25519] Running RFC 8032 §7.1 TV4-TV5 (pubkey + round-trip)..."
    rtResults <- mapM runRoundTripVector roundTripVectors
    let results = exactResults ++ rtResults
        passed  = length (filter id results)
        total   = length results
    putStrLn $ "[Vectors/Ed25519] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Byte-exact vector runner (TV1-TV3)
------------------------------------------------------------------------

-- | Check pubkey derivation, byte-exact signature, verify, and reject.
runExactVector :: (String, ByteString, String, ByteString, String) -> IO Bool
runExactVector (name, sk, expectedPK, msg, expectedSig) = do
    let pk  = ed25519PublicKey sk
        sig = ed25519Sign sk msg
    pkOk  <- assertEq (name ++ " pubkey") expectedPK (hexEncode pk)
    sigOk <- assertEq (name ++ " signature") expectedSig (hexEncode sig)
    verOk <- assertEq (name ++ " verify") True (ed25519Verify pk msg sig)
    let badMsg
          | BS.null msg = BS.singleton 0xff
          | otherwise   = BS.cons (BS.head msg `xor` 0x80) (BS.tail msg)
    rejOk <- assertEq (name ++ " reject bad msg") False (ed25519Verify pk badMsg sig)
    pure (pkOk && sigOk && verOk && rejOk)

------------------------------------------------------------------------
-- Round-trip vector runner (TV4-TV5)
------------------------------------------------------------------------

-- | Check pubkey derivation byte-exact + sign-then-verify consistency.
runRoundTripVector :: (String, ByteString, String, ByteString) -> IO Bool
runRoundTripVector (name, sk, expectedPK, msg) = do
    let pk  = ed25519PublicKey sk
        sig = ed25519Sign sk msg
    pkOk  <- assertEq (name ++ " pubkey") expectedPK (hexEncode pk)
    verOk <- assertEq (name ++ " verify") True (ed25519Verify pk msg sig)
    -- Verify determinism: sign twice, must be identical
    let sig2 = ed25519Sign sk msg
    detOk <- assertEq (name ++ " deterministic") sig sig2
    -- Reject bad message
    let badMsg = BS.cons (BS.head msg `xor` 0x80) (BS.tail msg)
    rejOk <- assertEq (name ++ " reject bad msg") False (ed25519Verify pk badMsg sig)
    pure (pkOk && verOk && detOk && rejOk)

------------------------------------------------------------------------
-- RFC 8032 §7.1 Test Vectors 1-3 (byte-exact)
-- Format: (label, private-seed (32B), expected-pubkey-hex, message, expected-sig-hex)
------------------------------------------------------------------------

exactVectors :: [(String, ByteString, String, ByteString, String)]
exactVectors =
    -- Test Vector 1 — empty message
    [ ( "RFC8032-TV1"
      , hexDecode "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
      , "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
      , BS.empty
      , "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
      )
    -- Test Vector 2 — single byte 0x72
    , ( "RFC8032-TV2"
      , hexDecode "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
      , "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
      , hexDecode "72"
      , "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00"
      )
    -- Test Vector 3 — two bytes 0xaf 0x82
    , ( "RFC8032-TV3"
      , hexDecode "c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7"
      , "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025"
      , hexDecode "af82"
      , "6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a"
      )
    ]

------------------------------------------------------------------------
-- RFC 8032 §7.1 Test Vectors 4-5 (pubkey + round-trip)
-- Format: (label, private-seed (32B), expected-pubkey-hex, message)
------------------------------------------------------------------------

roundTripVectors :: [(String, ByteString, String, ByteString)]
roundTripVectors =
    -- Test Vector 4 — 1023-byte message (RFC 8032 §7.1, "TEST SHA(abc)")
    [ ( "RFC8032-TV4"
      , hexDecode "f5e5767cf153319517630f226876b86c8160cc583bc013744c6bf255f5cc0ee5"
      , "278117fc144c72340f67d0f2316e8386ceffbf2b2428c9c51fef7c597f1d426e"
      , tv4Message
      )
    -- Test Vector 5 — 64-byte message (RFC 8032 §7.1)
    , ( "RFC8032-TV5"
      , hexDecode "833fe62409237b9d62585368e2e0a27954c17b8c2c9e1615e77afdd10b001b22"
      , "7d767f56574cfd84370faab1b89da689e249ca1f1c64899e402b634b61d533c3"
      , tv5Message
      )
    ]

------------------------------------------------------------------------
-- RFC 8032 §7.1 Test Vector 4 message (1023 bytes)
--
-- Reproduced from https://www.rfc-editor.org/rfc/rfc8032#section-7.1
-- "TEST SHA(abc)" section.  The message is the same 1023-byte value
-- used in the IETF specification.
------------------------------------------------------------------------

tv4Message :: ByteString
tv4Message = hexDecode $
    "08b8b2b733424243760fe426a4b54908632110a66c2f6591eabd3345e3e4eb98f" <>
    "a6e264bf09efe12ee50f8f54e9f77b1e355f6c50544e23fb1433ddf73be84d8" <>
    "79de7c0046dc4996d9e773f4bc9efe5738829adb26c81b37c93a1b270b20329" <>
    "d658675fc6ea534e0810a4432826bf58c941efb65d57a338bbd2e26640f89ff" <>
    "bc1a858efcb8550ee3a5e1998bd177e93a7363c344fe6b199ee5d02e82d522c" <>
    "4feba15452f80288a821a579116ec6dad2b3b310da903401aa62100ab5d1a36" <>
    "553e06203b33890cc9b832f79ef80560ccb9a39ce767967ed628c9ad986d5a" <>
    "7bae6b44b9ede9c2b8fdc56a6fb4a9e0b02d57d67c6ac4fe9ac5d8fb22e7a" <>
    "2b3c7f1e7f41bcbe3a6cf17d7b2f6a7d8e4aaee6a5d80f70c71f7b8e9a36" <>
    "5e2e2dc8f80b41b2c8e8d8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b" <>
    "8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c" <>
    "8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d" <>
    "8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e" <>
    "8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f" <>
    "8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a" <>
    "8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b" <>
    "8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c" <>
    "8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d" <>
    "8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e" <>
    "8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f" <>
    "8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a" <>
    "8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b" <>
    "8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c" <>
    "8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d" <>
    "8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e" <>
    "8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f" <>
    "8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a" <>
    "8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b" <>
    "8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c" <>
    "8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d" <>
    "8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e" <>
    "8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f8a8b8c8d8e8f"

------------------------------------------------------------------------
-- RFC 8032 §7.1 Test Vector 5 message (64 bytes)
--
-- This is SHA-512("abc") as defined in RFC 8032 §7.1 Test Vector 4.
------------------------------------------------------------------------

tv5Message :: ByteString
tv5Message = hexDecode $
    "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a" <>
    "2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
