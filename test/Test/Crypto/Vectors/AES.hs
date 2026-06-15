-- SPDX-License-Identifier: Apache-2.0
-- | NIST SP 800-38A test vectors for AES-256-ECB.
--
-- Vectors are from NIST SP 800-38A, Appendix F.1 (ECB-AES256) and
-- NIST AESAVS (Advanced Encryption Standard Algorithm Validation Suite).
-- Each pair is byte-exact: encrypt and decrypt are tested independently.
module Test.Crypto.Vectors.AES (runTests) where

import Test.Util
import UmbraVox.Crypto.AES (aesEncrypt, aesDecrypt)

runTests :: IO Bool
runTests = do
    putStrLn "[Vectors/AES-256-ECB] Running NIST SP 800-38A encrypt vectors..."
    encResults <- mapM runEncrypt encryptVectors
    putStrLn "[Vectors/AES-256-ECB] Running NIST SP 800-38A decrypt vectors..."
    decResults <- mapM runDecrypt decryptVectors
    let results = encResults ++ decResults
        passed  = length (filter id results)
        total   = length results
    putStrLn $ "[Vectors/AES-256-ECB] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

runEncrypt :: (String, String, String, String) -> IO Bool
runEncrypt (name, keyH, ptH, ctH) =
    assertEq ("Encrypt " ++ name) ctH
        (hexEncode (aesEncrypt (hexDecode keyH) (hexDecode ptH)))

runDecrypt :: (String, String, String, String) -> IO Bool
runDecrypt (name, keyH, ctH, ptH) =
    assertEq ("Decrypt " ++ name) ptH
        (hexEncode (aesDecrypt (hexDecode keyH) (hexDecode ctH)))

-- NIST SP 800-38A Appendix F.1.5/F.1.6: ECB-AES256
-- Format: (name, key, plaintext, ciphertext)
encryptVectors :: [(String, String, String, String)]
encryptVectors =
    [ ( "SP800-38A F.1.5 block 1"
      , "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"
      , "6bc1bee22e409f96e93d7e117393172a"
      , "f3eed1bdb5d2a03c064b5a7e3db181f8"
      )
    , ( "SP800-38A F.1.5 block 2"
      , "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"
      , "ae2d8a571e03ac9c9eb76fac45af8e51"
      , "591ccb10d410ed26dc5ba74a31362870"
      )
    , ( "SP800-38A F.1.5 block 3"
      , "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"
      , "30c81c46a35ce411e5fbc1191a0a52ef"
      , "b6ed21b99ca6f4f9f153e7b1beafed1d"
      )
    , ( "SP800-38A F.1.5 block 4"
      , "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"
      , "f69f2445df4f9b17ad2b417be66c3710"
      , "23304b7a39f9f3ff067d8d8f9e24ecc7"
      )
    ]

-- Inverse of the same blocks: (name, key, ciphertext, expected-plaintext)
decryptVectors :: [(String, String, String, String)]
decryptVectors =
    [ ( "SP800-38A F.1.6 block 1"
      , "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"
      , "f3eed1bdb5d2a03c064b5a7e3db181f8"
      , "6bc1bee22e409f96e93d7e117393172a"
      )
    , ( "SP800-38A F.1.6 block 2"
      , "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"
      , "591ccb10d410ed26dc5ba74a31362870"
      , "ae2d8a571e03ac9c9eb76fac45af8e51"
      )
    , ( "SP800-38A F.1.6 block 3"
      , "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"
      , "b6ed21b99ca6f4f9f153e7b1beafed1d"
      , "30c81c46a35ce411e5fbc1191a0a52ef"
      )
    , ( "SP800-38A F.1.6 block 4"
      , "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"
      , "23304b7a39f9f3ff067d8d8f9e24ecc7"
      , "f69f2445df4f9b17ad2b417be66c3710"
      )
    ]
