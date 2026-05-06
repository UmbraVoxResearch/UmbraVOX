-- SPDX-License-Identifier: Apache-2.0
-- | Poly1305 test suite: RFC 8439 Section 2.5.2 KAT vector.
module Test.Crypto.Poly1305 (runTests) where

import Test.Util
import UmbraVox.Crypto.Poly1305 (poly1305)

runTests :: IO Bool
runTests = do
    putStrLn "[Poly1305] Running RFC 8439 KAT vector..."
    katResults <- sequence [testRFC8439]
    let passed = length (filter id katResults)
        total  = length katResults
    putStrLn $ "[Poly1305] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and katResults)

-- RFC 8439 Section 2.5.2
testRFC8439 :: IO Bool
testRFC8439 = do
    let key = hexDecode "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"
        msg = strToBS "Cryptographic Forum Research Group"
        tag = poly1305 key msg
    assertEq "RFC 8439 2.5.2 tag" "a8061dc1305136c6c22b8baf0c0127a9" (hexEncode tag)
