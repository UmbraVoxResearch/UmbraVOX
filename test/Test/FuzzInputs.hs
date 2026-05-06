{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- | Comprehensive input fuzzing for all UmbraVOX parsers and handlers.
--
-- Tests that parsers never crash on arbitrary input and that serializers
-- correctly round-trip.  All tests use deterministic PRNG (seed 42).
module Test.FuzzInputs (runTests) where

import Control.Exception (SomeException, evaluate, try)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word32)
import qualified Network.Socket as NS

import Test.Util

import UmbraVox.Protocol.Encoding
    ( parseHostPort, splitOn, putWord32BE, getWord32BE
    , safeReadPort
    )
import UmbraVox.Protocol.CBOR (encodeMessage, decodeMessage)
import UmbraVox.Chat.Wire (decodeWire)
import UmbraVox.TUI.Handshake (deserializeBundle)
import UmbraVox.Network.MDNS (parseAnnouncement, buildAnnouncement)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

iterations :: Int
iterations = 1000

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

runTests :: IO Bool
runTests = do
    putStrLn "[FuzzInputs] Running input fuzz tests..."
    results <- sequence
        [ fuzzParseHostPort
        , fuzzSplitOn
        , fuzzSafeReadPort
        , fuzzDecodeWire
        , fuzzDeserializeBundle
        , fuzzDecodeMessage
        , fuzzParseAnnouncement
        , fuzzBuildParseAnnouncement
        , fuzzWord32RoundTrip
        , fuzzEncodeDecodeMessage
        ]
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "[FuzzInputs] " ++ show passed ++ "/" ++ show total ++ " passed."
    pure (and results)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Generate a random printable ASCII string (chars 32-126) of length 0-maxLen.
nextPrintableString :: Int -> PRNG -> (String, PRNG)
nextPrintableString maxLen g0 =
    let (w, g1) = nextWord32 g0
        len = fromIntegral (w `mod` fromIntegral (maxLen + 1))
    in  go len g1 []
  where
    go 0 g acc = (reverse acc, g)
    go !n g acc =
        let (b, g') = nextWord8 g
            ch = toEnum (32 + fromIntegral b `mod` 95)  -- printable ASCII
        in  go (n - 1) g' (ch : acc)

-- | Build a random SockAddr (IPv4) from PRNG.
nextSockAddr :: PRNG -> (NS.SockAddr, PRNG)
nextSockAddr g =
    let (w1, g1) = nextWord32 g
        (w2, g2) = nextWord32 g1
        port = fromIntegral (w1 `mod` 65536) :: NS.PortNumber
        hostAddr = NS.tupleToHostAddress
            ( fromIntegral (w2 `mod` 256)
            , fromIntegral ((w2 `shiftL` 8) `mod` 256)
            , fromIntegral ((w2 `shiftL` 16) `mod` 256)
            , fromIntegral ((w2 `shiftL` 24) `mod` 256)
            )
    in  (NS.SockAddrInet port hostAddr, g2)

-- | Try evaluating a value; return True if no exception is thrown.
noCrash :: forall a. a -> IO Bool
noCrash x = do
    (result :: Either SomeException a) <- try (evaluate x)
    case result of
        Left _  -> pure False
        Right _ -> pure True

-- | Convert PRNG Word32 to Word32 (identity, for readability).
prngWord32 :: PRNG -> (Word32, PRNG)
prngWord32 = nextWord32

------------------------------------------------------------------------
-- 1. Fuzz parseHostPort: 1000 random strings -> never crashes
------------------------------------------------------------------------

fuzzParseHostPort :: IO Bool
fuzzParseHostPort = checkPropertyIO
    "parseHostPort: never crashes on random input" iterations prop
  where
    prop g = do
        let (s, _) = nextPrintableString 100 g
        noCrash (parseHostPort s)

------------------------------------------------------------------------
-- 2. Fuzz splitOn: 1000 random strings -> never crashes, non-empty
------------------------------------------------------------------------

fuzzSplitOn :: IO Bool
fuzzSplitOn = checkPropertyIO
    "splitOn: never crashes, always non-empty" iterations prop
  where
    prop g = do
        let (w, g1) = nextWord32 g
            delim = toEnum (fromIntegral (w `mod` 128)) :: Char
            (s, _) = nextPrintableString 100 g1
            result = splitOn delim s
        ok1 <- noCrash result
        -- Force the spine of the list to check non-empty
        ok2 <- noCrash (length result)
        pure (ok1 && ok2 && not (null result))

------------------------------------------------------------------------
-- 3. Fuzz safeReadPort: 1000 random strings -> always returns Int
------------------------------------------------------------------------

fuzzSafeReadPort :: IO Bool
fuzzSafeReadPort = checkPropertyIO
    "safeReadPort: never crashes on random input" iterations prop
  where
    prop g = do
        let (s, _) = nextPrintableString 100 g
            result = safeReadPort s
        noCrash (result `seq` result)

------------------------------------------------------------------------
-- 4. Fuzz decodeWire: 1000 random ByteStrings -> Nothing or Just
------------------------------------------------------------------------

fuzzDecodeWire :: IO Bool
fuzzDecodeWire = checkPropertyIO
    "decodeWire: never crashes on random bytes" iterations prop
  where
    prop g = do
        let (bs, _) = nextBytesRange 0 200 g
        result <- try (evaluate (decodeWire bs)) :: IO (Either SomeException (Maybe _))
        case result of
            Left _  -> pure False
            Right _ -> pure True

------------------------------------------------------------------------
-- 5. Fuzz deserializeBundle: 1000 random ByteStrings -> Nothing or Just
------------------------------------------------------------------------

fuzzDeserializeBundle :: IO Bool
fuzzDeserializeBundle = checkPropertyIO
    "deserializeBundle: never crashes on random bytes" iterations prop
  where
    prop g = do
        let (bs, _) = nextBytesRange 0 500 g
        result <- try (evaluate (deserializeBundle bs)) :: IO (Either SomeException (Maybe _))
        case result of
            Left _  -> pure False
            Right _ -> pure True

------------------------------------------------------------------------
-- 6. Fuzz decodeMessage: 1000 random ByteStrings -> Nothing or Just
------------------------------------------------------------------------

fuzzDecodeMessage :: IO Bool
fuzzDecodeMessage = checkPropertyIO
    "decodeMessage: never crashes on random bytes" iterations prop
  where
    prop g = do
        let (bs, _) = nextBytesRange 0 100 g
        result <- try (evaluate (decodeMessage bs))
                      :: IO (Either SomeException (Maybe (ByteString, ByteString)))
        case result of
            Left _  -> pure False
            Right _ -> pure True

------------------------------------------------------------------------
-- 7. Fuzz parseAnnouncement: 1000 random ByteStrings + SockAddr
------------------------------------------------------------------------

fuzzParseAnnouncement :: IO Bool
fuzzParseAnnouncement = checkPropertyIO
    "parseAnnouncement: never crashes on random bytes" iterations prop
  where
    prop g = do
        let (bs, g1) = nextBytesRange 0 200 g
            (addr, _) = nextSockAddr g1
        result <- try (evaluate (parseAnnouncement bs addr))
                      :: IO (Either SomeException (Maybe _))
        case result of
            Left _  -> pure False
            Right _ -> pure True

------------------------------------------------------------------------
-- 8. Fuzz buildAnnouncement/parseAnnouncement round-trip
------------------------------------------------------------------------

fuzzBuildParseAnnouncement :: IO Bool
fuzzBuildParseAnnouncement = checkPropertyIO
    "buildAnnouncement/parseAnnouncement round-trip" 500 prop
  where
    prop g = do
        let (w, g1) = nextWord32 g
            port = fromIntegral (w `mod` 65535) + 1
            (pubkey, _) = nextBytes 32 g1
            payload = buildAnnouncement port pubkey
            addr = NS.SockAddrInet 5353
                       (NS.tupleToHostAddress (192, 168, 1, 42))
        result <- try (evaluate (parseAnnouncement payload addr))
                      :: IO (Either SomeException (Maybe _))
        case result of
            Left _       -> pure False
            Right Nothing -> pure False
            Right (Just _) -> pure True

------------------------------------------------------------------------
-- 9. Fuzz putWord32BE/getWord32BE round-trip
------------------------------------------------------------------------

fuzzWord32RoundTrip :: IO Bool
fuzzWord32RoundTrip = checkProperty
    "putWord32BE/getWord32BE round-trip" iterations prop
  where
    prop g =
        let (w, _) = prngWord32 g
            encoded = putWord32BE w
            decoded = getWord32BE encoded
        in  decoded == w && BS.length encoded == 4

------------------------------------------------------------------------
-- 10. Fuzz encodeMessage/decodeMessage round-trip
------------------------------------------------------------------------

fuzzEncodeDecodeMessage :: IO Bool
fuzzEncodeDecodeMessage = checkProperty
    "encodeMessage/decodeMessage round-trip" 500 prop
  where
    prop g =
        let (payload, _) = nextBytesRange 0 500 g
            encoded = encodeMessage payload
        in  case decodeMessage encoded of
                Just (decoded, remaining) ->
                    decoded == payload && BS.null remaining
                Nothing -> False
