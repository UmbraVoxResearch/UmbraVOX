-- | Noise protocol types, constants, and nonce management.
module UmbraVox.Network.Noise.State
  ( NoiseState(..)
  , prologue
  , protocolName
  , macLen
  , makeNonce
  , packASCII
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word64)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Post-handshake session state for encrypting/decrypting messages.
-- Uses separate keys for encryption and MAC (key separation).
data NoiseState = NoiseState
    { nsSendEncKey :: !ByteString   -- ^ 32-byte ChaCha20 key for sending
    , nsSendMacKey :: !ByteString   -- ^ 32-byte HMAC key for send authentication
    , nsRecvEncKey :: !ByteString   -- ^ 32-byte ChaCha20 key for receiving
    , nsRecvMacKey :: !ByteString   -- ^ 32-byte HMAC key for recv authentication
    , nsSendN      :: !Word64       -- ^ Send nonce counter
    , nsRecvN      :: !Word64       -- ^ Recv nonce counter
    } deriving (Show)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | Protocol prologue, mixed into the handshake hash.
prologue :: ByteString
prologue = packASCII "UmbraVox_v1"

-- | Protocol name for the initial hash.
protocolName :: ByteString
protocolName = packASCII "Noise_IK_25519_ChaChaPoly_SHA256"

-- | HMAC tag length appended to each encrypted message.
macLen :: Int
macLen = 32

------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------

-- | Build a 12-byte nonce: 4 zero bytes followed by 8-byte LE counter.
makeNonce :: Word64 -> ByteString
makeNonce !n = BS.pack
    [ 0, 0, 0, 0
    , w8 n 0, w8 n 1, w8 n 2, w8 n 3
    , w8 n 4, w8 n 5, w8 n 6, w8 n 7
    ]
  where
    w8 :: Word64 -> Int -> Word8
    w8 !v !i = fromIntegral (shiftR v (8 * i) .&. 0xff)

-- | Convert an ASCII string literal to ByteString.
packASCII :: String -> ByteString
packASCII = BS.pack . map (fromIntegral . fromEnum)
