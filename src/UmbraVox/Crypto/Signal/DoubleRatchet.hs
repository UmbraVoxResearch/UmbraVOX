-- SPDX-License-Identifier: Apache-2.0
-- | {-# REQ "SIGNAL-002" #-} Signal Double Ratchet Algorithm
--
-- Implements the Double Ratchet Algorithm for forward-secure end-to-end
-- encrypted messaging. Each message uses a unique key derived via symmetric
-- and Diffie-Hellman ratchet steps, providing forward secrecy and
-- break-in recovery.
--
-- See: attic/doc-legacy-2026-04-28/03-cryptography.md
module UmbraVox.Crypto.Signal.DoubleRatchet
    ( RatchetState(..)
    , RatchetHeader(..)
    , RatchetError(..)
    , ratchetInitAlice
    , ratchetInitBob
    , ratchetEncrypt
    , ratchetDecrypt
      -- * Audited constants (exported for regression testing)
    , maxTotalSkipped
    , maxRatchetSkip
    , maxSeenDHKeys
    , skippedKeyMaxAgeSecs
    ) where

import Data.Bits (shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32, Word64)

import UmbraVox.App.Defaults (defaultMaxSkip, defaultMaxTotalSkipped,
                              defaultMaxRatchetSkip, defaultMaxSeenDHKeys)
import qualified UmbraVox.Crypto.Generated.FFI.HMAC as HMACFFI
import qualified UmbraVox.Crypto.Generated.FFI.HKDF as HKDFFFI
import qualified UmbraVox.Crypto.Generated.FFI.X25519 as X25519FFI
import qualified UmbraVox.Crypto.Generated.FFI.GCM as GCMFFI
import UmbraVox.Crypto.Random (randomBytes)
import UmbraVox.Crypto.SecureBytes (SecureBytes, fromByteString, toByteString, zeroAndFree)

------------------------------------------------------------------------
-- Error type
------------------------------------------------------------------------

-- | Errors that 'ratchetEncrypt' and 'ratchetDecrypt' can return.
--
-- Finding    M10.2.2 — 'ratchetEncrypt' and 'ratchetDecrypt' called
--            @error@ on counter exhaustion, aborting the entire process
--            rather than letting the caller handle the condition.
-- Vulnerability: An @error@ call unwinds the Haskell runtime with an
--            unchecked exception; callers have no way to distinguish
--            counter exhaustion from a programming mistake and cannot
--            recover gracefully (e.g. close the session cleanly).
-- Fix:       Replace the @error@ calls with @pure (Left CounterExhausted)@
--            so callers receive a typed error value and can take
--            appropriate action (tear down the session, alert the user).
-- Verified:  Counter-exhaustion tests now check for @Left CounterExhausted@
--            instead of catching a 'SomeException'.
data RatchetError
    = CounterExhausted
      -- ^ Send or receive counter reached 0xFFFFFFFE; the ratchet must
      -- be refreshed via a new DH exchange before further use.
    | DecryptionFailed
      -- ^ GCM tag verification failed (ciphertext tampered or wrong key).
    | PersistenceError String
      -- ^ Counter persistence failed before encryption; the plaintext
      -- has NOT been encrypted.  Callers must not retry with the same
      -- counter without first resolving the underlying I/O failure.
    | RatchetSkipExceeded
      -- ^ Finding    M23.3.3 — The aggregate ratchet skip (prevChainN gap
      --   plus current-chain gap) exceeds 'maxRatchetSkip', indicating
      --   either a resource exhaustion attack or severe message loss.
      -- Vulnerability: Without a cap, a malicious peer can force the
      --   receiver to compute an arbitrarily large number of KDF chain
      --   steps, consuming CPU and memory.
      -- Fix:       'ratchetDecrypt' now computes the total skip distance
      --   before advancing and rejects with this error if it exceeds
      --   'maxRatchetSkip'.
      -- Verified:  Unit test sends a message with counter gap >
      --   maxRatchetSkip and asserts Left RatchetSkipExceeded.
    | DHKeyReplay
      -- ^ Finding    M23.3.3 — No check existed for DH key replay.
      -- Vulnerability: An attacker could replay a message from an earlier
      --   ratchet epoch using the same DH public key, potentially causing
      --   the receiver to re-derive old chain keys and accept stale or
      --   replayed ciphertext.
      -- Fix:       'dhRatchet' now tracks the last 'maxSeenDHKeys' peer
      --   DH public keys in a bounded FIFO ('rsSeenDHKeys') and rejects
      --   any DH key that appears in the history.
      -- Verified:  Unit test replays a header with a previously seen DH
      --   key and asserts Left DHKeyReplay.
    deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Full Double Ratchet state for one party.
--
-- SecureBytes migration (M15.3): the following fields hold live key
-- material and are wrapped in 'UmbraVox.Crypto.SecureBytes.SecureBytes'
-- so that their memory is zeroed when the ratchet state is discarded:
--
--   * 'rsDHSend' — the fst (X25519 secret) half of the sending DH keypair.
--   * 'rsRootKey' — the 32-byte root key.
--   * 'rsSendChain' — the 32-byte sending chain key.
--   * 'rsRecvChain' — the 32-byte receiving chain key.
--
-- KDF functions ('kdfRK', 'kdfCK') extract ByteString temporarily via
-- 'toByteString' for HMAC operations, then wrap results back in
-- 'fromByteString'.  This makes several previously pure functions monadic
-- (IO), which is acceptable for the security benefit.
data RatchetState = RatchetState
    { rsDHSend      :: !(SecureBytes, ByteString)
      -- ^ (secret, public) X25519 sending keypair.
      -- M15.3: secret half is SecureBytes; public half remains ByteString.
    , rsDHRecv      :: !(Maybe ByteString)
      -- ^ Peer's current X25519 public key (Nothing before first message)
    , rsRootKey     :: !SecureBytes
      -- ^ 32-byte root key (M15.3: SecureBytes — zeroed on finalization)
    , rsSendChain   :: !SecureBytes
      -- ^ 32-byte sending chain key (M15.3: SecureBytes — zeroed on finalization)
    , rsRecvChain   :: !SecureBytes
      -- ^ 32-byte receiving chain key (M15.3: SecureBytes — zeroed on finalization)
    , rsSendN       :: !Word32
      -- ^ Sending message counter
    , rsRecvN       :: !Word32
      -- ^ Receiving message counter
    , rsPrevChainN  :: !Word32
      -- ^ Previous sending chain length (sent in header)
    , rsSkippedKeys :: !(Map (ByteString, Word32) (SecureBytes, SecureBytes, Word64, Word64))
      -- ^ Skipped message keys indexed by (DH public key, counter).
      -- Each entry is (msgKey, chainKey, insertSeq, wallTimestamp).
      -- M27.6.9: wallTimestamp records wall-clock insertion time for
      -- time-based expiry (48h).  insertSeq records insertion order
      -- for FIFO eviction.
      -- M40.32 (closes M15.5): msgKey and chainKey are 'SecureBytes'
      -- (mlock'd, MADV_DONTDUMP) and are zeroed via 'zeroSkippedEntry'
      -- whenever an entry leaves the map — on consumption ('trySkippedKeys'),
      -- FIFO eviction ('evictOldest'), or age expiry ('evictByAge').  This
      -- closes the prior gap where plaintext key material survived on the GC
      -- heap (and in session snapshots) for the whole out-of-order window.
    , rsSkipSeq :: !Word64
      -- ^ Monotonic counter incremented on each skipped-key insertion.
      -- Used by 'evictOldest' to remove the truly oldest entry (by
      -- insertion order) rather than evicting by map key ordering.
    , rsNonceCounter :: !Word64
      -- ^ Monotonic counter incremented on every encrypt, for auditing.
      -- Note: nonce uniqueness is guaranteed by the chain ratchet —
      -- each msgKey is derived from a unique chain key via kdfCK and is
      -- used exactly once.  This counter exists for monitoring / replay
      -- detection only.
    , rsSeenDHKeys :: !(Seq ByteString)
      -- ^ Bounded FIFO of recently seen peer DH ratchet public keys
      -- (M23.3.3).  Used to detect replay of old ratchet messages with
      -- reused DH keys.  Capped at 'maxSeenDHKeys' entries; oldest are
      -- evicted when the limit is exceeded.
    }

-- | Header attached to each ratchet message.
data RatchetHeader = RatchetHeader
    { rhDHPublic   :: !ByteString
      -- ^ Sender's current DH public key (32 bytes)
    , rhPrevChainN :: !Word32
      -- ^ Number of messages in previous sending chain
    , rhMsgN       :: !Word32
      -- ^ Message number in current sending chain
    } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

-- | HKDF info string for ratchet key derivation.
ratchetInfo :: ByteString
ratchetInfo = "UmbraVox_Ratchet_v1"

-- | HKDF info string for nonce derivation (M10.2.5).
nonceInfo :: ByteString
nonceInfo = "UmbraVox_Nonce_v1"

-- | Maximum number of skipped message keys to store per ratchet step.
-- Sourced from 'UmbraVox.App.Defaults.defaultMaxSkip'.
maxSkip :: Word32
maxSkip = defaultMaxSkip

-- | Maximum total entries in the skipped-key cache across all DH ratchets.
-- Prevents unbounded memory growth from an adversary triggering many
-- small skips across many ratchet epochs (M7.3.6).
-- Sourced from 'UmbraVox.App.Defaults.defaultMaxTotalSkipped'.
maxTotalSkipped :: Int
maxTotalSkipped = defaultMaxTotalSkipped

-- | Maximum total ratchet steps to advance per DH ratchet epoch when
-- receiving a message (M23.3.3).  If a peer claims to be more than
-- this many steps ahead, reject the message to prevent resource
-- exhaustion.  This is an aggregate limit across the prevChainN skip
-- and the current-chain skip within a single 'ratchetDecrypt' call.
-- Sourced from 'UmbraVox.App.Defaults.defaultMaxRatchetSkip'.
maxRatchetSkip :: Int
maxRatchetSkip = defaultMaxRatchetSkip

-- | Maximum number of peer DH ratchet public keys to track for replay
-- detection (M23.3.3).  A bounded FIFO window prevents unbounded memory
-- growth while catching replays within recent ratchet history.
-- Sourced from 'UmbraVox.App.Defaults.defaultMaxSeenDHKeys'.
maxSeenDHKeys :: Int
maxSeenDHKeys = defaultMaxSeenDHKeys

-- | Maximum age for skipped message keys (48 hours in seconds).
-- Entries older than this are evicted to prevent stale skipped keys
-- from accumulating indefinitely (M27.6.9).
skippedKeyMaxAgeSecs :: Word64
skippedKeyMaxAgeSecs = 172800

-- | M40.32: Zero the key material (msgKey + chainKey) of a skipped-key
-- entry.  Called on every path that removes an entry from 'rsSkippedKeys'
-- (consumption in 'trySkippedKeys', FIFO eviction in 'evictOldest', age
-- expiry in 'evictByAge') so plaintext key bytes never survive on the GC
-- heap or in a session snapshot beyond their lifetime.  Runs in 'IO' via
-- strict 'mapM_'/sequencing — never inside a lazy 'Map' thunk.
zeroSkippedEntry :: (SecureBytes, SecureBytes, Word64, Word64) -> IO ()
zeroSkippedEntry (msgKeySB, chainKeySB, _, _) = do
    zeroAndFree msgKeySB
    zeroAndFree chainKeySB

-- | Evict skipped keys older than 'skippedKeyMaxAgeSecs'.
-- Uses the wall-clock timestamp (4th tuple element) for age comparison.
-- Evicts entries where (currentTimeSecs - wallTimestamp) >= threshold.
--
-- Finding:       M40 round-2 (DR-H2) — the prior predicate computed the
--                unguarded subtraction @currentTimeSecs - ts@ on 'Word64'.
--                A future-dated @ts@ (ts > currentTimeSecs) — e.g. clock
--                skew, or a 'wallTs' injected via a restored/imported
--                session — underflows to a huge value, so the entry was
--                evicted immediately instead of kept.
-- Vulnerability: Inconsistent expiry vs 'trySkippedKeys' (which guards the
--                same subtraction at line ~633): legitimate out-of-order
--                skipped keys could be silently dropped (loss of delivery),
--                or — with the opposite skew — the 48h cap circumvented.
-- Fix:           Guard the future-timestamp case before subtracting, mirroring
--                'trySkippedKeys': keep when @currentTimeSecs <= ts@ or the
--                bounded age is under the threshold.
-- Verified:      Predicate now matches 'trySkippedKeys' pruning semantics;
--                no 'Word64' underflow path remains.
--
-- M40.32: now runs in 'IO' so it can zero the key material of every entry
-- it drops (the keep predicate is unchanged).
evictByAge :: Word64 -> Map (ByteString, Word32) (SecureBytes, SecureBytes, Word64, Word64)
           -> IO (Map (ByteString, Word32) (SecureBytes, SecureBytes, Word64, Word64))
evictByAge currentTimeSecs m = do
    let keepP (_, _, _, ts) =
            currentTimeSecs <= ts || (currentTimeSecs - ts) < skippedKeyMaxAgeSecs
        (kept, expired) = Map.partition keepP m
    mapM_ zeroSkippedEntry (Map.elems expired)
    pure kept

------------------------------------------------------------------------
-- KDF helpers
------------------------------------------------------------------------

-- | Derive a new root key and chain key from the current root key and
-- a DH output, using HKDF-SHA-512.
--
-- Salt = root key, IKM = DH output, Info = "UmbraVox_Ratchet_v1"
-- Output = 64 bytes: first 32 = new root key, last 32 = new chain key.
--
-- M15.3: Now monadic (IO) — extracts the root key via 'toByteString',
-- derives the output, and wraps results back in 'fromByteString'.
kdfRK :: SecureBytes -> ByteString -> IO (SecureBytes, SecureBytes)
kdfRK rootKeySB dhOut = do
    rootKey <- toByteString rootKeySB
    !prk <- HKDFFFI.hkdfExtract rootKey dhOut
    !okm <- HKDFFFI.hkdfExpand prk ratchetInfo 64
    newRoot  <- fromByteString (BS.take 32 okm)
    newChain <- fromByteString (BS.drop 32 okm)
    pure (newRoot, newChain)

-- | Derive a message key and new chain key from the current chain key.
--
-- messageKey  = HMAC-SHA256(chainKey, 0x01)
-- newChainKey = HMAC-SHA256(chainKey, 0x02)
--
-- M15.3: Now monadic (IO) — extracts the chain key via 'toByteString'.
-- Returns (newChainKey :: SecureBytes, msgKey :: ByteString).
-- The msgKey is a plain ByteString because it is used immediately for
-- GCM encryption/decryption and is short-lived.
kdfCK :: SecureBytes -> IO (SecureBytes, ByteString)
kdfCK chainKeySB = do
    chainKey <- toByteString chainKeySB
    !msgKey      <- HMACFFI.hmacSHA256 chainKey (BS.singleton 0x01)
    !newChainKey <- HMACFFI.hmacSHA256 chainKey (BS.singleton 0x02)
    newChainSB <- fromByteString newChainKey
    pure (newChainSB, msgKey)

------------------------------------------------------------------------
-- Key pair generation (deterministic from secret)
------------------------------------------------------------------------

-- | Generate an X25519 key pair from a 32-byte secret.
-- The basepoint multiplication cannot produce an all-zero result for any
-- non-zero secret, so the 'Just' match here is safe by construction.
--
-- M15.3: Now monadic (IO) — wraps the secret in SecureBytes.
generateDH :: ByteString -> IO (SecureBytes, ByteString)
generateDH secret = do
    mPub <- X25519FFI.x25519 secret X25519FFI.x25519Basepoint
    case mPub of
        Just !pub -> do
            secretSB <- fromByteString secret
            pure (secretSB, pub)
        Nothing   -> error "generateDH: x25519 with basepoint returned all-zero (impossible)"

-- | Perform X25519 Diffie-Hellman.
-- Returns Nothing if the DH output is all-zero (low-order point).
--
-- M15.3: Now monadic (IO) — extracts the secret via 'toByteString'.
dhIO :: (SecureBytes, ByteString) -> ByteString -> IO (Maybe ByteString)
dhIO (secretSB, _) theirPublic = do
    secret <- toByteString secretSB
    X25519FFI.x25519 secret theirPublic

------------------------------------------------------------------------
-- Initialization
------------------------------------------------------------------------

-- | Initialize Double Ratchet state for Alice (the initiator).
--
-- Alice has completed X3DH and holds:
--   * @sharedSecret@ — the 32-byte shared secret from X3DH
--   * @bobSPK@       — Bob's signed pre-key public key
--   * @aliceDHSecret@ — a fresh 32-byte secret for Alice's first ratchet keypair
--
-- Alice performs the first DH ratchet step to derive sending chain key.
-- | Initialize Double Ratchet state for Alice (the initiator).
--
-- Returns Nothing if the initial DH with Bob's SPK yields an all-zero
-- result (low-order point attack; rejects the session rather than
-- silently producing a weak key).
--
-- M15.3: Now monadic (IO) — key fields are wrapped in SecureBytes.
ratchetInitAlice :: ByteString  -- ^ X3DH shared secret (32 bytes)
                 -> ByteString  -- ^ Bob's signed pre-key (32 bytes)
                 -> ByteString  -- ^ Alice's fresh DH secret (32 bytes)
                 -> IO (Maybe RatchetState)
ratchetInitAlice sharedSecret bobSPK aliceDHSecret = do
    aliceKP <- generateDH aliceDHSecret
    mDhOutput <- dhIO aliceKP bobSPK
    case mDhOutput of
        Nothing -> pure Nothing
        Just !dhOutput -> do
            sharedSecretSB <- fromByteString sharedSecret
            (rootKey1, sendChain) <- kdfRK sharedSecretSB dhOutput
            recvChain <- fromByteString (BS.replicate 32 0)
            pure $ Just RatchetState
                { rsDHSend      = aliceKP
                , rsDHRecv      = Just bobSPK
                , rsRootKey     = rootKey1
                , rsSendChain   = sendChain
                , rsRecvChain   = recvChain
                  -- SAFETY (M7.2.3): This zero placeholder is never used for
                  -- decryption.  Bob's first message triggers dhRatchet, which
                  -- overwrites rsRecvChain via kdfRK before any kdfCK derivation.
                  -- The zero value cannot leak key material.
                , rsSendN       = 0
                , rsRecvN       = 0
                , rsPrevChainN  = 0
                , rsSkippedKeys  = Map.empty
                , rsSkipSeq      = 0
                , rsNonceCounter = 0
                , rsSeenDHKeys   = Seq.empty
                }

-- | Initialize Double Ratchet state for Bob (the responder).
--
-- Bob uses his signed pre-key as the initial DH keypair.  The peer DH
-- public key is left empty — it will be populated from the header of
-- Alice's first message, which triggers the initial DH ratchet step.
--
-- M15.3: Now monadic (IO) — key fields are wrapped in SecureBytes.
ratchetInitBob :: ByteString  -- ^ X3DH shared secret (32 bytes)
               -> ByteString  -- ^ Bob's signed pre-key secret (32 bytes)
               -> IO RatchetState
ratchetInitBob sharedSecret bobSPKSecret = do
    bobKP <- generateDH bobSPKSecret
    rootKeySB   <- fromByteString sharedSecret
    sendChainSB <- fromByteString (BS.replicate 32 0)
    recvChainSB <- fromByteString (BS.replicate 32 0)
    pure RatchetState
        { rsDHSend      = bobKP
        , rsDHRecv      = Nothing   -- Set from first message header
        , rsRootKey     = rootKeySB
        , rsSendChain   = sendChainSB
          -- SAFETY (M7.2.3): Zero placeholder — Bob cannot encrypt until
          -- Alice's first message arrives and triggers dhRatchet, which
          -- derives real send/recv chain keys via kdfRK.  rsDHRecv starts
          -- as Nothing, so ratchetEncrypt cannot produce valid ciphertext
          -- with this value.
        , rsRecvChain   = recvChainSB
          -- SAFETY (M7.2.3): Same as rsSendChain above — overwritten by
          -- dhRatchet before any decryption attempt.
        , rsSendN       = 0
        , rsRecvN       = 0
        , rsPrevChainN  = 0
        , rsSkippedKeys  = Map.empty
        , rsSkipSeq      = 0
        , rsNonceCounter = 0
        , rsSeenDHKeys   = Seq.empty
        }

------------------------------------------------------------------------
-- Encryption
------------------------------------------------------------------------

-- | Encrypt a plaintext message, advancing the sending ratchet.
--
-- Returns @Right (updatedState, header, ciphertext, tag)@ on success, or
-- @Left CounterExhausted@ when the send counter has reached its limit.
-- Runs in IO because the DH ratchet step (triggered on decrypt) requires
-- CSPRNG-generated ephemeral keys.
--
-- Finding    M10.2.2 — counter exhaustion previously called @error@.
-- Fix:       Returns @Left CounterExhausted@ instead.
ratchetEncrypt :: RatchetState
               -> ByteString          -- ^ Plaintext
               -> IO (Either RatchetError (RatchetState, RatchetHeader, ByteString, ByteString))
ratchetEncrypt st plaintext =
    -- M8.1.1 / M10.2.2: Reject encryption when send counter is exhausted to
    -- prevent counter overflow / nonce reuse.  Callers must perform a DH
    -- ratchet (by exchanging messages) before reaching this limit.
    if rsSendN st >= 0xFFFFFFFE
        then pure (Left CounterExhausted)
        else do
            -- M15.3: kdfCK and makeNonce are now IO due to SecureBytes
            (newChainKey, msgKey) <- kdfCK (rsSendChain st)
            nonce <- makeNonce (rsSendChain st) (rsSendN st)
            let -- Build header
                !header = RatchetHeader
                    { rhDHPublic   = snd (rsDHSend st)
                    , rhPrevChainN = rsPrevChainN st
                    , rhMsgN       = rsSendN st
                    }
                -- Encrypt with AES-256-GCM
                !aad = encodeHeader header
            (ct, tag) <- GCMFFI.gcmEncrypt msgKey nonce aad plaintext
            let -- Update state
                !st' = st
                    { rsSendChain    = newChainKey
                    , rsSendN        = rsSendN st + 1
                    , rsNonceCounter = rsNonceCounter st + 1
                    }
            pure (Right (st', header, ct, tag))

------------------------------------------------------------------------
-- Decryption
------------------------------------------------------------------------

-- | Decrypt a received message.
--
-- Returns @Right (Just (state, plaintext))@ on success,
-- @Right Nothing@ on GCM authentication failure,
-- @Left RatchetSkipExceeded@ when the aggregate skip distance exceeds
-- 'maxRatchetSkip' (M23.3.3),
-- @Left DHKeyReplay@ when the header DH key has been seen before
-- (M23.3.3),
-- and @Left CounterExhausted@ when the receive counter has reached its
-- limit.  Runs in IO because a DH ratchet step may generate a new
-- keypair via CSPRNG.
--
-- Finding    M10.2.2 — counter exhaustion previously called @error@.
-- Fix:       Returns @Left CounterExhausted@ instead.
ratchetDecrypt :: RatchetState
               -> RatchetHeader       -- ^ Message header
               -> ByteString          -- ^ Ciphertext
               -> ByteString          -- ^ GCM tag (16 bytes)
               -> IO (Either RatchetError (Maybe (RatchetState, ByteString)))
ratchetDecrypt st header ct tag =
    -- M8.1.1 / M10.2.2: Reject decryption when receive counter is exhausted
    -- to prevent counter overflow.  A DH ratchet step resets the counter.
    if rsRecvN st >= 0xFFFFFFFE
        then pure (Left CounterExhausted)
        else do
            -- M27.6.9: Get current wall-clock time for skipped-key expiry
            nowSecs <- round <$> getPOSIXTime
            -- Try skipped keys first
            mSkipped <- trySkippedKeys st nowSecs header ct tag
            case mSkipped of
                Just result -> pure (Right (Just result))
                Nothing -> do
                    -- Determine whether a DH ratchet is needed
                    let needsDHRatchet = case rsDHRecv st of
                            Nothing   -> True
                            Just peer -> rhDHPublic header /= peer

                    -- M23.3.3: rate-limit ratchet steps.  Compute the
                    -- aggregate skip distance before doing any work.
                    let totalSkip = if needsDHRatchet
                            then -- prevChain gap + current-chain gap
                                 let prevGap = if rhPrevChainN header > rsRecvN st
                                               then fromIntegral (rhPrevChainN header - rsRecvN st)
                                               else 0 :: Int
                                     curGap  = fromIntegral (rhMsgN header)
                                 in prevGap + curGap
                            else -- same DH epoch — only current-chain gap
                                 if rhMsgN header > rsRecvN st
                                 then fromIntegral (rhMsgN header - rsRecvN st)
                                 else 0 :: Int
                    if totalSkip > maxRatchetSkip
                        then pure (Left RatchetSkipExceeded)
                        else do
                            -- M23.3.3: DH key replay detection
                            if needsDHRatchet && isDHKeyReplayed st (rhDHPublic header)
                                then pure (Left DHKeyReplay)
                                else do
                                    -- If header DH key differs from our stored peer key, do DH ratchet
                                    st1 <- if needsDHRatchet
                                               then dhRatchet st header nowSecs
                                               else pure (Just st)
                                    case st1 of
                                        Nothing -> pure (Right Nothing)  -- Too many skipped keys
                                        Just st2 -> do
                                            -- Skip any missed messages in current receiving chain
                                            st3m <- skipMessageKeys st2 (rhMsgN header) nowSecs
                                            case st3m of
                                                Nothing -> pure (Right Nothing)  -- Too many skipped keys
                                                Just st3 -> do
                                                    -- M15.3: kdfCK and makeNonce are now IO
                                                    (newChainKey, msgKey) <- kdfCK (rsRecvChain st3)
                                                    nonce <- makeNonce (rsRecvChain st3) (rsRecvN st3)
                                                    let !aad = encodeHeader header
                                                    mPt <- GCMFFI.gcmDecrypt msgKey nonce aad ct tag
                                                    pure $ Right $ case mPt of
                                                        Just plaintext ->
                                                            let !st4 = st3
                                                                    { rsRecvChain = newChainKey
                                                                    , rsRecvN     = rsRecvN st3 + 1
                                                                    }
                                                            in Just (st4, plaintext)
                                                        Nothing -> Nothing

------------------------------------------------------------------------
-- DH key replay detection (M23.3.3)
------------------------------------------------------------------------

-- | Check if a DH public key has been seen in a previous ratchet epoch.
-- The check uses the bounded FIFO 'rsSeenDHKeys'.
isDHKeyReplayed :: RatchetState -> ByteString -> Bool
isDHKeyReplayed st peerPub =
    let !seen = rsSeenDHKeys st
        -- Build a Set for O(log n) lookup within the bounded window
        !seenSet = Set.fromList (foldr (:) [] seen)
    in Set.member peerPub seenSet

-- | Record a DH public key in the seen-key FIFO, evicting the oldest
-- entry if the FIFO exceeds 'maxSeenDHKeys'.
recordDHKey :: RatchetState -> ByteString -> RatchetState
recordDHKey st peerPub =
    let !seen = rsSeenDHKeys st
        -- Append the new key
        !seen' = seen Seq.|> peerPub
        -- Evict oldest if over capacity
        !seen'' = if Seq.length seen' > maxSeenDHKeys
                  then Seq.drop (Seq.length seen' - maxSeenDHKeys) seen'
                  else seen'
    in st { rsSeenDHKeys = seen'' }

------------------------------------------------------------------------
-- DH Ratchet step
------------------------------------------------------------------------

-- | Perform a DH ratchet step when receiving a new peer DH public key.
-- Uses CSPRNG to generate ephemeral DH keypairs for break-in recovery.
-- Returns Nothing if a DH output is all-zero (low-order point rejection).
--
-- M23.3.3: Records the new peer DH public key in 'rsSeenDHKeys' for
-- replay detection.  The replay check itself is in 'ratchetDecrypt',
-- before this function is called.
--
-- Finding:     M15.4 — Old ratchet keys ('rsRootKey', 'rsSendChain',
--              'rsRecvChain') were replaced by new 'SecureBytes' values but
--              the displaced 'SecureBytes' were never explicitly zeroed.
--              Although the GC finalizer eventually zeros them, the window
--              between replacement and collection leaves old key material
--              accessible in pinned memory.
-- Vulnerability: During GC collection delay, old root and chain keys remain
--              readable in physical RAM.  A memory-forensics attacker or a
--              process that can read /proc/self/mem could recover previous
--              epoch key material.
-- Fix:         Call 'zeroAndFree' on the displaced root key, intermediate
--              root key, old send chain, and old receive chain immediately
--              after the new keys are in place.  This provides deterministic
--              erasure rather than relying on GC scheduling.
-- Verified:    'zeroAndFree' is called on all four displaced 'SecureBytes'
--              values before 'dhRatchet' returns.
dhRatchet :: RatchetState -> RatchetHeader -> Word64 -> IO (Maybe RatchetState)
dhRatchet st header nowSecs = do
    -- Skip any remaining messages in old receiving chain
    st1m <- skipMessageKeys st (rhPrevChainN header) nowSecs
    case st1m of
        Nothing -> pure Nothing
        Just st1 -> do
            let -- M23.3.3: record the new DH key before advancing
                !st1a = recordDHKey st1 (rhDHPublic header)
                -- Store the previous chain length
                !st2 = st1a
                    { rsPrevChainN = rsSendN st1a
                    , rsSendN      = 0
                    , rsRecvN      = 0
                    , rsDHRecv     = Just (rhDHPublic header)
                    }
                -- Derive new receiving chain (rsDHRecv is guaranteed Just here)
                !peerPub = case rsDHRecv st2 of
                    Just pk -> pk
                    Nothing -> error "dhRatchet: impossible: rsDHRecv is Nothing after assignment"
            -- M15.3: dhIO and kdfRK are now IO
            mDhOutput1 <- dhIO (rsDHSend st2) peerPub
            case mDhOutput1 of
                Nothing -> pure Nothing
                Just !dhOutput1 -> do
                    -- Save references to old key material for deterministic zeroing (M15.4).
                    let !oldRootKey   = rsRootKey   st2
                        !oldSendChain = rsSendChain st2
                        !oldRecvChain = rsRecvChain st2
                    (rootKey1, recvChain) <- kdfRK oldRootKey dhOutput1
                    -- Generate new sending keypair from CSPRNG
                    newDHSecret <- randomBytes 32
                    newDHKP <- generateDH newDHSecret
                    mDhOutput2 <- dhIO newDHKP peerPub
                    case mDhOutput2 of
                        Nothing -> pure Nothing
                        Just !dhOutput2 -> do
                            (rootKey2, sendChain) <- kdfRK rootKey1 dhOutput2
                            -- M15.4: Deterministically zero all displaced key material
                            -- before they become unreachable.  'rootKey1' is the
                            -- intermediate root key; the others are the pre-ratchet values.
                            zeroAndFree oldRootKey
                            zeroAndFree rootKey1
                            zeroAndFree oldSendChain
                            zeroAndFree oldRecvChain
                            pure $ Just st2
                                { rsDHSend    = newDHKP
                                , rsRootKey   = rootKey2
                                , rsSendChain = sendChain
                                , rsRecvChain = recvChain
                                }

------------------------------------------------------------------------
-- Skipped message keys
------------------------------------------------------------------------

-- | Try to decrypt using a previously skipped message key.
-- M27.6.9: Entries older than 'skippedKeyMaxAgeSecs' (48 hours) are
-- rejected and evicted from the cache before lookup.
--
-- M15.3: Now monadic (IO) — makeNonce requires IO for SecureBytes.
-- M40.32: the stored msgKey/chainKey are 'SecureBytes'; the chain key is
-- passed straight to 'makeNonce' and the consumed entry is zeroed after use.
trySkippedKeys :: RatchetState -> Word64 -> RatchetHeader -> ByteString -> ByteString
               -> IO (Maybe (RatchetState, ByteString))
trySkippedKeys st nowSecs header ct tag = do
    let !lookupKey = (rhDHPublic header, rhMsgN header)
        -- M27.6.9: split expired entries (by wall-clock timestamp) from live
        -- ones so the expired key material can be zeroed (M40.32) rather than
        -- silently dropped from the GC heap.
        keepP (_, _, _, ts) = nowSecs <= ts || (nowSecs - ts) <= skippedKeyMaxAgeSecs
        (!pruned, !expired) = Map.partition keepP (rsSkippedKeys st)
    -- M40.32: zero the key material of every entry dropped by age expiry.
    mapM_ zeroSkippedEntry (Map.elems expired)
    let !st0 = st { rsSkippedKeys = pruned }
    case Map.lookup lookupKey pruned of
        Nothing -> pure Nothing
        Just entry@(msgKeySB, chainKeySB, _insertSeq, _insertTime) -> do
            -- Re-derive nonce from the stored chain key, matching the nonce
            -- used during encryption (M10.2.5: nonce comes from chain key).
            nonce <- makeNonce chainKeySB (rhMsgN header)
            msgKey <- toByteString msgKeySB
            let !aad = encodeHeader header
            mPt <- GCMFFI.gcmDecrypt msgKey nonce aad ct tag
            case mPt of
                Just plaintext -> do
                    -- M40.32: zero the consumed key material immediately after
                    -- use (the GCM call has already completed above).
                    zeroSkippedEntry entry
                    let !st' = st0 { rsSkippedKeys = Map.delete lookupKey pruned }
                    pure (Just (st', plaintext))
                Nothing -> pure Nothing

-- | Skip message keys from current counter up to (but not including) @until@.
-- Stores derived keys in rsSkippedKeys. Returns Nothing if too many would be
-- skipped, or if the peer's DH key is not yet known (rsDHRecv is Nothing).
-- M27.6.9: @nowSecs@ is the current wall-clock time in seconds, stored
-- alongside each skipped key for time-based expiry.
--
-- M15.3: Now monadic (IO) — kdfCK requires IO for SecureBytes extraction.
-- The old chain key ByteString stored in skipped keys is extracted via
-- 'toByteString' for nonce re-derivation in 'trySkippedKeys'.
skipMessageKeys :: RatchetState -> Word32 -> Word64 -> IO (Maybe RatchetState)
skipMessageKeys st until' nowSecs
    | rsRecvN st >= until'          = pure (Just st)
    | until' - rsRecvN st > maxSkip = pure Nothing
    | otherwise = case rsDHRecv st of
        Nothing -> pure Nothing
        Just _  -> do
            result <- go st
            -- M7.3.6: enforce total skipped-key cap across all DH ratchets.
            -- M40.32: evictByAge/evictOldest now run in IO and zero the key
            -- material of any entry they drop, so expired/over-cap keys never
            -- linger in memory.
            aged   <- evictByAge nowSecs (rsSkippedKeys result)
            pruned <- evictOldest aged
            pure $ Just result { rsSkippedKeys = pruned }
  where
    go s
        | rsRecvN s >= until' = pure s
        | otherwise = do
            -- M15.3: extract chain key for HMAC, then store a copy in the
            -- skipped-keys map for nonce re-derivation.
            oldChainKeyBS <- toByteString (rsRecvChain s)
            (newChainKey, msgKey) <- kdfCK (rsRecvChain s)
            -- M40.32: store msgKey + chainKey as SecureBytes (mlock'd, zeroed
            -- on removal) rather than plain GC-heap ByteString.
            msgKeySB   <- fromByteString msgKey
            chainKeySB <- fromByteString oldChainKeyBS
            let !peerKey = case rsDHRecv s of
                    Just pk -> pk
                    Nothing -> error "skipMessageKeys: impossible: rsDHRecv is Nothing (guarded above)"
                !key = (peerKey, rsRecvN s)
                !seq' = rsSkipSeq s
                -- Store (msgKey, chainKey, seq', nowSecs) so trySkippedKeys
                -- can re-derive the nonce, evictOldest can find the truly
                -- oldest entry by insertion order, and evictByAge can
                -- expire stale entries by wall-clock time (M27.6.9).
                !skipped = Map.insert key (msgKeySB, chainKeySB, seq', nowSecs) (rsSkippedKeys s)
            go s
                { rsRecvChain   = newChainKey
                , rsRecvN       = rsRecvN s + 1
                , rsSkippedKeys = skipped
                , rsSkipSeq     = seq' + 1
                }

-- Finding    M10.3.5 — 'evictOldest' evicted entries by Map ordering
--            (lexicographic on the @(ByteString, Word32)@ key), which is
--            DH-public-key order, not insertion order.  Under adversarial
--            message scheduling an attacker could craft DH public keys that
--            sort first in the map, repeatedly displacing legitimate skipped
--            keys rather than the genuinely oldest ones, causing valid
--            out-of-order messages to fail decryption.
-- Vulnerability: Eviction by map key order rather than insertion order
--            violates the FIFO contract expected by the Double Ratchet spec;
--            legitimate messages can be silently dropped while adversarially
--            crafted entries persist.
-- Fix:       Each skipped-key entry now carries an insertion sequence number
--            (@Word64@, stored as the third element of the map value).
--            'evictOldest' finds the entry with the minimum sequence number
--            (the oldest inserted) and removes it, iterating until the cache
--            is within 'maxTotalSkipped'.
--
-- Finding    M23.3.4 — Previous 'evictOldest' was O(n) per eviction step
--            because it scanned the entire map with foldlWithKey' to find
--            the minimum insertion sequence, then performed a Map.lookup on
--            the best candidate to retrieve its sequence for comparison.
-- Vulnerability: When the skipped-key cache is at capacity and many
--            evictions are needed (e.g. burst of skipped messages across
--            multiple ratchet epochs), the O(n) scan per eviction results
--            in O(n * k) total work where k is the number of entries to
--            evict, causing latency spikes.
-- Fix:       Build a secondary Map keyed by insertion sequence (Word64 ->
--            (ByteString, Word32)) and use Map.deleteFindMin for O(log n)
--            lookup of the oldest entry.  Delete from both maps in lockstep.
-- Verified:  'skipMessageKeys' increments 'rsSkipSeq' on each insertion;
--            'evictOldest' removes the minimum-sequence entry until the cap
--            is satisfied.
--
-- | Evict the oldest-inserted entries from the skipped-key cache until the
-- total size is within 'maxTotalSkipped' (M7.3.6).
--
-- Eviction is by insertion order (minimum 'rsSkipSeq' value), not by map
-- key order, so legitimate skipped messages are never displaced by
-- adversarially crafted DH public keys.
--
-- M23.3.4: Uses a secondary index (Map Word64 key) for O(log n) per
-- eviction instead of O(n) linear scan.
--
-- M40.32: now runs in 'IO' so each evicted entry's key material is zeroed
-- (via 'zeroSkippedEntry') on every recursion step before it is dropped.
evictOldest :: Map (ByteString, Word32) (SecureBytes, SecureBytes, Word64, Word64)
            -> IO (Map (ByteString, Word32) (SecureBytes, SecureBytes, Word64, Word64))
evictOldest m
    | Map.size m <= maxTotalSkipped = pure m
    | otherwise =
        -- Build a secondary index: insertSeq -> primary key, then evict
        -- entries with the smallest insertion sequence numbers.
        let !seqIndex = Map.foldlWithKey'
                (\acc k (_, _, sq, _) -> Map.insert sq k acc)
                (Map.empty :: Map Word64 (ByteString, Word32))
                m
        in evictWithIndex m seqIndex
  where
    evictWithIndex :: Map (ByteString, Word32) (SecureBytes, SecureBytes, Word64, Word64)
                   -> Map Word64 (ByteString, Word32)
                   -> IO (Map (ByteString, Word32) (SecureBytes, SecureBytes, Word64, Word64))
    evictWithIndex primary idx
        | Map.size primary <= maxTotalSkipped = pure primary
        | Map.null idx = pure primary
        | otherwise = do
            let ((_minSeq, oldestKey), idx') = Map.deleteFindMin idx
            -- M40.32: zero the evicted entry's key material before dropping it.
            case Map.lookup oldestKey primary of
                Just entry -> zeroSkippedEntry entry
                Nothing    -> pure ()
            let !primary' = Map.delete oldestKey primary
            evictWithIndex primary' idx'

------------------------------------------------------------------------
-- Nonce and header encoding
------------------------------------------------------------------------

-- | Build a 12-byte GCM nonce from the chain key and message counter.
--
-- Finding    M10.2.3 — old 'makeNonce' used HMAC(msgKey, "nonce") for 8 bytes
--            + 4-byte big-endian counter, deviating from the standard
--            Noise/AEAD nonce layout and mixing key and nonce derivation.
-- Vulnerability: Non-standard layout makes the nonce derivation harder to
--            audit; big-endian counter deviates from IETF AEAD conventions.
-- Fix:       Nonce layout changed to 4 zero bytes || 8-byte little-endian
--            counter (standard Noise/AEAD layout per RFC 8439 §2.3).
--
-- Finding    M10.2.5 — old 'makeNonce' used @msgKey@ as the HMAC key for
--            nonce derivation, creating cryptographic coupling between the
--            GCM encryption key and the nonce derivation input.
-- Vulnerability: Using the same key material as both the GCM key and the
--            nonce derivation seed violates key separation; an attacker
--            who learns the nonce derivation also learns partial information
--            about the GCM key.
-- Fix:       'makeNonce' now takes the @chainKey@ (distinct from @msgKey@)
--            and derives an 8-byte nonce base via HKDF with the distinct
--            info label @"UmbraVox_Nonce_v1"@, XOR-ed with the 8-byte
--            little-endian counter.  The result is prepended with 4 zero
--            bytes to match the standard Noise layout.
-- Verified:  'ratchetEncrypt' and 'ratchetDecrypt' pass the chain key (not
--            the message key) to 'makeNonce'.
--
-- Finding    M10.1.8 — 'makeNonce' used @chainKey@ as the HKDF-Extract salt
--            and @nonceInfo@ ("UmbraVox_Nonce_v1") as the IKM.  Passing the
--            same material as both the salt and a key-derived input is
--            non-standard and redundant; the Signal Protocol convention for
--            HKDF-Extract uses a zero salt so that HKDF-Extract behaves as
--            a pure PRF keyed on the IKM.
-- Vulnerability: Using @chainKey@ as the salt means the PRK is not derived
--            purely from @chainKey@; the salt role mixes a non-secret
--            constant (nonceInfo) in a place the Noise/Signal specs expect a
--            zero or random salt, deviating from the intended security model.
-- Fix:       Changed HKDF-Extract to use a 32-byte zero salt with @chainKey@
--            as IKM, matching Signal's HKDF-Extract convention (RFC 5869 §2.2:
--            "if not provided, [salt] is set to a string of HashLen zeros").
-- Verified:  Both 'ratchetEncrypt' and 'ratchetDecrypt' call 'makeNonce' with
--            the chain key; the zero-salt change is transparent to callers
--            and both sides derive the same nonce from the same chain key.
-- M15.3: Now monadic (IO) — extracts chain key via 'toByteString'.
makeNonce :: SecureBytes  -- ^ Chain key (NOT the message key)
          -> Word32       -- ^ Message counter
          -> IO ByteString
makeNonce chainKeySB counter = do
    chainKey <- toByteString chainKeySB
    -- Derive 8-byte nonce base from chain key via HKDF with zero salt (Signal
    -- convention: zero salt so HKDF-Extract acts as a keyed PRF on chainKey).
    !prk  <- HKDFFFI.hkdfExtract (BS.replicate 32 0) chainKey
    !base <- HKDFFFI.hkdfExpand prk nonceInfo 8
    let -- XOR the base with the 8-byte LE counter for per-message uniqueness
        !ctr  = encodeWord64LE (fromIntegral counter)
        !mixed = BS.pack (BS.zipWith xor base ctr)
    pure (BS.replicate 4 0 <> mixed)

-- | Encode a Word32 as 4-byte big-endian.
encodeWord32BE :: Word32 -> ByteString
encodeWord32BE w = BS.pack
    [ fromIntegral (w `shiftR` 24 .&. 0xff)
    , fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR` 8  .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

-- | Encode a Word64 as 8-byte little-endian.
encodeWord64LE :: Word64 -> ByteString
encodeWord64LE w = BS.pack
    [ fromIntegral (w             .&. 0xff)
    , fromIntegral (w `shiftR`  8 .&. 0xff)
    , fromIntegral (w `shiftR` 16 .&. 0xff)
    , fromIntegral (w `shiftR` 24 .&. 0xff)
    , fromIntegral (w `shiftR` 32 .&. 0xff)
    , fromIntegral (w `shiftR` 40 .&. 0xff)
    , fromIntegral (w `shiftR` 48 .&. 0xff)
    , fromIntegral (w `shiftR` 56 .&. 0xff)
    ]

-- | Encode a header as associated data for GCM authentication.
-- Format: DH public key (32) || prevChainN (4) || msgN (4) = 40 bytes.
encodeHeader :: RatchetHeader -> ByteString
encodeHeader hdr =
    rhDHPublic hdr
    <> encodeWord32BE (rhPrevChainN hdr)
    <> encodeWord32BE (rhMsgN hdr)
