-- SessionState Specification — Signal Session State Serialization (M21.1.7)
--
-- This .spec file encodes the serialization and deserialization of the
-- UmbraVOX Signal double-ratchet session state (RatchetState) into a
-- length-prefixed binary format suitable for durable storage.
--
-- All operations are constant-time with respect to the session structure
-- to avoid leaking ratchet topology (number of skipped keys, chain
-- lengths, etc.) via timing side channels.
--
-- Serialize:
--   1. Encode each RatchetState field with fixed-size or length-prefixed
--      binary encoding (big-endian integers)
--   2. Concatenate in canonical field order
--   3. Append HMAC-SHA-256 integrity tag over the serialized body
--
-- Deserialize:
--   1. Verify HMAC-SHA-256 tag (constant-time)
--   2. Parse each field by length prefix / fixed size
--   3. Reconstruct RatchetState
--
-- Binary layout (all multi-byte integers are big-endian):
--
--   Offset  Size     Field
--   ------  ----     -----
--   0       32       dh_send_secret   (X25519 secret key)
--   32      32       dh_send_public   (X25519 public key)
--   64      1        dh_recv_present  (0x00 = absent, 0x01 = present)
--   65      32       dh_recv_public   (peer X25519 public key, zeroed if absent)
--   97      32       root_key         (32-byte root key)
--   129     32       send_chain       (32-byte sending chain key)
--   161     32       recv_chain       (32-byte receiving chain key)
--   193     4        send_n           (sending message counter, uint32)
--   197     4        recv_n           (receiving message counter, uint32)
--   201     4        prev_chain_n     (previous sending chain length, uint32)
--   205     8        skip_seq         (monotonic skip counter, uint64)
--   213     8        nonce_counter    (monotonic nonce counter, uint64)
--   221     4        skipped_count    (number of skipped key entries, uint32)
--   225     N*109    skipped_keys     (each entry: 32 pubkey + 4 counter
--                                      + 32 msg_key + 32 chain_key + 8 insert_seq
--                                      + 1 padding = 109 bytes)
--   225+N*109  32    hmac             (HMAC-SHA-256 over bytes [0..225+N*109-1])
--
-- Fixed body size (no skipped keys): 225 bytes
-- Maximum skipped entries: bounded by MAX_TOTAL_SKIPPED (500)

algorithm SessionState {

  params {
    -- Serialize inputs (RatchetState fields)
    dh_send_secret  : Bytes(32)    -- X25519 sending secret key
    dh_send_public  : Bytes(32)    -- X25519 sending public key
    dh_recv_present : UInt8        -- 0x00 or 0x01
    dh_recv_public  : Bytes(32)    -- Peer's X25519 public key (zeroed if absent)
    root_key        : Bytes(32)    -- Root key
    send_chain      : Bytes(32)    -- Sending chain key
    recv_chain      : Bytes(32)    -- Receiving chain key
    send_n          : UInt32       -- Sending message counter
    recv_n          : UInt32       -- Receiving message counter
    prev_chain_n    : UInt32       -- Previous sending chain length
    skip_seq        : UInt64       -- Monotonic skip-key insertion counter
    nonce_counter   : UInt64       -- Monotonic nonce counter
    skipped_keys    : SkippedKeyList  -- List of skipped key entries
    mac_key         : Bytes(32)    -- HMAC key for integrity

    -- Deserialize input
    blob            : Bytes        -- Serialized session state
  }

  constants {
    -- Field sizes in bytes
    KEY_SIZE        = 32
    COUNTER_SIZE    = 4
    COUNTER64_SIZE  = 8
    PRESENCE_SIZE   = 1
    MAC_SIZE        = 32

    -- Fixed header size (before skipped keys)
    FIXED_BODY_SIZE = 225

    -- Each skipped key entry:
    --   32 (pubkey) + 4 (counter) + 32 (msg_key) + 32 (chain_key)
    --   + 8 (insert_seq) + 1 (padding) = 109 bytes
    SKIPPED_ENTRY_SIZE = 109

    -- Maximum skipped key entries (matches MAX_TOTAL_SKIPPED)
    MAX_SKIPPED = 500

    -- Minimum blob size: FIXED_BODY_SIZE + MAC_SIZE (zero skipped keys)
    MIN_BLOB_SIZE = 257
  }

  steps {
    -- ==================================================================
    -- Serialize Path — RatchetState fields → length-prefixed binary
    -- ==================================================================

    -- Step 1: Encode DH sending keypair (64 bytes, fixed)
    w_dh_send = dh_send_secret || dh_send_public

    -- Step 2: Encode DH receiving key presence + value (33 bytes, fixed)
    -- When absent, dh_recv_public is all-zero (constant-size regardless of presence)
    w_dh_recv = encodeBE(dh_recv_present, PRESENCE_SIZE) || dh_recv_public

    -- Step 3: Encode symmetric keys (96 bytes, fixed)
    w_keys = root_key || send_chain || recv_chain

    -- Step 4: Encode counters (12 bytes, fixed)
    w_counters = encodeBE(send_n, COUNTER_SIZE) ||
                 encodeBE(recv_n, COUNTER_SIZE) ||
                 encodeBE(prev_chain_n, COUNTER_SIZE)

    -- Step 5: Encode 64-bit counters (16 bytes, fixed)
    w_counters64 = encodeBE(skip_seq, COUNTER64_SIZE) ||
                   encodeBE(nonce_counter, COUNTER64_SIZE)

    -- Step 6: Encode skipped key count (4 bytes)
    skipped_count = length(skipped_keys)
    w_skip_count = encodeBE(skipped_count, COUNTER_SIZE)

    -- Step 7: Encode each skipped key entry (constant-size per entry)
    -- Each entry: pubkey(32) || counter(4) || msg_key(32) || chain_key(32)
    --             || insert_seq(8) || padding(1)
    -- Padding byte ensures alignment and constant entry size.
    w_skip_entries = FOR_EACH entry IN skipped_keys:
        entry.pubkey || encodeBE(entry.counter, COUNTER_SIZE) ||
        entry.msg_key || entry.chain_key ||
        encodeBE(entry.insert_seq, COUNTER64_SIZE) || 0x00

    -- Step 8: Assemble serialized body
    body = w_dh_send || w_dh_recv || w_keys || w_counters ||
           w_counters64 || w_skip_count || w_skip_entries

    -- Step 9: Compute HMAC-SHA-256 integrity tag over body
    mac = HMAC_SHA256(mac_key, body)

    -- Step 10: Assemble final blob
    serialized = body || mac

    -- ==================================================================
    -- Deserialize Path — binary → RatchetState fields
    -- ==================================================================

    -- Step 11: Validate minimum blob size
    blob_len = length(blob)
    size_ok = blob_len >= MIN_BLOB_SIZE

    -- Step 12: Split body and MAC
    d_body = blob[0 .. blob_len - MAC_SIZE - 1]
    d_mac  = blob[blob_len - MAC_SIZE .. blob_len - 1]

    -- Step 13: Verify HMAC-SHA-256 (constant-time comparison)
    expected_mac = HMAC_SHA256(mac_key, d_body)
    mac_ok = constantTimeEq(d_mac, expected_mac)

    -- Step 14: Parse DH sending keypair
    d_dh_send_secret = d_body[0 .. 31]
    d_dh_send_public = d_body[32 .. 63]

    -- Step 15: Parse DH receiving key
    d_dh_recv_present = d_body[64]
    d_dh_recv_public  = d_body[65 .. 96]
    -- Constant-time: always read 32 bytes regardless of presence flag

    -- Step 16: Parse symmetric keys
    d_root_key   = d_body[97 .. 128]
    d_send_chain = d_body[129 .. 160]
    d_recv_chain = d_body[161 .. 192]

    -- Step 17: Parse 32-bit counters
    d_send_n      = decodeBE(d_body[193 .. 196])
    d_recv_n      = decodeBE(d_body[197 .. 200])
    d_prev_chain_n = decodeBE(d_body[201 .. 204])

    -- Step 18: Parse 64-bit counters
    d_skip_seq     = decodeBE(d_body[205 .. 212])
    d_nonce_counter = decodeBE(d_body[213 .. 220])

    -- Step 19: Parse skipped key count
    d_skipped_count = decodeBE(d_body[221 .. 224])

    -- Step 20: Validate skipped key count
    skip_count_ok = d_skipped_count <= MAX_SKIPPED

    -- Step 21: Validate body length matches skipped key count
    expected_body_len = FIXED_BODY_SIZE + d_skipped_count * SKIPPED_ENTRY_SIZE
    body_len_ok = length(d_body) == expected_body_len

    -- Step 22: Parse skipped key entries (constant-time per entry)
    d_skipped_keys = FOR i IN [0 .. d_skipped_count - 1]:
        offset = FIXED_BODY_SIZE + i * SKIPPED_ENTRY_SIZE
        { pubkey     = d_body[offset .. offset + 31]
        , counter    = decodeBE(d_body[offset + 32 .. offset + 35])
        , msg_key    = d_body[offset + 36 .. offset + 67]
        , chain_key  = d_body[offset + 68 .. offset + 99]
        , insert_seq = decodeBE(d_body[offset + 100 .. offset + 107])
        }

    -- Step 23: Validate presence flag
    presence_ok = (d_dh_recv_present == 0x00) | (d_dh_recv_present == 0x01)

    -- Step 24: Final validation
    -- All checks must pass: size, MAC, skip count, body length, presence
    valid = size_ok & mac_ok & skip_count_ok & body_len_ok & presence_ok
  }
}
