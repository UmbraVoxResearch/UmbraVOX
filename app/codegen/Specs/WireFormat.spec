-- WireFormat Specification — Envelope Serialization
--
-- This .spec file encodes the on-the-wire envelope format for UmbraVOX
-- protocol messages.  Every transmitted message is wrapped in a fixed
-- header followed by a length-prefixed payload.
--
-- Envelope layout (all multi-byte integers are big-endian):
--
--   Offset  Size   Field
--   ------  ----   -----
--   0       1      version      (protocol version, currently 0x01)
--   1       1      msg_type     (message type tag)
--   2       4      sequence     (monotonic sequence number, uint32)
--   6       32     source       (sender identity, 32-byte public key hash)
--   38      32     dest         (recipient identity, 32-byte public key hash)
--   70      2      payload_len  (length of payload in bytes, uint16, big-endian)
--   72      N      payload      (encrypted message body, N = payload_len)
--   72+N    32     mac          (HMAC-SHA-256 over bytes [0..72+N-1])
--
-- Total envelope size: 72 + payload_len + 32 = 104 + payload_len bytes
-- Maximum payload: 65535 bytes (uint16 limit)

algorithm WireFormat {

  params {
    -- Serialize inputs
    version     : UInt8
    msg_type    : UInt8
    sequence    : UInt32
    source      : Bytes(32)
    dest        : Bytes(32)
    payload     : Bytes
    mac_key     : Bytes(32)

    -- Deserialize input
    envelope    : Bytes
  }

  constants {
    -- Current protocol version
    VERSION_1 = 0x01

    -- Header size (bytes before payload): 1 + 1 + 4 + 32 + 32 + 2 = 72
    HEADER_SIZE = 72

    -- MAC size (HMAC-SHA-256 output)
    MAC_SIZE = 32

    -- Minimum envelope size: HEADER_SIZE + 0 (empty payload) + MAC_SIZE
    MIN_ENVELOPE = 104

    -- Maximum payload length (uint16 max)
    MAX_PAYLOAD = 65535

    -- Field offsets
    OFF_VERSION     = 0
    OFF_MSG_TYPE    = 1
    OFF_SEQUENCE    = 2
    OFF_SOURCE      = 6
    OFF_DEST        = 38
    OFF_PAYLOAD_LEN = 70
    OFF_PAYLOAD     = 72

    -- Message type constants
    MSG_HANDSHAKE    = 0x01
    MSG_DATA         = 0x02
    MSG_ACK          = 0x03
    MSG_REKEY        = 0x04
    MSG_CLOSE        = 0x05
    MSG_HEARTBEAT    = 0x06
  }

  steps {
    -- ==================================================================
    -- Serialize Path — Encode fields into wire envelope
    -- ==================================================================

    -- Step 1: Validate inputs
    -- version must be non-zero
    -- payload length must fit in uint16
    payload_len = length(payload)
    inputs_ok = (version != 0) & (payload_len <= MAX_PAYLOAD)

    -- Step 2: Encode version (1 byte)
    w_version = encodeBE(version, 1)

    -- Step 3: Encode message type (1 byte)
    w_msg_type = encodeBE(msg_type, 1)

    -- Step 4: Encode sequence number (4 bytes, big-endian)
    w_sequence = encodeBE(sequence, 4)

    -- Step 5: Encode source identity (32 bytes, verbatim)
    w_source = source

    -- Step 6: Encode destination identity (32 bytes, verbatim)
    w_dest = dest

    -- Step 7: Encode payload length (2 bytes, big-endian)
    w_payload_len = encodeBE(payload_len, 2)

    -- Step 8: Assemble header and body
    -- header_and_body = version || msg_type || sequence || source ||
    --                   dest || payload_len || payload
    header_and_body = w_version || w_msg_type || w_sequence ||
                      w_source || w_dest || w_payload_len || payload

    -- Step 9: Compute MAC over header and body
    -- mac = HMAC-SHA-256(mac_key, header_and_body)
    mac = HMAC_SHA256(mac_key, header_and_body)

    -- Step 10: Assemble final envelope
    -- envelope_out = header_and_body || mac
    envelope_out = header_and_body || mac

    -- ==================================================================
    -- Deserialize Path — Decode wire envelope into fields
    -- ==================================================================

    -- Step 11: Validate minimum envelope size
    env_len = length(envelope)
    size_ok = env_len >= MIN_ENVELOPE

    -- Step 12: Parse fixed header fields
    d_version     = envelope[OFF_VERSION]
    d_msg_type    = envelope[OFF_MSG_TYPE]
    d_sequence    = decodeBE(envelope[OFF_SEQUENCE .. OFF_SEQUENCE + 3])
    d_source      = envelope[OFF_SOURCE .. OFF_SOURCE + 31]
    d_dest        = envelope[OFF_DEST .. OFF_DEST + 31]
    d_payload_len = decodeBE(envelope[OFF_PAYLOAD_LEN .. OFF_PAYLOAD_LEN + 1])

    -- Step 13: Validate payload length against envelope size
    -- Expected: HEADER_SIZE + d_payload_len + MAC_SIZE == env_len
    expected_len = HEADER_SIZE + d_payload_len + MAC_SIZE
    len_ok = (expected_len == env_len)

    -- Step 14: Extract payload and MAC
    d_payload = envelope[OFF_PAYLOAD .. OFF_PAYLOAD + d_payload_len - 1]
    d_mac     = envelope[env_len - MAC_SIZE .. env_len - 1]

    -- Step 15: Verify MAC (constant-time comparison)
    -- Recompute MAC over header_and_body portion
    d_header_and_body = envelope[0 .. env_len - MAC_SIZE - 1]
    expected_mac = HMAC_SHA256(mac_key, d_header_and_body)
    mac_ok = constantTimeEq(d_mac, expected_mac)

    -- Step 16: Validate version
    version_ok = (d_version == VERSION_1)

    -- Step 17: Final validation
    -- All checks must pass: size, length consistency, MAC, version
    valid = size_ok & len_ok & mac_ok & version_ok
  }
}
