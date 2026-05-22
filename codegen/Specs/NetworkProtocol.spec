-- NetworkProtocol Specification — P2P Wire Message Encoding (M21.3.6)
--
-- This .spec file encodes the UmbraVOX P2P wire protocol message format.
-- Every protocol message is serialized as:
--
--   [type:1][length:4BE][payload:N]
--
-- where:
--   type   — 1-byte message type tag
--   length — 4-byte big-endian payload length (uint32)
--   payload — N bytes of type-specific payload
--
-- Message types and their payload formats:
--
--   Type  Tag   Payload layout
--   ----  ---   --------------
--   Handshake  0x01  [version:1][public_key:32][capabilities:2BE]
--   Data       0x02  [sequence:4BE][data:N]
--   Ack        0x03  [sequence:4BE]
--   Peer       0x04  [count:2BE]([host_len:2BE][host:M][port:2BE])*
--   Ping       0x05  (empty)
--   Pong       0x06  (empty)
--
-- All multi-byte integers are big-endian.
-- Maximum payload: 2^32 - 1 bytes (uint32 limit).

algorithm NetworkProtocol {

  params {
    -- Encode inputs
    msg_type        : UInt8        -- Message type tag
    -- Handshake fields
    hs_version      : UInt8        -- Protocol version
    hs_public_key   : Bytes(32)    -- X25519 public key
    hs_capabilities : UInt16       -- Capability bitfield

    -- Data fields
    data_sequence   : UInt32       -- Monotonic sequence number
    data_payload    : Bytes        -- Application data

    -- Ack fields
    ack_sequence    : UInt32       -- Acknowledged sequence number

    -- Peer fields
    peer_list       : PeerList     -- List of (host, port) entries

    -- Decode input
    wire_bytes      : Bytes        -- Raw wire bytes to decode
  }

  constants {
    -- Message type tags
    MSG_HANDSHAKE = 0x01
    MSG_DATA      = 0x02
    MSG_ACK       = 0x03
    MSG_PEER      = 0x04
    MSG_PING      = 0x05
    MSG_PONG      = 0x06

    -- Header size: type (1) + length (4) = 5 bytes
    HEADER_SIZE = 5

    -- Field sizes
    TYPE_SIZE     = 1
    LENGTH_SIZE   = 4
    VERSION_SIZE  = 1
    KEY_SIZE      = 32
    CAPS_SIZE     = 2
    SEQUENCE_SIZE = 4
    PORT_SIZE     = 2
    HOSTLEN_SIZE  = 2
    COUNT_SIZE    = 2

    -- Fixed payload sizes
    HANDSHAKE_PAYLOAD_SIZE = 35   -- 1 + 32 + 2
    ACK_PAYLOAD_SIZE       = 4    -- 4
    PING_PAYLOAD_SIZE      = 0
    PONG_PAYLOAD_SIZE      = 0

    -- Minimum wire message size (type + length + empty payload)
    MIN_WIRE_SIZE = 5
  }

  steps {
    -- ==================================================================
    -- Encode Path — P2PMessage type → [type:1][length:4BE][payload:N]
    -- ==================================================================

    -- ------------------------------------------------------------------
    -- Handshake Encoding (type 0x01)
    -- ------------------------------------------------------------------

    -- Step 1: Encode handshake payload
    -- [version:1][public_key:32][capabilities:2BE]
    hs_payload = encodeBE(hs_version, VERSION_SIZE) ||
                 hs_public_key ||
                 encodeBE(hs_capabilities, CAPS_SIZE)

    -- Step 2: Wrap in wire frame
    hs_wire = encodeBE(MSG_HANDSHAKE, TYPE_SIZE) ||
              encodeBE(HANDSHAKE_PAYLOAD_SIZE, LENGTH_SIZE) ||
              hs_payload

    -- ------------------------------------------------------------------
    -- Data Encoding (type 0x02)
    -- ------------------------------------------------------------------

    -- Step 3: Encode data payload
    -- [sequence:4BE][data:N]
    data_inner = encodeBE(data_sequence, SEQUENCE_SIZE) || data_payload
    data_len = SEQUENCE_SIZE + length(data_payload)

    -- Step 4: Wrap in wire frame
    data_wire = encodeBE(MSG_DATA, TYPE_SIZE) ||
                encodeBE(data_len, LENGTH_SIZE) ||
                data_inner

    -- ------------------------------------------------------------------
    -- Ack Encoding (type 0x03)
    -- ------------------------------------------------------------------

    -- Step 5: Encode ack payload
    -- [sequence:4BE]
    ack_payload = encodeBE(ack_sequence, SEQUENCE_SIZE)

    -- Step 6: Wrap in wire frame
    ack_wire = encodeBE(MSG_ACK, TYPE_SIZE) ||
               encodeBE(ACK_PAYLOAD_SIZE, LENGTH_SIZE) ||
               ack_payload

    -- ------------------------------------------------------------------
    -- Peer Encoding (type 0x04)
    -- ------------------------------------------------------------------

    -- Step 7: Encode peer list payload
    -- [count:2BE]([host_len:2BE][host:M][port:2BE])*
    peer_count = length(peer_list)
    peer_entries = FOR_EACH peer IN peer_list:
        host_bytes = encodeUTF8(peer.host)
        host_len = length(host_bytes)
        encodeBE(host_len, HOSTLEN_SIZE) || host_bytes || encodeBE(peer.port, PORT_SIZE)

    peer_payload = encodeBE(peer_count, COUNT_SIZE) || peer_entries
    peer_payload_len = length(peer_payload)

    -- Step 8: Wrap in wire frame
    peer_wire = encodeBE(MSG_PEER, TYPE_SIZE) ||
                encodeBE(peer_payload_len, LENGTH_SIZE) ||
                peer_payload

    -- ------------------------------------------------------------------
    -- Ping Encoding (type 0x05)
    -- ------------------------------------------------------------------

    -- Step 9: Encode ping (empty payload)
    ping_wire = encodeBE(MSG_PING, TYPE_SIZE) ||
                encodeBE(PING_PAYLOAD_SIZE, LENGTH_SIZE)

    -- ------------------------------------------------------------------
    -- Pong Encoding (type 0x06)
    -- ------------------------------------------------------------------

    -- Step 10: Encode pong (empty payload)
    pong_wire = encodeBE(MSG_PONG, TYPE_SIZE) ||
                encodeBE(PONG_PAYLOAD_SIZE, LENGTH_SIZE)

    -- ==================================================================
    -- Decode Path — wire bytes → message type + payload
    -- ==================================================================

    -- Step 11: Validate minimum wire size
    wire_len = length(wire_bytes)
    size_ok = wire_len >= MIN_WIRE_SIZE

    -- Step 12: Parse wire header
    d_msg_type = wire_bytes[0]
    d_payload_len = decodeBE(wire_bytes[1 .. 4])
    d_payload = wire_bytes[HEADER_SIZE .. HEADER_SIZE + d_payload_len - 1]

    -- Step 13: Validate payload length against wire size
    -- wire_len must equal HEADER_SIZE + d_payload_len
    expected_wire_len = HEADER_SIZE + d_payload_len
    len_ok = (expected_wire_len == wire_len)

    -- Step 14: Validate message type is known
    type_ok = (d_msg_type >= MSG_HANDSHAKE) & (d_msg_type <= MSG_PONG)

    -- ------------------------------------------------------------------
    -- Decode Handshake (type 0x01)
    -- ------------------------------------------------------------------

    -- Step 15: Parse handshake payload
    -- Requires exactly 35 bytes: [version:1][public_key:32][capabilities:2]
    hs_len_ok = (d_payload_len == HANDSHAKE_PAYLOAD_SIZE)
    d_hs_version = d_payload[0]
    d_hs_public_key = d_payload[1 .. 32]
    d_hs_capabilities = decodeBE(d_payload[33 .. 34])

    -- ------------------------------------------------------------------
    -- Decode Data (type 0x02)
    -- ------------------------------------------------------------------

    -- Step 16: Parse data payload
    -- Requires at least 4 bytes: [sequence:4][data:N]
    data_min_ok = (d_payload_len >= SEQUENCE_SIZE)
    d_data_sequence = decodeBE(d_payload[0 .. 3])
    d_data_payload = d_payload[SEQUENCE_SIZE .. d_payload_len - 1]

    -- ------------------------------------------------------------------
    -- Decode Ack (type 0x03)
    -- ------------------------------------------------------------------

    -- Step 17: Parse ack payload
    -- Requires exactly 4 bytes: [sequence:4]
    ack_len_ok = (d_payload_len == ACK_PAYLOAD_SIZE)
    d_ack_sequence = decodeBE(d_payload[0 .. 3])

    -- ------------------------------------------------------------------
    -- Decode Peer (type 0x04)
    -- ------------------------------------------------------------------

    -- Step 18: Parse peer list payload
    -- Requires at least 2 bytes: [count:2]([host_len:2][host:M][port:2])*
    peer_min_ok = (d_payload_len >= COUNT_SIZE)
    d_peer_count = decodeBE(d_payload[0 .. 1])

    -- Step 19: Iteratively parse peer entries
    -- Each entry: [host_len:2BE][host:host_len][port:2BE]
    -- Parse sequentially, advancing cursor through payload.
    d_peer_list = PARSE_LOOP(d_payload, COUNT_SIZE, d_peer_count):
        entry_host_len = decodeBE(cursor[0 .. 1])
        entry_host = decodeUTF8(cursor[HOSTLEN_SIZE .. HOSTLEN_SIZE + entry_host_len - 1])
        entry_port = decodeBE(cursor[HOSTLEN_SIZE + entry_host_len ..
                                     HOSTLEN_SIZE + entry_host_len + PORT_SIZE - 1])
        advance(HOSTLEN_SIZE + entry_host_len + PORT_SIZE)

    -- ------------------------------------------------------------------
    -- Decode Ping (type 0x05)
    -- ------------------------------------------------------------------

    -- Step 20: Validate ping has empty payload
    ping_ok = (d_payload_len == PING_PAYLOAD_SIZE)

    -- ------------------------------------------------------------------
    -- Decode Pong (type 0x06)
    -- ------------------------------------------------------------------

    -- Step 21: Validate pong has empty payload
    pong_ok = (d_payload_len == PONG_PAYLOAD_SIZE)

    -- ------------------------------------------------------------------
    -- Final validation
    -- ------------------------------------------------------------------

    -- Step 22: Combine all validation checks
    -- Type-specific validation is applied based on d_msg_type.
    -- The overall valid flag requires header checks plus the relevant
    -- payload check for the parsed message type.
    valid = size_ok & len_ok & type_ok
  }
}
