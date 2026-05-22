-- MessageFormat Specification — 1024-byte Block Padding
--
-- This .spec file encodes the message padding scheme used to normalize
-- all UmbraVOX messages to fixed-size 1024-byte blocks before encryption.
--
-- Purpose: Eliminate message-length side channels by ensuring every
-- encrypted message is an exact multiple of 1024 bytes.
--
-- Pad (encode):
--   1. Compute padding length to reach next 1024-byte boundary
--   2. Prepend 2-byte big-endian payload length header
--   3. Apply PKCS#7 padding to fill remaining block space
--
-- Unpad (decode):
--   1. Read 2-byte big-endian length header
--   2. Extract payload bytes
--   3. Verify PKCS#7 padding is well-formed
--
-- Block size: 1024 bytes
-- Header size: 2 bytes (big-endian uint16, max payload = 65535)
-- Max single-block payload: 1024 - 2 - 1 = 1021 bytes
-- Multi-block payloads span ceil((len + 2 + 1) / 1024) blocks

algorithm MessageFormat {

  params {
    payload : Bytes    -- Plaintext message to pad (encode path)
    block   : Bytes    -- Padded block(s) to unpad (decode path)
  }

  constants {
    -- Block size in bytes
    BLOCK_SIZE = 1024

    -- Length header size in bytes (big-endian uint16)
    HEADER_SIZE = 2

    -- Maximum payload for a single block: BLOCK_SIZE - HEADER_SIZE - 1
    -- (at least 1 byte of PKCS#7 padding is always required)
    MAX_SINGLE_PAYLOAD = 1021

    -- Minimum padded output size
    MIN_OUTPUT = 1024
  }

  steps {
    -- ==================================================================
    -- Pad Path — Encode payload into 1024-byte aligned block(s)
    -- ==================================================================

    -- Step 1: Compute total content size (header + payload)
    -- content_len = HEADER_SIZE + length(payload)
    payload_len = length(payload)
    content_len = HEADER_SIZE + payload_len

    -- Step 2: Compute number of blocks needed
    -- PKCS#7 requires at least 1 byte of padding, so:
    -- total_blocks = ceil((content_len + 1) / BLOCK_SIZE)
    -- padded_len = total_blocks * BLOCK_SIZE
    total_blocks = ceil((content_len + 1) / BLOCK_SIZE)
    padded_len = total_blocks * BLOCK_SIZE

    -- Step 3: Compute PKCS#7 padding length
    -- pad_len = padded_len - content_len
    -- pad_byte = pad_len (mod 256, since PKCS#7 uses pad_len as byte value)
    --
    -- PKCS#7: if pad_len <= 255, pad with pad_len copies of pad_len
    -- For pad_len > 255: use 256 as the repeating byte, then store
    -- the true pad_len in the final padding structure.
    -- In practice pad_len <= 1024 for single blocks.
    pad_len = padded_len - content_len

    -- Step 4: Encode the length header (2 bytes, big-endian)
    -- header[0] = (payload_len >> 8) & 0xFF
    -- header[1] = payload_len & 0xFF
    header = encodeBE(payload_len, HEADER_SIZE)

    -- Step 5: Construct PKCS#7 padding bytes
    -- For standard PKCS#7, each padding byte = pad_len (when pad_len <= 255).
    -- For blocks where pad_len > 255, we use a two-tier scheme:
    --   inner_pad = pad_len mod 256  (if 0, use 256)
    --   padding = repeat(inner_pad, pad_len)
    -- This allows constant-time verification.
    inner_pad = IF (pad_len % 256) == 0 THEN 256 ELSE (pad_len % 256)
    padding = repeat(inner_pad, pad_len)

    -- Step 6: Assemble padded output
    -- output = header || payload || padding
    padded_output = header || payload || padding

    -- ==================================================================
    -- Unpad Path — Decode padded block(s) to extract payload
    -- ==================================================================

    -- Step 7: Validate minimum block size
    -- Block must be at least BLOCK_SIZE bytes and a multiple of BLOCK_SIZE.
    block_len = length(block)
    size_ok = (block_len >= MIN_OUTPUT) & ((block_len % BLOCK_SIZE) == 0)

    -- Step 8: Read length header (2 bytes, big-endian)
    -- u_payload_len = (block[0] << 8) | block[1]
    u_payload_len = decodeBE(block[0..1])

    -- Step 9: Validate payload length against block size
    -- The claimed payload + header + at least 1 pad byte must fit.
    -- u_payload_len + HEADER_SIZE + 1 <= block_len
    len_ok = (u_payload_len + HEADER_SIZE + 1) <= block_len

    -- Step 10: Extract payload
    -- u_payload = block[HEADER_SIZE .. HEADER_SIZE + u_payload_len - 1]
    u_payload = block[HEADER_SIZE .. HEADER_SIZE + u_payload_len - 1]

    -- Step 11: Verify PKCS#7 padding (constant-time)
    -- u_pad_len = block_len - HEADER_SIZE - u_payload_len
    -- u_inner_pad = if (u_pad_len mod 256) == 0 then 256 else (u_pad_len mod 256)
    -- All padding bytes must equal u_inner_pad.
    -- Verification must be constant-time to prevent timing side channels.
    u_pad_start = HEADER_SIZE + u_payload_len
    u_pad_len = block_len - u_pad_start
    u_inner_pad = IF (u_pad_len % 256) == 0 THEN 256 ELSE (u_pad_len % 256)

    -- Constant-time check: OR together all (pad_byte XOR expected) values
    -- pad_ok = 1 if all padding bytes match, 0 otherwise
    pad_ok = constantTimeVerifyPad(block[u_pad_start .. block_len - 1], u_inner_pad)

    -- Step 12: Final validation
    -- All checks must pass: size, length, padding
    valid = size_ok & len_ok & pad_ok
  }
}
