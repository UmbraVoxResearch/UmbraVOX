-- Poly1305 One-Time Authenticator (RFC 8439 Section 2.5)
-- Reference: RFC 8439, "ChaCha20 and Poly1305 for IETF Protocols"
--
-- Poly1305 computes a 16-byte MAC over a message using a one-time
-- 32-byte key. The key is split into r (clamped) and s.
-- Arithmetic is performed in GF(2^130 - 5).

algorithm Poly1305 {
  params {
    key : Bytes
    message : Bytes
  }

  constants {
    -- Prime for GF(2^130-5)
    P = 0x3fffffffffffffffffffffffffffffffb

    -- Clamping mask for r (RFC 8439 Section 2.5.1)
    -- Clamp: r[3],r[7],r[11],r[15] AND 0x0f; r[4],r[8],r[12] AND 0xfc
    clamp_mask_0 = 0x0ffffffc
    clamp_mask_1 = 0x0ffffffc
    clamp_mask_2 = 0x0ffffffc
    clamp_mask_3 = 0x0fffffff
  }

  steps {
    -- RFC 8439 Section 2.5.1 — Key partitioning
    -- r = le_bytes_to_num(key[0..15]) with clamping
    -- s = le_bytes_to_num(key[16..31])
    r_raw = le_bytes(key, 0, 16)
    r = r_raw & clamp_mask

    s = le_bytes(key, 16, 16)

    -- RFC 8439 Section 2.5.1 — Accumulator initialization
    accumulator = 0x00000000

    -- RFC 8439 Section 2.5.1 — Process each 16-byte block
    -- For each complete 16-byte block i:
    --   n = le_bytes_to_num(block[i]) | (1 << 128)
    --   accumulator = ((accumulator +mod n) *mod r) mod P
    --
    -- For the final partial block (if message length not multiple of 16):
    --   pad block with 0x01 followed by zeros to 17 bytes
    --   n = le_bytes_to_num(padded_block)
    --   accumulator = ((accumulator +mod n) *mod r) mod P

    -- Block processing (shown for block i):
    block_n = le_bytes(message, i_offset, 16)
    block_with_hibit = block_n | hibit_128
    acc_plus_block = accumulator +mod block_with_hibit
    accumulator_next = (acc_plus_block *mod r) mod P

    -- Final partial block (if any):
    partial_len = message_len mod 16
    partial_block = le_bytes(message, last_offset, partial_len)
    partial_padded = partial_block | (0x01 << (partial_len * 8))
    acc_plus_partial = accumulator +mod partial_padded
    accumulator_final = (acc_plus_partial *mod r) mod P

    -- RFC 8439 Section 2.5.1 — Finalization
    -- tag = (accumulator + s) mod 2^128
    tag = (accumulator_final +mod s) & 0xffffffffffffffffffffffffffffffff
  }
}
