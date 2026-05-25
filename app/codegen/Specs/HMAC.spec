-- HMAC Specification (RFC 2104)
--
-- This .spec file encodes the HMAC (Keyed-Hash Message Authentication
-- Code) construction as defined in RFC 2104.
--
-- HMAC(K, m) = H((K' XOR opad) || H((K' XOR ipad) || m))

algorithm HMAC {

  params {
    key        : Bytes
    message    : Bytes
    hash_fn    : Bytes     -- Underlying hash function (e.g. SHA-256, SHA-512)
    block_size : UInt32    -- Hash function block size in bytes (64 for SHA-256, 128 for SHA-512)
  }

  constants {
    -- RFC 2104, Section 2 — Fixed padding bytes
    ipad = 0x36
    opad = 0x5c
  }

  steps {
    -- ==================================================================
    -- RFC 2104, Section 2 — Key preparation
    --
    -- If |key| > block_size:
    --   K' = hash_fn(key)            (hash to reduce length)
    -- Else:
    --   K' = key
    -- Then pad K' with zeros on the right to block_size bytes.
    -- ==================================================================
    key_hashed = hash_fn(key)                                        -- pre-hash for long keys
    key_selected = IF length(key) > block_size THEN key_hashed ELSE key
    key_padded = key_selected || zeros(block_size - length(key_selected))

    -- ==================================================================
    -- RFC 2104, Section 2 — Inner and outer keys
    --
    -- ipadKey = K' XOR (ipad repeated block_size times)
    -- opadKey = K' XOR (opad repeated block_size times)
    -- ==================================================================
    ipad_block = repeat(ipad, block_size)      -- 0x363636...36 (block_size bytes)
    opad_block = repeat(opad, block_size)      -- 0x5c5c5c...5c (block_size bytes)
    ipad_key = key_padded ^ ipad_block         -- bytewise XOR
    opad_key = key_padded ^ opad_block         -- bytewise XOR

    -- ==================================================================
    -- RFC 2104, Section 2 — Inner hash
    --
    -- inner = H(ipadKey || message)
    -- ==================================================================
    inner_data = ipad_key || message
    inner_hash = hash_fn(inner_data)

    -- ==================================================================
    -- RFC 2104, Section 2 — Outer hash (final MAC)
    --
    -- HMAC = H(opadKey || inner)
    -- ==================================================================
    outer_data = opad_key || inner_hash
    hmac = hash_fn(outer_data)
  }
}
