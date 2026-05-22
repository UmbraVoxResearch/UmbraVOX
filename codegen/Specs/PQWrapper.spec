-- PQWrapper Specification — Post-Quantum Hybrid KEM + AEAD Composition
--
-- This .spec file encodes the composition of ML-KEM-768 (FIPS 203) with
-- AES-256-GCM (NIST SP 800-38D) to provide a post-quantum secure
-- encrypt/decrypt primitive.
--
-- Seal (encrypt):
--   1. ML-KEM.Encaps  -> (shared_secret, ciphertext_kem)
--   2. HKDF-Expand    -> symmetric key from shared_secret
--   3. AES-256-GCM    -> encrypt plaintext
--
-- Open (decrypt):
--   1. ML-KEM.Decaps  -> shared_secret from ciphertext_kem
--   2. HKDF-Expand    -> symmetric key from shared_secret
--   3. AES-256-GCM    -> decrypt ciphertext
--
-- References: FIPS 203 (ML-KEM), NIST SP 800-38D (AES-GCM), RFC 5869 (HKDF)

algorithm PQWrapper {

  params {
    -- Seal inputs
    ek_recipient : Bytes(1184)  -- ML-KEM-768 encapsulation key
    plaintext    : Bytes        -- Message to encrypt
    aad          : Bytes        -- Additional authenticated data (may be empty)

    -- Open inputs (alternative path)
    dk_recipient : Bytes(2400)  -- ML-KEM-768 decapsulation key
    sealed_msg   : Bytes        -- KEM ciphertext || nonce || AEAD ciphertext || tag
  }

  constants {
    -- ML-KEM-768 sizes (FIPS 203, Table 2)
    KEM_CT_LEN  = 1088   -- ML-KEM-768 ciphertext length in bytes
    KEM_SS_LEN  = 32     -- ML-KEM-768 shared secret length in bytes

    -- AES-256-GCM parameters (NIST SP 800-38D)
    AES_KEY_LEN = 32     -- 256-bit key
    GCM_IV_LEN  = 12     -- 96-bit nonce (recommended)
    GCM_TAG_LEN = 16     -- 128-bit authentication tag

    -- HKDF context label for key derivation
    HKDF_LABEL = "UmbraVOX-PQWrapper-v1"
  }

  steps {
    -- ==================================================================
    -- Seal Path — Encapsulate, Derive Key, Encrypt
    -- ==================================================================

    -- Step 1: ML-KEM-768 Encapsulation (FIPS 203, Algorithm 16)
    -- Generate a random message m, encapsulate to produce shared secret
    -- and KEM ciphertext.
    -- (shared_secret, ct_kem) = ML-KEM.Encaps(ek_recipient)
    m = random(32)
    (shared_secret, ct_kem) = MLKEM768_Encaps(ek_recipient, m)

    -- Step 2: Key Derivation via HKDF (RFC 5869)
    -- Derive a 256-bit AES key and 96-bit nonce from the shared secret.
    -- salt = ct_kem (binds derivation to this encapsulation)
    -- PRK = HKDF-Extract(salt=ct_kem, IKM=shared_secret)
    -- aes_key = HKDF-Expand(PRK, info=HKDF_LABEL || "key", 32)
    -- nonce   = HKDF-Expand(PRK, info=HKDF_LABEL || "nonce", 12)
    prk = HMAC_SHA512(ct_kem, shared_secret)
    aes_key = HKDF_Expand(prk, HKDF_LABEL || "key", AES_KEY_LEN)
    nonce   = HKDF_Expand(prk, HKDF_LABEL || "nonce", GCM_IV_LEN)

    -- Step 3: AES-256-GCM Encryption (NIST SP 800-38D)
    -- Encrypt the plaintext with the derived key and nonce.
    -- AAD provides additional context binding.
    -- (ciphertext, tag) = AES-256-GCM.Encrypt(aes_key, nonce, plaintext, aad)
    (ciphertext, tag) = AES256GCM_Encrypt(aes_key, nonce, plaintext, aad)

    -- Step 4: Assemble sealed output
    -- sealed = ct_kem || nonce || ciphertext || tag
    sealed = ct_kem || nonce || ciphertext || tag

    -- ==================================================================
    -- Open Path — Decapsulate, Derive Key, Decrypt
    -- ==================================================================

    -- Step 5: Parse sealed message
    -- Extract KEM ciphertext, nonce, AEAD ciphertext, and tag
    o_ct_kem    = sealed_msg[0 .. KEM_CT_LEN - 1]
    o_nonce     = sealed_msg[KEM_CT_LEN .. KEM_CT_LEN + GCM_IV_LEN - 1]
    o_ct_aead   = sealed_msg[KEM_CT_LEN + GCM_IV_LEN ..
                             length(sealed_msg) - GCM_TAG_LEN - 1]
    o_tag       = sealed_msg[length(sealed_msg) - GCM_TAG_LEN ..
                             length(sealed_msg) - 1]

    -- Step 6: ML-KEM-768 Decapsulation (FIPS 203, Algorithm 17)
    -- Recover the shared secret from the KEM ciphertext.
    -- Uses implicit rejection: if ciphertext is invalid, returns
    -- a pseudorandom value derived from the secret key and ciphertext.
    -- shared_secret' = ML-KEM.Decaps(dk_recipient, o_ct_kem)
    o_shared_secret = MLKEM768_Decaps(dk_recipient, o_ct_kem)

    -- Step 7: Key Derivation (same as seal path)
    -- PRK' = HKDF-Extract(salt=o_ct_kem, IKM=shared_secret')
    -- aes_key' = HKDF-Expand(PRK', info=HKDF_LABEL || "key", 32)
    -- nonce is already extracted from sealed_msg (must match)
    o_prk = HMAC_SHA512(o_ct_kem, o_shared_secret)
    o_aes_key = HKDF_Expand(o_prk, HKDF_LABEL || "key", AES_KEY_LEN)
    o_nonce_derived = HKDF_Expand(o_prk, HKDF_LABEL || "nonce", GCM_IV_LEN)

    -- Step 8: Verify nonce consistency
    -- The nonce embedded in the sealed message must match the derived nonce.
    -- This catches tampering with the KEM ciphertext portion.
    nonce_ok = constantTimeEq(o_nonce, o_nonce_derived)

    -- Step 9: AES-256-GCM Decryption (NIST SP 800-38D)
    -- Decrypt and authenticate the AEAD ciphertext.
    -- plaintext' = AES-256-GCM.Decrypt(aes_key', o_nonce, o_ct_aead, aad, o_tag)
    -- Returns failure if tag verification fails.
    (o_plaintext, auth_ok) = AES256GCM_Decrypt(o_aes_key, o_nonce, o_ct_aead, aad, o_tag)

    -- Step 10: Final validation
    -- Both nonce consistency and AEAD authentication must pass.
    valid = nonce_ok & auth_ok
  }
}
