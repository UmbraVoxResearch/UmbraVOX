-- AES-256 Block Cipher (FIPS 197)
-- Reference: Federal Information Processing Standards Publication 197
-- Advanced Encryption Standard (AES)
--
-- AES-256: Nk=8, Nr=14, 32-byte key, 16-byte block

algorithm AES256 {
  params {
    key : Bytes
    block : Bytes
  }

  constants {
    -- FIPS 197 Section 5.2 — Round constants (Rcon)
    Rcon_1 = 0x01000000
    Rcon_2 = 0x02000000
    Rcon_3 = 0x04000000
    Rcon_4 = 0x08000000
    Rcon_5 = 0x10000000
    Rcon_6 = 0x20000000
    Rcon_7 = 0x40000000

    -- FIPS 197 Section 5.1.1 — S-box (SubBytes transformation)
    -- 256-entry lookup table: S[xy] = {xy} mapped through GF(2^8) inversion + affine
    S_0x0 = 0x637c777b
    S_0x1 = 0xf26b6fc5
    S_0x2 = 0x3001672b
    S_0x3 = 0xfed7ab76
    S_0x4 = 0xca82c97d
    S_0x5 = 0xfa5947f0
    S_0x6 = 0xadd4a2af
    S_0x7 = 0x9ca472c0
    S_0x8 = 0xb7fd9326
    S_0x9 = 0x3614b5f7
    S_0xa = 0x09831f7d
    S_0xb = 0x2f7ee71d
    S_0xc = 0xe37281de
    S_0xd = 0xa53bdd47
    S_0xe = 0xf7caeb48
    S_0xf = 0xb45bef15

    -- GF(2^8) reduction polynomial
    GF_poly = 0x1b

    -- AES-256 parameters
    Nk = 0x00000008
    Nr = 0x0000000e
  }

  steps {
    -- FIPS 197 Section 5.2 — Key Expansion
    -- Expand 32-byte key into 60-word key schedule (15 round keys)
    -- W[i] for i < 8: W[i] = key_word[i]
    -- W[i] for i >= 8:
    --   if i mod 8 == 0: W[i] = W[i-8] ^ SubWord(RotWord(W[i-1])) ^ Rcon[i/8]
    --   if i mod 8 == 4: W[i] = W[i-8] ^ SubWord(W[i-1])
    --   else:            W[i] = W[i-8] ^ W[i-1]

    -- FIPS 197 Section 5.1.1 — SubBytes
    -- Apply S-box substitution to each byte of state
    -- state[i] = S[state[i]] for all 16 bytes

    -- FIPS 197 Section 5.1.2 — ShiftRows
    -- Row 0: no shift
    -- Row 1: left shift 1
    -- Row 2: left shift 2
    -- Row 3: left shift 3

    -- FIPS 197 Section 5.1.3 — MixColumns
    -- Each column c: s'[0,c] = 2*s[0,c] ^ 3*s[1,c] ^ s[2,c] ^ s[3,c]
    --                s'[1,c] = s[0,c] ^ 2*s[1,c] ^ 3*s[2,c] ^ s[3,c]
    --                s'[2,c] = s[0,c] ^ s[1,c] ^ 2*s[2,c] ^ 3*s[3,c]
    --                s'[3,c] = 3*s[0,c] ^ s[1,c] ^ s[2,c] ^ 2*s[3,c]
    -- where multiplication is in GF(2^8) mod GF_poly

    -- FIPS 197 Section 5.1.4 — AddRoundKey
    -- state[i] = state[i] ^ round_key[i] for all 16 bytes

    -- FIPS 197 Section 5.1 — Cipher (Encryption)
    -- Initial: AddRoundKey(state, W[0..3])
    -- Rounds 1-13: SubBytes, ShiftRows, MixColumns, AddRoundKey(state, W[4r..4r+3])
    -- Round 14: SubBytes, ShiftRows, AddRoundKey(state, W[56..59])

    -- Initial round key addition
    state_init = block ^ key_schedule_0

    -- Rounds 1 through 13 (SubBytes + ShiftRows + MixColumns + AddRoundKey)
    -- Round 1
    r1_sub = SubBytes(state_init)
    r1_shift = ShiftRows(r1_sub)
    r1_mix = MixColumns(r1_shift)
    r1_state = r1_mix ^ key_schedule_1

    -- Rounds 2-13 follow identical pattern
    -- (In generated code, this unrolls to 13 identical blocks)

    -- Round 14 (final: no MixColumns)
    r14_sub = SubBytes(r13_state)
    r14_shift = ShiftRows(r14_sub)
    ciphertext = r14_shift ^ key_schedule_14
  }
}
