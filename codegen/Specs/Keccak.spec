-- Keccak Specification (FIPS 202)
--
-- This .spec file encodes the Keccak-f[1600] permutation and sponge
-- construction as defined in NIST FIPS 202 (SHA-3 Standard).
--
-- State size: 1600 bits (25 x 64-bit lanes)
-- Lane size: 64 bits (UInt64)
-- Rounds: 24
-- Indexing: state[x + 5*y], x,y in [0..4]

algorithm Keccak {

  params {
    message    : Bytes
    rate       : UInt32    -- Rate in bytes (e.g. 136 for SHA3-256)
    suffix     : UInt32    -- Domain separation byte (0x06 for SHA-3, 0x1F for SHAKE)
    output_len : UInt32    -- Desired output length in bytes
  }

  constants {
    -- FIPS 202, Section 3.2 — Round constants RC[0..23] (iota step)
    RC_0  = 0x0000000000000001 : UInt64
    RC_1  = 0x0000000000008082 : UInt64
    RC_2  = 0x800000000000808A : UInt64
    RC_3  = 0x8000000080008000 : UInt64
    RC_4  = 0x000000000000808B : UInt64
    RC_5  = 0x0000000080000001 : UInt64
    RC_6  = 0x8000000080008081 : UInt64
    RC_7  = 0x8000000000008009 : UInt64
    RC_8  = 0x000000000000008A : UInt64
    RC_9  = 0x0000000000000088 : UInt64
    RC_10 = 0x0000000080008009 : UInt64
    RC_11 = 0x000000008000000A : UInt64
    RC_12 = 0x000000008000808B : UInt64
    RC_13 = 0x800000000000008B : UInt64
    RC_14 = 0x8000000000008089 : UInt64
    RC_15 = 0x8000000000008003 : UInt64
    RC_16 = 0x8000000000008002 : UInt64
    RC_17 = 0x8000000000000080 : UInt64
    RC_18 = 0x000000000000800A : UInt64
    RC_19 = 0x800000008000000A : UInt64
    RC_20 = 0x8000000080008081 : UInt64
    RC_21 = 0x8000000000008080 : UInt64
    RC_22 = 0x0000000080000001 : UInt64
    RC_23 = 0x8000000080008008 : UInt64

    -- FIPS 202, Section 3.2.2 — Rotation offsets ROT[0..24]
    -- Indexed by x + 5*y (row-major over the 5x5 lane grid)
    ROT_0  =  0 : UInt32    -- (0,0)
    ROT_1  =  1 : UInt32    -- (1,0)
    ROT_2  = 62 : UInt32    -- (2,0)
    ROT_3  = 28 : UInt32    -- (3,0)
    ROT_4  = 27 : UInt32    -- (4,0)
    ROT_5  = 36 : UInt32    -- (0,1)
    ROT_6  = 44 : UInt32    -- (1,1)
    ROT_7  =  6 : UInt32    -- (2,1)
    ROT_8  = 55 : UInt32    -- (3,1)
    ROT_9  = 20 : UInt32    -- (4,1)
    ROT_10 =  3 : UInt32    -- (0,2)
    ROT_11 = 10 : UInt32    -- (1,2)
    ROT_12 = 43 : UInt32    -- (2,2)
    ROT_13 = 25 : UInt32    -- (3,2)
    ROT_14 = 39 : UInt32    -- (4,2)
    ROT_15 = 41 : UInt32    -- (0,3)
    ROT_16 = 45 : UInt32    -- (1,3)
    ROT_17 = 15 : UInt32    -- (2,3)
    ROT_18 = 21 : UInt32    -- (3,3)
    ROT_19 =  8 : UInt32    -- (4,3)
    ROT_20 = 18 : UInt32    -- (0,4)
    ROT_21 =  2 : UInt32    -- (1,4)
    ROT_22 = 61 : UInt32    -- (2,4)
    ROT_23 = 56 : UInt32    -- (3,4)
    ROT_24 = 14 : UInt32    -- (4,4)
  }

  steps {
    -- ==================================================================
    -- FIPS 202, Section 3.2.1 — Theta step (column parity diffusion)
    --
    -- C[x] = state[x,0] XOR state[x,1] XOR state[x,2] XOR state[x,3] XOR state[x,4]
    -- D[x] = C[x-1 mod 5] XOR ROT(C[x+1 mod 5], 1)
    -- state[x,y] = state[x,y] XOR D[x]
    -- ==================================================================

    -- Column parities: C[x] = XOR of all 5 lanes in column x
    C_0 = state[0] ^ state[5]  ^ state[10] ^ state[15] ^ state[20]
    C_1 = state[1] ^ state[6]  ^ state[11] ^ state[16] ^ state[21]
    C_2 = state[2] ^ state[7]  ^ state[12] ^ state[17] ^ state[22]
    C_3 = state[3] ^ state[8]  ^ state[13] ^ state[18] ^ state[23]
    C_4 = state[4] ^ state[9]  ^ state[14] ^ state[19] ^ state[24]

    -- Diffusion: D[x] = C[x-1] XOR ROT(C[x+1], 1)
    D_0 = C_4 ^ (C_1 <<< 1)
    D_1 = C_0 ^ (C_2 <<< 1)
    D_2 = C_1 ^ (C_3 <<< 1)
    D_3 = C_2 ^ (C_4 <<< 1)
    D_4 = C_3 ^ (C_0 <<< 1)

    -- Apply theta: XOR D[x] into every lane of column x
    -- Column 0:  state[0,5,10,15,20]  ^= D_0
    -- Column 1:  state[1,6,11,16,21]  ^= D_1
    -- Column 2:  state[2,7,12,17,22]  ^= D_2
    -- Column 3:  state[3,8,13,18,23]  ^= D_3
    -- Column 4:  state[4,9,14,19,24]  ^= D_4
    theta_i = state[i] ^ D_(i mod 5)    -- for i in [0..24]

    -- ==================================================================
    -- FIPS 202, Section 3.2.2 — Rho step (lane rotation)
    --
    -- state[x,y] = ROT(state[x,y], ROT[x + 5*y])
    -- ==================================================================
    rho_i = theta_i <<< ROT_i            -- for i in [0..24]

    -- ==================================================================
    -- FIPS 202, Section 3.2.3 — Pi step (lane permutation)
    --
    -- dst[y + 5*(2*x + 3*y mod 5)] = src[x + 5*y]
    --
    -- Permutation mapping (src index -> dst index):
    --   0->0   1->10  2->20  3->5   4->15
    --   5->16  6->1   7->11  8->21  9->6
    --  10->7  11->17 12->2  13->12 14->22
    --  15->23 16->8  17->18 18->3  19->13
    --  20->14 21->24 22->9  23->19 24->4
    -- ==================================================================
    pi_0  = rho_0
    pi_10 = rho_1
    pi_20 = rho_2
    pi_5  = rho_3
    pi_15 = rho_4
    pi_16 = rho_5
    pi_1  = rho_6
    pi_11 = rho_7
    pi_21 = rho_8
    pi_6  = rho_9
    pi_7  = rho_10
    pi_17 = rho_11
    pi_2  = rho_12
    pi_12 = rho_13
    pi_22 = rho_14
    pi_23 = rho_15
    pi_8  = rho_16
    pi_18 = rho_17
    pi_3  = rho_18
    pi_13 = rho_19
    pi_14 = rho_20
    pi_24 = rho_21
    pi_9  = rho_22
    pi_19 = rho_23
    pi_4  = rho_24

    -- ==================================================================
    -- FIPS 202, Section 3.2.4 — Chi step (nonlinear mixing)
    --
    -- For each row y (base = 5*y):
    --   a[x] = a[x] XOR (NOT a[x+1 mod 5] AND a[x+2 mod 5])
    -- ==================================================================

    -- Row 0 (y=0): indices 0..4
    chi_0 = pi_0 ^ ((NOT pi_1) & pi_2)
    chi_1 = pi_1 ^ ((NOT pi_2) & pi_3)
    chi_2 = pi_2 ^ ((NOT pi_3) & pi_4)
    chi_3 = pi_3 ^ ((NOT pi_4) & pi_0)
    chi_4 = pi_4 ^ ((NOT pi_0) & pi_1)

    -- Row 1 (y=1): indices 5..9
    chi_5 = pi_5 ^ ((NOT pi_6) & pi_7)
    chi_6 = pi_6 ^ ((NOT pi_7) & pi_8)
    chi_7 = pi_7 ^ ((NOT pi_8) & pi_9)
    chi_8 = pi_8 ^ ((NOT pi_9) & pi_5)
    chi_9 = pi_9 ^ ((NOT pi_5) & pi_6)

    -- Row 2 (y=2): indices 10..14
    chi_10 = pi_10 ^ ((NOT pi_11) & pi_12)
    chi_11 = pi_11 ^ ((NOT pi_12) & pi_13)
    chi_12 = pi_12 ^ ((NOT pi_13) & pi_14)
    chi_13 = pi_13 ^ ((NOT pi_14) & pi_10)
    chi_14 = pi_14 ^ ((NOT pi_10) & pi_11)

    -- Row 3 (y=3): indices 15..19
    chi_15 = pi_15 ^ ((NOT pi_16) & pi_17)
    chi_16 = pi_16 ^ ((NOT pi_17) & pi_18)
    chi_17 = pi_17 ^ ((NOT pi_18) & pi_19)
    chi_18 = pi_18 ^ ((NOT pi_19) & pi_15)
    chi_19 = pi_19 ^ ((NOT pi_15) & pi_16)

    -- Row 4 (y=4): indices 20..24
    chi_20 = pi_20 ^ ((NOT pi_21) & pi_22)
    chi_21 = pi_21 ^ ((NOT pi_22) & pi_23)
    chi_22 = pi_22 ^ ((NOT pi_23) & pi_24)
    chi_23 = pi_23 ^ ((NOT pi_24) & pi_20)
    chi_24 = pi_24 ^ ((NOT pi_20) & pi_21)

    -- ==================================================================
    -- FIPS 202, Section 3.2.5 — Iota step (round constant XOR)
    --
    -- state[0,0] = state[0,0] XOR RC[round]
    -- Only lane 0 is affected; all other lanes pass through unchanged.
    -- ==================================================================
    iota_0 = chi_0 ^ RC_r    -- where r is the round index [0..23]
    -- iota_i = chi_i          -- for i in [1..24] (unchanged)

    -- ==================================================================
    -- FIPS 202, Section 4 — Sponge construction
    -- ==================================================================

    -- ---- Padding (pad10*1) ----
    -- Append domain separation suffix byte to message.
    -- Append zero bytes until length = rate - 1 mod rate.
    -- Set high bit (0x80) of last padding byte.
    -- If suffix and 0x80 fall on the same byte, they are OR'd together.
    padded = pad(message, rate, suffix)

    -- ---- Absorb phase ----
    -- Split padded message into rate-sized blocks.
    -- For each block:
    --   1. XOR block lanes into state[0 .. rate/8 - 1]  (little-endian)
    --   2. Apply keccakF1600 (24 rounds of theta, rho, pi, chi, iota)
    absorbed = absorb(padded, rate)

    -- ---- Squeeze phase ----
    -- Extract rate bytes from state (little-endian lane encoding).
    -- If output_len > rate, apply keccakF1600 and extract again.
    -- Repeat until output_len bytes have been produced.
    -- Truncate final block to output_len.
    output = squeeze(absorbed, rate, output_len)
  }
}
