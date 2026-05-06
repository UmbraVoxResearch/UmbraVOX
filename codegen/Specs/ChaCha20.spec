-- ChaCha20 Specification (RFC 8439)
--
-- This .spec file encodes the ChaCha20 stream cipher as defined in
-- IETF RFC 8439 (ChaCha20 and Poly1305 for IETF Protocols).
--
-- Word size: 32 bits (UInt32)
-- State size: 512 bits (64 bytes, 16 x UInt32 words)
-- Key size: 256 bits (32 bytes)
-- Nonce size: 96 bits (12 bytes)
-- Rounds: 20 (10 double-rounds)

algorithm ChaCha20 {

  params {
    key     : Bytes(32)
    nonce   : Bytes(12)
    counter : UInt32
  }

  constants {
    -- RFC 8439, Section 2.3 — "expand 32-byte k" as four little-endian UInt32
    sigma_0 = 0x61707865
    sigma_1 = 0x3320646e
    sigma_2 = 0x79622d32
    sigma_3 = 0x6b206574
  }

  steps {
    -- ==================================================================
    -- RFC 8439, Section 2.1 — Quarter Round
    --
    -- QR(a, b, c, d):
    --   a += b; d ^= a; d <<<= 16;
    --   c += d; b ^= c; b <<<= 12;
    --   a += b; d ^= a; d <<<= 8;
    --   c += d; b ^= c; b <<<= 7;
    -- ==================================================================
    qr_a1 = a +mod b
    qr_d1 = (d ^ qr_a1) <<< 16
    qr_c1 = c +mod qr_d1
    qr_b1 = (b ^ qr_c1) <<< 12
    qr_a2 = qr_a1 +mod qr_b1
    qr_d2 = (qr_d1 ^ qr_a2) <<< 8
    qr_c2 = qr_c1 +mod qr_d2
    qr_b2 = (qr_b1 ^ qr_c2) <<< 7

    -- ==================================================================
    -- RFC 8439, Section 2.3 — Initial State Setup
    --
    -- The 16-word initial state is:
    --   [ sigma_0,  sigma_1,  sigma_2,  sigma_3  ]   constants
    --   [ key[0],   key[1],   key[2],   key[3]   ]   key words 0..3
    --   [ key[4],   key[5],   key[6],   key[7]   ]   key words 4..7
    --   [ counter,  nonce[0], nonce[1], nonce[2]  ]   counter + nonce
    --
    -- All key and nonce words are decoded little-endian.
    -- ==================================================================
    s_0  = sigma_0
    s_1  = sigma_1
    s_2  = sigma_2
    s_3  = sigma_3
    s_4  = getLE32(key, 0)
    s_5  = getLE32(key, 4)
    s_6  = getLE32(key, 8)
    s_7  = getLE32(key, 12)
    s_8  = getLE32(key, 16)
    s_9  = getLE32(key, 20)
    s_10 = getLE32(key, 24)
    s_11 = getLE32(key, 28)
    s_12 = counter
    s_13 = getLE32(nonce, 0)
    s_14 = getLE32(nonce, 4)
    s_15 = getLE32(nonce, 8)

    -- ==================================================================
    -- RFC 8439, Section 2.3 — 20 Rounds (10 double-rounds)
    --
    -- Each double-round consists of:
    --   Column round:
    --     QR(s0, s4, s8,  s12)
    --     QR(s1, s5, s9,  s13)
    --     QR(s2, s6, s10, s14)
    --     QR(s3, s7, s11, s15)
    --   Diagonal round:
    --     QR(s0, s5, s10, s15)
    --     QR(s1, s6, s11, s12)
    --     QR(s2, s7, s8,  s13)
    --     QR(s3, s4, s9,  s14)
    --
    -- Below: fully unrolled, 10 double-rounds.
    -- Each QR is expanded inline using the quarter round definition.
    -- ==================================================================

    -- ---- Double-round 1 — Column round ----

    -- QR(s_0, s_4, s_8, s_12)
    r1c0_a1 = s_0  +mod s_4;   r1c0_d1 = (s_12 ^ r1c0_a1) <<< 16
    r1c0_c1 = s_8  +mod r1c0_d1; r1c0_b1 = (s_4  ^ r1c0_c1) <<< 12
    r1c0_a2 = r1c0_a1 +mod r1c0_b1; r1c0_d2 = (r1c0_d1 ^ r1c0_a2) <<< 8
    r1c0_c2 = r1c0_c1 +mod r1c0_d2; r1c0_b2 = (r1c0_b1 ^ r1c0_c2) <<< 7

    -- QR(s_1, s_5, s_9, s_13)
    r1c1_a1 = s_1  +mod s_5;   r1c1_d1 = (s_13 ^ r1c1_a1) <<< 16
    r1c1_c1 = s_9  +mod r1c1_d1; r1c1_b1 = (s_5  ^ r1c1_c1) <<< 12
    r1c1_a2 = r1c1_a1 +mod r1c1_b1; r1c1_d2 = (r1c1_d1 ^ r1c1_a2) <<< 8
    r1c1_c2 = r1c1_c1 +mod r1c1_d2; r1c1_b2 = (r1c1_b1 ^ r1c1_c2) <<< 7

    -- QR(s_2, s_6, s_10, s_14)
    r1c2_a1 = s_2  +mod s_6;   r1c2_d1 = (s_14 ^ r1c2_a1) <<< 16
    r1c2_c1 = s_10 +mod r1c2_d1; r1c2_b1 = (s_6  ^ r1c2_c1) <<< 12
    r1c2_a2 = r1c2_a1 +mod r1c2_b1; r1c2_d2 = (r1c2_d1 ^ r1c2_a2) <<< 8
    r1c2_c2 = r1c2_c1 +mod r1c2_d2; r1c2_b2 = (r1c2_b1 ^ r1c2_c2) <<< 7

    -- QR(s_3, s_7, s_11, s_15)
    r1c3_a1 = s_3  +mod s_7;   r1c3_d1 = (s_15 ^ r1c3_a1) <<< 16
    r1c3_c1 = s_11 +mod r1c3_d1; r1c3_b1 = (s_7  ^ r1c3_c1) <<< 12
    r1c3_a2 = r1c3_a1 +mod r1c3_b1; r1c3_d2 = (r1c3_d1 ^ r1c3_a2) <<< 8
    r1c3_c2 = r1c3_c1 +mod r1c3_d2; r1c3_b2 = (r1c3_b1 ^ r1c3_c2) <<< 7

    -- ---- Double-round 1 — Diagonal round ----
    -- After column round, state is:
    --   [ r1c0_a2, r1c1_a2, r1c2_a2, r1c3_a2 ]
    --   [ r1c0_b2, r1c1_b2, r1c2_b2, r1c3_b2 ]
    --   [ r1c0_c2, r1c1_c2, r1c2_c2, r1c3_c2 ]
    --   [ r1c0_d2, r1c1_d2, r1c2_d2, r1c3_d2 ]

    -- QR(s0, s5, s10, s15)
    r1d0_a1 = r1c0_a2 +mod r1c1_b2; r1d0_d1 = (r1c3_d2 ^ r1d0_a1) <<< 16
    r1d0_c1 = r1c2_c2 +mod r1d0_d1; r1d0_b1 = (r1c1_b2 ^ r1d0_c1) <<< 12
    r1d0_a2 = r1d0_a1 +mod r1d0_b1; r1d0_d2 = (r1d0_d1 ^ r1d0_a2) <<< 8
    r1d0_c2 = r1d0_c1 +mod r1d0_d2; r1d0_b2 = (r1d0_b1 ^ r1d0_c2) <<< 7

    -- QR(s1, s6, s11, s12)
    r1d1_a1 = r1c1_a2 +mod r1c2_b2; r1d1_d1 = (r1c0_d2 ^ r1d1_a1) <<< 16
    r1d1_c1 = r1c3_c2 +mod r1d1_d1; r1d1_b1 = (r1c2_b2 ^ r1d1_c1) <<< 12
    r1d1_a2 = r1d1_a1 +mod r1d1_b1; r1d1_d2 = (r1d1_d1 ^ r1d1_a2) <<< 8
    r1d1_c2 = r1d1_c1 +mod r1d1_d2; r1d1_b2 = (r1d1_b1 ^ r1d1_c2) <<< 7

    -- QR(s2, s7, s8, s13)
    r1d2_a1 = r1c2_a2 +mod r1c3_b2; r1d2_d1 = (r1c1_d2 ^ r1d2_a1) <<< 16
    r1d2_c1 = r1c0_c2 +mod r1d2_d1; r1d2_b1 = (r1c3_b2 ^ r1d2_c1) <<< 12
    r1d2_a2 = r1d2_a1 +mod r1d2_b1; r1d2_d2 = (r1d2_d1 ^ r1d2_a2) <<< 8
    r1d2_c2 = r1d2_c1 +mod r1d2_d2; r1d2_b2 = (r1d2_b1 ^ r1d2_c2) <<< 7

    -- QR(s3, s4, s9, s14)
    r1d3_a1 = r1c3_a2 +mod r1c0_b2; r1d3_d1 = (r1c2_d2 ^ r1d3_a1) <<< 16
    r1d3_c1 = r1c1_c2 +mod r1d3_d1; r1d3_b1 = (r1c0_b2 ^ r1d3_c1) <<< 12
    r1d3_a2 = r1d3_a1 +mod r1d3_b1; r1d3_d2 = (r1d3_d1 ^ r1d3_a2) <<< 8
    r1d3_c2 = r1d3_c1 +mod r1d3_d2; r1d3_b2 = (r1d3_b1 ^ r1d3_c2) <<< 7

    -- End of double-round 1. Remap to state vector:
    --   s0=r1d0_a2, s1=r1d1_a2, s2=r1d2_a2, s3=r1d3_a2
    --   s4=r1d3_b2, s5=r1d0_b2, s6=r1d1_b2, s7=r1d2_b2
    --   s8=r1d2_c2, s9=r1d3_c2, s10=r1d0_c2, s11=r1d1_c2
    --   s12=r1d1_d2, s13=r1d2_d2, s14=r1d3_d2, s15=r1d0_d2

    -- ---- Double-round 2 — Column round ----
    -- For brevity, subsequent double-rounds follow the same pattern.
    -- The naming convention is: r{round}{c|d}{col}_{var}{step}
    -- where round=2..10, c=column, d=diagonal, col=0..3.
    --
    -- Round 2 inputs from round 1 diagonal outputs:
    --   [0]=r1d0_a2  [1]=r1d1_a2  [2]=r1d2_a2  [3]=r1d3_a2
    --   [4]=r1d3_b2  [5]=r1d0_b2  [6]=r1d1_b2  [7]=r1d2_b2
    --   [8]=r1d2_c2  [9]=r1d3_c2  [10]=r1d0_c2 [11]=r1d1_c2
    --   [12]=r1d1_d2 [13]=r1d2_d2 [14]=r1d3_d2 [15]=r1d0_d2

    -- QR([0], [4], [8], [12])
    r2c0_a1 = r1d0_a2 +mod r1d3_b2; r2c0_d1 = (r1d1_d2 ^ r2c0_a1) <<< 16
    r2c0_c1 = r1d2_c2 +mod r2c0_d1; r2c0_b1 = (r1d3_b2 ^ r2c0_c1) <<< 12
    r2c0_a2 = r2c0_a1 +mod r2c0_b1; r2c0_d2 = (r2c0_d1 ^ r2c0_a2) <<< 8
    r2c0_c2 = r2c0_c1 +mod r2c0_d2; r2c0_b2 = (r2c0_b1 ^ r2c0_c2) <<< 7

    -- QR([1], [5], [9], [13])
    r2c1_a1 = r1d1_a2 +mod r1d0_b2; r2c1_d1 = (r1d2_d2 ^ r2c1_a1) <<< 16
    r2c1_c1 = r1d3_c2 +mod r2c1_d1; r2c1_b1 = (r1d0_b2 ^ r2c1_c1) <<< 12
    r2c1_a2 = r2c1_a1 +mod r2c1_b1; r2c1_d2 = (r2c1_d1 ^ r2c1_a2) <<< 8
    r2c1_c2 = r2c1_c1 +mod r2c1_d2; r2c1_b2 = (r2c1_b1 ^ r2c1_c2) <<< 7

    -- QR([2], [6], [10], [14])
    r2c2_a1 = r1d2_a2 +mod r1d1_b2; r2c2_d1 = (r1d3_d2 ^ r2c2_a1) <<< 16
    r2c2_c1 = r1d0_c2 +mod r2c2_d1; r2c2_b1 = (r1d1_b2 ^ r2c2_c1) <<< 12
    r2c2_a2 = r2c2_a1 +mod r2c2_b1; r2c2_d2 = (r2c2_d1 ^ r2c2_a2) <<< 8
    r2c2_c2 = r2c2_c1 +mod r2c2_d2; r2c2_b2 = (r2c2_b1 ^ r2c2_c2) <<< 7

    -- QR([3], [7], [11], [15])
    r2c3_a1 = r1d3_a2 +mod r1d2_b2; r2c3_d1 = (r1d0_d2 ^ r2c3_a1) <<< 16
    r2c3_c1 = r1d1_c2 +mod r2c3_d1; r2c3_b1 = (r1d2_b2 ^ r2c3_c1) <<< 12
    r2c3_a2 = r2c3_a1 +mod r2c3_b1; r2c3_d2 = (r2c3_d1 ^ r2c3_a2) <<< 8
    r2c3_c2 = r2c3_c1 +mod r2c3_d2; r2c3_b2 = (r2c3_b1 ^ r2c3_c2) <<< 7

    -- ---- Double-round 2 — Diagonal round ----

    -- QR([0], [5], [10], [15])
    r2d0_a1 = r2c0_a2 +mod r2c1_b2; r2d0_d1 = (r2c3_d2 ^ r2d0_a1) <<< 16
    r2d0_c1 = r2c2_c2 +mod r2d0_d1; r2d0_b1 = (r2c1_b2 ^ r2d0_c1) <<< 12
    r2d0_a2 = r2d0_a1 +mod r2d0_b1; r2d0_d2 = (r2d0_d1 ^ r2d0_a2) <<< 8
    r2d0_c2 = r2d0_c1 +mod r2d0_d2; r2d0_b2 = (r2d0_b1 ^ r2d0_c2) <<< 7

    -- QR([1], [6], [11], [12])
    r2d1_a1 = r2c1_a2 +mod r2c2_b2; r2d1_d1 = (r2c0_d2 ^ r2d1_a1) <<< 16
    r2d1_c1 = r2c3_c2 +mod r2d1_d1; r2d1_b1 = (r2c2_b2 ^ r2d1_c1) <<< 12
    r2d1_a2 = r2d1_a1 +mod r2d1_b1; r2d1_d2 = (r2d1_d1 ^ r2d1_a2) <<< 8
    r2d1_c2 = r2d1_c1 +mod r2d1_d2; r2d1_b2 = (r2d1_b1 ^ r2d1_c2) <<< 7

    -- QR([2], [7], [8], [13])
    r2d2_a1 = r2c2_a2 +mod r2c3_b2; r2d2_d1 = (r2c1_d2 ^ r2d2_a1) <<< 16
    r2d2_c1 = r2c0_c2 +mod r2d2_d1; r2d2_b1 = (r2c3_b2 ^ r2d2_c1) <<< 12
    r2d2_a2 = r2d2_a1 +mod r2d2_b1; r2d2_d2 = (r2d2_d1 ^ r2d2_a2) <<< 8
    r2d2_c2 = r2d2_c1 +mod r2d2_d2; r2d2_b2 = (r2d2_b1 ^ r2d2_c2) <<< 7

    -- QR([3], [4], [9], [14])
    r2d3_a1 = r2c3_a2 +mod r2c0_b2; r2d3_d1 = (r2c2_d2 ^ r2d3_a1) <<< 16
    r2d3_c1 = r2c1_c2 +mod r2d3_d1; r2d3_b1 = (r2c0_b2 ^ r2d3_c1) <<< 12
    r2d3_a2 = r2d3_a1 +mod r2d3_b1; r2d3_d2 = (r2d3_d1 ^ r2d3_a2) <<< 8
    r2d3_c2 = r2d3_c1 +mod r2d3_d2; r2d3_b2 = (r2d3_b1 ^ r2d3_c2) <<< 7

    -- ==================================================================
    -- Double-rounds 3 through 10 follow the identical pattern.
    -- Each double-round reads from the previous round's diagonal outputs
    -- and applies 4 column QRs then 4 diagonal QRs.
    --
    -- For codegen compactness, rounds 3-10 use the loop form:
    -- ==================================================================

    -- ---- Double-round 3 ----
    loop r3 from r2 {
      column  { QR(0,4,8,12); QR(1,5,9,13); QR(2,6,10,14); QR(3,7,11,15) }
      diagonal { QR(0,5,10,15); QR(1,6,11,12); QR(2,7,8,13); QR(3,4,9,14) }
    }

    -- ---- Double-round 4 ----
    loop r4 from r3 {
      column  { QR(0,4,8,12); QR(1,5,9,13); QR(2,6,10,14); QR(3,7,11,15) }
      diagonal { QR(0,5,10,15); QR(1,6,11,12); QR(2,7,8,13); QR(3,4,9,14) }
    }

    -- ---- Double-round 5 ----
    loop r5 from r4 {
      column  { QR(0,4,8,12); QR(1,5,9,13); QR(2,6,10,14); QR(3,7,11,15) }
      diagonal { QR(0,5,10,15); QR(1,6,11,12); QR(2,7,8,13); QR(3,4,9,14) }
    }

    -- ---- Double-round 6 ----
    loop r6 from r5 {
      column  { QR(0,4,8,12); QR(1,5,9,13); QR(2,6,10,14); QR(3,7,11,15) }
      diagonal { QR(0,5,10,15); QR(1,6,11,12); QR(2,7,8,13); QR(3,4,9,14) }
    }

    -- ---- Double-round 7 ----
    loop r7 from r6 {
      column  { QR(0,4,8,12); QR(1,5,9,13); QR(2,6,10,14); QR(3,7,11,15) }
      diagonal { QR(0,5,10,15); QR(1,6,11,12); QR(2,7,8,13); QR(3,4,9,14) }
    }

    -- ---- Double-round 8 ----
    loop r8 from r7 {
      column  { QR(0,4,8,12); QR(1,5,9,13); QR(2,6,10,14); QR(3,7,11,15) }
      diagonal { QR(0,5,10,15); QR(1,6,11,12); QR(2,7,8,13); QR(3,4,9,14) }
    }

    -- ---- Double-round 9 ----
    loop r9 from r8 {
      column  { QR(0,4,8,12); QR(1,5,9,13); QR(2,6,10,14); QR(3,7,11,15) }
      diagonal { QR(0,5,10,15); QR(1,6,11,12); QR(2,7,8,13); QR(3,4,9,14) }
    }

    -- ---- Double-round 10 ----
    loop r10 from r9 {
      column  { QR(0,4,8,12); QR(1,5,9,13); QR(2,6,10,14); QR(3,7,11,15) }
      diagonal { QR(0,5,10,15); QR(1,6,11,12); QR(2,7,8,13); QR(3,4,9,14) }
    }

    -- ==================================================================
    -- RFC 8439, Section 2.3 — Final State Addition
    --
    -- Add the original state to the result of 20 rounds (mod 2^32).
    -- The final output is the 16-word state serialised as 64 bytes
    -- in little-endian order.
    --
    -- final[i] = round10[i] +mod s[i]   for i = 0..15
    -- ==================================================================

    -- r10 outputs map to state positions [0..15]:
    --   [0]=r10d0_a2  [1]=r10d1_a2  [2]=r10d2_a2  [3]=r10d3_a2
    --   [4]=r10d3_b2  [5]=r10d0_b2  [6]=r10d1_b2  [7]=r10d2_b2
    --   [8]=r10d2_c2  [9]=r10d3_c2  [10]=r10d0_c2 [11]=r10d1_c2
    --   [12]=r10d1_d2 [13]=r10d2_d2 [14]=r10d3_d2 [15]=r10d0_d2

    out_0  = r10[0]  +mod s_0
    out_1  = r10[1]  +mod s_1
    out_2  = r10[2]  +mod s_2
    out_3  = r10[3]  +mod s_3
    out_4  = r10[4]  +mod s_4
    out_5  = r10[5]  +mod s_5
    out_6  = r10[6]  +mod s_6
    out_7  = r10[7]  +mod s_7
    out_8  = r10[8]  +mod s_8
    out_9  = r10[9]  +mod s_9
    out_10 = r10[10] +mod s_10
    out_11 = r10[11] +mod s_11
    out_12 = r10[12] +mod s_12
    out_13 = r10[13] +mod s_13
    out_14 = r10[14] +mod s_14
    out_15 = r10[15] +mod s_15
  }
}
