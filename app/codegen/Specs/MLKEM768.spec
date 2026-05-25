-- ML-KEM-768 Key Encapsulation Mechanism (FIPS 203)
-- Reference: Federal Information Processing Standards Publication 203
-- Module-Lattice-Based Key-Encapsulation Mechanism Standard
--
-- ML-KEM-768: k=3, n=256, q=3329, eta1=eta2=2, du=10, dv=4
-- Hash functions: H=SHA3-256, G=SHA3-512, PRF=SHAKE-256, XOF=SHAKE-128

algorithm MLKEM768 {
  params {
    seed_d : Bytes
    seed_z : Bytes
    message : Bytes
  }

  constants {
    -- FIPS 203 Table 2 — ML-KEM-768 parameters
    Q = 0x00000d01
    N = 0x00000100
    K = 0x00000003
    ETA1 = 0x00000002
    ETA2 = 0x00000002
    DU = 0x0000000a
    DV = 0x00000004

    -- Primitive 256th root of unity in Z_q: zeta = 17
    -- 17^128 = -1 (mod 3329), 17^256 = 1 (mod 3329)
    ZETA = 0x00000011

    -- n^(-1) mod q = 128^(-1) mod 3329
    -- Used in inverse NTT final scaling
    N_INV = 0x00000f01

    -- Zeta powers in bit-reversed order for NTT (128 entries)
    -- zetaTable[i] = 17^(bitrev7(i)) mod 3329
    -- These are precomputed from the primitive root
    zeta_0 = 0x00000001
    zeta_1 = 0x000006e5
    zeta_2 = 0x00000331
    zeta_3 = 0x00000239
    -- (remaining 124 entries computed by code generator)
  }

  steps {
    -- ================================================================
    -- FIPS 203 Algorithm 9 — Number Theoretic Transform (NTT)
    -- ================================================================
    -- Input: polynomial f with 256 coefficients in [0, q-1]
    -- Output: NTT(f) with 256 coefficients in [0, q-1]
    --
    -- k = 1; len = 128
    -- while len >= 2:
    --   for start = 0 to 255 by 2*len:
    --     z = zetaTable[k]; k++
    --     for j = start to start+len-1:
    --       t = z * f[j+len] mod q
    --       f[j+len] = (f[j] - t + q) mod q
    --       f[j]     = (f[j] + t) mod q
    --   len = len / 2

    -- ================================================================
    -- FIPS 203 Algorithm 10 — Inverse NTT
    -- ================================================================
    -- k = 127; len = 2
    -- while len <= 128:
    --   for start = 0 to 255 by 2*len:
    --     z = zetaTable[k]; k--
    --     for j = start to start+len-1:
    --       t = f[j]
    --       f[j]     = (t + f[j+len]) mod q
    --       f[j+len] = z * (f[j+len] - t + q) mod q
    --   len = len * 2
    -- for i = 0 to 255: f[i] = f[i] * N_INV mod q

    -- ================================================================
    -- FIPS 203 Algorithm 11 — Polynomial base-case multiply
    -- ================================================================
    -- For pairs i = 0..63:
    --   gamma = zetaTable[64 + i]
    --   c[4i]   = (a[4i]*b[4i] + a[4i+1]*b[4i+1]*gamma) mod q
    --   c[4i+1] = (a[4i]*b[4i+1] + a[4i+1]*b[4i]) mod q
    --   gamma2 = (q - gamma) mod q
    --   c[4i+2] = (a[4i+2]*b[4i+2] + a[4i+3]*b[4i+3]*gamma2) mod q
    --   c[4i+3] = (a[4i+2]*b[4i+3] + a[4i+3]*b[4i+2]) mod q

    -- ================================================================
    -- FIPS 203 Algorithm 7 — Centered Binomial Distribution (CBD)
    -- ================================================================
    -- Input: eta, 64*eta random bytes
    -- Output: polynomial with coefficients in [-eta, eta]
    -- For each coefficient i (0..255):
    --   x = sum of bits[2*eta*i .. 2*eta*i+eta-1]
    --   y = sum of bits[2*eta*i+eta .. 2*eta*i+2*eta-1]
    --   coeff[i] = (x - y) mod q

    -- ================================================================
    -- FIPS 203 Algorithm 8 — SampleNTT (rejection sampling)
    -- ================================================================
    -- Input: XOF stream (SHAKE-128)
    -- Output: polynomial in NTT domain with coefficients in [0, q-1]
    -- Process 3 bytes at a time:
    --   d1 = b0 + 256*(b1 & 0x0F)
    --   d2 = (b1 >> 4) + 16*b2
    --   if d1 < q: accept d1
    --   if d2 < q: accept d2
    -- Continue until 256 coefficients accepted

    -- ================================================================
    -- FIPS 203 Algorithms 4-5 — Compress/Decompress
    -- ================================================================
    -- Compress_d(x) = round(2^d / q * x) mod 2^d
    -- Decompress_d(y) = round(q / 2^d * y)

    -- ================================================================
    -- FIPS 203 Algorithms 6-7 — ByteEncode/ByteDecode
    -- ================================================================
    -- Encode: d bits per coefficient, LSB first
    -- Decode: inverse, with mod q for d=12

    -- ================================================================
    -- FIPS 203 Algorithm 12 — K-PKE.KeyGen
    -- ================================================================
    -- (rho, sigma) = G(seed_d)           -- SHA3-512
    -- A[i][j] = SampleNTT(XOF(rho, i, j))  -- SHAKE-128, for i,j in [0,K)
    -- s[i] = NTT(CBD_eta1(PRF(sigma, i)))   -- SHAKE-256, for i in [0,K)
    -- e[i] = NTT(CBD_eta1(PRF(sigma, K+i))) -- SHAKE-256, for i in [0,K)
    -- t_hat = A * s + e                     -- polynomial matrix multiply in NTT domain
    -- ek = ByteEncode_12(t_hat) || rho      -- 1184 bytes
    -- dk = ByteEncode_12(s)                 -- 1152 bytes

    -- ================================================================
    -- FIPS 203 Algorithm 13 — K-PKE.Encrypt
    -- ================================================================
    -- (t_hat, rho) = decodeEK(ek)
    -- A[i][j] = SampleNTT(XOF(rho, i, j))
    -- r[i] = NTT(CBD_eta1(PRF(rand, i)))
    -- e1[i] = CBD_eta2(PRF(rand, K+i))
    -- e2 = CBD_eta2(PRF(rand, 2*K))
    -- u = InvNTT(A^T * r) + e1
    -- mu = Decompress_1(ByteDecode_1(m))
    -- v = InvNTT(t_hat^T * r) + e2 + mu
    -- c1 = ByteEncode_du(Compress_du(u))    -- K*N*du/8 bytes
    -- c2 = ByteEncode_dv(Compress_dv(v))    -- N*dv/8 bytes

    -- ================================================================
    -- FIPS 203 Algorithm 14 — K-PKE.Decrypt
    -- ================================================================
    -- s_hat = decodeDK(dk)
    -- u = Decompress_du(ByteDecode_du(c1))
    -- v = Decompress_dv(ByteDecode_dv(c2))
    -- w = v - InvNTT(s_hat^T * NTT(u))
    -- m = ByteEncode_1(Compress_1(w))

    -- ================================================================
    -- FIPS 203 Algorithm 15 — ML-KEM.KeyGen
    -- ================================================================
    -- (ek, dk_pke) = K-PKE.KeyGen(seed_d)
    -- dk = dk_pke || ek || H(ek) || seed_z   -- 2400 bytes

    -- ================================================================
    -- FIPS 203 Algorithm 16 — ML-KEM.Encaps
    -- ================================================================
    -- (K, r) = G(m || H(ek))                 -- SHA3-512
    -- ct = K-PKE.Encrypt(ek, m, r)
    -- return (ct, K)

    -- ================================================================
    -- FIPS 203 Algorithm 17 — ML-KEM.Decaps
    -- ================================================================
    -- m' = K-PKE.Decrypt(dk_pke, ct)
    -- (K', r') = G(m' || H(ek))
    -- ct' = K-PKE.Encrypt(ek, m', r')
    -- if ct' == ct:                          -- constant-time comparison!
    --   return K'
    -- else:
    --   return J(z || ct)                    -- SHAKE-256 implicit rejection
  }
}
