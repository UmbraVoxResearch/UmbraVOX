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

    -- n^(-1) mod q = 128^(-1) mod 3329 = 3303
    -- Derivation: 3329 = 26*128 + 1, so 128*(-26) ≡ 1 (mod 3329), -26 mod 3329 = 3303.
    -- Used in inverse NTT final scaling step (multiply all coefficients by N_INV mod q).
    N_INV = 0x00000CEF

    -- Zeta powers in bit-reversed order for NTT (128 entries).
    -- zetaTable[i] = 17^(bitrev7(i)) mod 3329, for i in [0..127].
    -- bitrev7(i) reverses the 7-bit binary representation of i.
    -- Cross-verified against PQClean ml-kem-768/clean/ntt.c Montgomery table:
    --   PQClean[i] = (zetaTable[i] * MONT) mod q, centered to [-q/2, q/2]
    --   where MONT = 2^16 mod 3329 = 2285.
    zeta_0   = 0x00000001   -- 17^0   = 1
    zeta_1   = 0x000006C1   -- 17^64  = 1729
    zeta_2   = 0x00000A14   -- 17^32  = 2580
    zeta_3   = 0x00000CD9   -- 17^96  = 3289
    zeta_4   = 0x00000A52   -- 17^16  = 2642
    zeta_5   = 0x00000276   -- 17^80  = 630
    zeta_6   = 0x00000769   -- 17^48  = 1897
    zeta_7   = 0x00000350   -- 17^112 = 848
    zeta_8   = 0x00000426   -- 17^8   = 1062
    zeta_9   = 0x0000077F   -- 17^72  = 1919
    zeta_10  = 0x000000C1   -- 17^40  = 193
    zeta_11  = 0x0000031D   -- 17^104 = 797
    zeta_12  = 0x00000AE2   -- 17^24  = 2786
    zeta_13  = 0x00000CBC   -- 17^88  = 3260
    zeta_14  = 0x00000239   -- 17^56  = 569
    zeta_15  = 0x000006D2   -- 17^120 = 1746
    zeta_16  = 0x00000128   -- 17^4   = 296
    zeta_17  = 0x0000098F   -- 17^68  = 2447
    zeta_18  = 0x0000053B   -- 17^36  = 1339
    zeta_19  = 0x000005C4   -- 17^100 = 1476
    zeta_20  = 0x00000BE6   -- 17^20  = 3046
    zeta_21  = 0x00000038   -- 17^84  = 56
    zeta_22  = 0x000008C0   -- 17^52  = 2240
    zeta_23  = 0x00000535   -- 17^116 = 1333
    zeta_24  = 0x00000592   -- 17^12  = 1426
    zeta_25  = 0x0000082E   -- 17^76  = 2094
    zeta_26  = 0x00000217   -- 17^44  = 535
    zeta_27  = 0x00000B42   -- 17^108 = 2882
    zeta_28  = 0x00000959   -- 17^28  = 2393
    zeta_29  = 0x00000B3F   -- 17^92  = 2879
    zeta_30  = 0x000007B6   -- 17^60  = 1974
    zeta_31  = 0x00000335   -- 17^124 = 821
    zeta_32  = 0x00000121   -- 17^2   = 289
    zeta_33  = 0x0000014B   -- 17^66  = 331
    zeta_34  = 0x00000CB5   -- 17^34  = 3253
    zeta_35  = 0x000006DC   -- 17^98  = 1756
    zeta_36  = 0x000004AD   -- 17^18  = 1197
    zeta_37  = 0x00000900   -- 17^82  = 2304
    zeta_38  = 0x000008E5   -- 17^50  = 2277
    zeta_39  = 0x00000807   -- 17^114 = 2055
    zeta_40  = 0x0000028A   -- 17^10  = 650
    zeta_41  = 0x000007B9   -- 17^74  = 1977
    zeta_42  = 0x000009D1   -- 17^42  = 2513
    zeta_43  = 0x00000278   -- 17^106 = 632
    zeta_44  = 0x00000B31   -- 17^26  = 2865
    zeta_45  = 0x00000021   -- 17^90  = 33
    zeta_46  = 0x00000528   -- 17^58  = 1320
    zeta_47  = 0x0000077B   -- 17^122 = 1915
    zeta_48  = 0x0000090F   -- 17^6   = 2319
    zeta_49  = 0x0000059B   -- 17^70  = 1435
    zeta_50  = 0x00000327   -- 17^38  = 807
    zeta_51  = 0x000001C4   -- 17^102 = 452
    zeta_52  = 0x0000059E   -- 17^22  = 1438
    zeta_53  = 0x00000B34   -- 17^86  = 2868
    zeta_54  = 0x000005FE   -- 17^54  = 1534
    zeta_55  = 0x00000962   -- 17^118 = 2402
    zeta_56  = 0x00000A57   -- 17^14  = 2647
    zeta_57  = 0x00000A39   -- 17^78  = 2617
    zeta_58  = 0x000005C9   -- 17^46  = 1481
    zeta_59  = 0x00000288   -- 17^110 = 648
    zeta_60  = 0x000009AA   -- 17^30  = 2474
    zeta_61  = 0x00000C26   -- 17^94  = 3110
    zeta_62  = 0x000004CB   -- 17^62  = 1227
    zeta_63  = 0x0000038E   -- 17^126 = 910
    zeta_64  = 0x00000011   -- 17^1   = 17
    zeta_65  = 0x00000AC9   -- 17^65  = 2761
    zeta_66  = 0x00000247   -- 17^33  = 583
    zeta_67  = 0x00000A59   -- 17^97  = 2649
    zeta_68  = 0x00000665   -- 17^17  = 1637
    zeta_69  = 0x000002D3   -- 17^81  = 723
    zeta_70  = 0x000008F0   -- 17^49  = 2288
    zeta_71  = 0x0000044C   -- 17^113 = 1100
    zeta_72  = 0x00000581   -- 17^9   = 1409
    zeta_73  = 0x00000A66   -- 17^73  = 2662
    zeta_74  = 0x00000CD1   -- 17^41  = 3281
    zeta_75  = 0x000000E9   -- 17^105 = 233
    zeta_76  = 0x000002F4   -- 17^25  = 756
    zeta_77  = 0x0000086C   -- 17^89  = 2156
    zeta_78  = 0x00000BC7   -- 17^57  = 3015
    zeta_79  = 0x00000BEA   -- 17^121 = 3050
    zeta_80  = 0x000006A7   -- 17^5   = 1703
    zeta_81  = 0x00000673   -- 17^69  = 1651
    zeta_82  = 0x00000AE5   -- 17^37  = 2789
    zeta_83  = 0x000006FD   -- 17^101 = 1789
    zeta_84  = 0x00000737   -- 17^21  = 1847
    zeta_85  = 0x000003B8   -- 17^85  = 952
    zeta_86  = 0x000005B5   -- 17^53  = 1461
    zeta_87  = 0x00000A7F   -- 17^117 = 2687
    zeta_88  = 0x000003AB   -- 17^13  = 939
    zeta_89  = 0x00000904   -- 17^77  = 2308
    zeta_90  = 0x00000985   -- 17^45  = 2437
    zeta_91  = 0x00000954   -- 17^109 = 2388
    zeta_92  = 0x000002DD   -- 17^29  = 733
    zeta_93  = 0x00000921   -- 17^93  = 2337
    zeta_94  = 0x0000010C   -- 17^61  = 268
    zeta_95  = 0x00000281   -- 17^125 = 641
    zeta_96  = 0x00000630   -- 17^3   = 1584
    zeta_97  = 0x000008FA   -- 17^67  = 2298
    zeta_98  = 0x000007F5   -- 17^35  = 2037
    zeta_99  = 0x00000C94   -- 17^99  = 3220
    zeta_100 = 0x00000177   -- 17^19  = 375
    zeta_101 = 0x000009F5   -- 17^83  = 2549
    zeta_102 = 0x0000082A   -- 17^51  = 2090
    zeta_103 = 0x0000066D   -- 17^115 = 1645
    zeta_104 = 0x00000427   -- 17^11  = 1063
    zeta_105 = 0x0000013F   -- 17^75  = 319
    zeta_106 = 0x00000AD5   -- 17^43  = 2773
    zeta_107 = 0x000002F5   -- 17^107 = 757
    zeta_108 = 0x00000833   -- 17^27  = 2099
    zeta_109 = 0x00000231   -- 17^91  = 561
    zeta_110 = 0x000009A2   -- 17^59  = 2466
    zeta_111 = 0x00000A22   -- 17^123 = 2594
    zeta_112 = 0x00000AF4   -- 17^7   = 2804
    zeta_113 = 0x00000444   -- 17^71  = 1092
    zeta_114 = 0x00000193   -- 17^39  = 403
    zeta_115 = 0x00000402   -- 17^103 = 1026
    zeta_116 = 0x00000477   -- 17^23  = 1143
    zeta_117 = 0x00000866   -- 17^87  = 2150
    zeta_118 = 0x00000AD7   -- 17^55  = 2775
    zeta_119 = 0x00000376   -- 17^119 = 886
    zeta_120 = 0x000006BA   -- 17^15  = 1722
    zeta_121 = 0x000004BC   -- 17^79  = 1212
    zeta_122 = 0x00000752   -- 17^47  = 1874
    zeta_123 = 0x00000405   -- 17^111 = 1029
    zeta_124 = 0x0000083E   -- 17^31  = 2110
    zeta_125 = 0x00000B77   -- 17^95  = 2935
    zeta_126 = 0x00000375   -- 17^63  = 885
    zeta_127 = 0x0000086A   -- 17^127 = 2154
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
