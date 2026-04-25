-- X25519 Specification (RFC 7748)
--
-- This .spec file encodes the X25519 Diffie-Hellman function as defined
-- in IETF RFC 7748 (Elliptic Curves for Security).
--
-- Curve: Curve25519 (Montgomery form: y^2 = x^3 + 486662*x^2 + x)
-- Field prime: p = 2^255 - 19
-- Scalar size: 256 bits (32 bytes)
-- Point encoding: 256 bits (32 bytes, u-coordinate only, little-endian)
-- Ladder iterations: 255 (bit 254 down to bit 0)

algorithm X25519 {

  params {
    scalar       : Bytes(32)
    u_coordinate : Bytes(32)
  }

  constants {
    -- RFC 7748, Section 5 — Curve constant a24 = (A + 2) / 4
    -- where A = 486662 for Curve25519.
    a24 = 121666

    -- Field prime p = 2^255 - 19
    p = 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed
  }

  steps {
    -- ==================================================================
    -- Field arithmetic — all operations are mod p = 2^255 - 19
    -- ==================================================================

    -- fAdd(a, b) = (a + b) mod p
    -- fSub(a, b) = (a - b) mod p
    -- fMul(a, b) = (a * b) mod p
    -- fInv(a)    = a^(p-2) mod p    (Fermat's little theorem)

    -- ==================================================================
    -- RFC 7748, Section 5 — Scalar Clamping
    --
    -- Before use, the scalar is clamped:
    --   scalar[0]  &= 248       (clear three lowest bits)
    --   scalar[31] &= 127       (clear bit 255)
    --   scalar[31] |= 64        (set bit 254)
    -- ==================================================================
    clamped = clamp(scalar)
    k = decodeLE(clamped)

    -- ==================================================================
    -- RFC 7748, Section 5 — Decode u-coordinate
    --
    -- Decode the u-coordinate as a little-endian integer, masking
    -- bit 255 (i.e. u = decodeLE(u_coordinate) AND (2^255 - 1)).
    -- ==================================================================
    u = decodeLE(u_coordinate) & 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

    -- ==================================================================
    -- RFC 7748, Section 5 — Montgomery Ladder Initialisation
    --
    -- x_1 = u
    -- x_2 = 1
    -- z_2 = 0
    -- x_3 = u
    -- z_3 = 1
    -- swap = 0
    -- ==================================================================
    x_1 = u
    x_2_init = 1
    z_2_init = 0
    x_3_init = u
    z_3_init = 1
    swap_init = 0

    -- ==================================================================
    -- RFC 7748, Section 5 — Montgomery Ladder (254 iterations)
    --
    -- for t = 254 down to 0:
    --     k_t = (k >> t) & 1
    --     swap ^= k_t
    --     conditional_swap(swap, x_2, x_3)
    --     conditional_swap(swap, z_2, z_3)
    --     swap = k_t
    --
    --     A  = x_2 + z_2           (fAdd)
    --     AA = A * A               (fMul)
    --     B  = x_2 - z_2           (fSub)
    --     BB = B * B               (fMul)
    --     E  = AA - BB             (fSub)
    --     C  = x_3 + z_3           (fAdd)
    --     D  = x_3 - z_3           (fSub)
    --     DA = D * A               (fMul)
    --     CB = C * B               (fMul)
    --     x_3 = (DA + CB)^2        (fAdd, fMul)
    --     z_3 = x_1 * (DA - CB)^2  (fSub, fMul, fMul)
    --     x_2 = AA * BB            (fMul)
    --     z_2 = E * (BB + a24*E)   (fMul, fAdd, fMul)
    --
    -- Below: fully unrolled for the first two iterations, then
    -- loop form for iterations 252 down to 0.
    -- ==================================================================

    -- ---- Iteration t = 254 ----
    k_254 = bit(k, 254)
    swap_254 = swap_init ^ k_254
    -- conditional swap (swap_254, x_2_init, x_3_init, z_2_init, z_3_init)
    sx2_254 = cswap(swap_254, x_2_init, x_3_init)
    sx3_254 = cswap(swap_254, x_3_init, x_2_init)
    sz2_254 = cswap(swap_254, z_2_init, z_3_init)
    sz3_254 = cswap(swap_254, z_3_init, z_2_init)

    -- Ladder body
    A_254   = fAdd(sx2_254, sz2_254)
    AA_254  = fMul(A_254, A_254)
    B_254   = fSub(sx2_254, sz2_254)
    BB_254  = fMul(B_254, B_254)
    E_254   = fSub(AA_254, BB_254)
    C_254   = fAdd(sx3_254, sz3_254)
    D_254   = fSub(sx3_254, sz3_254)
    DA_254  = fMul(D_254, A_254)
    CB_254  = fMul(C_254, B_254)
    s_254   = fAdd(DA_254, CB_254)
    df_254  = fSub(DA_254, CB_254)
    nx2_254 = fMul(AA_254, BB_254)
    nz2_254 = fMul(E_254, fAdd(BB_254, fMul(a24, E_254)))
    nx3_254 = fMul(s_254, s_254)
    nz3_254 = fMul(u, fMul(df_254, df_254))
    -- swap state for next iteration: swap = k_t
    nswap_254 = k_254

    -- ---- Iteration t = 253 ----
    k_253 = bit(k, 253)
    swap_253 = nswap_254 ^ k_253
    sx2_253 = cswap(swap_253, nx2_254, nx3_254)
    sx3_253 = cswap(swap_253, nx3_254, nx2_254)
    sz2_253 = cswap(swap_253, nz2_254, nz3_254)
    sz3_253 = cswap(swap_253, nz3_254, nz2_254)

    A_253   = fAdd(sx2_253, sz2_253)
    AA_253  = fMul(A_253, A_253)
    B_253   = fSub(sx2_253, sz2_253)
    BB_253  = fMul(B_253, B_253)
    E_253   = fSub(AA_253, BB_253)
    C_253   = fAdd(sx3_253, sz3_253)
    D_253   = fSub(sx3_253, sz3_253)
    DA_253  = fMul(D_253, A_253)
    CB_253  = fMul(C_253, B_253)
    s_253   = fAdd(DA_253, CB_253)
    df_253  = fSub(DA_253, CB_253)
    nx2_253 = fMul(AA_253, BB_253)
    nz2_253 = fMul(E_253, fAdd(BB_253, fMul(a24, E_253)))
    nx3_253 = fMul(s_253, s_253)
    nz3_253 = fMul(u, fMul(df_253, df_253))
    nswap_253 = k_253

    -- ==================================================================
    -- Iterations t = 252 down to 0 follow the identical pattern.
    -- Each iteration reads (nx2, nx3, nz2, nz3, nswap) from the
    -- previous iteration and produces the next set.
    --
    -- For codegen compactness, use the loop form:
    -- ==================================================================

    loop t from 252 downto 0 using (nx2_prev, nx3_prev, nz2_prev, nz3_prev, nswap_prev) init (nx2_253, nx3_253, nz2_253, nz3_253, nswap_253) {
      k_t = bit(k, t)
      swap_t = nswap_prev ^ k_t
      sx2 = cswap(swap_t, nx2_prev, nx3_prev)
      sx3 = cswap(swap_t, nx3_prev, nx2_prev)
      sz2 = cswap(swap_t, nz2_prev, nz3_prev)
      sz3 = cswap(swap_t, nz3_prev, nz2_prev)

      A   = fAdd(sx2, sz2)
      AA  = fMul(A, A)
      B   = fSub(sx2, sz2)
      BB  = fMul(B, B)
      E   = fSub(AA, BB)
      C   = fAdd(sx3, sz3)
      D   = fSub(sx3, sz3)
      DA  = fMul(D, A)
      CB  = fMul(C, B)
      s   = fAdd(DA, CB)
      df  = fSub(DA, CB)
      nx2 = fMul(AA, BB)
      nz2 = fMul(E, fAdd(BB, fMul(a24, E)))
      nx3 = fMul(s, s)
      nz3 = fMul(u, fMul(df, df))
      nswap = k_t
    }
    -- Loop outputs: nx2_0, nx3_0, nz2_0, nz3_0, nswap_0

    -- ==================================================================
    -- RFC 7748, Section 5 — Final Conditional Swap and Inversion
    --
    -- After the loop, perform one final conditional swap based on
    -- the last swap bit, then compute:
    --   result = x_2 * (z_2 ^ (p - 2)) mod p
    --
    -- This is the u-coordinate of the resulting point, encoded as
    -- a 32-byte little-endian ByteString.
    -- ==================================================================

    -- Final conditional swap
    final_x2 = cswap(nswap_0, nx2_0, nx3_0)
    final_z2 = cswap(nswap_0, nz2_0, nz3_0)

    -- Modular inversion: z2^(p-2) via Fermat's little theorem
    z2_inv = fInv(final_z2)

    -- Final result: x2 * z2^(-1) mod p
    result = fMul(final_x2, z2_inv)

    -- Encode as 32-byte little-endian output
    output = encodeLE(result, 32)
  }
}
