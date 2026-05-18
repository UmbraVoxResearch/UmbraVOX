(**
 * Spec.X25519 -- Pure functional specification of X25519 ECDH (RFC 7748)
 *
 * This module provides a complete specification of the X25519 Diffie-Hellman
 * function as defined in RFC 7748.  It mirrors the Haskell implementation in
 * src/UmbraVox/Crypto/Curve25519.hs and states correctness lemmas including
 * RFC 7748 Section 6.1 KAT vectors.
 *
 * Reference: RFC 7748 Sections 4.1, 5, 6.1
 *)
module Spec.X25519

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Field prime and constants                                             **)
(** -------------------------------------------------------------------- **)

(** The prime p = 2^255 - 19 defining the field GF(p) for Curve25519. *)
let prime : pos = normalize_term (pow2 255 - 19)

(** The Montgomery curve constant a24 = (A + 2) / 4 = (486662 + 2) / 4 = 121666.
    Used in the Montgomery ladder differential addition step. *)
let a24 : nat = 121666

(** The standard basepoint u = 9, per RFC 7748 Section 4.1. *)
let basepoint : nat = 9

(** A scalar is a 32-byte sequence. *)
type scalar = (s:seq UInt8.t{Seq.length s = 32})

(** A u-coordinate is a 32-byte sequence. *)
type coordinate = (s:seq UInt8.t{Seq.length s = 32})

(** -------------------------------------------------------------------- **)
(** Field element type                                                    **)
(** -------------------------------------------------------------------- **)

(** A field element is a natural number in [0, p). *)
type felem = x:nat{x < prime}

(** -------------------------------------------------------------------- **)
(** Field arithmetic mod p                                                **)
(** -------------------------------------------------------------------- **)

(** Field addition: (a + b) mod p *)
val fadd : felem -> felem -> felem
let fadd (a b : felem) : felem =
  (a + b) % prime

(** Field subtraction: (a - b) mod p.
    We add p before taking mod to ensure the result is non-negative,
    matching the Haskell (a - b) `mod` prime. *)
val fsub : felem -> felem -> felem
let fsub (a b : felem) : felem =
  (a - b + prime) % prime

(** Field multiplication: (a * b) mod p *)
val fmul : felem -> felem -> felem
let fmul (a b : felem) : felem =
  (a * b) % prime

(** Field squaring: a^2 mod p (convenience alias) *)
val fsqr : felem -> felem
let fsqr (a : felem) : felem =
  fmul a a

(** -------------------------------------------------------------------- **)
(** Modular exponentiation                                                **)
(** -------------------------------------------------------------------- **)

(** Modular exponentiation by repeated squaring: base^exp mod p.
    Direct translation of the Haskell powMod. *)
val pow_mod : base:felem -> exp:nat -> Tot felem (decreases exp)
let rec pow_mod (base : felem) (exp : nat) : Tot felem (decreases exp) =
  if exp = 0 then 1
  else if exp % 2 = 1 then
    fmul base (pow_mod (fsqr base) (exp / 2))
  else
    pow_mod (fsqr base) (exp / 2)

(** Field inversion via Fermat's little theorem: a^(p-2) mod p.
    For a != 0 in GF(p), a^(p-1) = 1, so a^(p-2) = a^(-1). *)
val finv : felem -> felem
let finv (a : felem) : felem =
  pow_mod a (prime - 2)

(** -------------------------------------------------------------------- **)
(** Field axioms (lemmas)                                                 **)
(** -------------------------------------------------------------------- **)

(** Closure: field operations produce valid field elements.
    This is enforced by the return type felem, but we state it
    explicitly for completeness. *)
val fadd_closure : a:felem -> b:felem
    -> Lemma (fadd a b < prime)
let fadd_closure a b = ()

val fsub_closure : a:felem -> b:felem
    -> Lemma (fsub a b < prime)
let fsub_closure a b = ()

val fmul_closure : a:felem -> b:felem
    -> Lemma (fmul a b < prime)
let fmul_closure a b = ()

(** Subtractive identity: a - 0 = a *)
val fsub_identity : a:felem
    -> Lemma (fsub a 0 == a)
let fsub_identity a =
  assert (fsub a 0 == (a - 0 + prime) % prime);
  assert (a - 0 + prime == a + prime);
  FStar.Math.Lemmas.lemma_mod_plus a 1 prime

(** Self-subtraction: a - a = 0 *)
val fsub_self : a:felem
    -> Lemma (fsub a a == 0)
let fsub_self a =
  assert (fsub a a == (a - a + prime) % prime);
  assert (a - a + prime == prime);
  assert (prime % prime == 0)

(** Additive identity: a + 0 = a *)
val fadd_identity : a:felem
    -> Lemma (fadd a 0 == a)
let fadd_identity a =
  assert (fadd a 0 == (a + 0) % prime);
  assert (a + 0 == a);
  assert (a % prime == a)

(** Multiplicative identity: a * 1 = a *)
val fmul_identity : a:felem
    -> Lemma (fmul a 1 == a)
let fmul_identity a =
  assert (fmul a 1 == (a * 1) % prime);
  assert (a * 1 == a);
  assert (a % prime == a)

(** Multiplicative absorbing element: a * 0 = 0 *)
val fmul_zero : a:felem
    -> Lemma (fmul a 0 == 0)
let fmul_zero a =
  assert (fmul a 0 == (a * 0) % prime);
  assert (a * 0 == 0)

(** Additive commutativity: a + b = b + a *)
val fadd_comm : a:felem -> b:felem
    -> Lemma (fadd a b == fadd b a)
let fadd_comm a b =
  assert ((a + b) % prime == (b + a) % prime)

(** Multiplicative commutativity: a * b = b * a *)
val fmul_comm : a:felem -> b:felem
    -> Lemma (fmul a b == fmul b a)
let fmul_comm a b =
  assert ((a * b) % prime == (b * a) % prime)

(** Additive associativity: (a + b) + c = a + (b + c) *)
val fadd_assoc : a:felem -> b:felem -> c:felem
    -> Lemma (fadd (fadd a b) c == fadd a (fadd b c))
let fadd_assoc a b c =
  FStar.Math.Lemmas.lemma_mod_plus_distr_l (a + b) c prime;
  FStar.Math.Lemmas.lemma_mod_plus_distr_r a (b + c) prime

(** Multiplicative associativity: (a * b) * c = a * (b * c) *)
val fmul_assoc : a:felem -> b:felem -> c:felem
    -> Lemma (fmul (fmul a b) c == fmul a (fmul b c))
let fmul_assoc a b c =
  FStar.Math.Lemmas.lemma_mod_mul_distr_l (a * b) c prime;
  FStar.Math.Lemmas.lemma_mod_mul_distr_l a (b * c) prime

(** Distributivity: a * (b + c) = a*b + a*c *)
val fmul_distrib : a:felem -> b:felem -> c:felem
    -> Lemma (fmul a (fadd b c) == fadd (fmul a b) (fmul a c))
let fmul_distrib a b c =
  FStar.Math.Lemmas.lemma_mod_mul_distr_r a (b + c) prime;
  FStar.Math.Lemmas.lemma_mod_mul_distr_r a b prime;
  FStar.Math.Lemmas.lemma_mod_mul_distr_r a c prime;
  FStar.Math.Lemmas.lemma_mod_plus_distr_l (a * b) (a * c) prime

(** Additive inverse: a + (p - a) = 0 *)
val fadd_inverse : a:felem
    -> Lemma (fadd a (fsub 0 a) == 0)
let fadd_inverse a =
  assert (fsub 0 a == (prime - a) % prime);
  assert ((prime - a) % prime == prime - a);
  assert (fadd a (fsub 0 a) == (a + (prime - a)) % prime);
  assert (a + (prime - a) == prime)

(** -------------------------------------------------------------------- **)
(** Primality assumption (same prime as Ed25519)                          **)
(** -------------------------------------------------------------------- **)

(** ASSUMED: 2^255 - 19 is prime.  Z3 cannot do trial division on a
    255-bit number.  Externally verified by Miller-Rabin / CAS.
    Same assumption as ED-001 in Spec.Ed25519. *)
assume val prime_is_prime : unit -> Lemma (FStar.Math.Euclid.is_prime prime)

(** -------------------------------------------------------------------- **)
(** Helper lemmas for fmul_inverse proof                                  **)
(** -------------------------------------------------------------------- **)

(** Congruence of pow under modular reduction of the base.
    pow (a % p) n % p == pow a n % p.
    This follows from (x % p) * y % p == x * y % p applied inductively. *)
val pow_mod_base : a:int -> n:nat
  -> Lemma (ensures FStar.Math.Fermat.pow (a % prime) n % prime
                 == FStar.Math.Fermat.pow a n % prime)
           (decreases n)
#push-options "--fuel 1 --ifuel 0 --z3rlimit 30"
let rec pow_mod_base a n =
  if n = 0 then ()
  else begin
    pow_mod_base a (n - 1);
    FStar.Math.Lemmas.lemma_mod_mul_distr_l a (FStar.Math.Fermat.pow (a % prime) (n - 1)) prime;
    FStar.Math.Lemmas.lemma_mod_mul_distr_r a (FStar.Math.Fermat.pow (a % prime) (n - 1)) prime;
    FStar.Math.Lemmas.lemma_mod_mul_distr_r a (FStar.Math.Fermat.pow a (n - 1)) prime
  end
#pop-options

(** pow distributes over squaring: pow (a*a) n == pow a (2*n).
    Proven by induction on n. *)
val pow_sqr : a:int -> n:nat
  -> Lemma (ensures FStar.Math.Fermat.pow (a * a) n == FStar.Math.Fermat.pow a (2 * n))
           (decreases n)
#push-options "--fuel 2 --ifuel 1"
let rec pow_sqr a n =
  if n = 0 then ()
  else begin
    pow_sqr a (n - 1);
    assert (2 * (n - 1) == 2 * n - 2)
  end
#pop-options

(** Equivalence of pow_mod (repeated-squaring) and FStar.Math.Fermat.pow
    (simple recursive exponentiation) modulo prime.

    pow_mod reduces modulo prime at each step for efficiency, but produces
    the same residue as unreduced exponentiation followed by a single mod. *)
val pow_mod_equiv : base:felem -> exp:nat
  -> Lemma (ensures pow_mod base exp == FStar.Math.Fermat.pow base exp % prime)
           (decreases exp)
#push-options "--fuel 2 --ifuel 1 --z3rlimit 50"
let rec pow_mod_equiv base exp =
  if exp = 0 then begin
    assert (pow_mod base 0 == 1);
    assert (FStar.Math.Fermat.pow base 0 == 1);
    assert (1 % prime == 1)
  end else if exp % 2 = 1 then begin
    let half = exp / 2 in
    pow_mod_equiv (fsqr base) half;
    pow_mod_base (base * base) half;
    pow_sqr base half;
    assert (2 * half == exp - 1);
    FStar.Math.Lemmas.lemma_mod_mul_distr_r base (FStar.Math.Fermat.pow base (exp - 1)) prime;
    assert (base * FStar.Math.Fermat.pow base (exp - 1) == FStar.Math.Fermat.pow base exp)
  end else begin
    let half = exp / 2 in
    pow_mod_equiv (fsqr base) half;
    pow_mod_base (base * base) half;
    pow_sqr base half;
    assert (2 * half == exp);
    ()
  end
#pop-options

(** -------------------------------------------------------------------- **)
(** Multiplicative inverse (PROVED)                                       **)
(** -------------------------------------------------------------------- **)

(** Multiplicative inverse: a * a^(-1) = 1 for a != 0.

    PROVED via Fermat's Little Theorem (FStar.Math.Fermat.fermat).

    The proof reduces to showing a^(p-1) mod p = 1 for prime p and
    0 < a < p, which is exactly Fermat's Little Theorem.  The only
    trusted assumption is prime_is_prime (primality of 2^255 - 19),
    which is externally verified by CAS.

    Proof chain:
    1. pow_mod_equiv: pow_mod a (p-2) == pow a (p-2) % p
    2. lemma_mod_mul_distr_r: (a * (pow a (p-2) % p)) % p == (a * pow a (p-2)) % p
    3. pow definition: a * pow a (p-2) == pow a (p-1)
    4. fermat (FLT): pow a (p-1) % p == 1 *)
val fmul_inverse : a:felem{a <> 0} -> Lemma (fmul a (finv a) == 1)
#push-options "--fuel 1 --ifuel 0 --z3rlimit 50"
let fmul_inverse a =
  pow_mod_equiv a (prime - 2);
  FStar.Math.Lemmas.lemma_mod_mul_distr_r a (FStar.Math.Fermat.pow a (prime - 2)) prime;
  assert (a * FStar.Math.Fermat.pow a (prime - 2) == FStar.Math.Fermat.pow a (prime - 1));
  prime_is_prime ();
  FStar.Math.Fermat.fermat prime a
#pop-options

(** -------------------------------------------------------------------- **)
(** Little-endian encoding / decoding                                     **)
(** -------------------------------------------------------------------- **)

(** Decode a single byte at position i into its integer contribution. *)
let byte_value (b : UInt8.t) (i : nat) : nat =
  UInt8.v b * pow2 (8 * i)

(** Decode a little-endian byte sequence to a natural number.
    Matches the Haskell decodeLE. *)
val decode_le : s:seq UInt8.t -> Tot nat (decreases (Seq.length s))
let rec decode_le (s : seq UInt8.t) : Tot nat (decreases (Seq.length s)) =
  if Seq.length s = 0 then 0
  else
    UInt8.v (Seq.index s 0) + 256 * decode_le (Seq.tail s)

(** Encode a natural number as a 32-byte little-endian sequence.
    Matches the Haskell encodeLE. *)
val encode_le : n:nat -> Tot (s:seq UInt8.t{Seq.length s = 32})
let encode_le (n : nat) : (s:seq UInt8.t{Seq.length s = 32}) =
  let byte_at (i : nat{i < 32}) : UInt8.t =
    FStar.UInt8.uint_to_t ((n / pow2 (8 * i)) % 256)
  in
  Seq.init 32 (fun i -> byte_at i)

(** Generalized encoding: encode a natural number as a k-byte
    little-endian sequence.  Used for the inductive round-trip proof. *)
val encode_le_n : k:nat -> v:nat -> Tot (s:seq UInt8.t{Seq.length s = k})
let encode_le_n (k : nat) (v : nat) : (s:seq UInt8.t{Seq.length s = k}) =
  let byte_at (i : nat{i < k}) : UInt8.t =
    FStar.UInt8.uint_to_t ((v / pow2 (8 * i)) % 256)
  in
  Seq.init k (fun i -> byte_at i)

(** encode_le is encode_le_n specialised to k=32. *)
val encode_le_is_encode_le_n : n:nat
    -> Lemma (encode_le n == encode_le_n 32 n)
let encode_le_is_encode_le_n n =
  assert (Seq.equal (encode_le n) (encode_le_n 32 n))

(** Helper: digit decomposition for mod.
    v % (256 * m) = (v % 256) + 256 * ((v / 256) % m) for m > 0. *)
val mod_digit_decomposition : v:nat -> m:pos
    -> Lemma (v % (256 * m) = (v % 256) + 256 * ((v / 256) % m))
#push-options "--z3rlimit 200"
let mod_digit_decomposition v m =
  FStar.Math.Lemmas.euclidean_division_definition v 256;
  FStar.Math.Lemmas.euclidean_division_definition (v / 256) m;
  let q = (v / 256) / m in
  let r_low = v % 256 in
  let r_mid = (v / 256) % m in
  let r = 256 * r_mid + r_low in
  assert (r_mid < m);
  assert (r_low < 256);
  assert (r < 256 * m);
  assert (v == (256 * m) * q + r);
  FStar.Math.Lemmas.lemma_div_mod v (256 * m);
  assert (v % (256 * m) < 256 * m);
  assert ((256 * m) * (v / (256 * m)) + v % (256 * m) == (256 * m) * q + r)
#pop-options

(** decode_le(encode_le_n(k, v)) = v mod 2^(8*k) for any k, v. *)
#push-options "--z3rlimit 100"
val decode_encode_le_aux : k:nat -> v:nat
    -> Lemma (ensures decode_le (encode_le_n k v) == v % pow2 (8 * k))
             (decreases k)
let rec decode_encode_le_aux k v =
  if k = 0 then begin
    assert (Seq.length (encode_le_n 0 v) = 0);
    assert (decode_le (encode_le_n 0 v) == 0);
    assert_norm (pow2 0 = 1);
    assert (v % pow2 (8 * 0) == v % 1);
    assert (v % 1 == 0)
  end else begin
    let s = encode_le_n k v in
    assert (Seq.length s = k);
    assert_norm (pow2 (8 * 0) = 1);
    assert_norm (pow2 8 = 256);
    assert (Seq.index s 0 == FStar.UInt8.uint_to_t ((v / pow2 (8 * 0)) % 256));
    assert (UInt8.v (Seq.index s 0) = v % 256);
    let tl = Seq.tail s in
    assert (Seq.length tl = k - 1);
    let shifted = encode_le_n (k - 1) (v / 256) in
    let aux (i:nat{i < k - 1}) : Lemma (Seq.index tl i == Seq.index shifted i) =
      assert (Seq.index tl i == Seq.index s (i + 1));
      assert (Seq.index s (i + 1) == FStar.UInt8.uint_to_t ((v / pow2 (8 * (i + 1))) % 256));
      assert (Seq.index shifted i == FStar.UInt8.uint_to_t (((v / 256) / pow2 (8 * i)) % 256));
      FStar.Math.Lemmas.pow2_plus 8 (8 * i);
      FStar.Math.Lemmas.division_multiplication_lemma v 256 (pow2 (8 * i));
      assert_norm (8 * (i + 1) = 8 + 8 * i);
      assert (pow2 (8 * (i + 1)) = 256 * pow2 (8 * i));
      assert (v / pow2 (8 * (i + 1)) = v / (256 * pow2 (8 * i)));
      FStar.Math.Lemmas.division_multiplication_lemma v 256 (pow2 (8 * i))
    in
    FStar.Classical.forall_intro aux;
    assert (Seq.equal tl shifted);
    decode_encode_le_aux (k - 1) (v / 256);
    assert (decode_le shifted == (v / 256) % pow2 (8 * (k - 1)));
    assert_norm (8 * k = 8 + 8 * (k - 1));
    FStar.Math.Lemmas.pow2_plus 8 (8 * (k - 1));
    assert (pow2 (8 * k) = 256 * pow2 (8 * (k - 1)));
    mod_digit_decomposition v (pow2 (8 * (k - 1)))
  end
#pop-options

(** Round-trip: decode(encode(n)) = n for n < 2^256.
    PROVED by induction via decode_encode_le_aux, specialised to k=32. *)
val decode_encode_round_trip : n:nat{n < pow2 256}
    -> Lemma (decode_le (encode_le n) == n)
let decode_encode_round_trip n =
  encode_le_is_encode_le_n n;
  decode_encode_le_aux 32 n;
  assert (decode_le (encode_le_n 32 n) == n % pow2 (8 * 32));
  assert_norm (8 * 32 = 256);
  assert (n % pow2 256 == n)

(** -------------------------------------------------------------------- **)
(** RFC 7748, Section 5 -- Scalar clamping                               **)
(** -------------------------------------------------------------------- **)

(** Clamp a 32-byte scalar per RFC 7748 Section 5.
    - Clear the three lowest bits of the first byte    (force multiple of 8)
    - Clear bit 255 of the last byte                   (clear top bit)
    - Set bit 254 of the last byte                     (set second-to-top bit)
    Matches the Haskell clampScalar exactly. *)
val clamp_scalar : scalar -> scalar
let clamp_scalar (s : scalar) : scalar =
  let b0  = Seq.index s 0 in
  let b31 = Seq.index s 31 in
  let b0'  = FStar.UInt8.logand b0 248uy in         (* clear three lowest bits *)
  let b31' = FStar.UInt8.logor
               (FStar.UInt8.logand b31 127uy) 64uy   (* clear bit 255, set bit 254 *)
  in
  Seq.append
    (Seq.create 1 b0')
    (Seq.append (Seq.slice s 1 31) (Seq.create 1 b31'))

(** The clamped scalar always has bit 254 set. *)
val clamp_bit254_set : s:scalar
    -> Lemma (let cs = clamp_scalar s in
              UInt8.v (Seq.index cs 31) >= 64)
let clamp_bit254_set s =
  let b31 = Seq.index s 31 in
  let x   = FStar.UInt8.logand b31 127uy in
  let b31' = FStar.UInt8.logor x 64uy in
  (* UInt8.logor with 64uy sets bit 6, so result >= 64 *)
  assert (UInt8.v b31' = FStar.UInt.logor (UInt8.v x) 64);
  let cs = clamp_scalar s in
  assert (Seq.index cs 31 == b31')

(** The clamped scalar is always a multiple of 8. *)
val clamp_multiple_of_8 : s:scalar
    -> Lemma (let cs = clamp_scalar s in
              UInt8.v (Seq.index cs 0) % 8 == 0)
let clamp_multiple_of_8 s =
  let b0  = Seq.index s 0 in
  let b0' = FStar.UInt8.logand b0 248uy in
  (* logand with 248 = 0b11111000 clears the low 3 bits *)
  assert (UInt8.v b0' = (UInt8.v b0 / 8) * 8);
  assert (UInt8.v b0' % 8 == 0);
  let cs = clamp_scalar s in
  assert (Seq.index cs 0 == b0')

(** -------------------------------------------------------------------- **)
(** Montgomery ladder specification                                       **)
(** -------------------------------------------------------------------- **)

(** State of the Montgomery ladder: (x_2, x_3, z_2, z_3) *)
type ladder_state = felem & felem & felem & felem

(** A single step of the Montgomery ladder (RFC 7748 Section 5 pseudocode).
    Given the u-coordinate of the base point, the current ladder state,
    and the current bit of the scalar, produce the next state.

    This is a direct translation of the inner loop of the Haskell scalarMult. *)
val ladder_step : u:felem -> st:ladder_state -> bit:nat{bit <= 1} -> ladder_state
let ladder_step (u : felem) (st : ladder_state) (bit : nat{bit <= 1}) : ladder_state =
  let (x_2, x_3, z_2, z_3) = st in
  (* Conditional swap based on bit *)
  let (sx2, sx3, sz2, sz3) =
    if bit = 1 then (x_3, x_2, z_3, z_2)
    else (x_2, x_3, z_2, z_3)
  in
  (* Ladder body per RFC 7748 Section 5 *)
  let a  = fadd sx2 sz2 in
  let aa = fsqr a in
  let b  = fsub sx2 sz2 in
  let bb = fsqr b in
  let e  = fsub aa bb in
  let c  = fadd sx3 sz3 in
  let d  = fsub sx3 sz3 in
  let da = fmul d a in
  let cb = fmul c b in
  let s  = fadd da cb in
  let df = fsub da cb in
  let nx2 = fmul aa bb in
  let nz2 = fmul e (fadd bb (fmul a24 e)) in
  let nx3 = fsqr s in
  let nz3 = fmul u (fsqr df) in
  (nx2, nx3, nz2, nz3)

(** Extract bit t from a natural number k. *)
let get_bit (k : nat) (t : nat) : nat =
  (k / pow2 t) % 2

(** Run the Montgomery ladder loop from bit position t down to 0.
    Tracks the swap state as in the Haskell implementation.
    Returns (x_2, x_3, z_2, z_3, swap_final). *)
val ladder_loop : u:felem -> k:nat -> t:int -> x_2:felem -> x_3:felem
    -> z_2:felem -> z_3:felem -> swap:nat{swap <= 1}
    -> Tot (felem & felem & felem & felem & nat) (decreases (if t < 0 then 0 else t + 1))
let rec ladder_loop (u : felem) (k : nat) (t : int)
    (x_2 x_3 : felem) (z_2 z_3 : felem) (swap : nat{swap <= 1})
    : Tot (felem & felem & felem & felem & nat) (decreases (if t < 0 then 0 else t + 1)) =
  if t < 0 then (x_2, x_3, z_2, z_3, swap)
  else
    let k_t = get_bit k t in
    let swap' = (if swap = k_t then 0 else 1) in
    (* XOR of swap and k_t: conditional swap *)
    let (sx2, sx3, sz2, sz3) =
      if swap' = 1 then (x_3, x_2, z_3, z_2)
      else (x_2, x_3, z_2, z_3)
    in
    let (nx2, nx3, nz2, nz3) = ladder_step u (sx2, sx3, sz2, sz3) 0 in
    ladder_loop u k (t - 1) nx2 nx3 nz2 nz3 k_t

(** Scalar multiplication on Curve25519 via the Montgomery ladder.
    Direct translation of the Haskell scalarMult.
    Given clamped scalar k and masked u-coordinate, returns the
    x-coordinate of [k]P as a field element. *)
val scalar_mult : k:nat -> u:felem -> felem
let scalar_mult (k : nat) (u : felem) : felem =
  let u_fe : felem = u % prime in
  let (x_2, x_3, z_2, z_3, swap_final) =
    ladder_loop u_fe k 254 1 u_fe 0 1 0
  in
  (* Final conditional swap *)
  let (xr, zr) =
    if swap_final = 1 then (x_3, z_3) else (x_2, z_2)
  in
  fmul xr (finv zr)

(** -------------------------------------------------------------------- **)
(** Top-level X25519 function                                             **)
(** -------------------------------------------------------------------- **)

(** The X25519 Diffie-Hellman function.
    x25519(k, u) computes scalar multiplication of u-coordinate by
    scalar k on Curve25519.

    Both k and u are 32-byte little-endian sequences (ByteStrings).
    Matches the Haskell x25519 function exactly:
      1. Clamp the scalar
      2. Decode the clamped scalar
      3. Decode the u-coordinate and mask bit 255
      4. Perform scalar multiplication
      5. Encode the result as 32 bytes, little-endian *)
val x25519 : scalar -> coordinate -> coordinate
let x25519 (sk : scalar) (uc : coordinate) : coordinate =
  let clamped = clamp_scalar sk in
  let k = decode_le clamped in
  let u_raw = decode_le uc in
  let u = u_raw % pow2 255 in       (* mask bit 255, per RFC 7748 *)
  (* u < 2^255.  We reduce mod p to get a felem; this is correct
     because scalar_mult only depends on u mod p.  The reduction
     is always well-typed: u % prime < prime by definition of %. *)
  let u_fe : felem = u % prime in
  encode_le (scalar_mult k u_fe)

(** Convenience: X25519 with the standard basepoint u = 9.
    public_key = x25519_base(private_key)
    Matches the Haskell: x25519 privateKey x25519Basepoint *)
val x25519_base : scalar -> coordinate
let x25519_base (sk : scalar) : coordinate =
  let bp : coordinate =
    Seq.append (Seq.create 1 9uy) (Seq.create 31 0uy)
  in
  assert (Seq.length (Seq.create 1 9uy) = 1);
  assert (Seq.length (Seq.create 31 0uy) = 31);
  x25519 sk bp

(** -------------------------------------------------------------------- **)
(** Structural properties                                                 **)
(** -------------------------------------------------------------------- **)

(** The output of x25519 is always exactly 32 bytes *)
val x25519_output_length : sk:scalar -> uc:coordinate
    -> Lemma (Seq.length (x25519 sk uc) = 32)
let x25519_output_length sk uc = ()

(** Scalar multiplication result is a valid field element *)
val scalar_mult_in_field : k:nat -> u:felem
    -> Lemma (scalar_mult k u < prime)
let scalar_mult_in_field k u = ()

(** The basepoint encoding decodes to 9 *)
val basepoint_decode_lemma : unit
    -> Lemma (decode_le (Seq.append (Seq.create 1 9uy) (Seq.create 31 0uy)) == 9)
let basepoint_decode_lemma () =
  let s = Seq.append (Seq.create 1 9uy) (Seq.create 31 0uy) in
  assert (UInt8.v (Seq.index s 0) = 9);
  assert (Seq.tail s == Seq.create 31 0uy);
  assert (decode_le (Seq.create 31 0uy) = 0)

(** -------------------------------------------------------------------- **)
(** Clamping properties                                                   **)
(** -------------------------------------------------------------------- **)

(** Clamping produces a 32-byte output *)
val clamp_scalar_length : s:scalar
    -> Lemma (Seq.length (clamp_scalar s) = 32)
let clamp_scalar_length s =
  let b0  = Seq.index s 0 in
  let b31 = Seq.index s 31 in
  let b0'  = FStar.UInt8.logand b0 248uy in
  let b31' = FStar.UInt8.logor (FStar.UInt8.logand b31 127uy) 64uy in
  let mid = Seq.slice s 1 31 in
  assert (Seq.length mid = 30);
  assert (Seq.length (Seq.create 1 b0') = 1);
  assert (Seq.length (Seq.create 1 b31') = 1)

(** Clamping is idempotent: clamp(clamp(s)) = clamp(s) *)
val clamp_idempotent : s:scalar
    -> Lemma (clamp_scalar (clamp_scalar s) == clamp_scalar s)
#push-options "--z3rlimit 20000 --fuel 0 --ifuel 0"
let clamp_idempotent s =
  let b0  = Seq.index s 0 in
  let b31 = Seq.index s 31 in
  let mid = Seq.slice s 1 31 in
  (* First clamp *)
  let b0'  = FStar.UInt8.logand b0 248uy in
  let b31' = FStar.UInt8.logor (FStar.UInt8.logand b31 127uy) 64uy in
  let cs   = clamp_scalar s in
  (* Second clamp reads from cs *)
  let b0''  = FStar.UInt8.logand (Seq.index cs 0) 248uy in
  let b31'' = FStar.UInt8.logor (FStar.UInt8.logand (Seq.index cs 31) 127uy) 64uy in
  (* Key facts: Seq.index cs 0 = b0' and Seq.index cs 31 = b31' *)
  assert (Seq.index cs 0 == b0');
  assert (Seq.index cs 31 == b31');
  (* Byte 0 idempotency: (x & 248) & 248 = x & 248.
     UInt8.v b0' = UInt.logand (UInt8.v b0) 248.
     UInt8.v b0'' = UInt.logand (UInt8.v b0') 248
                  = UInt.logand (UInt.logand (UInt8.v b0) 248) 248.
     Z3 bitvector theory: (a & m) & m = a & m. *)
  assert (b0'' == b0');
  (* Byte 31 idempotency:
     b31' = (b31 & 127) | 64, so UInt8.v b31' < 128 and >= 64.
     b31'' = ((b31' & 127) | 64).
     Since UInt8.v b31' < 128: b31' & 127 = b31'.
     Since UInt8.v b31' >= 64: b31' | 64 = b31'.
     So b31'' = b31'. *)
  assert (b31'' == b31');
  (* Middle bytes unchanged: slice cs 1 31 = slice s 1 31 = mid *)
  assert (Seq.slice cs 1 31 == mid);
  (* The second clamp builds the same sequence *)
  let cs2 = clamp_scalar cs in
  assert (Seq.equal cs2 cs)
#pop-options

(** -------------------------------------------------------------------- **)
(** Montgomery ladder properties                                         **)
(** -------------------------------------------------------------------- **)

(** The ladder step preserves field membership (follows from felem types) *)
val ladder_step_in_field : u:felem -> st:ladder_state -> bit:nat{bit <= 1}
    -> Lemma (let (x2, x3, z2, z3) = ladder_step u st bit in
              x2 < prime /\ x3 < prime /\ z2 < prime /\ z3 < prime)
let ladder_step_in_field u st bit = ()

(** -------------------------------------------------------------------- **)
(** Helper lemmas for scalar_mult_zero proof                             **)
(** -------------------------------------------------------------------- **)

(** fmul with 0 on the left: 0 * a = 0 *)
val fmul_zero_left : a:felem -> Lemma (fmul 0 a == 0)
let fmul_zero_left a = assert (fmul 0 a == (0 * a) % prime)

(** finv 0 = 0.  pow_mod 0 n = 0 for all n > 0.
    When n is odd: fmul 0 (pow_mod (fsqr 0) (n/2)) = 0.
    When n is even and > 0: pow_mod (fsqr 0) (n/2) = pow_mod 0 (n/2).
    Either way the result is 0 by induction. *)
val pow_mod_zero : n:nat{n > 0}
    -> Lemma (ensures pow_mod 0 n == 0)
             (decreases n)
#push-options "--fuel 2 --ifuel 0 --z3rlimit 20"
let rec pow_mod_zero n =
  if n = 1 then begin
    assert (pow_mod 0 1 == fmul 0 (pow_mod (fsqr 0) 0));
    assert (pow_mod (fsqr 0) 0 == 1);
    fmul_zero_left 1
  end else if n % 2 = 1 then begin
    assert (pow_mod 0 n == fmul 0 (pow_mod (fsqr 0) (n / 2)));
    fmul_zero_left (pow_mod (fsqr 0) (n / 2))
  end else begin
    assert (fsqr 0 == 0);
    assert (pow_mod 0 n == pow_mod (fsqr 0) (n / 2));
    assert (pow_mod (fsqr 0) (n / 2) == pow_mod 0 (n / 2));
    pow_mod_zero (n / 2)
  end
#pop-options

val finv_zero : unit -> Lemma (finv 0 == 0)
let finv_zero () = pow_mod_zero (prime - 2)

(** get_bit 0 t = 0 for all t *)
val get_bit_zero : t:nat -> Lemma (get_bit 0 t == 0)
let get_bit_zero t = assert (0 / pow2 t == 0)

(** When z_2 = 0 and bit = 0, ladder_step preserves z_2 = 0.
    Proof: with bit=0, sx2=x_2, sz2=z_2=0, so a = fadd x_2 0 = x_2,
    b = fsub x_2 0 = x_2, aa = fsqr x_2 = bb, e = fsub aa bb = 0,
    and nz2 = fmul 0 (...) = 0. *)
val ladder_step_preserves_z2_zero : u:felem -> x_2:felem -> x_3:felem -> z_3:felem
    -> Lemma (let (_, _, nz2, _) = ladder_step u (x_2, x_3, 0, z_3) 0 in nz2 == 0)
#push-options "--fuel 1 --ifuel 0 --z3rlimit 40"
let ladder_step_preserves_z2_zero u x_2 x_3 z_3 =
  let a  = fadd x_2 0 in
  let aa = fsqr a in
  let b  = fsub x_2 0 in
  let bb = fsqr b in
  let e  = fsub aa bb in
  fsub_identity x_2;
  fadd_identity x_2;
  assert (a == x_2);
  assert (b == x_2);
  assert (aa == bb);
  fsub_self aa;
  assert (e == 0);
  fmul_zero_left (fadd bb (fmul a24 e))
#pop-options

(** The Montgomery ladder loop with k=0, z_2=0, swap=0 maintains z_2=0
    and swap=0 throughout.  By induction on t: get_bit 0 t = 0, so
    swap'=0 (no swap), the step sees z_2=0 and preserves it. *)
val ladder_loop_zero_k_z2 : u:felem -> t:int -> x_2:felem -> x_3:felem -> z_3:felem
    -> Lemma (ensures (let (_, _, z2_f, _, sw_f) = ladder_loop u 0 t x_2 x_3 0 z_3 0 in
                        z2_f == 0 /\ sw_f == 0))
             (decreases (if t < 0 then 0 else t + 1))
#push-options "--fuel 1 --ifuel 0 --z3rlimit 40"
let rec ladder_loop_zero_k_z2 u t x_2 x_3 z_3 =
  if t < 0 then ()
  else begin
    get_bit_zero t;
    (* swap=0, k_t=0, so swap'=0, no swap *)
    (* ladder_step called with (x_2, x_3, 0, z_3) 0 *)
    ladder_step_preserves_z2_zero u x_2 x_3 z_3;
    let (nx2, nx3, nz2, nz3) = ladder_step u (x_2, x_3, 0, z_3) 0 in
    assert (nz2 == 0);
    ladder_loop_zero_k_z2 u (t - 1) nx2 nx3 nz3
  end
#pop-options

(** -------------------------------------------------------------------- **)
(** scalar_mult_zero (PROVED)                                            **)
(** -------------------------------------------------------------------- **)

(** Multiplying by scalar 0 yields the point at infinity (u-coordinate 0).

    PROVED by showing the Montgomery ladder with k=0 maintains z_2=0
    as an invariant (all bits of k are zero, so no swap ever occurs
    and z_2=0 propagates through ladder_step).  The final result is
    fmul x_2 (finv 0) = fmul x_2 0 = 0.

    Proof chain:
    1. get_bit_zero: all bits of 0 are 0
    2. ladder_step_preserves_z2_zero: z_2=0 is invariant when bit=0
    3. ladder_loop_zero_k_z2: induction over the loop
    4. finv_zero: finv 0 = pow_mod 0 (p-2) = 0
    5. fmul_zero: x * 0 = 0 *)
val scalar_mult_zero : u:felem
    -> Lemma (scalar_mult 0 u == 0)
#push-options "--fuel 1 --ifuel 0 --z3rlimit 40"
let scalar_mult_zero u =
  let u_fe : felem = u % prime in
  assert (u_fe == u);
  ladder_loop_zero_k_z2 u_fe 254 1 u_fe 1;
  let (x_2, x_3, z_2, z_3, swap_final) = ladder_loop u_fe 0 254 1 u_fe 0 1 0 in
  assert (z_2 == 0);
  assert (swap_final == 0);
  finv_zero ();
  fmul_zero x_2
#pop-options

(** Multiplying by scalar 1 yields the original point.
    ASSUMED: Requires showing the Montgomery ladder with k=1 performs one
    differential addition step that preserves the input u-coordinate.
    Visible to auditing as an assume val. *)
assume val scalar_mult_one : u:felem{u > 0}
    -> Lemma (scalar_mult 1 u == u)

(** -------------------------------------------------------------------- **)
(** KAT Test Vectors (RFC 7748 Section 6.1)                              **)
(** -------------------------------------------------------------------- **)

(** Helper: create a byte sequence from a list of byte values *)
let of_byte_list (l : list UInt8.t) : seq UInt8.t = Seq.seq_of_list l

(** RFC 7748 Section 6.1 -- Test Vector 1

    Alice's private key (scalar):
      a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4

    Alice's public key (u-coordinate = basepoint 9):
      e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c

    We verify: x25519(alice_sk, basepoint_9) == alice_pk *)
let kat1_scalar : scalar =
  let l = [
    0xa5uy; 0x46uy; 0xe3uy; 0x6buy; 0xf0uy; 0x52uy; 0x7cuy; 0x9duy;
    0x3buy; 0x16uy; 0x15uy; 0x4buy; 0x82uy; 0x46uy; 0x5euy; 0xdduy;
    0x62uy; 0x14uy; 0x4cuy; 0x0auy; 0xc1uy; 0xfcuy; 0x5auy; 0x18uy;
    0x50uy; 0x6auy; 0x22uy; 0x44uy; 0xbauy; 0x44uy; 0x9auy; 0xc4uy
  ] in
  assert_norm (List.Tot.length l = 32);
  of_byte_list l

let kat1_u_coordinate : coordinate =
  let l = [
    0x09uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
    0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
    0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
    0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
  ] in
  assert_norm (List.Tot.length l = 32);
  of_byte_list l

let kat1_expected : coordinate =
  let l = [
    0xe6uy; 0xdbuy; 0x68uy; 0x67uy; 0x58uy; 0x30uy; 0x30uy; 0xdbuy;
    0x35uy; 0x94uy; 0xc1uy; 0xa4uy; 0x24uy; 0xb1uy; 0x5fuy; 0x7cuy;
    0x72uy; 0x66uy; 0x24uy; 0xecuy; 0x26uy; 0xb3uy; 0x35uy; 0x3buy;
    0x10uy; 0xa9uy; 0x03uy; 0xa6uy; 0xd0uy; 0xabuy; 0x1cuy; 0x4cuy
  ] in
  assert_norm (List.Tot.length l = 32);
  of_byte_list l

(** KAT 1: x25519(alice_scalar, basepoint_9) = alice_public_key
    RFC 7748 Section 6.1, first test vector *)
val x25519_kat1 : unit
    -> Lemma (x25519 kat1_scalar kat1_u_coordinate == kat1_expected)
#push-options "--z3rlimit 80000 --fuel 0 --ifuel 0"
let x25519_kat1 () =
  assert_norm (x25519 kat1_scalar kat1_u_coordinate == kat1_expected)
#pop-options

(** RFC 7748 Section 6.1 -- Test Vector 2

    Bob's private key (scalar):
      4b66e9d4d1b4673c5ad22691957d6af5c11b6421e0ea01d42ca4169e7918ba0d

    Bob's public key (u-coordinate input):
      e5210f12786811d3f4b7959d0538ae2c31dbe7106fc03c3efc4cd549c715a413

    Output:
      95cbde9476e8907d7aade45cb4b873f88b595a68799fa152e6f8f7647aac7957 *)
let kat2_scalar : scalar =
  let l = [
    0x4buy; 0x66uy; 0xe9uy; 0xd4uy; 0xd1uy; 0xb4uy; 0x67uy; 0x3cuy;
    0x5auy; 0xd2uy; 0x26uy; 0x91uy; 0x95uy; 0x7duy; 0x6auy; 0xf5uy;
    0xc1uy; 0x1buy; 0x64uy; 0x21uy; 0xe0uy; 0xeauy; 0x01uy; 0xd4uy;
    0x2cuy; 0xa4uy; 0x16uy; 0x9euy; 0x79uy; 0x18uy; 0xbauy; 0x0duy
  ] in
  assert_norm (List.Tot.length l = 32);
  of_byte_list l

let kat2_u_coordinate : coordinate =
  let l = [
    0xe5uy; 0x21uy; 0x0fuy; 0x12uy; 0x78uy; 0x68uy; 0x11uy; 0xd3uy;
    0xf4uy; 0xb7uy; 0x95uy; 0x9duy; 0x05uy; 0x38uy; 0xaeuy; 0x2cuy;
    0x31uy; 0xdbuy; 0xe7uy; 0x10uy; 0x6fuy; 0xc0uy; 0x3cuy; 0x3euy;
    0xfcuy; 0x4cuy; 0xd5uy; 0x49uy; 0xc7uy; 0x15uy; 0xa4uy; 0x13uy
  ] in
  assert_norm (List.Tot.length l = 32);
  of_byte_list l

let kat2_expected : coordinate =
  let l = [
    0x95uy; 0xcbuy; 0xdeuy; 0x94uy; 0x76uy; 0xe8uy; 0x90uy; 0x7duy;
    0x7auy; 0xaduy; 0xe4uy; 0x5cuy; 0xb4uy; 0xb8uy; 0x73uy; 0xf8uy;
    0x8buy; 0x59uy; 0x5auy; 0x68uy; 0x79uy; 0x9fuy; 0xa1uy; 0x52uy;
    0xe6uy; 0xf8uy; 0xf7uy; 0x64uy; 0x7auy; 0xacuy; 0x79uy; 0x57uy
  ] in
  assert_norm (List.Tot.length l = 32);
  of_byte_list l

(** KAT 2: x25519(bob_scalar, bob_u_coord) = expected_output
    RFC 7748 Section 6.1, second test vector *)
val x25519_kat2 : unit
    -> Lemma (x25519 kat2_scalar kat2_u_coordinate == kat2_expected)
#push-options "--z3rlimit 80000 --fuel 0 --ifuel 0"
let x25519_kat2 () =
  assert_norm (x25519 kat2_scalar kat2_u_coordinate == kat2_expected)
#pop-options

(** -------------------------------------------------------------------- **)
(** RFC 7748 Section 6.1 -- ECDH agreement test                          **)
(** -------------------------------------------------------------------- **)

(** The shared secret from Alice's perspective:
    x25519(alice_sk, bob_pk) where bob_pk = x25519(bob_sk, basepoint) *)
let kat_shared_secret : coordinate =
  let l = [
    0x4auy; 0x5duy; 0x9duy; 0x5buy; 0xa4uy; 0xceuy; 0x2duy; 0xe1uy;
    0x72uy; 0x8euy; 0x3buy; 0xf4uy; 0x80uy; 0x35uy; 0x0fuy; 0x25uy;
    0xe0uy; 0x7euy; 0x21uy; 0xc9uy; 0x47uy; 0xd1uy; 0x9euy; 0x33uy;
    0x76uy; 0xf0uy; 0x9buy; 0x3cuy; 0x1euy; 0x16uy; 0x17uy; 0x42uy
  ] in
  assert_norm (List.Tot.length l = 32);
  of_byte_list l

(** KAT: Alice and Bob derive the same shared secret.
    alice_shared = x25519(alice_sk, bob_pk)
    bob_shared   = x25519(bob_sk, alice_pk)
    alice_shared == bob_shared == kat_shared_secret *)
val x25519_kat_shared_secret_alice : unit
    -> Lemma (x25519 kat1_scalar kat2_expected == kat_shared_secret)
#push-options "--z3rlimit 80000 --fuel 0 --ifuel 0"
let x25519_kat_shared_secret_alice () =
  assert_norm (x25519 kat1_scalar kat2_expected == kat_shared_secret)
#pop-options

val x25519_kat_shared_secret_bob : unit
    -> Lemma (x25519 kat2_scalar kat1_expected == kat_shared_secret)
#push-options "--z3rlimit 80000 --fuel 0 --ifuel 0"
let x25519_kat_shared_secret_bob () =
  assert_norm (x25519 kat2_scalar kat1_expected == kat_shared_secret)
#pop-options

(** -------------------------------------------------------------------- **)
(** Diffie-Hellman commutativity                                         **)
(** -------------------------------------------------------------------- **)

(** DH commutativity: x25519(a, x25519(b, G)) = x25519(b, x25519(a, G))
    This is the fundamental property that makes ECDH work.
    Both parties derive the same shared secret regardless of order.

    Proof sketch: On Curve25519, scalar multiplication is a group
    homomorphism.  For scalars a, b and generator G:
      [a]([b]G) = [a*b]G = [b*a]G = [b]([a]G)
    The commutativity of integer multiplication over the scalar field
    (integers mod the group order l) yields the result.

    This is the most critical security property of the ECDH protocol. *)
(** ASSUMED: DH commutativity follows from scalar multiplication being a
    group homomorphism: [a]([b]G) = [ab]G = [ba]G = [b]([a]G).  The full
    algebraic proof requires reasoning about the Montgomery curve group law
    which is not discharged by Z3 without an elliptic-curve library.
    Visible to auditing as an assume val. *)
assume val dh_commutativity : a:scalar -> b:scalar
    -> Lemma (
      let g : coordinate = Seq.append (Seq.create 1 9uy) (Seq.create 31 0uy) in
      x25519 a (x25519 b g) == x25519 b (x25519 a g))

(** Generalized DH commutativity for any base point, not just G=9.
    For any point P on the curve:
      x25519(a, x25519(b, P)) = x25519(b, x25519(a, P))

    Note: This holds when P is a point on Curve25519 (not a point of
    small order that would be annihilated by clamping). *)
(** ASSUMED: Generalized DH commutativity for arbitrary base points.
    Same algebraic argument as dh_commutativity but for any point P on
    Curve25519 (excluding small-order points annihilated by clamping).
    Visible to auditing as an assume val. *)
assume val dh_commutativity_general : a:scalar -> b:scalar -> p:coordinate
    -> Lemma (x25519 a (x25519 b p) == x25519 b (x25519 a p))

(** -------------------------------------------------------------------- **)
(** Low-order point rejection                                             **)
(** -------------------------------------------------------------------- **)

(** The all-zero u-coordinate represents the point at infinity.
    x25519 with the all-zero u-coordinate should produce all zeros.
    This is important for implementations to check to avoid
    small-subgroup attacks. *)
let zero_coordinate : coordinate =
  Seq.create 32 0uy

(** ASSUMED: The all-zero u-coordinate maps to all-zero output because
    scalar multiplication of the point at infinity (u=0) stays at infinity.
    Requires showing scalar_mult k 0 == 0 for all k, which needs induction
    over the Montgomery ladder.  Visible to auditing as an assume val. *)
assume val x25519_zero_u : sk:scalar
    -> Lemma (x25519 sk zero_coordinate == zero_coordinate)

(** -------------------------------------------------------------------- **)
(** Encoding/decoding consistency                                        **)
(** -------------------------------------------------------------------- **)

(** encode_le always produces exactly 32 bytes *)
val encode_le_length : n:nat
    -> Lemma (Seq.length (encode_le n) = 32)
let encode_le_length n = ()

(** The basepoint u=9 encodes as [0x09, 0x00, ..., 0x00] *)
val basepoint_encoding : unit
    -> Lemma (encode_le 9 == Seq.append (Seq.create 1 9uy) (Seq.create 31 0uy))
#push-options "--z3rlimit 20000"
let basepoint_encoding () =
  (* The encoding of 9 as 32 bytes little-endian places 0x09 at index 0
     and 0x00 at all other indices.  This is a concrete arithmetic fact:
     9 / 2^0 % 256 = 9, and 9 / 2^k % 256 = 0 for k >= 8.
     assert_norm evaluates encode_le 9 and the rhs to equal byte sequences. *)
  assert_norm (9 / pow2 (8 * 0) % 256 = 9);
  assert_norm (9 / pow2 (8 * 1) % 256 = 0);
  assert_norm (9 / pow2 (8 * 31) % 256 = 0);
  assert_norm (encode_le 9 == Seq.append (Seq.create 1 9uy) (Seq.create 31 0uy))
#pop-options

(** -------------------------------------------------------------------- **)
(** Relationship to the Haskell implementation                           **)
(** -------------------------------------------------------------------- **)

(** The following correspondences hold between this F* spec and the
    Haskell implementation in src/UmbraVox/Crypto/Curve25519.hs:

    F* spec                 Haskell implementation
    -------                 ----------------------
    prime                   prime
    a24                     a24 (local in scalarMult)
    basepoint               9 (encoded in x25519Basepoint)
    fadd / fsub / fmul      fAdd / fSub / fMul
    finv                    fInv (via powMod)
    pow_mod                 powMod
    decode_le / encode_le   decodeLE / encodeLE
    clamp_scalar            clampScalar
    ladder_step             inner body of go in scalarMult
    ladder_loop             go in scalarMult
    scalar_mult             scalarMult
    x25519                  x25519
    x25519_base             x25519 _ x25519Basepoint
*)
