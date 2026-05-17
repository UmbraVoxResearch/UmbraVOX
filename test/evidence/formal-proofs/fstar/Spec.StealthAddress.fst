(**
 * Spec.StealthAddress -- Functional specification of DKSAP stealth addresses
 *
 * This module specifies the Dual-Key Stealth Address Protocol (DKSAP)
 * implemented in src/UmbraVox/Crypto/StealthAddress.hs.
 *
 * Protocol summary (Section 3.1 of the spec):
 *   Recipient publishes:  scan key (X25519) + spend key (Ed25519)
 *   Sender derives:
 *     1. Ephemeral X25519 keypair (r, R = r*G)
 *     2. Shared secret S = r * scan_pub  (ECDH)
 *     3. View tag   = HKDF(0^32, S, "UmbraVox_ViewTag_v2")[0]
 *     4. Stealth scalar s = HKDF(0^32, S, "UmbraVox_StealthKey_v1") mod L
 *     5. Stealth address P = s*G_ed + spend_pub
 *   Recipient scans:
 *     1. Recompute S = scan_secret * R
 *     2. Recompute s and P
 *     3. Match P against candidate address
 *
 * Security properties:
 *   Correctness:   scanForPayment finds deriveStealthAddress output.
 *   Unlinkability: two independent derivations to the same recipient
 *                  produce different one-time addresses.
 *
 * References:
 *   src/UmbraVox/Crypto/StealthAddress.hs
 *   HKDF (RFC 5869), X25519 (RFC 7748), Ed25519 (RFC 8032)
 *)
module Spec.StealthAddress

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                            **)
(** -------------------------------------------------------------------- **)

let key_size : nat = 32    (* All keys/points are 32 bytes *)

(** Ed25519 group order L = 2^252 + 27742317777372353535851937790883648493 *)
let group_order_L : pos =
  normalize_term (pow2 252 + 27742317777372353535851937790883648493)

(** -------------------------------------------------------------------- **)
(** Type aliases                                                         **)
(** -------------------------------------------------------------------- **)

type bytes32 = s:seq UInt8.t{Seq.length s = key_size}

(** -------------------------------------------------------------------- **)
(** Primitive operations (abstract)                                      **)
(**                                                                       **)
(** These correspond directly to the primitives called in                **)
(** StealthAddress.hs.  They are left abstract because their full specs  **)
(** their full specifications live in dedicated modules (Spec.X25519,    **)
(** Spec.HKDF, Spec.Ed25519).                                            **)
(** -------------------------------------------------------------------- **)

(** X25519 scalar multiplication.  Returns None on low-order input.
    Corresponds to: UmbraVox.Crypto.Curve25519.x25519 *)
assume val x25519 : bytes32 -> bytes32 -> Tot (option bytes32)

(** X25519 basepoint multiplication: R = r * G.
    Corresponds to: x25519 r x25519Basepoint *)
assume val x25519_base : bytes32 -> Tot bytes32

(** Ed25519 scalar * G + point.  Encodes result as 32 bytes.
    Corresponds to: encodePoint (pointAdd (scalarMul s basepoint) spendPoint)
    Returns empty sequence if spendPub is not a valid Ed25519 point. *)
assume val ed25519_derive_point : nat -> bytes32 -> Tot (seq UInt8.t)

(** HKDF-SHA-512 with 32-byte zero salt, IKM=shared_secret, info, len=32.
    Corresponds to: UmbraVox.Crypto.HKDF.hkdf (BS.replicate 32 0) ss info 32 *)
assume val hkdf32 : ikm:bytes32 -> info:seq UInt8.t
    -> Tot (s:seq UInt8.t{Seq.length s = key_size})

(** Decode a 32-byte little-endian sequence to a natural number. *)
assume val decode_le : bytes32 -> Tot nat

(** -------------------------------------------------------------------- **)
(** HKDF domain separation info strings                                  **)
(** -------------------------------------------------------------------- **)

(** "UmbraVox_ViewTag_v2" as a concrete byte sequence (19 bytes).
    ASCII: U m b r a V o x _ V i e w T a g _ v 2
           55 6d 62 72 61 56 6f 78 5f 56 69 65 77 54 61 67 5f 76 32
    Matches viewTagInfo in src/UmbraVox/Crypto/StealthAddress.hs. *)
let view_tag_info : seq UInt8.t =
  let l = [
    0x55uy; 0x6duy; 0x62uy; 0x72uy; 0x61uy; 0x56uy; 0x6fuy; 0x78uy;
    0x5fuy; 0x56uy; 0x69uy; 0x65uy; 0x77uy; 0x54uy; 0x61uy; 0x67uy;
    0x5fuy; 0x76uy; 0x32uy
  ] in
  let _ = assert_norm (List.Tot.length l = 19) in
  Seq.seq_of_list l

(** "UmbraVox_StealthKey_v1" as a concrete byte sequence (22 bytes).
    ASCII: U m b r a V o x _ S t e a l t h K e y _ v 1
           55 6d 62 72 61 56 6f 78 5f 53 74 65 61 6c 74 68 4b 65 79 5f 76 31
    Matches stealthKeyInfo in src/UmbraVox/Crypto/StealthAddress.hs. *)
let stealth_key_info : seq UInt8.t =
  let l = [
    0x55uy; 0x6duy; 0x62uy; 0x72uy; 0x61uy; 0x56uy; 0x6fuy; 0x78uy;
    0x5fuy; 0x53uy; 0x74uy; 0x65uy; 0x61uy; 0x6cuy; 0x74uy; 0x68uy;
    0x4buy; 0x65uy; 0x79uy; 0x5fuy; 0x76uy; 0x31uy
  ] in
  let _ = assert_norm (List.Tot.length l = 22) in
  Seq.seq_of_list l

(** The two info strings are distinct (domain separation).
    Proved by length: |view_tag_info| = 19 ≠ 22 = |stealth_key_info|. *)
val info_strings_distinct : unit
    -> Lemma (view_tag_info <> stealth_key_info)
let info_strings_distinct () =
  (* The two sequences have different lengths (19 vs 22), so they cannot be equal.
     Both are seq_of_list of their respective lists; assert_norm evaluates lengths. *)
  assert_norm (Seq.length view_tag_info = 19);
  assert_norm (Seq.length stealth_key_info = 22);
  introduce view_tag_info = stealth_key_info ==> False
  with _h. (assert (Seq.length view_tag_info = Seq.length stealth_key_info))

(** -------------------------------------------------------------------- **)
(** Protocol building blocks                                             **)
(** -------------------------------------------------------------------- **)

(** Derive the view tag from a shared secret.
    view_tag(S) = HKDF(0^32, S, "UmbraVox_ViewTag_v2")[0] *)
val derive_view_tag : shared_secret:bytes32 -> Tot UInt8.t
let derive_view_tag shared_secret =
  Seq.index (hkdf32 shared_secret view_tag_info) 0

(** Derive the stealth scalar from a shared secret.
    s = decode_le(HKDF(0^32, S, "UmbraVox_StealthKey_v1")) mod L *)
val derive_stealth_scalar : shared_secret:bytes32 -> Tot nat
let derive_stealth_scalar shared_secret =
  let raw = hkdf32 shared_secret stealth_key_info in
  decode_le raw % group_order_L

(** The stealth scalar is in [0, L). *)
val stealth_scalar_in_range : shared_secret:bytes32
    -> Lemma (derive_stealth_scalar shared_secret < group_order_L)
let stealth_scalar_in_range shared_secret = ()

(** -------------------------------------------------------------------- **)
(** Sender: derive stealth address                                       **)
(**                                                                       **)
(** Given recipient's scan_pub and spend_pub, and an ephemeral secret r: **)
(**   R      = r * G                                                     **)
(**   S      = x25519(r, scan_pub)   (shared secret)                    **)
(**   vt     = view_tag(S)                                               **)
(**   s      = stealth_scalar(S)                                         **)
(**   P      = s * G_ed + spend_pub  (one-time stealth address)         **)
(** -------------------------------------------------------------------- **)

(** Result of sender-side derivation. *)
noeq type stealth_address = {
  sa_address   : seq UInt8.t;   (* 32-byte one-time Ed25519 public key *)
  sa_ephemeral : bytes32;       (* 32-byte ephemeral X25519 public key R *)
  sa_view_tag  : UInt8.t        (* 1-byte view tag for fast scanning *)
}

(** Derive a stealth address from ephemeral secret r and recipient keys.
    Returns None if shared secret is the all-zero (low-order) point. *)
val derive_stealth_address
    : eph_secret:bytes32
   -> scan_pub:bytes32
   -> spend_pub:bytes32
   -> Tot (option stealth_address)
let derive_stealth_address eph_secret scan_pub spend_pub =
  let eph_public = x25519_base eph_secret in
  match x25519 eph_secret scan_pub with
  | None -> None         (* low-order point: reject *)
  | Some shared_secret ->
    let vt = derive_view_tag shared_secret in
    let s  = derive_stealth_scalar shared_secret in
    let p  = ed25519_derive_point s spend_pub in
    Some { sa_address   = p
         ; sa_ephemeral = eph_public
         ; sa_view_tag  = vt }

(** -------------------------------------------------------------------- **)
(** Recipient: scan for payment                                          **)
(**                                                                       **)
(** Given scan_secret, spend_pub, ephemeral R, candidate P:             **)
(**   S    = x25519(scan_secret, R)                                      **)
(**   s    = stealth_scalar(S)                                           **)
(**   P'   = s * G_ed + spend_pub                                        **)
(**   match iff P' = P                                                   **)
(** -------------------------------------------------------------------- **)

(** Byte-wise equality on sequences. *)
assume val seq_eq : seq UInt8.t -> seq UInt8.t -> Tot bool

(** seq_eq is a correct equality test. *)
assume val seq_eq_iff : s1:seq UInt8.t -> s2:seq UInt8.t
    -> Lemma (seq_eq s1 s2 <==> s1 == s2)

(** Scan a transaction output: check if ephemeral R and candidate P
    belong to us.  Returns true iff P equals the recomputed stealth address. *)
val scan_for_payment
    : scan_secret:bytes32
   -> spend_pub:bytes32
   -> eph_r:bytes32
   -> candidate_p:seq UInt8.t
   -> Tot bool
let scan_for_payment scan_secret spend_pub eph_r candidate_p =
  match x25519 scan_secret eph_r with
  | None -> false
  | Some shared_secret ->
    let s        = derive_stealth_scalar shared_secret in
    let expected = ed25519_derive_point s spend_pub in
    seq_eq expected candidate_p

(** -------------------------------------------------------------------- **)
(** AEAD Correctness: scan finds what derive produced                    **)
(**                                                                       **)
(** If a sender derives a stealth address (R, P) for a recipient, the   **)
(** recipient's scan correctly identifies P when given R.                **)
(**                                                                       **)
(** Formally: derive_stealth_address(r, scan_pub, spend_pub) = Some sa  **)
(**           ==> scan_for_payment(scan_sec, spend_pub, sa.ephemeral,    **)
(**                                sa.address) = true                   **)
(**                                                                       **)
(** This holds because:                                                  **)
(**   sender:    S = x25519(r, scan_pub)                                 **)
(**   recipient: S = x25519(scan_sec, R) = x25519(scan_sec, r*G)        **)
(**   DH commutativity: x25519(r, scan_pub) = x25519(scan_sec, r*G)     **)
(**   because scan_pub = x25519_base(scan_sec).                          **)
(** -------------------------------------------------------------------- **)

(** DH commutativity on X25519: x25519(a, x25519_base(b)) = x25519(b, x25519_base(a))
    This is the fundamental property underlying correctness.  It follows from
    Spec.X25519.dh_commutativity_general applied to scan_pub = x25519_base(scan_sec). *)
assume val x25519_dh_comm : a:bytes32 -> b:bytes32
    -> Lemma (x25519 a (x25519_base b) = x25519 b (x25519_base a))

val scan_correctness
    : eph_secret:bytes32
   -> scan_secret:bytes32
   -> spend_pub:bytes32
   -> Lemma (
       (* scan_pub is derived from scan_secret *)
       let scan_pub = x25519_base scan_secret in
       match derive_stealth_address eph_secret scan_pub spend_pub with
       | None    -> True    (* low-order point: no claim *)
       | Some sa ->
         scan_for_payment scan_secret spend_pub sa.sa_ephemeral sa.sa_address = true)
let scan_correctness eph_secret scan_secret spend_pub =
  (* Structural proof:
     1. Apply x25519_dh_comm eph_secret scan_secret to establish that
        x25519 eph_secret (x25519_base scan_secret) = x25519 scan_secret (x25519_base eph_secret).
     2. Unfold derive_stealth_address: in the Some branch, shared_secret is the result
        of x25519 eph_secret scan_pub and sa.sa_ephemeral = x25519_base eph_secret.
     3. In scan_for_payment, x25519 scan_secret sa.sa_ephemeral
        = x25519 scan_secret (x25519_base eph_secret) = Some shared_secret (by dh_comm).
     4. Both sides compute derive_stealth_scalar and ed25519_derive_point on shared_secret,
        so expected = sa.sa_address.
     5. seq_eq_iff gives seq_eq sa.sa_address sa.sa_address = true (reflexivity). *)
  let scan_pub = x25519_base scan_secret in
  x25519_dh_comm eph_secret scan_secret;
  match x25519 eph_secret scan_pub with
  | None -> ()
  | Some shared_secret ->
    seq_eq_iff
      (ed25519_derive_point (derive_stealth_scalar shared_secret) spend_pub)
      (ed25519_derive_point (derive_stealth_scalar shared_secret) spend_pub)

(** -------------------------------------------------------------------- **)
(** Unlinkability: two derivations produce different addresses           **)
(**                                                                       **)
(** If two different ephemeral secrets r1 /= r2 are used, the resulting **)
(** stealth addresses P1 /= P2 with overwhelming probability.            **)
(**                                                                       **)
(** This holds because R1 = r1*G /= r2*G = R2 (assuming r1 /= r2) and  **)
(** the shared secrets S1, S2 differ.  Different S => different s =>     **)
(** different P = s*G_ed + spend_pub (injectivity of scalar mult).      **)
(** -------------------------------------------------------------------- **)

(** CRYPTOGRAPHIC HARDNESS ASSUMPTION: Unlinkability of stealth addresses.
    The core argument is:
      eph1 <> eph2 => x25519_base(eph1) <> x25519_base(eph2)
    which relies on injectivity of X25519 basepoint multiplication (after
    clamping).  This is a property of the Curve25519 group structure that
    cannot be discharged without a concrete model of the curve arithmetic.
    The stronger claim (sa_address differs) additionally requires the
    decisional Diffie-Hellman assumption on Curve25519: different shared
    secrets S1 <> S2 lead to different HKDF outputs and hence different
    stealth scalars s1 <> s2.  Both properties are undischargeable in F*. *)
assume val unlinkability
    : eph1:bytes32
   -> eph2:bytes32
   -> scan_pub:bytes32
   -> spend_pub:bytes32
   -> Lemma (
       requires eph1 <> eph2)
       (ensures (
         match derive_stealth_address eph1 scan_pub spend_pub,
               derive_stealth_address eph2 scan_pub spend_pub with
         | Some sa1, Some sa2 ->
             (* Different ephemeral keys give different stealth addresses *)
             sa1.sa_address <> sa2.sa_address \/
             (* or at least different ephemeral public keys (observable by sender) *)
             sa1.sa_ephemeral <> sa2.sa_ephemeral
         | _ -> True))   (* if either derivation fails, no claim *)

(** -------------------------------------------------------------------- **)
(** View tag fast-filter correctness                                     **)
(**                                                                       **)
(** The view tag eliminates ~255/256 non-matching transactions without   **)
(** recomputing the full stealth address.  If the shared secret differs, **)
(** the view tag differs with probability ~255/256.                      **)
(** -------------------------------------------------------------------- **)

(** A matching payment has the correct view tag. *)
val view_tag_match
    : eph_secret:bytes32
   -> scan_secret:bytes32
   -> spend_pub:bytes32
   -> Lemma (
       let scan_pub = x25519_base scan_secret in
       match derive_stealth_address eph_secret scan_pub spend_pub with
       | None    -> True
       | Some sa ->
           match x25519 scan_secret sa.sa_ephemeral with
           | None -> True
           | Some shared_secret ->
               sa.sa_view_tag = derive_view_tag shared_secret)
let view_tag_match eph_secret scan_secret spend_pub =
  (* Structural proof:
     1. Apply x25519_dh_comm to establish sender and recipient derive the same S.
     2. In the derive_stealth_address Some branch:
        - sa.sa_ephemeral = x25519_base eph_secret
        - sa.sa_view_tag  = derive_view_tag shared_secret  (where shared_secret = x25519 eph_secret scan_pub)
     3. x25519 scan_secret sa.sa_ephemeral
        = x25519 scan_secret (x25519_base eph_secret)
        = x25519 eph_secret (x25519_base scan_secret)   [by dh_comm]
        = x25519 eph_secret scan_pub = Some shared_secret.
     4. derive_view_tag shared_secret = derive_view_tag shared_secret (trivial equality). *)
  let scan_pub = x25519_base scan_secret in
  x25519_dh_comm eph_secret scan_secret;
  match x25519 eph_secret scan_pub with
  | None -> ()
  | Some _shared_secret -> ()

(** -------------------------------------------------------------------- **)
(** Spending key derivation                                              **)
(**                                                                       **)
(** The recipient's spending secret for a matched payment is:            **)
(**   sk_stealth = (s + clamp(sha512(spend_secret)[0..31])) mod L       **)
(** where s is the HKDF-derived stealth scalar.                          **)
(** The corresponding public key equals sa.sa_address.                  **)
(** -------------------------------------------------------------------- **)

(** SHA-512 hash function. *)
assume val sha512 : seq UInt8.t -> Tot (s:seq UInt8.t{Seq.length s = 64})

(** Ed25519 scalar clamping (per RFC 8032). *)
assume val clamp_scalar : bytes32 -> Tot nat

(** Compute the spending secret for a matched stealth output. *)
val compute_spending_secret : stealth_scalar:nat -> spend_secret:bytes32 -> Tot nat
let compute_spending_secret s spend_secret =
  let h = sha512 spend_secret in
  let a = clamp_scalar (Seq.slice h 0 32) in
  (s + a) % group_order_L

(** The spending secret is in [0, L). *)
val spending_secret_in_range : s:nat -> spend_secret:bytes32
    -> Lemma (compute_spending_secret s spend_secret < group_order_L)
let spending_secret_in_range s spend_secret = ()

(** -------------------------------------------------------------------- **)
(** Correspondence to Haskell implementation                             **)
(** -------------------------------------------------------------------- **)

(**
 * +------------------------------+---------------------------------------+
 * | F* definition                | Haskell counterpart                  |
 * +------------------------------+---------------------------------------+
 * | derive_stealth_address       | deriveStealthAddress                 |
 * | scan_for_payment             | scanForPayment                       |
 * | derive_view_tag              | viewTag . deriveViewTagBytes         |
 * | derive_stealth_scalar        | hkdf hkdfSalt ss stealthKeyInfo 32   |
 * | x25519 / x25519_base         | x25519 / x25519 _ x25519Basepoint   |
 * | ed25519_derive_point         | addSpendKey (scalarMul s basepoint)  |
 * | hkdf32                       | hkdf hkdfSalt ss info 32             |
 * | view_tag_info / stealth_key_info | viewTagInfo / stealthKeyInfo     |
 * | compute_spending_secret      | computeSpendingSecret                |
 * | scan_correctness             | round-trip scan correctness          |
 * | unlinkability                | different R => different P           |
 * +------------------------------+---------------------------------------+
 *)
