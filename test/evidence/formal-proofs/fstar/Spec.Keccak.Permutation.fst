(**
 * Spec.Keccak.Permutation -- Keccak-f[1600] permutation (FIPS 202, Section 3.2)
 *
 * Split from Spec.Keccak for faster per-module F* verification.
 * Contains the state type, round constants, rotation offsets,
 * step functions (theta, rho, pi, chi, iota), and the full
 * Keccak-f[1600] permutation.
 *
 * Reference: FIPS 202, Sections 3.2, 3.4
 *
 * M13.2.3 proof work:
 *   - keccak_f1600_preserves_size: proved by return-type refinement (no assume)
 *   - pi_table bounds: proved via assert_norm on all 25 concrete values (no assume)
 *   - pi state length: proved by Seq.lemma_upd_len (no assume)
 *   - chi_identity: proved by Seq.seq_of_list_index + case split on x (no assume)
 *)
module Spec.Keccak.Permutation

open FStar.Seq
open FStar.UInt8
open FStar.UInt64
open FStar.Mul

#push-options "--z3rlimit 50000 --fuel 1 --ifuel 1"

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

(** Number of rounds in Keccak-f[1600] *)
let num_rounds : nat = 24

(** State is 25 lanes of 64 bits each, indexed as state[x + 5*y] *)
let state_size : nat = 25

(** -------------------------------------------------------------------- **)
(** FIPS 202, Section 3.2 -- Keccak-f[1600] round constants (iota step)  **)
(** -------------------------------------------------------------------- **)

let round_constants : (s:seq UInt64.t{Seq.length s = 24}) =
  let l = [
    0x0000000000000001uL; 0x0000000000008082uL;
    0x800000000000808AuL; 0x8000000080008000uL;
    0x000000000000808BuL; 0x0000000080000001uL;
    0x8000000080008081uL; 0x8000000000008009uL;
    0x000000000000008AuL; 0x0000000000000088uL;
    0x0000000080008009uL; 0x000000008000000AuL;
    0x000000008000808BuL; 0x800000000000008BuL;
    0x8000000000008089uL; 0x8000000000008003uL;
    0x8000000000008002uL; 0x8000000000000080uL;
    0x000000000000800AuL; 0x800000008000000AuL;
    0x8000000080008081uL; 0x8000000000008080uL;
    0x0000000080000001uL; 0x8000000080008008uL
  ] in
  let _ = assert_norm (List.Tot.length l = 24) in
  Seq.seq_of_list l

(** -------------------------------------------------------------------- **)
(** FIPS 202, Section 3.2.2 -- Rotation offsets (rho step)               **)
(** Indexed by x + 5*y, where x,y in [0..4]                             **)
(** -------------------------------------------------------------------- **)

let rotation_offsets : (s:seq nat{Seq.length s = 25}) =
  let l = [
     0;  1; 62; 28; 27;   (* y=0: (0,0) (1,0) (2,0) (3,0) (4,0) *)
    36; 44;  6; 55; 20;   (* y=1: (0,1) (1,1) (2,1) (3,1) (4,1) *)
     3; 10; 43; 25; 39;   (* y=2: (0,2) (1,2) (2,2) (3,2) (4,2) *)
    41; 45; 15; 21;  8;   (* y=3: (0,3) (1,3) (2,3) (3,3) (4,3) *)
    18;  2; 61; 56; 14    (* y=4: (0,4) (1,4) (2,4) (3,4) (4,4) *)
  ] in
  let _ = assert_norm (List.Tot.length l = 25) in
  Seq.seq_of_list l

(** Pi permutation table backing list (exposed for index lemmas). *)
let pi_table_list : list nat =
  [ 0; 10; 20;  5; 15;   (* src 0..4  -> dst *)
   16;  1; 11; 21;  6;   (* src 5..9  -> dst *)
    7; 17;  2; 12; 22;   (* src 10..14 -> dst *)
   23;  8; 18;  3; 13;   (* src 15..19 -> dst *)
   14; 24;  9; 19;  4 ]  (* src 20..24 -> dst *)

let _ = assert_norm (List.Tot.length pi_table_list = 25)

(** Pi permutation table: maps source index (x + 5*y) to destination index
    (y + 5*((2*x + 3*y) mod 5)).  Precomputed for all 25 positions. *)
let pi_table : (s:seq nat{Seq.length s = 25}) =
  assert_norm (List.Tot.length pi_table_list = 25);
  Seq.seq_of_list pi_table_list

(** All entries of pi_table are < 25: proved by unfolding to list + case split. *)
let pi_table_bounds_lemma (i : nat{i < 25})
    : Lemma (Seq.index pi_table i < 25) =
  (* Unfold Seq.index pi_table i = List.Tot.index pi_table_list i *)
  assert_norm (pi_table == Seq.seq_of_list pi_table_list);
  Seq.lemma_seq_of_list_index pi_table_list i;
  (* Now Seq.index pi_table i = List.Tot.index pi_table_list i; case split. *)
  if      i = 0  then assert_norm (List.Tot.index pi_table_list 0  < 25)
  else if i = 1  then assert_norm (List.Tot.index pi_table_list 1  < 25)
  else if i = 2  then assert_norm (List.Tot.index pi_table_list 2  < 25)
  else if i = 3  then assert_norm (List.Tot.index pi_table_list 3  < 25)
  else if i = 4  then assert_norm (List.Tot.index pi_table_list 4  < 25)
  else if i = 5  then assert_norm (List.Tot.index pi_table_list 5  < 25)
  else if i = 6  then assert_norm (List.Tot.index pi_table_list 6  < 25)
  else if i = 7  then assert_norm (List.Tot.index pi_table_list 7  < 25)
  else if i = 8  then assert_norm (List.Tot.index pi_table_list 8  < 25)
  else if i = 9  then assert_norm (List.Tot.index pi_table_list 9  < 25)
  else if i = 10 then assert_norm (List.Tot.index pi_table_list 10 < 25)
  else if i = 11 then assert_norm (List.Tot.index pi_table_list 11 < 25)
  else if i = 12 then assert_norm (List.Tot.index pi_table_list 12 < 25)
  else if i = 13 then assert_norm (List.Tot.index pi_table_list 13 < 25)
  else if i = 14 then assert_norm (List.Tot.index pi_table_list 14 < 25)
  else if i = 15 then assert_norm (List.Tot.index pi_table_list 15 < 25)
  else if i = 16 then assert_norm (List.Tot.index pi_table_list 16 < 25)
  else if i = 17 then assert_norm (List.Tot.index pi_table_list 17 < 25)
  else if i = 18 then assert_norm (List.Tot.index pi_table_list 18 < 25)
  else if i = 19 then assert_norm (List.Tot.index pi_table_list 19 < 25)
  else if i = 20 then assert_norm (List.Tot.index pi_table_list 20 < 25)
  else if i = 21 then assert_norm (List.Tot.index pi_table_list 21 < 25)
  else if i = 22 then assert_norm (List.Tot.index pi_table_list 22 < 25)
  else if i = 23 then assert_norm (List.Tot.index pi_table_list 23 < 25)
  else               assert_norm (List.Tot.index pi_table_list 24 < 25)

(** -------------------------------------------------------------------- **)
(** Keccak state type                                                     **)
(** -------------------------------------------------------------------- **)

(** A Keccak state is 25 UInt64 lanes *)
type keccak_state = (s:seq UInt64.t{Seq.length s = 25})

(** The all-zero initial state *)
let empty_state : keccak_state =
  Seq.create 25 0uL

(** -------------------------------------------------------------------- **)
(** Byte encoding helpers (little-endian, as per FIPS 202)               **)
(** -------------------------------------------------------------------- **)

(** Encode a UInt64 as 8 little-endian bytes *)
let uint64_to_le_bytes (w : UInt64.t) : (s:seq UInt8.t{Seq.length s = 8}) =
  let open FStar.Int.Cast in
  let l = [
    uint64_to_uint8 w;
    uint64_to_uint8 (UInt64.shift_right w 8ul);
    uint64_to_uint8 (UInt64.shift_right w 16ul);
    uint64_to_uint8 (UInt64.shift_right w 24ul);
    uint64_to_uint8 (UInt64.shift_right w 32ul);
    uint64_to_uint8 (UInt64.shift_right w 40ul);
    uint64_to_uint8 (UInt64.shift_right w 48ul);
    uint64_to_uint8 (UInt64.shift_right w 56ul)
  ] in
  let _ = assert_norm (List.Tot.length l = 8) in
  Seq.seq_of_list l

(** Decode 8 little-endian bytes at offset i into a UInt64 *)
let le_bytes_to_uint64 (b : seq UInt8.t) (i : nat{i + 8 <= Seq.length b})
    : UInt64.t =
  let open FStar.Int.Cast in
  UInt64.logor
    (UInt64.logor
      (UInt64.logor
        (uint8_to_uint64 (Seq.index b i))
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 1))) 8ul))
      (UInt64.logor
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 2))) 16ul)
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 3))) 24ul)))
    (UInt64.logor
      (UInt64.logor
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 4))) 32ul)
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 5))) 40ul))
      (UInt64.logor
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 6))) 48ul)
        (UInt64.shift_left (uint8_to_uint64 (Seq.index b (i + 7))) 56ul)))

(** -------------------------------------------------------------------- **)
(** FIPS 202, Section 3.2.1 -- Theta step                                **)
(** -------------------------------------------------------------------- **)

(** Compute column parity: C[x] = state[x] XOR state[x+5] XOR state[x+10]
    XOR state[x+15] XOR state[x+20] *)
let column_parity (st : keccak_state) (x : nat{x < 5}) : UInt64.t =
  UInt64.logxor (Seq.index st x)
    (UInt64.logxor (Seq.index st (x + 5))
      (UInt64.logxor (Seq.index st (x + 10))
        (UInt64.logxor (Seq.index st (x + 15))
                       (Seq.index st (x + 20)))))

(** Rotate left by n bits (mod 64).
    F* UInt64 does not have rotate_left, so we define it. *)
let rotl64 (w : UInt64.t) (r : UInt32.t{UInt32.v r < 64}) : UInt64.t =
  if r = 0ul then w
  else
    UInt64.logor (UInt64.shift_left w r)
                 (UInt64.shift_right w (FStar.UInt32.sub 64ul r))

(** Theta diffusion: D[x] = C[x-1] XOR rotl(C[x+1], 1)
    Then state'[x + 5*y] = state[x + 5*y] XOR D[x] *)
let theta (st : keccak_state) : keccak_state =
  let c0 = column_parity st 0 in
  let c1 = column_parity st 1 in
  let c2 = column_parity st 2 in
  let c3 = column_parity st 3 in
  let c4 = column_parity st 4 in
  let d0 = UInt64.logxor c4 (rotl64 c1 1ul) in
  let d1 = UInt64.logxor c0 (rotl64 c2 1ul) in
  let d2 = UInt64.logxor c1 (rotl64 c3 1ul) in
  let d3 = UInt64.logxor c2 (rotl64 c4 1ul) in
  let d4 = UInt64.logxor c3 (rotl64 c0 1ul) in
  (* XOR D[x] into every lane in column x *)
  let xor_d (i : nat{i < 25}) : UInt64.t =
    let x = i % 5 in
    let d = if x = 0 then d0
            else if x = 1 then d1
            else if x = 2 then d2
            else if x = 3 then d3
            else d4 in
    UInt64.logxor (Seq.index st i) d
  in
  let l = [
    xor_d 0;  xor_d 1;  xor_d 2;  xor_d 3;  xor_d 4;
    xor_d 5;  xor_d 6;  xor_d 7;  xor_d 8;  xor_d 9;
    xor_d 10; xor_d 11; xor_d 12; xor_d 13; xor_d 14;
    xor_d 15; xor_d 16; xor_d 17; xor_d 18; xor_d 19;
    xor_d 20; xor_d 21; xor_d 22; xor_d 23; xor_d 24
  ] in
  let _ = assert_norm (List.Tot.length l = 25) in
  Seq.seq_of_list l

(** -------------------------------------------------------------------- **)
(** FIPS 202, Section 3.2.2 -- Rho step (lane rotation)                  **)
(** -------------------------------------------------------------------- **)

(** Rho: rotate each lane by its rotation offset *)
let rho (st : keccak_state) : keccak_state =
  let rot_lane (i : nat{i < 25}) : UInt64.t =
    let offset = Seq.index rotation_offsets i in
    rotl64 (Seq.index st i) (FStar.UInt32.uint_to_t (offset % 64))
  in
  let l = [
    rot_lane 0;  rot_lane 1;  rot_lane 2;  rot_lane 3;  rot_lane 4;
    rot_lane 5;  rot_lane 6;  rot_lane 7;  rot_lane 8;  rot_lane 9;
    rot_lane 10; rot_lane 11; rot_lane 12; rot_lane 13; rot_lane 14;
    rot_lane 15; rot_lane 16; rot_lane 17; rot_lane 18; rot_lane 19;
    rot_lane 20; rot_lane 21; rot_lane 22; rot_lane 23; rot_lane 24
  ] in
  let _ = assert_norm (List.Tot.length l = 25) in
  Seq.seq_of_list l

(** -------------------------------------------------------------------- **)
(** FIPS 202, Section 3.2.3 -- Pi step (lane permutation)                **)
(** -------------------------------------------------------------------- **)

(** Pi: permute lane positions.
    dst[y + 5*((2*x + 3*y) mod 5)] = src[x + 5*y]
    pi_table_bounds_lemma proves all destination indices < 25;
    Seq.lemma_upd_len proves Seq.upd preserves length. *)
let pi (st : keccak_state) : keccak_state =
  let dst (i : nat{i < 25}) : UInt64.t =
    (* Find which source index maps to destination i via pi_table *)
    Seq.index st i
  in
  (* Build result by writing src[i] to pi_table[i] *)
  let result = Seq.create 25 0uL in
  let rec apply_pi (src : keccak_state) (dst : seq UInt64.t{Seq.length dst = 25})
                   (i : nat)
      : Tot (s:seq UInt64.t{Seq.length s = 25}) (decreases (25 - i)) =
    if i >= 25 then dst
    else
      let dst_idx = Seq.index pi_table i in
      pi_table_bounds_lemma i;
      let dst' = Seq.upd dst dst_idx (Seq.index src i) in
      Seq.lemma_len_upd dst_idx (Seq.index src i) dst;
      apply_pi src dst' (i + 1)
  in
  apply_pi st result 0

(** -------------------------------------------------------------------- **)
(** FIPS 202, Section 3.2.4 -- Chi step (nonlinear mixing)               **)
(** -------------------------------------------------------------------- **)

(** Chi: for each row y, a'[x,y] = a[x,y] XOR (NOT a[x+1,y] AND a[x+2,y]) *)
let chi_row (st : keccak_state) (y : nat{y < 5}) : (s:seq UInt64.t{Seq.length s = 5}) =
  let base = 5 * y in
  let t0 = Seq.index st (base + 0) in
  let t1 = Seq.index st (base + 1) in
  let t2 = Seq.index st (base + 2) in
  let t3 = Seq.index st (base + 3) in
  let t4 = Seq.index st (base + 4) in
  let l = [
    UInt64.logxor t0 (UInt64.logand (UInt64.lognot t1) t2);
    UInt64.logxor t1 (UInt64.logand (UInt64.lognot t2) t3);
    UInt64.logxor t2 (UInt64.logand (UInt64.lognot t3) t4);
    UInt64.logxor t3 (UInt64.logand (UInt64.lognot t4) t0);
    UInt64.logxor t4 (UInt64.logand (UInt64.lognot t0) t1)
  ] in
  let _ = assert_norm (List.Tot.length l = 5) in
  Seq.seq_of_list l

let chi (st : keccak_state) : keccak_state =
  let r0 = chi_row st 0 in
  let r1 = chi_row st 1 in
  let r2 = chi_row st 2 in
  let r3 = chi_row st 3 in
  let r4 = chi_row st 4 in
  Seq.append r0 (Seq.append r1 (Seq.append r2 (Seq.append r3 r4)))

(** -------------------------------------------------------------------- **)
(** FIPS 202, Section 3.2.5 -- Iota step (round constant addition)       **)
(** -------------------------------------------------------------------- **)

(** Iota: XOR round constant into lane (0,0) *)
let iota (st : keccak_state) (round_idx : nat{round_idx < 24}) : keccak_state =
  let lane0 = UInt64.logxor (Seq.index st 0) (Seq.index round_constants round_idx) in
  Seq.upd st 0 lane0

(** -------------------------------------------------------------------- **)
(** Keccak-f[1600] permutation                                           **)
(** -------------------------------------------------------------------- **)

(** A single round of Keccak-f[1600]: theta, rho, pi, chi, iota *)
let keccak_round (st : keccak_state) (round_idx : nat{round_idx < 24})
    : keccak_state =
  iota (chi (pi (rho (theta st)))) round_idx

(** Apply all 24 rounds of Keccak-f[1600] *)
let rec keccak_f1600_rounds (st : keccak_state) (round : nat)
    : Tot keccak_state (decreases (24 - round)) =
  if round >= 24 then st
  else keccak_f1600_rounds (keccak_round st round) (round + 1)

(** Keccak-f[1600]: the full permutation *)
val keccak_f1600 : st:keccak_state -> Tot keccak_state
let keccak_f1600 (st : keccak_state) : keccak_state =
  keccak_f1600_rounds st 0

(** -------------------------------------------------------------------- **)
(** Structural lemmas for the permutation layer                          **)
(** -------------------------------------------------------------------- **)

(** Keccak-f[1600] state is always 25 UInt64 lanes *)
val keccak_state_size_lemma : st:keccak_state
    -> Lemma (Seq.length st = 25)
let keccak_state_size_lemma st = ()

(** Keccak-f[1600] preserves state size.
    Proved by the return-type refinement: keccak_f1600 returns keccak_state,
    which is defined as (s:seq UInt64.t{Seq.length s = 25}). No assume needed. *)
val keccak_f1600_preserves_size : st:keccak_state
    -> Lemma (Seq.length (keccak_f1600 st) = 25)
let keccak_f1600_preserves_size st = ()

(** The round constants table has exactly 24 entries *)
val round_constants_length_lemma : unit
    -> Lemma (Seq.length round_constants = 24)
let round_constants_length_lemma () = ()

(** The rotation offsets table has exactly 25 entries *)
val rotation_offsets_length_lemma : unit
    -> Lemma (Seq.length rotation_offsets = 25)
let rotation_offsets_length_lemma () = ()

(** The pi permutation table has exactly 25 entries *)
val pi_table_length_lemma : unit
    -> Lemma (Seq.length pi_table = 25)
let pi_table_length_lemma () = ()

(** Chi satisfies the non-linear mixing identity.
    Proved by: (1) unfolding chi_row to reveal the explicit list construction,
    (2) applying Seq.seq_of_list_index to map Seq.index to List.Tot.index,
    (3) case-splitting on x in {0,1,2,3,4} to evaluate the list index,
    (4) using assert_norm to discharge the modular arithmetic for (x+1)%5, (x+2)%5. *)
val chi_identity : st:keccak_state -> y:nat{y < 5} -> x:nat{x < 5}
    -> Lemma (
        let base = 5 * y in
        let row = chi_row st y in
        Seq.index row x ==
          UInt64.logxor (Seq.index st (base + x))
            (UInt64.logand
              (UInt64.lognot (Seq.index st (base + ((x + 1) % 5))))
              (Seq.index st (base + ((x + 2) % 5)))))
let chi_identity st y x =
  let base = 5 * y in
  let t0 = Seq.index st (base + 0) in
  let t1 = Seq.index st (base + 1) in
  let t2 = Seq.index st (base + 2) in
  let t3 = Seq.index st (base + 3) in
  let t4 = Seq.index st (base + 4) in
  let l = [
    UInt64.logxor t0 (UInt64.logand (UInt64.lognot t1) t2);
    UInt64.logxor t1 (UInt64.logand (UInt64.lognot t2) t3);
    UInt64.logxor t2 (UInt64.logand (UInt64.lognot t3) t4);
    UInt64.logxor t3 (UInt64.logand (UInt64.lognot t4) t0);
    UInt64.logxor t4 (UInt64.logand (UInt64.lognot t0) t1)
  ] in
  assert_norm (List.Tot.length l = 5);
  (* chi_row st y = Seq.seq_of_list l by definition *)
  let row = chi_row st y in
  (* Use Seq.seq_of_list_index to reduce Seq.index (seq_of_list l) x *)
  Seq.lemma_seq_of_list_index l x;
  (* Now Seq.index row x = List.Tot.index l x *)
  (* Case split on x to evaluate List.Tot.index and the modular arithmetic *)
  if x = 0 then (
    assert_norm ((0 + 1) % 5 = 1);
    assert_norm ((0 + 2) % 5 = 2)
  ) else if x = 1 then (
    assert_norm ((1 + 1) % 5 = 2);
    assert_norm ((1 + 2) % 5 = 3)
  ) else if x = 2 then (
    assert_norm ((2 + 1) % 5 = 3);
    assert_norm ((2 + 2) % 5 = 4)
  ) else if x = 3 then (
    assert_norm ((3 + 1) % 5 = 4);
    assert_norm ((3 + 2) % 5 = 0)
  ) else (
    assert_norm ((4 + 1) % 5 = 0);
    assert_norm ((4 + 2) % 5 = 1)
  )

#pop-options
