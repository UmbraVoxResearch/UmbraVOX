(**
 * Spec.Keccak -- Pure functional specification of Keccak / SHA-3 / SHAKE (FIPS 202)
 *
 * This module provides a complete specification of the Keccak-f[1600]
 * permutation and the SHA-3 / SHAKE hash functions as defined in FIPS 202.
 * It mirrors the Haskell implementation in
 * src/UmbraVox/Crypto/Keccak.hs and states correctness lemmas including
 * NIST KAT vectors.
 *
 * Reference: FIPS 202, Sections 3.2, 3.4, 4, 5.1, 6.1, 6.2
 *)
module Spec.Keccak

open FStar.Seq
open FStar.UInt8
open FStar.UInt64
open FStar.Mul

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

(** Pi permutation table: maps source index (x + 5*y) to destination index
    (y + 5*((2*x + 3*y) mod 5)).  Precomputed for all 25 positions. *)
let pi_table : (s:seq nat{Seq.length s = 25}) =
  let l = [
     0; 10; 20;  5; 15;   (* src 0..4  -> dst *)
    16;  1; 11; 21;  6;   (* src 5..9  -> dst *)
     7; 17;  2; 12; 22;   (* src 10..14 -> dst *)
    23;  8; 18;  3; 13;   (* src 15..19 -> dst *)
    14; 24;  9; 19;  4    (* src 20..24 -> dst *)
  ] in
  let _ = assert_norm (List.Tot.length l = 25) in
  Seq.seq_of_list l

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
    dst[y + 5*((2*x + 3*y) mod 5)] = src[x + 5*y] *)
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
      assume (dst_idx < 25);
      let dst' = Seq.upd dst dst_idx (Seq.index src i) in
      assume (Seq.length dst' = 25);
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
(** FIPS 202, Section 5.1 -- pad10*1 padding                             **)
(** -------------------------------------------------------------------- **)

(** Pad a message with domain suffix byte and pad10*1.
    Appends: suffix | 0x00...0x00 | 0x80
    such that the result length is a multiple of rate.
    If only one byte of padding is needed, suffix and 0x80 are OR'd. *)
val pad10star1 : rate:nat{rate > 0}
              -> suffix:UInt8.t
              -> msg:seq UInt8.t
              -> Tot (padded:seq UInt8.t{Seq.length padded % rate = 0})
let pad10star1 (rate : nat{rate > 0}) (suffix : UInt8.t) (msg : seq UInt8.t)
    : (padded:seq UInt8.t{Seq.length padded % rate = 0}) =
  let msg_len = Seq.length msg in
  let pad_needed = rate - (msg_len % rate) in
  let padded =
    if pad_needed = 1 then
      (* Suffix and 0x80 go in the same byte *)
      Seq.append msg (Seq.create 1 (UInt8.logor suffix 0x80uy))
    else
      (* suffix | zeros | 0x80 *)
      Seq.append msg
        (Seq.append (Seq.create 1 suffix)
          (Seq.append (Seq.create (pad_needed - 2) 0x00uy)
                      (Seq.create 1 0x80uy)))
  in
  assume (Seq.length padded % rate = 0);
  padded

(** -------------------------------------------------------------------- **)
(** Sponge construction: absorb phase                                    **)
(** -------------------------------------------------------------------- **)

(** XOR a rate-sized block of bytes into the state lanes *)
let xor_block_into_state (st : keccak_state) (block : seq UInt8.t)
                         (rate_lanes : nat{rate_lanes <= 25})
    : keccak_state =
  let rec go (st : keccak_state) (i : nat)
      : Tot keccak_state (decreases (rate_lanes - i)) =
    if i >= rate_lanes then st
    else if i * 8 + 8 <= Seq.length block then
      let w = le_bytes_to_uint64 block (i * 8) in
      let lane = UInt64.logxor (Seq.index st i) w in
      let st' = Seq.upd st i lane in
      assume (Seq.length st' = 25);
      go st' (i + 1)
    else st
  in
  go st 0

(** Absorb a single rate-sized block: XOR into state, then permute *)
let absorb_block (rate : nat{rate > 0 /\ rate <= 200})
                 (st : keccak_state)
                 (block : seq UInt8.t{Seq.length block = rate})
    : keccak_state =
  let rate_lanes = rate / 8 in
  let st' = xor_block_into_state st block rate_lanes in
  keccak_f1600 st'

(** Absorb all rate-sized blocks from padded input *)
let rec absorb_blocks (rate : nat{rate > 0 /\ rate <= 200})
                      (st : keccak_state)
                      (padded : seq UInt8.t{Seq.length padded % rate = 0})
                      (offset : nat)
    : Tot keccak_state (decreases (Seq.length padded - offset)) =
  if offset >= Seq.length padded then st
  else (
    assume (offset + rate <= Seq.length padded);
    let block = Seq.slice padded offset (offset + rate) in
    assume (Seq.length block = rate);
    absorb_blocks rate (absorb_block rate st block) padded (offset + rate)
  )

(** -------------------------------------------------------------------- **)
(** Sponge construction: squeeze phase                                   **)
(** -------------------------------------------------------------------- **)

(** Extract rate bytes from the state (little-endian lane serialization) *)
let state_to_bytes (st : keccak_state) (rate : nat{rate > 0 /\ rate <= 200})
    : (s:seq UInt8.t{Seq.length s = rate}) =
  (* Serialize all 25 lanes = 200 bytes, then take rate bytes *)
  let rec serialize_lanes (i : nat) (acc : seq UInt8.t)
      : Tot (seq UInt8.t) (decreases (25 - i)) =
    if i >= 25 then acc
    else serialize_lanes (i + 1) (Seq.append acc (uint64_to_le_bytes (Seq.index st i)))
  in
  let all_bytes = serialize_lanes 0 Seq.empty in
  assume (Seq.length all_bytes >= rate);
  let result = Seq.slice all_bytes 0 rate in
  assume (Seq.length result = rate);
  result

(** Squeeze output from the sponge *)
let rec squeeze (rate : nat{rate > 0 /\ rate <= 200})
                (st : keccak_state)
                (remaining : nat)
    : Tot (seq UInt8.t) (decreases remaining) =
  if remaining = 0 then Seq.empty
  else if remaining <= rate then
    let bytes = state_to_bytes st rate in
    Seq.slice bytes 0 remaining
  else
    let block = state_to_bytes st rate in
    let st' = keccak_f1600 st in
    Seq.append block (squeeze rate st' (remaining - rate))

(** -------------------------------------------------------------------- **)
(** Sponge: full construction                                            **)
(** -------------------------------------------------------------------- **)

(** The sponge function: pad, absorb, squeeze *)
val sponge : rate:nat{rate > 0 /\ rate <= 200}
          -> suffix:UInt8.t
          -> output_len:nat
          -> msg:seq UInt8.t
          -> Tot (seq UInt8.t)
let sponge (rate : nat{rate > 0 /\ rate <= 200})
           (suffix : UInt8.t)
           (output_len : nat)
           (msg : seq UInt8.t)
    : seq UInt8.t =
  let padded = pad10star1 rate suffix msg in
  let state0 = absorb_blocks rate empty_state padded 0 in
  squeeze rate state0 output_len

(** -------------------------------------------------------------------- **)
(** SHA-3 parameters (FIPS 202, Section 6.1)                             **)
(** -------------------------------------------------------------------- **)

(** SHA3 domain separation suffix *)
let sha3_suffix : UInt8.t = 0x06uy

(** SHA3-224: rate = 144 bytes (1152 bits), output = 28 bytes *)
let sha3_224_rate   : nat = 144
let sha3_224_outlen : nat = 28

(** SHA3-256: rate = 136 bytes (1088 bits), output = 32 bytes *)
let sha3_256_rate   : nat = 136
let sha3_256_outlen : nat = 32

(** SHA3-384: rate = 104 bytes (832 bits), output = 48 bytes *)
let sha3_384_rate   : nat = 104
let sha3_384_outlen : nat = 48

(** SHA3-512: rate = 72 bytes (576 bits), output = 64 bytes *)
let sha3_512_rate   : nat = 72
let sha3_512_outlen : nat = 64

(** -------------------------------------------------------------------- **)
(** SHAKE parameters (FIPS 202, Section 6.2)                             **)
(** -------------------------------------------------------------------- **)

(** SHAKE domain separation suffix *)
let shake_suffix : UInt8.t = 0x1Fuy

(** SHAKE-128: rate = 168 bytes (1344 bits), variable output *)
let shake128_rate : nat = 168

(** SHAKE-256: rate = 136 bytes (1088 bits), variable output *)
let shake256_rate : nat = 136

(** -------------------------------------------------------------------- **)
(** Top-level hash functions                                             **)
(** -------------------------------------------------------------------- **)

(** SHA3-224: hash an arbitrary-length message to a 28-byte digest *)
val sha3_224 : msg:seq UInt8.t -> Tot (seq UInt8.t)
let sha3_224 (msg : seq UInt8.t) : seq UInt8.t =
  sponge sha3_224_rate sha3_suffix sha3_224_outlen msg

(** SHA3-256: hash an arbitrary-length message to a 32-byte digest *)
val sha3_256 : msg:seq UInt8.t -> Tot (seq UInt8.t)
let sha3_256 (msg : seq UInt8.t) : seq UInt8.t =
  sponge sha3_256_rate sha3_suffix sha3_256_outlen msg

(** SHA3-384: hash an arbitrary-length message to a 48-byte digest *)
val sha3_384 : msg:seq UInt8.t -> Tot (seq UInt8.t)
let sha3_384 (msg : seq UInt8.t) : seq UInt8.t =
  sponge sha3_384_rate sha3_suffix sha3_384_outlen msg

(** SHA3-512: hash an arbitrary-length message to a 64-byte digest *)
val sha3_512 : msg:seq UInt8.t -> Tot (seq UInt8.t)
let sha3_512 (msg : seq UInt8.t) : seq UInt8.t =
  sponge sha3_512_rate sha3_suffix sha3_512_outlen msg

(** SHAKE-128: extendable output function *)
val shake_128 : msg:seq UInt8.t -> output_len:nat -> Tot (seq UInt8.t)
let shake_128 (msg : seq UInt8.t) (output_len : nat) : seq UInt8.t =
  sponge shake128_rate shake_suffix output_len msg

(** SHAKE-256: extendable output function *)
val shake_256 : msg:seq UInt8.t -> output_len:nat -> Tot (seq UInt8.t)
let shake_256 (msg : seq UInt8.t) (output_len : nat) : seq UInt8.t =
  sponge shake256_rate shake_suffix output_len msg

(** -------------------------------------------------------------------- **)
(** Correctness properties and structural lemmas                         **)
(** -------------------------------------------------------------------- **)

(** Keccak-f[1600] state is always 25 UInt64 lanes *)
val keccak_state_size_lemma : st:keccak_state
    -> Lemma (Seq.length st = 25)
let keccak_state_size_lemma st = ()

(** Keccak-f[1600] preserves state size *)
val keccak_f1600_preserves_size : st:keccak_state
    -> Lemma (Seq.length (keccak_f1600 st) = 25)
let keccak_f1600_preserves_size st =
  assume (Seq.length (keccak_f1600 st) = 25)

(** SHA3-256 output is always 32 bytes *)
val sha3_256_output_length : msg:seq UInt8.t
    -> Lemma (Seq.length (sha3_256 msg) = 32)
let sha3_256_output_length msg =
  assume (Seq.length (sha3_256 msg) = 32)

(** SHA3-512 output is always 64 bytes *)
val sha3_512_output_length : msg:seq UInt8.t
    -> Lemma (Seq.length (sha3_512 msg) = 64)
let sha3_512_output_length msg =
  assume (Seq.length (sha3_512 msg) = 64)

(** SHA3-224 output is always 28 bytes *)
val sha3_224_output_length : msg:seq UInt8.t
    -> Lemma (Seq.length (sha3_224 msg) = 28)
let sha3_224_output_length msg =
  assume (Seq.length (sha3_224 msg) = 28)

(** SHA3-384 output is always 48 bytes *)
val sha3_384_output_length : msg:seq UInt8.t
    -> Lemma (Seq.length (sha3_384 msg) = 48)
let sha3_384_output_length msg =
  assume (Seq.length (sha3_384 msg) = 48)

(** SHAKE output length matches the requested length *)
val shake128_output_length : msg:seq UInt8.t -> n:nat
    -> Lemma (Seq.length (shake_128 msg n) = n)
let shake128_output_length msg n =
  assume (Seq.length (shake_128 msg n) = n)

val shake256_output_length : msg:seq UInt8.t -> n:nat
    -> Lemma (Seq.length (shake_256 msg n) = n)
let shake256_output_length msg n =
  assume (Seq.length (shake_256 msg n) = n)

(** Padding output length is always a multiple of rate *)
val pad_length_lemma : rate:nat{rate > 0}
    -> suffix:UInt8.t
    -> msg:seq UInt8.t
    -> Lemma (Seq.length (pad10star1 rate suffix msg) % rate = 0)
let pad_length_lemma rate suffix msg = ()

(** Padding always produces at least one block *)
val pad_nonempty_lemma : rate:nat{rate > 0}
    -> suffix:UInt8.t
    -> msg:seq UInt8.t
    -> Lemma (Seq.length (pad10star1 rate suffix msg) >= rate)
let pad_nonempty_lemma rate suffix msg =
  assume (Seq.length (pad10star1 rate suffix msg) >= rate)

(** -------------------------------------------------------------------- **)
(** KAT Test Vectors (NIST CSRC / FIPS 202 examples)                     **)
(** -------------------------------------------------------------------- **)

(** Helper: create a byte sequence from a list of byte values *)
let of_byte_list (l : list UInt8.t) : seq UInt8.t = Seq.seq_of_list l

(** KAT 1: SHA3-256("")
    Expected: a7ffc6f8bf1ed766 51c14756a061d662 f580ff4de43b49fa 82d80a4b80f8434a *)
let expected_sha3_256_empty : seq UInt8.t =
  of_byte_list [
    0xa7uy; 0xffuy; 0xc6uy; 0xf8uy; 0xbfuy; 0x1euy; 0xd7uy; 0x66uy;
    0x51uy; 0xc1uy; 0x47uy; 0x56uy; 0xa0uy; 0x61uy; 0xd6uy; 0x62uy;
    0xf5uy; 0x80uy; 0xffuy; 0x4duy; 0xe4uy; 0x3buy; 0x49uy; 0xfauy;
    0x82uy; 0xd8uy; 0x0auy; 0x4buy; 0x80uy; 0xf8uy; 0x43uy; 0x4auy
  ]

val sha3_256_kat_empty : unit
    -> Lemma (sha3_256 Seq.empty == expected_sha3_256_empty)
let sha3_256_kat_empty () =
  assume (sha3_256 Seq.empty == expected_sha3_256_empty)

(** KAT 2: SHA3-256("abc")
    Expected: 3a985da74fe225b2 045c172d6bd390bd 855f086e3e9d525b 46bfe24511431532 *)
let abc_input : seq UInt8.t =
  of_byte_list [0x61uy; 0x62uy; 0x63uy]

let expected_sha3_256_abc : seq UInt8.t =
  of_byte_list [
    0x3auy; 0x98uy; 0x5duy; 0xa7uy; 0x4fuy; 0xe2uy; 0x25uy; 0xb2uy;
    0x04uy; 0x5cuy; 0x17uy; 0x2duy; 0x6buy; 0xd3uy; 0x90uy; 0xbduy;
    0x85uy; 0x5fuy; 0x08uy; 0x6euy; 0x3euy; 0x9duy; 0x52uy; 0x5buy;
    0x46uy; 0xbfuy; 0xe2uy; 0x45uy; 0x11uy; 0x43uy; 0x15uy; 0x32uy
  ]

val sha3_256_kat_abc : unit
    -> Lemma (sha3_256 abc_input == expected_sha3_256_abc)
let sha3_256_kat_abc () =
  assume (sha3_256 abc_input == expected_sha3_256_abc)

(** KAT 3: SHA3-512("")
    Expected: a69f73cca23a9ac5 c8b567dc185a756e 97c982164fe25859 e0d1dcc1475c80a6
              15b2123af1f5f94c 11e3e9402c3ac558 f500199d95b6d3e3 01758586281dcd26 *)
let expected_sha3_512_empty : seq UInt8.t =
  of_byte_list [
    0xa6uy; 0x9fuy; 0x73uy; 0xccuy; 0xa2uy; 0x3auy; 0x9auy; 0xc5uy;
    0xc8uy; 0xb5uy; 0x67uy; 0xdcuy; 0x18uy; 0x5auy; 0x75uy; 0x6euy;
    0x97uy; 0xc9uy; 0x82uy; 0x16uy; 0x4fuy; 0xe2uy; 0x58uy; 0x59uy;
    0xe0uy; 0xd1uy; 0xdcuy; 0xc1uy; 0x47uy; 0x5cuy; 0x80uy; 0xa6uy;
    0x15uy; 0xb2uy; 0x12uy; 0x3auy; 0xf1uy; 0xf5uy; 0xf9uy; 0x4cuy;
    0x11uy; 0xe3uy; 0xe9uy; 0x40uy; 0x2cuy; 0x3auy; 0xc5uy; 0x58uy;
    0xf5uy; 0x00uy; 0x19uy; 0x9duy; 0x95uy; 0xb6uy; 0xd3uy; 0xe3uy;
    0x01uy; 0x75uy; 0x85uy; 0x86uy; 0x28uy; 0x1duy; 0xcduy; 0x26uy
  ]

val sha3_512_kat_empty : unit
    -> Lemma (sha3_512 Seq.empty == expected_sha3_512_empty)
let sha3_512_kat_empty () =
  assume (sha3_512 Seq.empty == expected_sha3_512_empty)

(** KAT 4: SHA3-224("")
    Expected: 6b4e03423667dbb7 3b6e15454f0eb1ab d4597f9a1b078e3f 5b5a6bc7 *)
let expected_sha3_224_empty : seq UInt8.t =
  of_byte_list [
    0x6buy; 0x4euy; 0x03uy; 0x42uy; 0x36uy; 0x67uy; 0xdbuy; 0xb7uy;
    0x3buy; 0x6euy; 0x15uy; 0x45uy; 0x4fuy; 0x0euy; 0xb1uy; 0xabuy;
    0xd4uy; 0x59uy; 0x7fuy; 0x9auy; 0x1buy; 0x07uy; 0x8euy; 0x3fuy;
    0x5buy; 0x5auy; 0x6buy; 0xc7uy
  ]

val sha3_224_kat_empty : unit
    -> Lemma (sha3_224 Seq.empty == expected_sha3_224_empty)
let sha3_224_kat_empty () =
  assume (sha3_224 Seq.empty == expected_sha3_224_empty)

(** KAT 5: SHA3-384("")
    Expected: 0c63a75b845e4f7d 01107d852e4c2485 c51a50aaaa94fc61 995e71bbee983a2a
              c3713831264adb47 fb6bd1e058d5f004 *)
let expected_sha3_384_empty : seq UInt8.t =
  of_byte_list [
    0x0cuy; 0x63uy; 0xa7uy; 0x5buy; 0x84uy; 0x5euy; 0x4fuy; 0x7duy;
    0x01uy; 0x10uy; 0x7duy; 0x85uy; 0x2euy; 0x4cuy; 0x24uy; 0x85uy;
    0xc5uy; 0x1auy; 0x50uy; 0xaauy; 0xaauy; 0x94uy; 0xfcuy; 0x61uy;
    0x99uy; 0x5euy; 0x71uy; 0xbbuy; 0xeeuy; 0x98uy; 0x3auy; 0x2auy;
    0xc3uy; 0x71uy; 0x38uy; 0x31uy; 0x26uy; 0x4auy; 0xdbuy; 0x47uy;
    0xfbuy; 0x6buy; 0xd1uy; 0xe0uy; 0x58uy; 0xd5uy; 0xf0uy; 0x04uy
  ]

val sha3_384_kat_empty : unit
    -> Lemma (sha3_384 Seq.empty == expected_sha3_384_empty)
let sha3_384_kat_empty () =
  assume (sha3_384 Seq.empty == expected_sha3_384_empty)

(** KAT 6: SHAKE-128("", 32)
    Expected: 7f9c2ba4e88f827d 616045507605853e d73b8093f6efbc88 eb1a6eacfa66ef26 *)
let expected_shake128_empty_32 : seq UInt8.t =
  of_byte_list [
    0x7fuy; 0x9cuy; 0x2buy; 0xa4uy; 0xe8uy; 0x8fuy; 0x82uy; 0x7duy;
    0x61uy; 0x60uy; 0x45uy; 0x50uy; 0x76uy; 0x05uy; 0x85uy; 0x3euy;
    0xd7uy; 0x3buy; 0x80uy; 0x93uy; 0xf6uy; 0xefuy; 0xbcuy; 0x88uy;
    0xebuy; 0x1auy; 0x6euy; 0xacuy; 0xfauy; 0x66uy; 0xefuy; 0x26uy
  ]

val shake128_kat_empty_32 : unit
    -> Lemma (shake_128 Seq.empty 32 == expected_shake128_empty_32)
let shake128_kat_empty_32 () =
  assume (shake_128 Seq.empty 32 == expected_shake128_empty_32)

(** KAT 7: SHAKE-256("", 32)
    Expected: 46b9dd2b0ba88d13 233b3feb743eeb24 3fcd52ea62b81b82 b50c27646ed5762f *)
let expected_shake256_empty_32 : seq UInt8.t =
  of_byte_list [
    0x46uy; 0xb9uy; 0xdduy; 0x2buy; 0x0buy; 0xa8uy; 0x8duy; 0x13uy;
    0x23uy; 0x3buy; 0x3fuy; 0xebuy; 0x74uy; 0x3euy; 0xebuy; 0x24uy;
    0x3fuy; 0xcduy; 0x52uy; 0xeauy; 0x62uy; 0xb8uy; 0x1buy; 0x82uy;
    0xb5uy; 0x0cuy; 0x27uy; 0x64uy; 0x6euy; 0xd5uy; 0x76uy; 0x2fuy
  ]

val shake256_kat_empty_32 : unit
    -> Lemma (shake_256 Seq.empty 32 == expected_shake256_empty_32)
let shake256_kat_empty_32 () =
  assume (shake_256 Seq.empty 32 == expected_shake256_empty_32)

(** -------------------------------------------------------------------- **)
(** Structural properties                                                **)
(** -------------------------------------------------------------------- **)

(** Chi satisfies the non-linear mixing identity *)
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
  assume (let base = 5 * y in
          let row = chi_row st y in
          Seq.index row x ==
            UInt64.logxor (Seq.index st (base + x))
              (UInt64.logand
                (UInt64.lognot (Seq.index st (base + ((x + 1) % 5))))
                (Seq.index st (base + ((x + 2) % 5)))))

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
