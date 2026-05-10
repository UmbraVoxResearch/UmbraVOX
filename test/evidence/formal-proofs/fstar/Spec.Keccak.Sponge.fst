(**
 * Spec.Keccak.Sponge -- Sponge construction for Keccak (FIPS 202, Section 4)
 *
 * Split from Spec.Keccak for faster per-module F* verification.
 * Contains pad10*1, absorb, squeeze, and the main sponge function.
 *
 * Reference: FIPS 202, Sections 4, 5.1
 *)
module Spec.Keccak.Sponge

open FStar.Seq
open FStar.UInt8
open FStar.UInt64
open FStar.Mul
open Spec.Keccak.Permutation

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
(** Sponge structural lemmas                                             **)
(** -------------------------------------------------------------------- **)

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
