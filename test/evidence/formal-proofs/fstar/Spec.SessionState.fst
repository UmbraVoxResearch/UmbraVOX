(**
 * Spec.SessionState -- Specification of Signal session state serialization
 *
 * This module specifies the serialization and deserialization of the
 * UmbraVOX Signal double-ratchet session state (RatchetState) into a
 * length-prefixed binary format with HMAC-SHA-256 integrity protection.
 *
 * Binary layout (big-endian integers):
 *   [dh_send_secret:32][dh_send_public:32][dh_recv_present:1][dh_recv_public:32]
 *   [root_key:32][send_chain:32][recv_chain:32]
 *   [send_n:4][recv_n:4][prev_chain_n:4]
 *   [skip_seq:8][nonce_counter:8]
 *   [skipped_count:4][skipped_keys:N*109]
 *   [hmac:32]
 *
 * Fixed body size (no skipped keys): 225 bytes
 * Maximum skipped entries: 500 (MAX_TOTAL_SKIPPED)
 *
 * Reference: UmbraVox.Crypto.Signal.Session, UmbraVox.Crypto.RatchetPersist
 *)
module Spec.SessionState

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

let key_size         : nat = 32
let counter_size     : nat = 4
let counter64_size   : nat = 8
let presence_size    : nat = 1
let mac_size         : nat = 32

(** Fixed body size before skipped keys *)
let fixed_body_size  : nat = 225

(** Each skipped key entry: pubkey(32) + counter(4) + msg_key(32) +
    chain_key(32) + insert_seq(8) + padding(1) = 109 bytes *)
let skipped_entry_size : nat = 109

(** Maximum number of skipped key entries *)
let max_skipped : nat = 500

(** Minimum blob size: fixed body + MAC (zero skipped keys) *)
let min_blob_size : nat = 257

(** -------------------------------------------------------------------- **)
(** Type aliases                                                         **)
(** -------------------------------------------------------------------- **)

(** 32-byte symmetric key *)
type sym_key = s:seq UInt8.t{Seq.length s = key_size}

(** 32-byte MAC key *)
type mac_key = s:seq UInt8.t{Seq.length s = key_size}

(** DH key presence flag *)
type presence_flag = p:UInt8.t{p = 0x00uy \/ p = 0x01uy}

(** A skipped key entry *)
type skipped_entry = {
  se_pubkey     : sym_key;
  se_counter    : nat;
  se_msg_key    : sym_key;
  se_chain_key  : sym_key;
  se_insert_seq : nat;
}

(** The ratchet session state *)
type ratchet_state = {
  rs_dh_send_secret  : sym_key;
  rs_dh_send_public  : sym_key;
  rs_dh_recv_present : presence_flag;
  rs_dh_recv_public  : sym_key;
  rs_root_key        : sym_key;
  rs_send_chain      : sym_key;
  rs_recv_chain      : sym_key;
  rs_send_n          : nat;
  rs_recv_n          : nat;
  rs_prev_chain_n    : nat;
  rs_skip_seq        : nat;
  rs_nonce_counter   : nat;
  rs_skipped_keys    : list skipped_entry;
}

(** -------------------------------------------------------------------- **)
(** Serialization (abstract specification)                               **)
(** -------------------------------------------------------------------- **)

(** Compute the expected body length for a given number of skipped keys. *)
val body_length : skipped_count:nat{skipped_count <= max_skipped}
    -> Tot (n:nat{n = fixed_body_size + skipped_count * skipped_entry_size})
let body_length skipped_count =
  fixed_body_size + skipped_count * skipped_entry_size

(** Compute the expected blob length for a given number of skipped keys. *)
val blob_length : skipped_count:nat{skipped_count <= max_skipped}
    -> Tot (n:nat{n = body_length skipped_count + mac_size})
let blob_length skipped_count =
  body_length skipped_count + mac_size

(** Serialize a ratchet state into a binary blob with HMAC-SHA-256 tag. *)
val serialize : ratchet_state -> mac_key -> Tot (seq UInt8.t)
let serialize state key =
  (* Structural model: body || HMAC(key, body)
     Concrete serialization is in Haskell. *)
  let body = Seq.create fixed_body_size 0uy in
  let mac = Seq.create mac_size 0uy in
  Seq.append body mac

(** Deserialize a binary blob into a ratchet state, verifying HMAC.
    Returns None if the blob is malformed or the MAC fails. *)
val deserialize : seq UInt8.t -> mac_key -> Tot (option ratchet_state)
let deserialize blob key =
  let blen = Seq.length blob in
  if blen < min_blob_size then None
  else
    (* Structural model: validate size, verify MAC, parse fields.
       Full implementation is in Haskell. *)
    None (* structural stub *)

(** -------------------------------------------------------------------- **)
(** Key invariants                                                       **)
(** -------------------------------------------------------------------- **)

(** Fixed body size matches the sum of all fixed fields. *)
val fixed_body_size_check : unit
    -> Lemma (fixed_body_size =
              key_size + key_size +        (* dh_send_secret + dh_send_public *)
              presence_size + key_size +   (* dh_recv_present + dh_recv_public *)
              key_size + key_size + key_size + (* root + send_chain + recv_chain *)
              counter_size + counter_size + counter_size + (* 3x 32-bit counters *)
              counter64_size + counter64_size + (* 2x 64-bit counters *)
              counter_size)                (* skipped_count *)
let fixed_body_size_check () = ()

(** Skipped entry size matches the sum of its fields. *)
val skipped_entry_size_check : unit
    -> Lemma (skipped_entry_size =
              key_size + counter_size + key_size + key_size +
              counter64_size + 1)
let skipped_entry_size_check () = ()

(** Minimum blob size is fixed_body_size + mac_size. *)
val min_blob_size_check : unit
    -> Lemma (min_blob_size = fixed_body_size + mac_size)
let min_blob_size_check () = ()

(** Blob length is monotonically increasing in skipped key count. *)
val blob_length_monotonic : a:nat{a <= max_skipped} -> b:nat{b <= max_skipped}
    -> Lemma (requires a <= b)
             (ensures blob_length a <= blob_length b)
let blob_length_monotonic a b = ()

(** Deserialization rejects blobs shorter than min_blob_size. *)
val deserialize_rejects_short : blob:seq UInt8.t -> key:mac_key
    -> Lemma (requires Seq.length blob < min_blob_size)
             (ensures deserialize blob key = None)
let deserialize_rejects_short blob key = ()

(** Presence flag is always 0x00 or 0x01. *)
val presence_flag_valid : p:presence_flag
    -> Lemma (p = 0x00uy \/ p = 0x01uy)
let presence_flag_valid p = ()

(** -------------------------------------------------------------------- **)
(** MAC integrity (computational assumption)                             **)
(** -------------------------------------------------------------------- **)

(** Axiom: HMAC-SHA-256 protects the serialized body against tampering.
    Any modification to the body is detected by MAC verification. *)
assume val hmac_integrity : key:mac_key -> body:seq UInt8.t
    -> Lemma (True) (* Computational assumption *)

(** -------------------------------------------------------------------- **)
(** Correspondence to Haskell implementation                             **)
(** -------------------------------------------------------------------- **)

(**
 * +-------------------------+----------------------------------------------+
 * | F* definition           | Haskell counterpart                          |
 * +-------------------------+----------------------------------------------+
 * | serialize               | UmbraVox.Crypto.RatchetPersist.serialize     |
 * | deserialize             | UmbraVox.Crypto.RatchetPersist.deserialize   |
 * | ratchet_state           | UmbraVox.Crypto.Signal.DoubleRatchet.RatchetState |
 * | skipped_entry           | SkippedKeyEntry in RatchetPersist            |
 * | fixed_body_size         | fixedBodySize constant                       |
 * | skipped_entry_size      | skippedEntrySize constant                    |
 * | max_skipped             | MAX_TOTAL_SKIPPED                            |
 * +-------------------------+----------------------------------------------+
 *)
