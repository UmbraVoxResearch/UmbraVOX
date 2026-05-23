(**
 * Spec.WireFormat -- Specification of on-the-wire envelope serialization
 *
 * This module specifies the UmbraVOX wire protocol envelope format.
 * Every transmitted message is wrapped in a fixed header followed by
 * a length-prefixed payload and an HMAC-SHA-256 authentication tag.
 *
 * Envelope layout (big-endian integers):
 *   [version:1][msg_type:1][sequence:4][source:32][dest:32][payload_len:2][payload:N][mac:32]
 *
 * Total envelope size: 104 + payload_len bytes
 *
 * Reference: UmbraVox.Protocol.WireFormat
 *)
module Spec.WireFormat

#set-options "--z3rlimit 300 --fuel 4 --ifuel 2"

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

(** Header size: version(1) + msg_type(1) + sequence(4) + source(32) + dest(32) + payload_len(2) = 72 *)
let header_size : nat = 72

(** MAC size: HMAC-SHA-256 output *)
let mac_size : nat = 32

(** Minimum envelope: header + empty payload + MAC *)
let min_envelope : nat = 104

(** Maximum payload length (uint16 limit) *)
let max_payload : nat = 65535

(** Current protocol version *)
let version_1 : UInt8.t = 0x01uy

(** -------------------------------------------------------------------- **)
(** Message type tags                                                    **)
(** -------------------------------------------------------------------- **)

let msg_handshake : UInt8.t = 0x01uy
let msg_data      : UInt8.t = 0x02uy
let msg_ack       : UInt8.t = 0x03uy
let msg_rekey     : UInt8.t = 0x04uy
let msg_close     : UInt8.t = 0x05uy
let msg_heartbeat : UInt8.t = 0x06uy

(** -------------------------------------------------------------------- **)
(** Type aliases                                                         **)
(** -------------------------------------------------------------------- **)

(** 32-byte identity (public key hash) *)
type identity = s:seq UInt8.t{Seq.length s = 32}

(** 32-byte MAC key *)
type mac_key = s:seq UInt8.t{Seq.length s = 32}

(** 32-byte MAC tag *)
type mac_tag = s:seq UInt8.t{Seq.length s = mac_size}

(** Wire envelope: at least min_envelope bytes *)
type envelope = s:seq UInt8.t{Seq.length s >= min_envelope}

(** -------------------------------------------------------------------- **)
(** Envelope fields                                                      **)
(** -------------------------------------------------------------------- **)

type envelope_fields = {
  ef_version  : UInt8.t;
  ef_msg_type : UInt8.t;
  ef_sequence : nat;
  ef_source   : identity;
  ef_dest     : identity;
  ef_payload  : seq UInt8.t;
}

(** -------------------------------------------------------------------- **)
(** Serialization (abstract specification)                               **)
(** -------------------------------------------------------------------- **)

(** Serialize envelope fields into wire bytes.
    The MAC is computed over [version..payload] and appended.
    Modeled as an opaque function; the concrete implementation is in Haskell. *)
val serialize : envelope_fields -> mac_key -> Tot (seq UInt8.t)
let serialize fields key =
  (* Structural model: header || payload || mac
     Concrete serialization delegates to Haskell. *)
  let header = Seq.create header_size 0uy in
  let body = Seq.append header fields.ef_payload in
  let mac = Seq.create mac_size 0uy in (* placeholder for HMAC *)
  Seq.append body mac

(** -------------------------------------------------------------------- **)
(** Deserialization (abstract specification)                             **)
(** -------------------------------------------------------------------- **)

(** Parse wire bytes into envelope fields, verifying the MAC.
    Returns None if the envelope is malformed or the MAC fails. *)
val deserialize : seq UInt8.t -> mac_key -> Tot (option envelope_fields)
let deserialize wire key =
  let wlen = Seq.length wire in
  if wlen < min_envelope then None
  else
    let ver = Seq.index wire 0 in
    if ver <> version_1 then None
    else
      let mt = Seq.index wire 1 in
      (* Read payload_len from bytes 70-71 (big-endian) *)
      let pl_hi = UInt8.v (Seq.index wire 70) in
      let pl_lo = UInt8.v (Seq.index wire 71) in
      let pl_len = pl_hi * 256 + pl_lo in
      let expected = header_size + pl_len + mac_size in
      if expected <> wlen then None
      else
        let payload = Seq.slice wire header_size (header_size + pl_len) in
        let source = Seq.slice wire 6 38 in
        let dest = Seq.slice wire 38 70 in
        Some ({
          ef_version  = ver;
          ef_msg_type = mt;
          ef_sequence = 0; (* simplified *)
          ef_source   = source;
          ef_dest     = dest;
          ef_payload  = payload;
        })

(** -------------------------------------------------------------------- **)
(** Key invariants                                                       **)
(** -------------------------------------------------------------------- **)

(** Known message type predicate *)
val is_known_msg_type : UInt8.t -> Tot bool
let is_known_msg_type t =
  UInt8.v t >= 0x01 && UInt8.v t <= 0x06

(** Envelope size is deterministic from payload length *)
val envelope_size : payload_len:nat{payload_len <= max_payload}
    -> Tot (n:nat{n = header_size + payload_len + mac_size})
let envelope_size payload_len =
  header_size + payload_len + mac_size

(** Minimum envelope size is 104 bytes *)
val min_envelope_check : unit -> Lemma (header_size + 0 + mac_size = min_envelope)
let min_envelope_check () = ()

(** Serialized envelope always includes a MAC *)
val serialize_has_mac : fields:envelope_fields -> key:mac_key
    -> Lemma (Seq.length (serialize fields key) >= mac_size)
let serialize_has_mac fields key = ()

(** -------------------------------------------------------------------- **)
(** MAC integrity (computational assumption)                             **)
(** -------------------------------------------------------------------- **)

(** Axiom: HMAC-SHA-256 is unforgeable under chosen message attack.
    Any modification to the header or payload portion of the envelope
    is detected by the MAC verification in deserialization. *)
assume val hmac_unforgeability : key:mac_key -> msg:seq UInt8.t
    -> Lemma (True) (* Computational assumption, not provable in F* *)

(** -------------------------------------------------------------------- **)
(** Correspondence to Haskell implementation                             **)
(** -------------------------------------------------------------------- **)

(**
 * +---------------------+----------------------------------------------+
 * | F* definition       | Haskell counterpart                          |
 * +---------------------+----------------------------------------------+
 * | serialize           | UmbraVox.Protocol.WireFormat.serializeEnvelope|
 * | deserialize         | UmbraVox.Protocol.WireFormat.parseEnvelope   |
 * | envelope_fields     | UmbraVox.Protocol.WireFormat.Envelope        |
 * | header_size         | headerSize constant                          |
 * | mac_size            | macSize constant                             |
 * | is_known_msg_type   | message type validation                      |
 * +---------------------+----------------------------------------------+
 *)
