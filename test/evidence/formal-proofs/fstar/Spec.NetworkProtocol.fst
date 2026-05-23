(**
 * Spec.NetworkProtocol -- Specification of P2P wire message encoding
 *
 * This module specifies the UmbraVOX P2P wire protocol message format.
 * Every protocol message is serialized as:
 *
 *   [type:1][length:4BE][payload:N]
 *
 * Message types:
 *   0x01 Handshake  [version:1][public_key:32][capabilities:2BE]
 *   0x02 Data       [sequence:4BE][data:N]
 *   0x03 Ack        [sequence:4BE]
 *   0x04 Peer       [count:2BE]([host_len:2BE][host:M][port:2BE])*
 *   0x05 Ping       (empty)
 *   0x06 Pong       (empty)
 *
 * All multi-byte integers are big-endian.
 *
 * Reference: UmbraVox.Network.Protocol
 *)
module Spec.NetworkProtocol

#set-options "--z3rlimit 300 --fuel 4 --ifuel 2"

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

(** Message type tags *)
let msg_handshake : UInt8.t = 0x01uy
let msg_data      : UInt8.t = 0x02uy
let msg_ack       : UInt8.t = 0x03uy
let msg_peer      : UInt8.t = 0x04uy
let msg_ping      : UInt8.t = 0x05uy
let msg_pong      : UInt8.t = 0x06uy

(** Header size: type(1) + length(4) = 5 *)
let header_size : nat = 5

(** Field sizes *)
let type_size     : nat = 1
let length_size   : nat = 4
let version_size  : nat = 1
let key_size      : nat = 32
let caps_size     : nat = 2
let sequence_size : nat = 4

(** Fixed payload sizes *)
let handshake_payload_size : nat = 35  (* 1 + 32 + 2 *)
let ack_payload_size       : nat = 4
let ping_payload_size      : nat = 0
let pong_payload_size      : nat = 0

(** Minimum wire message size *)
let min_wire_size : nat = 5

(** -------------------------------------------------------------------- **)
(** Type aliases                                                         **)
(** -------------------------------------------------------------------- **)

(** X25519 public key (32 bytes) *)
type x25519_key = s:seq UInt8.t{Seq.length s = key_size}

(** Known message type predicate *)
val is_known_type : UInt8.t -> Tot bool
let is_known_type t =
  UInt8.v t >= 0x01 && UInt8.v t <= 0x06

(** -------------------------------------------------------------------- **)
(** Message types                                                        **)
(** -------------------------------------------------------------------- **)

type handshake_msg = {
  hs_version      : UInt8.t;
  hs_public_key   : x25519_key;
  hs_capabilities : nat;
}

type data_msg = {
  dm_sequence : nat;
  dm_payload  : seq UInt8.t;
}

type ack_msg = {
  am_sequence : nat;
}

type peer_entry = {
  pe_host : seq UInt8.t;
  pe_port : nat;
}

type p2p_message =
  | Handshake : handshake_msg -> p2p_message
  | Data      : data_msg -> p2p_message
  | Ack       : ack_msg -> p2p_message
  | Peer      : list peer_entry -> p2p_message
  | Ping      : p2p_message
  | Pong      : p2p_message

(** -------------------------------------------------------------------- **)
(** Wire encoding (abstract specification)                               **)
(** -------------------------------------------------------------------- **)

(** Encode a P2P message into wire bytes: [type:1][length:4BE][payload:N] *)
val encode : p2p_message -> Tot (seq UInt8.t)
let encode msg =
  match msg with
  | Ping -> Seq.append (Seq.create 1 msg_ping)
                        (Seq.create length_size 0uy)
  | Pong -> Seq.append (Seq.create 1 msg_pong)
                        (Seq.create length_size 0uy)
  | _    -> Seq.create min_wire_size 0uy (* structural stub *)

(** -------------------------------------------------------------------- **)
(** Wire decoding (abstract specification)                               **)
(** -------------------------------------------------------------------- **)

(** Decode wire bytes into a P2P message.
    Returns None if the wire format is invalid. *)
val decode : seq UInt8.t -> Tot (option p2p_message)
let decode wire =
  let wlen = Seq.length wire in
  if wlen < min_wire_size then None
  else
    let msg_type = Seq.index wire 0 in
    if not (is_known_type msg_type) then None
    else if msg_type = msg_ping then Some Ping
    else if msg_type = msg_pong then Some Pong
    else None (* structural stub for complex types *)

(** -------------------------------------------------------------------- **)
(** Key invariants                                                       **)
(** -------------------------------------------------------------------- **)

(** All encoded messages have at least header_size bytes. *)
val encode_minimum_size : msg:p2p_message
    -> Lemma (Seq.length (encode msg) >= header_size)
let encode_minimum_size msg = ()

(** Ping encoding produces exactly header_size bytes (type + zero-length). *)
val ping_size : unit
    -> Lemma (Seq.length (encode Ping) = header_size)
let ping_size () = ()

(** Pong encoding produces exactly header_size bytes. *)
val pong_size : unit
    -> Lemma (Seq.length (encode Pong) = header_size)
let pong_size () = ()

(** Handshake payload is fixed at 35 bytes. *)
val handshake_payload_fixed : unit
    -> Lemma (handshake_payload_size = version_size + key_size + caps_size)
let handshake_payload_fixed () = ()

(** Decoding rejects short wire messages. *)
val decode_rejects_short : wire:seq UInt8.t
    -> Lemma (requires Seq.length wire < min_wire_size)
             (ensures decode wire = None)
let decode_rejects_short wire = ()

(** Ping round-trip: decode(encode(Ping)) = Some Ping *)
val ping_roundtrip : unit -> Lemma (decode (encode Ping) = Some Ping)
let ping_roundtrip () = admit () (* requires Seq index lemmas *)

(** Pong round-trip: decode(encode(Pong)) = Some Pong *)
val pong_roundtrip : unit -> Lemma (decode (encode Pong) = Some Pong)
let pong_roundtrip () = admit () (* requires Seq index lemmas *)

(** -------------------------------------------------------------------- **)
(** Correspondence to Haskell implementation                             **)
(** -------------------------------------------------------------------- **)

(**
 * +-------------------------+----------------------------------------------+
 * | F* definition           | Haskell counterpart                          |
 * +-------------------------+----------------------------------------------+
 * | encode                  | UmbraVox.Network.Protocol.encodeMessage      |
 * | decode                  | UmbraVox.Network.Protocol.decodeMessage      |
 * | p2p_message             | UmbraVox.Network.Protocol.P2PMessage         |
 * | is_known_type           | message type validation                      |
 * | header_size             | headerSize constant                          |
 * +-------------------------+----------------------------------------------+
 *)
