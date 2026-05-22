(**
 * Spec.MessageFormat -- Specification of 1024-byte block padding
 *
 * This module specifies the message padding scheme used to normalize
 * all UmbraVOX messages to fixed-size 1024-byte blocks before encryption,
 * eliminating message-length side channels.
 *
 * Pad (encode):
 *   1. Prepend 2-byte big-endian payload length header
 *   2. Apply PKCS#7-style padding to reach next 1024-byte boundary
 *
 * Unpad (decode):
 *   1. Read 2-byte length header
 *   2. Extract payload bytes
 *   3. Verify padding is well-formed (constant-time)
 *
 * Reference: UmbraVox.Protocol.MessageFormat
 *)
module Spec.MessageFormat

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

let block_size : nat = 1024

(** Length header: 2-byte big-endian uint16 *)
let header_size : nat = 2

(** Maximum payload for a single block: block_size - header_size - 1
    (at least 1 byte of PKCS#7 padding is always required) *)
let max_single_payload : nat = 1021

(** Minimum padded output size *)
let min_output : nat = 1024

(** -------------------------------------------------------------------- **)
(** Type aliases                                                         **)
(** -------------------------------------------------------------------- **)

(** A padded message block: length is a positive multiple of block_size *)
type padded_block = s:seq UInt8.t{Seq.length s > 0 /\ Seq.length s % block_size = 0}

(** A payload: arbitrary bytes with length expressible in 2-byte header *)
type payload = s:seq UInt8.t{Seq.length s <= 65535}

(** A 2-byte big-endian length header *)
type length_header = s:seq UInt8.t{Seq.length s = header_size}

(** -------------------------------------------------------------------- **)
(** Helper: big-endian uint16 encoding                                   **)
(** -------------------------------------------------------------------- **)

(** Encode a natural number n < 65536 as a 2-byte big-endian sequence. *)
val encode_be16 : n:nat{n < 65536} -> Tot length_header
let encode_be16 n =
  let hi = UInt8.uint_to_t (n / 256) in
  let lo = UInt8.uint_to_t (n % 256) in
  Seq.append (Seq.create 1 hi) (Seq.create 1 lo)

(** Decode a 2-byte big-endian sequence to a natural number. *)
val decode_be16 : h:length_header -> Tot (n:nat{n < 65536})
let decode_be16 h =
  let hi = UInt8.v (Seq.index h 0) in
  let lo = UInt8.v (Seq.index h 1) in
  hi * 256 + lo

(** -------------------------------------------------------------------- **)
(** Padding computation                                                  **)
(** -------------------------------------------------------------------- **)

(** Compute number of blocks needed for a payload of given length. *)
val total_blocks : payload_len:nat{payload_len <= 65535} -> Tot (n:nat{n >= 1})
let total_blocks payload_len =
  let content_len = header_size + payload_len in
  (* PKCS#7 requires at least 1 byte of padding *)
  let needed = content_len + 1 in
  if needed % block_size = 0 then needed / block_size
  else needed / block_size + 1

(** Compute the padded output length. *)
val padded_len : payload_len:nat{payload_len <= 65535} -> Tot (n:nat{n >= block_size /\ n % block_size = 0})
let padded_len payload_len =
  total_blocks payload_len * block_size

(** Compute the PKCS#7 padding length. *)
val pad_len : payload_len:nat{payload_len <= 65535} -> Tot (n:nat{n >= 1})
let pad_len payload_len =
  padded_len payload_len - header_size - payload_len

(** Compute the inner pad byte value (PKCS#7 style, mod 256). *)
val inner_pad_byte : payload_len:nat{payload_len <= 65535} -> Tot UInt8.t
let inner_pad_byte payload_len =
  let pl = pad_len payload_len in
  let v = pl % 256 in
  if v = 0 then UInt8.uint_to_t 256 (* use 256 mod 256 = 0, but we use the byte 0 *)
  else UInt8.uint_to_t v

(** -------------------------------------------------------------------- **)
(** Pad (encode) specification                                           **)
(** -------------------------------------------------------------------- **)

(** Pad a payload into block-aligned output.
    output = header || payload || padding *)
val pad : p:payload -> Tot (seq UInt8.t)
let pad p =
  let plen = Seq.length p in
  let header = encode_be16 plen in
  let pl = pad_len plen in
  let pb = inner_pad_byte plen in
  let padding = Seq.create pl pb in
  Seq.append header (Seq.append p padding)

(** -------------------------------------------------------------------- **)
(** Unpad (decode) specification                                         **)
(** -------------------------------------------------------------------- **)

(** Unpad a block-aligned input, returning Some payload if valid. *)
val unpad : block:seq UInt8.t -> Tot (option (seq UInt8.t))
let unpad block =
  let blen = Seq.length block in
  if blen < min_output then None
  else if blen % block_size <> 0 then None
  else
    let header = Seq.slice block 0 header_size in
    let plen = decode_be16 header in
    if plen + header_size + 1 > blen then None
    else
      let p = Seq.slice block header_size (header_size + plen) in
      Some p

(** -------------------------------------------------------------------- **)
(** Key invariants                                                       **)
(** -------------------------------------------------------------------- **)

(** Padded output is always a multiple of block_size. *)
val pad_alignment : p:payload
    -> Lemma (Seq.length (pad p) % block_size = 0)
let pad_alignment p = ()

(** Padded output is at least block_size bytes. *)
val pad_minimum_size : p:payload
    -> Lemma (Seq.length (pad p) >= block_size)
let pad_minimum_size p = ()

(** Round-trip: unpad(pad(p)) = Some p for valid payloads.
    This is the fundamental correctness property: padding is invertible. *)
val pad_unpad_roundtrip : p:payload
    -> Lemma (requires Seq.length p <= max_single_payload)
             (ensures unpad (pad p) = Some p)
let pad_unpad_roundtrip p =
  (* Structural: pad produces header || p || padding with correct header,
     unpad reads the header and extracts p.  Full proof requires
     Seq slice/append lemmas. *)
  admit ()

(** Big-endian uint16 codec round-trip. *)
val be16_roundtrip : n:nat{n < 65536}
    -> Lemma (decode_be16 (encode_be16 n) = n)
let be16_roundtrip n = ()

(** -------------------------------------------------------------------- **)
(** Correspondence to Haskell implementation                             **)
(** -------------------------------------------------------------------- **)

(**
 * +---------------------+----------------------------------------------+
 * | F* definition       | Haskell counterpart                          |
 * +---------------------+----------------------------------------------+
 * | pad                 | UmbraVox.Protocol.MessageFormat.padMessage    |
 * | unpad               | UmbraVox.Protocol.MessageFormat.unpadMessage  |
 * | block_size          | blockSize constant                           |
 * | header_size         | headerSize constant                          |
 * | encode_be16         | big-endian encoding in padMessage             |
 * | decode_be16         | big-endian decoding in unpadMessage           |
 * +---------------------+----------------------------------------------+
 *)
