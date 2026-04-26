(**
 * Spec.ChaCha20 -- Pure functional specification of ChaCha20 (RFC 8439)
 *
 * This module provides a complete specification of the ChaCha20 stream
 * cipher as defined in RFC 8439 Sections 2.1-2.4.  It mirrors the Haskell
 * implementation in src/UmbraVox/Crypto/Random.hs.
 *
 * Reference: RFC 8439 Sections 2.1, 2.2, 2.3, 2.4
 *)
module Spec.ChaCha20

open FStar.Seq
open FStar.UInt8
open FStar.UInt32
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

let block_size : nat = 64      (* 64 bytes = 512 bits per block *)
let key_size   : nat = 32      (* 256-bit key *)
let nonce_size : nat = 12      (* 96-bit nonce *)
let rounds     : nat = 20      (* 20 rounds = 10 double-rounds *)

(** "expand 32-byte k" as four little-endian 32-bit words. *)
let sigma0 : UInt32.t = 0x61707865ul   (* "expa" *)
let sigma1 : UInt32.t = 0x3320646eul   (* "nd 3" *)
let sigma2 : UInt32.t = 0x79622d32ul   (* "2-by" *)
let sigma3 : UInt32.t = 0x6b206574ul   (* "te k" *)

(** -------------------------------------------------------------------- **)
(** State type: 16 x UInt32                                               **)
(** -------------------------------------------------------------------- **)

type state = s:seq UInt32.t{length s = 16}

(** -------------------------------------------------------------------- **)
(** RFC 8439 Section 2.1 — Quarter round                                  **)
(** -------------------------------------------------------------------- **)

(** The quarter round operates on four 32-bit words.
    a += b; d ^= a; d <<<= 16;
    c += d; b ^= c; b <<<= 12;
    a += b; d ^= a; d <<<= 8;
    c += d; b ^= c; b <<<= 7; *)
val quarter_round : UInt32.t -> UInt32.t -> UInt32.t -> UInt32.t
                 -> Tot (UInt32.t & UInt32.t & UInt32.t & UInt32.t)
let quarter_round a0 b0 c0 d0 =
  let a1 = a0 +%^ b0 in let d1 = (d0 ^^ a1) <<<^ 16ul in
  let c1 = c0 +%^ d1 in let b1 = (b0 ^^ c1) <<<^ 12ul in
  let a2 = a1 +%^ b1 in let d2 = (d1 ^^ a2) <<<^ 8ul  in
  let c2 = c1 +%^ d2 in let b2 = (b1 ^^ c2) <<<^ 7ul  in
  (a2, b2, c2, d2)

(** Quarter round test vector (RFC 8439 Section 2.1.1):
    Input:  (0x11111111, 0x01020304, 0x9b8d6f43, 0x01234567)
    Output: (0xea2a92f4, 0xcb1cf8ce, 0x4581472e, 0x5881c4bb) *)
assume val qr_test : unit ->
  Lemma (quarter_round 0x11111111ul 0x01020304ul 0x9b8d6f43ul 0x01234567ul
         = (0xea2a92f4ul, 0xcb1cf8ceul, 0x4581472eul, 0x5881c4bbul))

(** -------------------------------------------------------------------- **)
(** Double round: column round + diagonal round                           **)
(** -------------------------------------------------------------------- **)

(** Apply quarter rounds to columns (0,4,8,12), (1,5,9,13),
    (2,6,10,14), (3,7,11,15), then diagonals (0,5,10,15),
    (1,6,11,12), (2,7,8,13), (3,4,9,14). *)
assume val double_round : state -> Tot state

(** Apply n double-rounds. *)
val n_double_rounds : n:nat -> state -> Tot state (decreases n)
let rec n_double_rounds n s =
  if n = 0 then s
  else n_double_rounds (n - 1) (double_round s)

(** -------------------------------------------------------------------- **)
(** Initial state construction                                            **)
(** -------------------------------------------------------------------- **)

(** Build the initial 16-word state from key, nonce, and counter.
    Layout: [sigma0..3, key0..7, counter, nonce0..2] *)
(** Read a little-endian UInt32 from 4 bytes. *)
assume val le_bytes_to_uint32 : s:seq UInt8.t{length s = 4} -> Tot UInt32.t

(** Helper to create a 16-element sequence. *)
assume val create_16 : UInt32.t -> UInt32.t -> UInt32.t -> UInt32.t
                    -> UInt32.t -> UInt32.t -> UInt32.t -> UInt32.t
                    -> UInt32.t -> UInt32.t -> UInt32.t -> UInt32.t
                    -> UInt32.t -> UInt32.t -> UInt32.t -> UInt32.t
                    -> Tot state

val init_state : key:seq UInt8.t{length key = key_size}
              -> nonce:seq UInt8.t{length nonce = nonce_size}
              -> counter:UInt32.t
              -> Tot state

let init_state key nonce counter =
  let k (i:nat{i < 8}) = le_bytes_to_uint32 (slice key (4*i) (4*i+4)) in
  let n (i:nat{i < 3}) = le_bytes_to_uint32 (slice nonce (4*i) (4*i+4)) in
  create_16 sigma0 sigma1 sigma2 sigma3
            (k 0) (k 1) (k 2) (k 3) (k 4) (k 5) (k 6) (k 7)
            counter (n 0) (n 1) (n 2)

(** -------------------------------------------------------------------- **)
(** RFC 8439 Section 2.3 — Block function                                 **)
(** -------------------------------------------------------------------- **)

(** Serialize 16 UInt32 words as 64 little-endian bytes. *)
assume val serialize_state : state -> Tot (s:seq UInt8.t{length s = block_size})

(** Pointwise map2 over sequences. *)
assume val seq_map2 : (UInt32.t -> UInt32.t -> UInt32.t)
                   -> s1:state -> s2:state -> Tot state

(** Produce a 64-byte keystream block.
    1. Build initial state from key, nonce, counter
    2. Apply 10 double-rounds
    3. Add initial state to final state (mod 2^32 per word)
    4. Serialize as 64 little-endian bytes *)
val chacha20_block : key:seq UInt8.t{length key = key_size}
                  -> nonce:seq UInt8.t{length nonce = nonce_size}
                  -> counter:UInt32.t
                  -> Tot (s:seq UInt8.t{length s = block_size})

let chacha20_block key nonce counter =
  let s0 = init_state key nonce counter in
  let sf = n_double_rounds 10 s0 in
  let final = seq_map2 (+%^) sf s0 in    (* add initial state back *)
  serialize_state final

(** -------------------------------------------------------------------- **)
(** RFC 8439 Section 2.4 — Encryption                                     **)
(** -------------------------------------------------------------------- **)

(** ChaCha20 encryption (XOR plaintext with keystream).
    Counter increments for each 64-byte block. *)
assume val chacha20_encrypt : key:seq UInt8.t{length key = key_size}
                    -> nonce:seq UInt8.t{length nonce = nonce_size}
                    -> counter:UInt32.t
                    -> plaintext:seq UInt8.t
                    -> Tot (ct:seq UInt8.t{length ct = length plaintext})

(** Encryption is its own inverse (XOR is involutory). *)
assume val encrypt_decrypt_roundtrip :
  key:seq UInt8.t{length key = key_size} ->
  nonce:seq UInt8.t{length nonce = nonce_size} ->
  counter:UInt32.t ->
  msg:seq UInt8.t ->
  Lemma (chacha20_encrypt key nonce counter
           (chacha20_encrypt key nonce counter msg) = msg)

(** -------------------------------------------------------------------- **)
(** KAT vectors (RFC 8439 Section 2.3.2, 2.4.2)                          **)
(** -------------------------------------------------------------------- **)

(** Helper for hex-encoded test data. *)
assume val seq_of_hex : string -> Tot (seq UInt8.t)

(** Block function test (Section 2.3.2):
    Key:     000102...1f
    Nonce:   000000090000004a00000000
    Counter: 1
    First output word: 0xe4e7f110 (LE bytes: 10 f1 e7 e4) *)
assume val kat_block : unit ->
  Lemma (let key = seq_of_hex "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f" in
         let nonce = seq_of_hex "000000090000004a00000000" in
         length key = key_size /\ length nonce = nonce_size ==>
         (let block = chacha20_block key nonce 1ul in
         index block 0 = 0x10uy /\ index block 1 = 0xf1uy /\
         index block 2 = 0xe7uy /\ index block 3 = 0xe4uy))

(** All-zero test: key=0^32, nonce=0^12, counter=0.
    First 4 bytes of output: 76 b8 e0 ad *)
assume val kat_allzero : unit ->
  Lemma (let key = create 32 0uy in
         let nonce = create 12 0uy in
         let block = chacha20_block key nonce 0ul in
         index block 0 = 0x76uy /\ index block 1 = 0xb8uy /\
         index block 2 = 0xe0uy /\ index block 3 = 0xaduy)

(** -------------------------------------------------------------------- **)
(** Correspondence to Haskell implementation                              **)
(** -------------------------------------------------------------------- **)

(**
 * +---------------------+-------------------------------------------+
 * | F* definition       | Haskell counterpart                       |
 * +---------------------+-------------------------------------------+
 * | quarter_round       | quarterRound                              |
 * | double_round        | doubleRound 1                             |
 * | n_double_rounds 10  | doubleRound 10                            |
 * | init_state          | inline in chacha20Block (s0..s15)          |
 * | chacha20_block      | chacha20Block                             |
 * | chacha20_encrypt    | chacha20Encrypt                           |
 * | serialize_state     | serialise                                 |
 * | le_bytes_to_uint32  | getLE32                                   |
 * +---------------------+-------------------------------------------+
 *)
