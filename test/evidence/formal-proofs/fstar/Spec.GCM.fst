(**
 * Spec.GCM -- Pure functional specification of AES-256-GCM (SP 800-38D)
 *
 * Galois/Counter Mode authenticated encryption with associated data.
 * This module mirrors the Haskell implementation in
 * src/UmbraVox/Crypto/GCM.hs.
 *
 * Reference: NIST SP 800-38D -- Recommendation for Block Cipher Modes
 *            of Operation: Galois/Counter Mode (GCM) and GMAC
 *)
module Spec.GCM

#set-options "--z3rlimit 300 --fuel 4 --ifuel 2"

open FStar.Seq
open FStar.UInt8
open FStar.UInt64
open FStar.UInt32
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Parameters                                                           **)
(** -------------------------------------------------------------------- **)

let block_size : nat = 16   (* 128-bit AES block *)
let key_size   : nat = 32   (* 256-bit key *)
let nonce_size : nat = 12   (* 96-bit nonce *)
let tag_size   : nat = 16   (* 128-bit authentication tag *)

(** -------------------------------------------------------------------- **)
(** Byte helpers                                                         **)
(** -------------------------------------------------------------------- **)

let xor_bytes (a b : seq UInt8.t{Seq.length a = Seq.length b})
    : (s:seq UInt8.t{Seq.length s = Seq.length a}) =
  Seq.init (Seq.length a) (fun i ->
    UInt8.logxor (Seq.index a i) (Seq.index b i))

let uint64_to_be_bytes (w : UInt64.t) : (s:seq UInt8.t{Seq.length s = 8}) =
  let l = [
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 56ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 48ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 40ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 32ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 24ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 16ul);
    FStar.Int.Cast.uint64_to_uint8 (UInt64.shift_right w 8ul);
    FStar.Int.Cast.uint64_to_uint8 w
  ] in
  assert_norm (List.Tot.length l = 8);
  Seq.seq_of_list l

(** Pad a byte sequence to a multiple of 16 bytes with zero bytes *)
let pad_to_16 (bs : seq UInt8.t) : (s:seq UInt8.t{Seq.length s % 16 = 0}) =
  let r = Seq.length bs % 16 in
  if r = 0 then bs
  else Seq.append bs (Seq.create (16 - r) 0uy)

(** Lemma: pad_to_16 produces a sequence whose length is a multiple of 16.
    Z3 can close this from the refinement in the return type of pad_to_16. *)
val pad_to_16_length : bs:seq UInt8.t
    -> Lemma (Seq.length (pad_to_16 bs) % 16 = 0)
let pad_to_16_length bs = ()

(** Helper: appending two sequences whose lengths are both multiples of n
    yields a sequence whose length is also a multiple of n. *)
val append_mod_lemma : n:pos -> a:seq UInt8.t -> b:seq UInt8.t
    -> Lemma (requires Seq.length a % n = 0 /\ Seq.length b % n = 0)
             (ensures  (Seq.length (Seq.append a b)) % n = 0)
let append_mod_lemma n a b =
  (* Seq.length (append a b) = Seq.length a + Seq.length b by FStar.Seq.Base.
     Since both are multiples of n, their sum is also a multiple of n:
     (k1 * n + k2 * n) % n = (k1 + k2) * n % n = 0. Z3 arithmetic closes this. *)
  Seq.lemma_len_append a b

(** Helper: the GCM GHASH input has length divisible by 16.
    ghash_input = pad_to_16(aad) ++ pad_to_16(ct) ++ be64(len_a) ++ be64(len_c)
    Each of the four pieces has length % 16 = 0 (the last two together = 16).
    This lemma is used to discharge the two length-divisibility goals in
    gcm_encrypt and gcm_decrypt. *)
val ghash_input_mod16 :
    aad_padded:seq UInt8.t{Seq.length aad_padded % 16 = 0}
    -> ct_padded:seq UInt8.t{Seq.length ct_padded % 16 = 0}
    -> len_a_bytes:seq UInt8.t{Seq.length len_a_bytes = 8}
    -> len_c_bytes:seq UInt8.t{Seq.length len_c_bytes = 8}
    -> Lemma (
        Seq.length (Seq.append aad_padded
                     (Seq.append ct_padded
                       (Seq.append len_a_bytes len_c_bytes))) % 16 = 0)
let ghash_input_mod16 aad_padded ct_padded len_a_bytes len_c_bytes =
  (* len_a_bytes ++ len_c_bytes has length 8+8 = 16, which is 0 mod 16. *)
  Seq.lemma_len_append len_a_bytes len_c_bytes;
  (* ct_padded ++ (len_a_bytes ++ len_c_bytes): both parts are 0 mod 16. *)
  Seq.lemma_len_append ct_padded (Seq.append len_a_bytes len_c_bytes);
  (* aad_padded ++ (...): both parts are 0 mod 16. *)
  Seq.lemma_len_append aad_padded (Seq.append ct_padded (Seq.append len_a_bytes len_c_bytes))

(** Split a byte sequence into 16-byte blocks (last may be shorter) *)
let rec split_blocks (bs : seq UInt8.t)
    : Tot (list (seq UInt8.t)) (decreases (Seq.length bs)) =
  if Seq.length bs = 0 then []
  else if Seq.length bs <= 16 then [bs]
  else Seq.slice bs 0 16 :: split_blocks (Seq.slice bs 16 (Seq.length bs))

(** -------------------------------------------------------------------- **)
(** Counter block operations                                             **)
(** -------------------------------------------------------------------- **)

(** Increment the rightmost 32 bits of a 16-byte counter block *)
let incr32 (cb : seq UInt8.t{Seq.length cb = 16})
    : (s:seq UInt8.t{Seq.length s = 16}) =
  let prefix = Seq.slice cb 0 12 in
  let ctr_bytes = Seq.slice cb 12 16 in
  let ctr = UInt32.v (
    UInt32.logor
      (UInt32.logor
        (UInt32.shift_left (FStar.Int.Cast.uint8_to_uint32 (Seq.index ctr_bytes 0)) 24ul)
        (UInt32.shift_left (FStar.Int.Cast.uint8_to_uint32 (Seq.index ctr_bytes 1)) 16ul))
      (UInt32.logor
        (UInt32.shift_left (FStar.Int.Cast.uint8_to_uint32 (Seq.index ctr_bytes 2)) 8ul)
        (FStar.Int.Cast.uint8_to_uint32 (Seq.index ctr_bytes 3)))) in
  let new_ctr = FStar.UInt32.uint_to_t ((ctr + 1) % pow2 32) in
  let new_ctr_list = [
      FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right new_ctr 24ul);
      FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right new_ctr 16ul);
      FStar.Int.Cast.uint32_to_uint8 (UInt32.shift_right new_ctr 8ul);
      FStar.Int.Cast.uint32_to_uint8 new_ctr
    ] in
  let _ = assert_norm (List.Tot.length new_ctr_list = 4) in
  let new_ctr_bytes : (s:seq UInt8.t{Seq.length s = 4}) =
    Seq.seq_of_list new_ctr_list in
  (* Proof: Seq.length prefix = 12 (from Seq.slice of length-16 sequence at 0..12)
            Seq.length new_ctr_bytes = 4 (from seq_of_list of 4-element list)
            Seq.length (append prefix new_ctr_bytes) = 12 + 4 = 16
            by Seq.lemma_len_append.  Z3 can close this arithmetic goal
            given the length refinements in scope. *)
  Seq.append prefix new_ctr_bytes

(** Initial counter block J0 for a 96-bit nonce: nonce || 0x00000001 *)
let make_j0 (nonce : seq UInt8.t{Seq.length nonce = nonce_size})
    : (s:seq UInt8.t{Seq.length s = 16}) =
  let suffix_list = [0x00uy; 0x00uy; 0x00uy; 0x01uy] in
  let _ = assert_norm (List.Tot.length suffix_list = 4) in
  let suffix : (s:seq UInt8.t{Seq.length s = 4}) = Seq.seq_of_list suffix_list in
  (* Proof: nonce_size = 12, Seq.length nonce = 12, Seq.length suffix = 4,
            Seq.length (append nonce suffix) = 12 + 4 = 16 = block_size.
            Z3 closes the arithmetic goal from the refinements in scope. *)
  Seq.append nonce suffix

(** -------------------------------------------------------------------- **)
(** SP 800-38D Section 6.4 -- GHASH                                      **)
(**                                                                       **)
(** GHASH_H(X) = X_1 * H XOR X_2 * H^2 XOR ... (iteratively)           **)
(** Computed as: Y_0 = 0; Y_i = (Y_{i-1} XOR X_i) * H                  **)
(** Input must be a multiple of 16 bytes.                                **)
(** -------------------------------------------------------------------- **)

(** Top-level GHASH inner loop for proof access.
    Iterates from offset `off` through input, accumulating into `y`. *)
let rec ghash_loop
    (h : Spec.GaloisField.gf128)
    (input : seq UInt8.t{Seq.length input % 16 = 0})
    (off : nat{off <= Seq.length input /\ off % 16 = 0})
    (y : Spec.GaloisField.gf128)
    : Tot Spec.GaloisField.gf128 (decreases (Seq.length input - off)) =
  let n = Seq.length input in
  if off = n then y
  else
    let xi_bytes : (s:seq UInt8.t{Seq.length s = 16}) =
      Seq.slice input off (off + 16) in
    let xi = Spec.GaloisField.bs_to_gf xi_bytes in
    let y' = Spec.GaloisField.gf_mul (Spec.GaloisField.gf_xor y xi) h in
    ghash_loop h input (off + 16) y'

val ghash : h:Spec.GaloisField.gf128
    -> input:seq UInt8.t{Seq.length input % 16 = 0}
    -> Tot Spec.GaloisField.gf128
let ghash (h : Spec.GaloisField.gf128)
          (input : seq UInt8.t{Seq.length input % 16 = 0})
    : Spec.GaloisField.gf128 =
  ghash_loop h input 0 Spec.GaloisField.gf_zero

(** -------------------------------------------------------------------- **)
(** SP 800-38D Section 6.5 -- GCTR                                       **)
(**                                                                       **)
(** GCTR encrypts/decrypts by XORing plaintext blocks with               **)
(** E_K(CB_1), E_K(CB_2), ... where CB_i = incr32^i(ICB).              **)
(** -------------------------------------------------------------------- **)

(** Abstract AES-256 encryption function type *)
type aes_encrypt_fn = seq UInt8.t -> seq UInt8.t

(** aes_encrypt_fn with the length postcondition needed by GCTR.
    The encrypt function must return at least as many bytes as the block
    it is called on (in practice exactly 16 for AES-256). *)
type aes_encrypt_fn_full =
  icb:seq UInt8.t{Seq.length icb = 16} ->
  Tot (ks:seq UInt8.t{Seq.length ks = 16})

(** -------------------------------------------------------------------- **)
(** split_blocks auxiliary lemmas                                         **)
(** -------------------------------------------------------------------- **)

(** All blocks produced by split_blocks have length >= 1 and <= 16. *)
val split_blocks_nonempty :
    bs:seq UInt8.t{Seq.length bs > 0}
    -> Lemma (let blocks = split_blocks bs in
              blocks =!= [] /\
              (forall (blk : seq UInt8.t).
                List.Tot.memP blk blocks ==>
                  Seq.length blk >= 1 /\ Seq.length blk <= 16))
let rec split_blocks_nonempty_aux (bs : seq UInt8.t{Seq.length bs > 0})
    (blk : seq UInt8.t)
    : Lemma (requires List.Tot.memP blk (split_blocks bs))
            (ensures  Seq.length blk >= 1 /\ Seq.length blk <= 16)
            (decreases (Seq.length bs)) =
  if Seq.length bs <= 16 then
    (* split_blocks bs = [bs], so blk = bs via memP singleton, length is 1..16 *)
    ()
  else begin
    (* split_blocks bs = (slice bs 0 16) :: split_blocks (slice bs 16 n) *)
    let hd = Seq.slice bs 0 16 in
    let tl_seq = Seq.slice bs 16 (Seq.length bs) in
    (* memP blk (hd :: split_blocks tl_seq) means blk == hd \/ memP blk (split_blocks tl_seq).
       If blk == hd, then Seq.length blk = 16, done.
       If memP blk (split_blocks tl_seq), recurse (tl_seq has length > 0 since bs > 16). *)
    if Seq.length tl_seq > 0 then
      FStar.Classical.or_elim
        #(blk == hd)
        #(List.Tot.memP blk (split_blocks tl_seq))
        #(fun _ -> Seq.length blk >= 1 /\ Seq.length blk <= 16)
        (fun (_:squash (blk == hd)) -> ())
        (fun (_:squash (List.Tot.memP blk (split_blocks tl_seq))) ->
          split_blocks_nonempty_aux tl_seq blk)
    else
      (* tl_seq is empty, so split_blocks tl_seq = [], memP only via blk == hd *)
      ()
  end

let split_blocks_nonempty bs =
  let blocks = split_blocks bs in
  (* Non-emptiness: bs has length > 0, so split_blocks takes a non-empty branch *)
  assert (blocks =!= []);
  (* Size bounds: by the auxiliary lemma *)
  let aux (blk : seq UInt8.t) : Lemma (requires List.Tot.memP blk blocks)
                                       (ensures  Seq.length blk >= 1 /\ Seq.length blk <= 16) =
    split_blocks_nonempty_aux bs blk
  in
  FStar.Classical.forall_intro (FStar.Classical.move_requires aux)

(** Helper: fold_left with additive accumulator shift.
    fold_left (+len) base xs = base + fold_left (+len) 0 xs *)
let rec fold_left_len_shift
  (base : nat)
  (xs : list (seq UInt8.t))
  : Lemma
    (ensures  List.Tot.fold_left (fun (acc:nat) (s:seq UInt8.t) -> acc + Seq.length s) base xs
            = base + List.Tot.fold_left (fun (acc:nat) (s:seq UInt8.t) -> acc + Seq.length s) 0 xs)
    (decreases xs) =
  match xs with
  | [] -> ()
  | hd :: tl ->
    fold_left_len_shift (base + Seq.length hd) tl;
    fold_left_len_shift (Seq.length hd) tl

(** Total length of all blocks in split_blocks equals original length.
    This is the key invariant needed for gctr_length. *)
val split_blocks_total_length :
    bs:seq UInt8.t
    -> Lemma (
        List.Tot.fold_left
          (fun (acc:nat) (s:seq UInt8.t) -> acc + Seq.length s)
          0
          (split_blocks bs)
        = Seq.length bs)
let rec split_blocks_total_length bs =
  let f = fun (acc:nat) (s:seq UInt8.t) -> acc + Seq.length s in
  if Seq.length bs = 0 then ()
  else if Seq.length bs <= 16 then
    (* split_blocks bs = [bs], fold gives 0 + Seq.length bs = Seq.length bs *)
    assert (List.Tot.fold_left f 0 [bs] = f 0 bs)
  else begin
    let tl = Seq.slice bs 16 (Seq.length bs) in
    (* IH: fold over split_blocks tl = Seq.length tl *)
    split_blocks_total_length tl;
    (* fold_left f 0 (hd :: rest) = fold_left f (f 0 hd) rest = fold_left f 16 rest *)
    (* We need: fold_left f 16 (split_blocks tl) = 16 + fold_left f 0 (split_blocks tl) *)
    fold_left_len_shift 16 (split_blocks tl)
  end

(** Helper: all blocks in a list have length <= 16 *)
let all_blocks_bounded (bs : list (seq UInt8.t)) : prop =
  forall (blk : seq UInt8.t). List.Tot.memP blk bs ==> Seq.length blk <= 16

(** Process blocks for GCTR — takes a list of blocks all bounded by 16 bytes *)
let rec gctr_process (encrypt : aes_encrypt_fn_full)
    (bs : list (seq UInt8.t))
    (cb : seq UInt8.t{Seq.length cb = 16})
    : Pure (list (seq UInt8.t))
           (requires all_blocks_bounded bs)
           (ensures fun _ -> True)
           (decreases bs) =
  match bs with
  | [] -> []
  | blk :: rest ->
    let ks = encrypt cb in
    (* encrypt returns 16 bytes, blk has length <= 16 by all_blocks_bounded *)
    let enc_blk = xor_bytes blk (Seq.slice ks 0 (Seq.length blk)) in
    enc_blk :: gctr_process encrypt rest (incr32 cb)

val gctr : encrypt:aes_encrypt_fn_full
    -> icb:seq UInt8.t{Seq.length icb = 16}
    -> plaintext:seq UInt8.t
    -> Tot (seq UInt8.t)
let gctr (encrypt : aes_encrypt_fn_full)
         (icb : seq UInt8.t{Seq.length icb = 16})
         (plaintext : seq UInt8.t)
    : seq UInt8.t =
  if Seq.length plaintext = 0 then Seq.empty
  else
    let blocks = split_blocks plaintext in
    (* Prove all blocks are bounded by 16 via split_blocks_nonempty *)
    split_blocks_nonempty plaintext;
    let result_blocks = gctr_process encrypt blocks icb in
    List.Tot.fold_left (fun acc s -> Seq.append acc s) Seq.empty result_blocks

(** -------------------------------------------------------------------- **)
(** GCTR correctness sub-lemmas                                           **)
(** -------------------------------------------------------------------- **)

(** GCTR on empty plaintext returns empty ciphertext. *)
val gctr_empty : encrypt:aes_encrypt_fn_full
    -> icb:seq UInt8.t{Seq.length icb = 16}
    -> Lemma (gctr encrypt icb (Seq.empty #UInt8.t) == Seq.empty #UInt8.t)
let gctr_empty encrypt icb = ()

(** gctr_process preserves the total length of the block list.
    Each block blk is replaced by xor_bytes blk (slice ks 0 (len blk)),
    which has exactly the same length as blk. *)
let rec gctr_process_total_length
    (encrypt : aes_encrypt_fn_full)
    (bs : list (seq UInt8.t))
    (cb : seq UInt8.t{Seq.length cb = 16})
    : Lemma
      (requires all_blocks_bounded bs)
      (ensures
        List.Tot.fold_left (fun (acc:nat) (s:seq UInt8.t) -> acc + Seq.length s) 0
          (gctr_process encrypt bs cb)
        = List.Tot.fold_left (fun (acc:nat) (s:seq UInt8.t) -> acc + Seq.length s) 0 bs)
      (decreases bs) =
  match bs with
  | [] -> ()
  | blk :: rest ->
    let ks = encrypt cb in
    let enc_blk = xor_bytes blk (Seq.slice ks 0 (Seq.length blk)) in
    (* enc_blk has same length as blk *)
    assert (Seq.length enc_blk = Seq.length blk);
    gctr_process_total_length encrypt rest (incr32 cb);
    (* Now: fold 0 (enc_blk :: process rest ...) = fold (len enc_blk) (process rest ...)
             = len enc_blk + fold 0 (process rest ...)     [by shift lemma]
             = len blk + fold 0 rest                        [by IH + enc_blk same length]
             = fold (len blk) rest                          [by shift lemma]
             = fold 0 (blk :: rest)                         *)
    fold_left_len_shift (Seq.length enc_blk) (gctr_process encrypt rest (incr32 cb));
    fold_left_len_shift (Seq.length blk) rest

(** Generalized: fold_left append acc xs has length = len acc + sum_lengths xs *)
let rec fold_append_length_gen
    (acc : seq UInt8.t)
    (xs : list (seq UInt8.t))
    : Lemma
      (ensures
        Seq.length (List.Tot.fold_left (fun a s -> Seq.append a s) acc xs)
        = Seq.length acc + List.Tot.fold_left (fun (a:nat) (s:seq UInt8.t) -> a + Seq.length s) 0 xs)
      (decreases xs) =
  match xs with
  | [] -> ()
  | hd :: tl ->
    let acc' = Seq.append acc hd in
    Seq.lemma_len_append acc hd;
    fold_append_length_gen acc' tl;
    fold_left_len_shift (Seq.length hd) tl

(** fold_left Seq.append Seq.empty xs has length equal to the sum of lengths *)
let fold_append_length
    (xs : list (seq UInt8.t))
    : Lemma
      (ensures
        Seq.length (List.Tot.fold_left (fun acc s -> Seq.append acc s) Seq.empty xs)
        = List.Tot.fold_left (fun (acc:nat) (s:seq UInt8.t) -> acc + Seq.length s) 0 xs) =
  fold_append_length_gen Seq.empty xs

(** GCTR output has the same length as the input plaintext.
    This is a fundamental correctness property: GCTR is a length-preserving
    stream cipher mode. *)
#push-options "--z3rlimit 30000"
val gctr_length : encrypt:aes_encrypt_fn_full
    -> icb:seq UInt8.t{Seq.length icb = 16}
    -> plaintext:seq UInt8.t
    -> Lemma (Seq.length (gctr encrypt icb plaintext) = Seq.length plaintext)
let gctr_length encrypt icb plaintext =
  if Seq.length plaintext = 0 then ()
  else begin
    let blocks = split_blocks plaintext in
    split_blocks_nonempty plaintext;
    let result_blocks = gctr_process encrypt blocks icb in
    (* (1) fold_append gives: length of concatenated result = sum of result lengths *)
    fold_append_length result_blocks;
    (* (2) gctr_process preserves total length: sum of result lengths = sum of block lengths *)
    gctr_process_total_length encrypt blocks icb;
    (* (3) split_blocks_total_length: sum of block lengths = length plaintext *)
    split_blocks_total_length plaintext
  end
#pop-options

(** xor_bytes is involutive: xor_bytes (xor_bytes a b) b == a *)
val xor_bytes_involutive :
    a:seq UInt8.t -> b:seq UInt8.t{Seq.length b = Seq.length a}
    -> Lemma (xor_bytes (xor_bytes a b) b == a)
let xor_bytes_involutive a b =
  let ab = xor_bytes a b in
  let result = xor_bytes ab b in
  let aux (i:nat{i < Seq.length a}) : Lemma (Seq.index result i == Seq.index a i) =
    (* (a[i] XOR b[i]) XOR b[i] = a[i] by logxor associativity + self-inverse *)
    UInt.logxor_associative #8 (UInt8.v (Seq.index a i))
                               (UInt8.v (Seq.index b i))
                               (UInt8.v (Seq.index b i));
    UInt.logxor_self #8 (UInt8.v (Seq.index b i));
    UInt.logxor_lemma_1 #8 (UInt8.v (Seq.index a i))
  in
  FStar.Classical.forall_intro aux;
  Seq.lemma_eq_intro result a

(** GCTR is its own inverse: applying GCTR twice with the same key-stream
    recovers the original input.  This holds because XOR is its own inverse:
    (p XOR ks) XOR ks = p.

    Proof structure:
      (1) gctr_process is involutive at the block level (per-block XOR involution).
      (2) gctr_process preserves individual block lengths and the bounded invariant.
      (3) split_blocks of the concatenated output recovers blocks of the same
          lengths, so the second gctr call splits identically.
      (4) Concatenating the involutive result recovers the original plaintext. *)

(** gctr_process preserves individual block lengths *)
let rec gctr_process_lengths
    (encrypt : aes_encrypt_fn_full)
    (bs : list (seq UInt8.t))
    (cb : seq UInt8.t{Seq.length cb = 16})
    : Lemma
      (requires all_blocks_bounded bs)
      (ensures (
        let rs = gctr_process encrypt bs cb in
        List.Tot.length rs = List.Tot.length bs /\
        (forall (i:nat{i < List.Tot.length bs}).
          Seq.length (List.Tot.index rs i) = Seq.length (List.Tot.index bs i))))
      (decreases bs) =
  match bs with
  | [] -> ()
  | blk :: rest ->
    gctr_process_lengths encrypt rest (incr32 cb)

(** gctr_process output satisfies all_blocks_bounded *)
let rec gctr_process_bounded
    (encrypt : aes_encrypt_fn_full)
    (bs : list (seq UInt8.t))
    (cb : seq UInt8.t{Seq.length cb = 16})
    : Lemma
      (requires all_blocks_bounded bs)
      (ensures all_blocks_bounded (gctr_process encrypt bs cb))
      (decreases bs) =
  match bs with
  | [] -> ()
  | blk :: rest ->
    let ks = encrypt cb in
    let enc_blk = xor_bytes blk (Seq.slice ks 0 (Seq.length blk)) in
    assert (Seq.length enc_blk = Seq.length blk);
    assert (Seq.length enc_blk <= 16);
    gctr_process_bounded encrypt rest (incr32 cb);
    (* Need to show: all_blocks_bounded (enc_blk :: gctr_process encrypt rest (incr32 cb))
       This requires: Seq.length enc_blk <= 16 (shown) and all_blocks_bounded of tail (IH) *)
    let tl = gctr_process encrypt rest (incr32 cb) in
    assert (all_blocks_bounded tl);
    let full = enc_blk :: tl in
    assert (forall (x:seq UInt8.t). List.Tot.memP x full ==> (x == enc_blk \/ List.Tot.memP x tl));
    assert (forall (x:seq UInt8.t). List.Tot.memP x full ==> Seq.length x <= 16)

(** gctr_process is involutive: applying it twice with the same encrypt and cb
    recovers the original block list. *)
let rec gctr_process_involutive
    (encrypt : aes_encrypt_fn_full)
    (bs : list (seq UInt8.t))
    (cb : seq UInt8.t{Seq.length cb = 16})
    : Lemma
      (requires all_blocks_bounded bs)
      (ensures (
        all_blocks_bounded (gctr_process encrypt bs cb) /\
        gctr_process encrypt (gctr_process encrypt bs cb) cb == bs))
      (decreases bs) =
  match bs with
  | [] -> ()
  | blk :: rest ->
    gctr_process_bounded encrypt bs cb;
    let ks = encrypt cb in
    let ks_slice = Seq.slice ks 0 (Seq.length blk) in
    let enc_blk = xor_bytes blk ks_slice in
    (* Second pass: xor enc_blk with same keystream slice *)
    assert (Seq.length enc_blk = Seq.length blk);
    (* xor_bytes (xor_bytes blk ks_slice) ks_slice == blk *)
    xor_bytes_involutive blk ks_slice;
    (* Recursive case *)
    gctr_process_bounded encrypt rest (incr32 cb);
    gctr_process_involutive encrypt rest (incr32 cb)

(** split_blocks is deterministic: it depends only on the input length.
    Two sequences of the same length produce blocks of the same lengths. *)
let rec split_blocks_length_det
    (a b : seq UInt8.t)
    : Lemma
      (requires Seq.length a = Seq.length b)
      (ensures (
        List.Tot.length (split_blocks a) = List.Tot.length (split_blocks b) /\
        (forall (i:nat{i < List.Tot.length (split_blocks a)}).
          Seq.length (List.Tot.index (split_blocks a) i) =
          Seq.length (List.Tot.index (split_blocks b) i))))
      (decreases (Seq.length a)) =
  if Seq.length a = 0 then ()
  else if Seq.length a <= 16 then ()
  else begin
    let ta = Seq.slice a 16 (Seq.length a) in
    let tb = Seq.slice b 16 (Seq.length b) in
    split_blocks_length_det ta tb
  end

(** Concatenation of blocks: fold_left append empty *)
let concat_blocks (bs : list (seq UInt8.t)) : seq UInt8.t =
  List.Tot.fold_left (fun acc s -> Seq.append acc s) Seq.empty bs

(** Helper: fold_left append is associative with initial accumulator *)
let rec fold_append_acc
    (acc : seq UInt8.t)
    (xs : list (seq UInt8.t))
    : Lemma
      (ensures
        List.Tot.fold_left (fun a s -> Seq.append a s) acc xs ==
        Seq.append acc (List.Tot.fold_left (fun a s -> Seq.append a s) Seq.empty xs))
      (decreases xs) =
  match xs with
  | [] ->
    (* fold empty [] = empty. append acc empty == acc *)
    Seq.lemma_eq_intro (Seq.append acc Seq.empty) acc
  | hd :: tl ->
    let acc' = Seq.append acc hd in
    Seq.append_empty_l hd;
    fold_append_acc acc' tl;
    fold_append_acc hd tl;
    (* fold (append acc hd) tl == append (append acc hd) (fold empty tl) [by IH on acc']
       fold hd tl == append hd (fold empty tl) [by IH on hd]
       fold empty (hd :: tl) == fold hd tl == append hd (fold empty tl)
       fold acc (hd :: tl) == fold (append acc hd) tl
                           == append (append acc hd) (fold empty tl)
                           == append acc (append hd (fold empty tl))    [by append_assoc]
                           == append acc (fold empty (hd :: tl))         [by above] *)
    Seq.append_assoc acc hd (List.Tot.fold_left (fun a s -> Seq.append a s) Seq.empty tl)

(** split_blocks roundtrip: splitting the concatenation of a well-formed block list
    recovers the original blocks.
    Well-formed: each block has 1 <= length <= 16, and all but possibly the last
    have length exactly 16. This is exactly what split_blocks produces. *)

(** Characterize the split_blocks output structure: all blocks except possibly
    the last have length exactly 16, and all have length 1..16. *)
let rec split_blocks_structure
    (bs : seq UInt8.t)
    : Lemma
      (requires Seq.length bs > 0)
      (ensures (
        let blocks = split_blocks bs in
        blocks =!= [] /\
        (forall (i:nat{i < List.Tot.length blocks - 1}).
          Seq.length (List.Tot.index blocks i) = 16) /\
        (let last = List.Tot.index blocks (List.Tot.length blocks - 1) in
          Seq.length last >= 1 /\ Seq.length last <= 16)))
      (decreases (Seq.length bs)) =
  if Seq.length bs <= 16 then ()
  else begin
    let tl = Seq.slice bs 16 (Seq.length bs) in
    assert (Seq.length tl > 0);
    split_blocks_structure tl;
    let hd = Seq.slice bs 0 16 in
    assert (Seq.length hd = 16);
    let tl_blocks = split_blocks tl in
    let blocks = split_blocks bs in
    assert (blocks == hd :: tl_blocks)
  end

(** Key lemma: split_blocks (concat_blocks blocks) == blocks
    when blocks satisfy the split_blocks structure (all but last = 16 bytes,
    all 1..16 bytes, non-empty list).
    Proof by induction on the block list. *)
#push-options "--z3rlimit 80 --fuel 2 --ifuel 1"
let rec split_blocks_concat_roundtrip
    (blocks : list (seq UInt8.t))
    : Lemma
      (requires (
        blocks =!= [] /\
        (forall (i:nat{i < List.Tot.length blocks - 1}).
          Seq.length (List.Tot.index blocks i) = 16) /\
        (let last = List.Tot.index blocks (List.Tot.length blocks - 1) in
          Seq.length last >= 1 /\ Seq.length last <= 16)))
      (ensures split_blocks (concat_blocks blocks) == blocks)
      (decreases blocks) =
  match blocks with
  | [single] ->
    (* concat_blocks [single] = single (fold_left append empty [single] = append empty single = single) *)
    Seq.append_empty_l single;
    (* split_blocks single: since 1 <= length single <= 16, returns [single] *)
    assert (Seq.length single >= 1 /\ Seq.length single <= 16)
  | hd :: tl ->
    (* hd has length 16 (not the last block) *)
    assert (Seq.length hd = 16);
    (* concat_blocks (hd :: tl) = append hd (concat_blocks tl) *)
    fold_append_acc Seq.empty blocks;
    Seq.append_empty_l hd;
    fold_append_acc hd tl;
    (* concat_blocks (hd :: tl) == append hd (concat_blocks tl) *)
    let cat_tl = concat_blocks tl in
    let cat_all = concat_blocks blocks in
    assert (cat_all == Seq.append hd cat_tl);
    (* concat_blocks tl has length > 0 (tl is non-empty with 1..16 byte blocks) *)
    fold_append_length tl;
    (* The fold total length of tl > 0 since tl is non-empty and last block >= 1 *)
    (* split_blocks (append hd cat_tl): since length >= 17 (16 + at least 1),
       it takes first 16 bytes = hd, then recurses on rest = cat_tl *)
    assert (Seq.length cat_all = Seq.length hd + Seq.length cat_tl);
    assert (Seq.length cat_all > 16);
    (* slice 0 16 of (append hd cat_tl) == hd since length hd = 16 *)
    Seq.lemma_eq_intro (Seq.slice cat_all 0 16) hd;
    (* slice 16 len of (append hd cat_tl) == cat_tl *)
    Seq.lemma_eq_intro (Seq.slice cat_all 16 (Seq.length cat_all)) cat_tl;
    (* Recursive case: split_blocks cat_tl == tl *)
    split_blocks_concat_roundtrip tl
#pop-options

(** gctr_process output has the split_blocks structure when the input does *)
let rec gctr_process_preserves_structure
    (encrypt : aes_encrypt_fn_full)
    (bs : list (seq UInt8.t))
    (cb : seq UInt8.t{Seq.length cb = 16})
    : Lemma
      (requires (
        all_blocks_bounded bs /\
        bs =!= [] /\
        (forall (i:nat{i < List.Tot.length bs - 1}).
          Seq.length (List.Tot.index bs i) = 16) /\
        (let last = List.Tot.index bs (List.Tot.length bs - 1) in
          Seq.length last >= 1 /\ Seq.length last <= 16)))
      (ensures (
        let rs = gctr_process encrypt bs cb in
        rs =!= [] /\
        (forall (i:nat{i < List.Tot.length rs - 1}).
          Seq.length (List.Tot.index rs i) = 16) /\
        (let last = List.Tot.index rs (List.Tot.length rs - 1) in
          Seq.length last >= 1 /\ Seq.length last <= 16)))
      (decreases bs) =
  match bs with
  | [single] -> ()
  | blk :: rest ->
    gctr_process_preserves_structure encrypt rest (incr32 cb);
    gctr_process_lengths encrypt bs cb

(** Splitting and re-concatenating is the identity *)
#push-options "--z3rlimit 80 --fuel 2 --ifuel 1"
let rec split_blocks_concat_inverse (bs : seq UInt8.t)
    : Lemma
      (ensures concat_blocks (split_blocks bs) == bs)
      (decreases (Seq.length bs)) =
  if Seq.length bs = 0 then
    Seq.lemma_eq_intro (concat_blocks (split_blocks bs)) bs
  else if Seq.length bs <= 16 then begin
    (* split_blocks bs = [bs], concat_blocks [bs] = append empty bs = bs *)
    Seq.append_empty_l bs;
    Seq.lemma_eq_intro (concat_blocks [bs]) bs
  end else begin
    let hd = Seq.slice bs 0 16 in
    let tl = Seq.slice bs 16 (Seq.length bs) in
    split_blocks_concat_inverse tl;
    (* split_blocks bs = hd :: split_blocks tl *)
    (* concat_blocks (hd :: split_blocks tl) = append hd (concat_blocks (split_blocks tl))
                                              = append hd tl = bs *)
    fold_append_acc Seq.empty (hd :: split_blocks tl);
    Seq.append_empty_l hd;
    fold_append_acc hd (split_blocks tl);
    (* concat_blocks (split_blocks tl) == tl by IH *)
    assert (concat_blocks (split_blocks tl) == tl);
    (* So concat_blocks (hd :: split_blocks tl) == append hd tl *)
    (* append hd tl == bs since hd = slice 0 16, tl = slice 16 len *)
    Seq.lemma_eq_intro (Seq.append hd tl) bs
  end
#pop-options

(** Main theorem: GCTR is its own inverse *)
#push-options "--z3rlimit 100 --fuel 1 --ifuel 0"
val gctr_involutive : encrypt:aes_encrypt_fn_full
    -> icb:seq UInt8.t{Seq.length icb = 16}
    -> plaintext:seq UInt8.t
    -> Lemma (gctr encrypt icb (gctr encrypt icb plaintext) == plaintext)
let gctr_involutive encrypt icb plaintext =
  if Seq.length plaintext = 0 then begin
    (* gctr on empty = empty, second application also empty *)
    assert (gctr encrypt icb plaintext == Seq.empty);
    assert (gctr encrypt icb Seq.empty == Seq.empty)
  end else begin
    let blocks = split_blocks plaintext in
    split_blocks_nonempty plaintext;
    split_blocks_structure plaintext;
    let ct_blocks = gctr_process encrypt blocks icb in
    (* ct_blocks preserves structure *)
    gctr_process_bounded encrypt blocks icb;
    gctr_process_preserves_structure encrypt blocks icb;
    gctr_process_lengths encrypt blocks icb;
    (* ct = concat_blocks ct_blocks by definition of gctr *)
    let ct = gctr encrypt icb plaintext in
    assert (ct == concat_blocks ct_blocks);
    gctr_length encrypt icb plaintext;
    assert (Seq.length ct > 0);
    (* split_blocks ct == ct_blocks (roundtrip: split what we just concatenated) *)
    split_blocks_concat_roundtrip ct_blocks;
    assert (split_blocks ct == ct_blocks);
    (* Second pass: gctr_process on ct_blocks with same icb recovers blocks *)
    gctr_process_involutive encrypt blocks icb;
    assert (gctr_process encrypt ct_blocks icb == blocks);
    (* gctr encrypt icb ct == concat_blocks (gctr_process encrypt (split_blocks ct) icb)
                           == concat_blocks blocks == plaintext *)
    split_blocks_concat_inverse plaintext;
    assert (concat_blocks blocks == plaintext);
    (* Final: gctr encrypt icb ct == concat_blocks (gctr_process encrypt ct_blocks icb)
                                  == concat_blocks blocks == plaintext *)
    assert (gctr encrypt icb ct == concat_blocks (gctr_process encrypt (split_blocks ct) icb))
  end
#pop-options

(** -------------------------------------------------------------------- **)
(** SP 800-38D Section 7.1 -- GCM-AE (Authenticated Encryption)         **)
(** -------------------------------------------------------------------- **)

(** SP 800-38D bound: input sizes must fit in a 64-bit bit-length field.
    This is 2^61 bytes = 2 exabytes, satisfied by all practical inputs. *)
val gcm_encrypt :
    encrypt:aes_encrypt_fn_full
    -> key:seq UInt8.t{Seq.length key = key_size}
    -> nonce:seq UInt8.t{Seq.length nonce = nonce_size}
    -> aad:seq UInt8.t{Seq.length aad * 8 < pow2 64}
    -> plaintext:seq UInt8.t{Seq.length plaintext * 8 < pow2 64}
    -> Tot (seq UInt8.t & seq UInt8.t)
let gcm_encrypt (encrypt : aes_encrypt_fn_full)
                (key : seq UInt8.t{Seq.length key = key_size})
                (nonce : seq UInt8.t{Seq.length nonce = nonce_size})
                (aad : seq UInt8.t{Seq.length aad * 8 < pow2 64})
                (plaintext : seq UInt8.t{Seq.length plaintext * 8 < pow2 64})
    : (seq UInt8.t & seq UInt8.t) =
  (* Step 1: H = E_K(0^128) *)
  let zero_block : (s:seq UInt8.t{Seq.length s = 16}) = Seq.create 16 0uy in
  (* encrypt returns exactly 16 bytes by aes_encrypt_fn_full's postcondition *)
  let h_bytes : (s:seq UInt8.t{Seq.length s = 16}) = encrypt zero_block in
  let h = Spec.GaloisField.bs_to_gf h_bytes in
  (* Step 2: J0 = nonce || 0x00000001 (for 96-bit nonce) *)
  let j0 = make_j0 nonce in
  (* Step 3: C = GCTR_K(inc32(J0), P) *)
  let ct = gctr encrypt (incr32 j0) plaintext in
  (* Step 4: Compute GHASH input = pad(A) || pad(C) || len(A) || len(C) *)
  (* gctr is length-preserving: Seq.length ct = Seq.length plaintext.
     The precondition Seq.length plaintext * 8 < pow2 64 carries through. *)
  gctr_length encrypt (incr32 j0) plaintext;
  let len_a = UInt64.uint_to_t (Seq.length aad * 8) in
  let len_c = UInt64.uint_to_t (Seq.length ct * 8) in
  let ghash_input = Seq.append (pad_to_16 aad)
                      (Seq.append (pad_to_16 ct)
                        (Seq.append (uint64_to_be_bytes len_a)
                                    (uint64_to_be_bytes len_c))) in
  (* Discharge the divisibility by 16 using the ghash_input_mod16 helper.
     pad_to_16 aad and pad_to_16 ct have length % 16 = 0 by their return types.
     uint64_to_be_bytes len_a and len_c each have length = 8. *)
  ghash_input_mod16 (pad_to_16 aad) (pad_to_16 ct)
                    (uint64_to_be_bytes len_a) (uint64_to_be_bytes len_c);
  (* Step 5: S = GHASH_H(ghash_input) *)
  let s = ghash h ghash_input in
  (* Step 6: T = MSB_t(GCTR_K(J0, S)) *)
  let s_bytes = Spec.GaloisField.gf_to_bs s in
  (* encrypt returns 16 bytes by aes_encrypt_fn_full *)
  let encrypted_s : (r:seq UInt8.t{Seq.length r = 16}) = encrypt j0 in
  let tag = xor_bytes s_bytes encrypted_s in
  let tag' = Seq.slice tag 0 tag_size in
  (ct, tag')

(** -------------------------------------------------------------------- **)
(** SP 800-38D Section 7.2 -- GCM-AD (Authenticated Decryption)         **)
(** -------------------------------------------------------------------- **)

(** Constant-time byte comparison accumulator loop (top-level for proof access) *)
let rec constant_eq_go (a b : seq UInt8.t{Seq.length a = Seq.length b})
    (i : nat) (acc : UInt8.t)
    : Tot UInt8.t (decreases (Seq.length a - i)) =
  if i >= Seq.length a then acc
  else constant_eq_go a b (i + 1) (UInt8.logor acc
                     (UInt8.logxor (Seq.index a i) (Seq.index b i)))

(** Constant-time byte sequence equality *)
let constant_eq (a b : seq UInt8.t{Seq.length a = Seq.length b}) : bool =
  UInt8.v (constant_eq_go a b 0 0uy) = 0

val gcm_decrypt :
    encrypt:aes_encrypt_fn_full
    -> key:seq UInt8.t{Seq.length key = key_size}
    -> nonce:seq UInt8.t{Seq.length nonce = nonce_size}
    -> aad:seq UInt8.t{Seq.length aad * 8 < pow2 64}
    -> ct:seq UInt8.t{Seq.length ct * 8 < pow2 64}
    -> tag:seq UInt8.t{Seq.length tag = tag_size}
    -> Tot (option (seq UInt8.t))
let gcm_decrypt (encrypt : aes_encrypt_fn_full)
                (key : seq UInt8.t{Seq.length key = key_size})
                (nonce : seq UInt8.t{Seq.length nonce = nonce_size})
                (aad : seq UInt8.t{Seq.length aad * 8 < pow2 64})
                (ct : seq UInt8.t{Seq.length ct * 8 < pow2 64})
                (tag : seq UInt8.t{Seq.length tag = tag_size})
    : option (seq UInt8.t) =
  (* Recompute the tag using the same steps as gcm_encrypt *)
  let zero_block : (s:seq UInt8.t{Seq.length s = 16}) = Seq.create 16 0uy in
  let h_bytes : (s:seq UInt8.t{Seq.length s = 16}) = encrypt zero_block in
  let h = Spec.GaloisField.bs_to_gf h_bytes in
  let j0 = make_j0 nonce in
  (* aad bound from precondition; ct bound from refined input type (SP 800-38D §5.2.1.1). *)
  let len_a = UInt64.uint_to_t (Seq.length aad * 8) in
  let len_c = UInt64.uint_to_t (Seq.length ct * 8) in
  let ghash_input = Seq.append (pad_to_16 aad)
                      (Seq.append (pad_to_16 ct)
                        (Seq.append (uint64_to_be_bytes len_a)
                                    (uint64_to_be_bytes len_c))) in
  (* Discharge the divisibility by 16 using the ghash_input_mod16 helper. *)
  ghash_input_mod16 (pad_to_16 aad) (pad_to_16 ct)
                    (uint64_to_be_bytes len_a) (uint64_to_be_bytes len_c);
  let s = ghash h ghash_input in
  let s_bytes = Spec.GaloisField.gf_to_bs s in
  let encrypted_s : (r:seq UInt8.t{Seq.length r = 16}) = encrypt j0 in
  let computed_tag = xor_bytes s_bytes encrypted_s in
  (* computed_tag has length 16 = tag_size, so the slice is in bounds *)
  let computed_tag' : (r:seq UInt8.t{Seq.length r = tag_size}) =
    Seq.slice computed_tag 0 tag_size in
  (* constant_eq requires equal lengths; both sides have length tag_size = 16 *)
  if constant_eq tag computed_tag' then
    Some (gctr encrypt (incr32 j0) ct)
  else
    None

(** -------------------------------------------------------------------- **)
(** Correctness properties                                               **)
(** -------------------------------------------------------------------- **)

(** Sub-lemma: gcm_encrypt tag has exactly tag_size bytes.
    Proof:
    - xor_bytes s_bytes encrypted_s has length = Seq.length s_bytes = 16
      (s_bytes = gf_to_bs s, which has length 16 by the gf_to_bs return type;
       encrypted_s has length 16 by aes_encrypt_fn_full).
    - Seq.slice tag 0 tag_size where tag_size = 16 = Seq.length tag
      gives a sequence of length tag_size.
    Z3 can close this from the return-type refinements alone. *)
val gcm_encrypt_tag_length :
    encrypt:aes_encrypt_fn_full
    -> key:seq UInt8.t{Seq.length key = key_size}
    -> nonce:seq UInt8.t{Seq.length nonce = nonce_size}
    -> aad:seq UInt8.t
    -> pt:seq UInt8.t
    -> Lemma (requires (Seq.length pt * 8 < pow2 64 /\ Seq.length aad * 8 < pow2 64))
             (ensures (
               let (_, tag) = gcm_encrypt encrypt key nonce aad pt in
               Seq.length tag = tag_size))
let gcm_encrypt_tag_length encrypt key nonce aad pt =
  (* Unfold gcm_encrypt enough for Z3 to see the tag construction.
     tag' = Seq.slice (xor_bytes s_bytes encrypted_s) 0 tag_size
     where s_bytes has length 16 (gf_to_bs), encrypted_s has length 16 (encrypt),
     so xor_bytes returns length 16, and Seq.slice 0 16 of a 16-byte seq has length 16. *)
  let zero_block : (s:seq UInt8.t{Seq.length s = 16}) = Seq.create 16 0uy in
  let h_bytes = encrypt zero_block in
  let h = Spec.GaloisField.bs_to_gf h_bytes in
  let j0 = make_j0 nonce in
  let ct = gctr encrypt (incr32 j0) pt in
  gctr_length encrypt (incr32 j0) pt;
  let len_a = UInt64.uint_to_t (Seq.length aad * 8) in
  let len_c = UInt64.uint_to_t (Seq.length ct * 8) in
  ghash_input_mod16 (pad_to_16 aad) (pad_to_16 ct)
                    (uint64_to_be_bytes len_a) (uint64_to_be_bytes len_c);
  let ghash_input = Seq.append (pad_to_16 aad)
                      (Seq.append (pad_to_16 ct)
                        (Seq.append (uint64_to_be_bytes len_a)
                                    (uint64_to_be_bytes len_c))) in
  let s = ghash h ghash_input in
  let s_bytes = Spec.GaloisField.gf_to_bs s in
  let encrypted_s : (r:seq UInt8.t{Seq.length r = 16}) = encrypt j0 in
  let full_tag = xor_bytes s_bytes encrypted_s in
  assert (Seq.length full_tag = 16);
  assert (Seq.length (Seq.slice full_tag 0 tag_size) = tag_size)

(** Invariant: constant_eq_go a b i acc = 0 iff acc = 0 and a[i..] == b[i..] *)

(** Helper: logor acc x = 0 iff acc = 0 and x = 0.
    Z3's bitvector theory handles this directly: OR of two non-negative
    integers is zero iff both are zero. *)
private val logor_zero_iff : acc:UInt8.t -> x:UInt8.t
    -> Lemma (UInt8.v (UInt8.logor acc x) = 0 <==> (UInt8.v acc = 0 /\ UInt8.v x = 0))
let logor_zero_iff acc x =
  (* Z3 bitvector decision procedure: logor a b = 0 <==> a = 0 /\ b = 0 *)
  assert (UInt.logor #8 (UInt8.v acc) (UInt8.v x) = 0 <==>
          (UInt8.v acc = 0 /\ UInt8.v x = 0))

(** Helper: logxor a b = 0 iff a = b.
    Z3 bitvector theory: XOR is zero exactly when operands are equal. *)
private val logxor_zero_iff : a:UInt8.t -> b:UInt8.t
    -> Lemma (UInt8.v (UInt8.logxor a b) = 0 <==> a == b)
let logxor_zero_iff a b =
  assert (UInt.logxor #8 (UInt8.v a) (UInt8.v b) = 0 <==> UInt8.v a = UInt8.v b)

(** Main induction: constant_eq_go a b i acc = 0 <==> acc = 0 /\ Seq.equal (slice a i n) (slice b i n) *)
#push-options "--z3rlimit 80 --fuel 1 --ifuel 0"
private let rec constant_eq_go_spec
    (a b : seq UInt8.t{Seq.length a = Seq.length b})
    (i : nat{i <= Seq.length a})
    (acc : UInt8.t)
    : Lemma
      (ensures (UInt8.v (constant_eq_go a b i acc) = 0 <==>
                (UInt8.v acc = 0 /\
                 (forall (j:nat{j >= i /\ j < Seq.length a}).
                   Seq.index a j == Seq.index b j))))
      (decreases (Seq.length a - i)) =
  if i >= Seq.length a then ()
  else begin
    let x = UInt8.logxor (Seq.index a i) (Seq.index b i) in
    let acc' = UInt8.logor acc x in
    logor_zero_iff acc x;
    logxor_zero_iff (Seq.index a i) (Seq.index b i);
    constant_eq_go_spec a b (i + 1) acc'
  end
#pop-options

val constant_eq_correct :
    a:seq UInt8.t
    -> b:seq UInt8.t{Seq.length b = Seq.length a}
    -> Lemma ((constant_eq a b = true) <==> (a == b))
#push-options "--z3rlimit 300"
let constant_eq_correct a b =
  constant_eq_go_spec a b 0 0uy;
  (* constant_eq_go_spec gives:
     constant_eq a b = true <==> forall j in [0,len). index a j == index b j
     (==>) direction: pointwise equality + equal lengths => a == b
           via Seq.lemma_eq_intro *)
  FStar.Classical.move_requires (Seq.lemma_eq_intro a) b
  (* (<==) direction: a == b => index a j == index b j for all j (Leibniz).
     Z3 handles this trivially since == is reflexive under substitution. *)
#pop-options

(** Sub-lemma: gcm_encrypt and gcm_decrypt compute the same GHASH tag.
    When encrypt/nonce/aad/ct are fixed, both functions compute
    H = E_K(0), J0 = nonce||1, then GHASH(pad(aad)||pad(ct)||len) XOR E_K(J0).
    By functional determinism (same arguments => same result), both tags agree. *)
(** PLACEHOLDER: proves True via elaborate let-bindings that disguise the
    vacuous postcondition. The actual GCM tag equality property (decrypt
    recomputes the same GHASH tag as encrypt) is structurally obvious but
    not stated in the ensures clause. Renamed to be honest. *)
val gcm_tag_equality_placeholder :
    encrypt:aes_encrypt_fn_full
    -> nonce:seq UInt8.t{Seq.length nonce = nonce_size}
    -> aad:seq UInt8.t
    -> ct:seq UInt8.t
    -> Lemma (requires (Seq.length aad * 8 < pow2 64 /\ Seq.length ct * 8 < pow2 64))
             (ensures (
               let zero_block : (s:seq UInt8.t{Seq.length s = 16}) = Seq.create 16 0uy in
               let h = Spec.GaloisField.bs_to_gf (encrypt zero_block) in
               let j0 = make_j0 nonce in
               let len_a = UInt64.uint_to_t (Seq.length aad * 8) in
               let len_c = UInt64.uint_to_t (Seq.length ct * 8) in
               True (* PLACEHOLDER: ensures is True despite let-bindings above *)))
let gcm_tag_equality_placeholder encrypt nonce aad ct = ()

(** Encryption followed by decryption recovers the plaintext.
    Proof depends on:
      (1) gcm_encrypt_tag_length: tag has length tag_size (PROVED).
      (2) gcm_tag_equality_placeholder: decrypt recomputes the same GHASH tag (PLACEHOLDER — proves True).
      (3) constant_eq_correct: constant_eq returns true on equal tags (PROVED).
      (4) gctr_involutive: gctr(encrypt, icb, gctr(encrypt, icb, pt)) = pt (PROVED).

    The proof manually inlines the key computation steps from gcm_encrypt and
    gcm_decrypt to guide Z3 through the structural identity of the tag
    computation and the application of gctr_involutive. *)
(** Helper: the tag computed by gcm_decrypt on the ciphertext from gcm_encrypt
    equals the tag returned by gcm_encrypt. This is by structural identity:
    both compute H, J0, GHASH, and XOR with E_K(J0) using the same inputs. *)
#push-options "--z3rlimit 200 --fuel 0 --ifuel 0"
private val gcm_decrypt_recomputes_tag :
    encrypt:aes_encrypt_fn_full
    -> key:seq UInt8.t{Seq.length key = key_size}
    -> nonce:seq UInt8.t{Seq.length nonce = nonce_size}
    -> aad:seq UInt8.t{Seq.length aad * 8 < pow2 64}
    -> pt:seq UInt8.t{Seq.length pt * 8 < pow2 64}
    -> Lemma (
        let (ct, tag) = gcm_encrypt encrypt key nonce aad pt in
        Seq.length tag = tag_size /\
        Seq.length ct * 8 < pow2 64 /\
        (let zero_block : (s:seq UInt8.t{Seq.length s = 16}) = Seq.create 16 0uy in
         let h_bytes : (s:seq UInt8.t{Seq.length s = 16}) = encrypt zero_block in
         let h = Spec.GaloisField.bs_to_gf h_bytes in
         let j0 = make_j0 nonce in
         let icb = incr32 j0 in
         gctr_length encrypt icb pt;
         let len_a = UInt64.uint_to_t (Seq.length aad * 8) in
         let len_c = UInt64.uint_to_t (Seq.length ct * 8) in
         let ghash_input = Seq.append (pad_to_16 aad)
                            (Seq.append (pad_to_16 ct)
                              (Seq.append (uint64_to_be_bytes len_a)
                                          (uint64_to_be_bytes len_c))) in
         ghash_input_mod16 (pad_to_16 aad) (pad_to_16 ct)
                           (uint64_to_be_bytes len_a) (uint64_to_be_bytes len_c);
         let s = ghash h ghash_input in
         let s_bytes = Spec.GaloisField.gf_to_bs s in
         let encrypted_s : (r:seq UInt8.t{Seq.length r = 16}) = encrypt j0 in
         let computed_tag = xor_bytes s_bytes encrypted_s in
         let computed_tag' = Seq.slice computed_tag 0 tag_size in
         computed_tag' == tag))
let gcm_decrypt_recomputes_tag encrypt key nonce aad pt =
  let zero_block : (s:seq UInt8.t{Seq.length s = 16}) = Seq.create 16 0uy in
  let h_bytes = encrypt zero_block in
  let h = Spec.GaloisField.bs_to_gf h_bytes in
  let j0 = make_j0 nonce in
  let icb = incr32 j0 in
  let ct = gctr encrypt icb pt in
  gctr_length encrypt icb pt;
  let len_a = UInt64.uint_to_t (Seq.length aad * 8) in
  let len_c = UInt64.uint_to_t (Seq.length ct * 8) in
  ghash_input_mod16 (pad_to_16 aad) (pad_to_16 ct)
                    (uint64_to_be_bytes len_a) (uint64_to_be_bytes len_c);
  let ghash_input = Seq.append (pad_to_16 aad)
                      (Seq.append (pad_to_16 ct)
                        (Seq.append (uint64_to_be_bytes len_a)
                                    (uint64_to_be_bytes len_c))) in
  let s = ghash h ghash_input in
  let s_bytes = Spec.GaloisField.gf_to_bs s in
  let encrypted_s : (r:seq UInt8.t{Seq.length r = 16}) = encrypt j0 in
  let full_tag = xor_bytes s_bytes encrypted_s in
  let tag = Seq.slice full_tag 0 tag_size in
  assert (Seq.length tag = tag_size)
#pop-options

#push-options "--z3rlimit 300 --fuel 1 --ifuel 0"
val gcm_roundtrip :
    encrypt:aes_encrypt_fn_full
    -> key:seq UInt8.t{Seq.length key = key_size}
    -> nonce:seq UInt8.t{Seq.length nonce = nonce_size}
    -> aad:seq UInt8.t
    -> pt:seq UInt8.t
    -> Lemma (requires (Seq.length pt * 8 < pow2 64 /\ Seq.length aad * 8 < pow2 64))
             (ensures (
               let (ct, tag) = gcm_encrypt encrypt key nonce aad pt in
               Seq.length tag = tag_size /\
               gcm_decrypt encrypt key nonce aad ct tag == Some pt))
let gcm_roundtrip encrypt key nonce aad pt =
  (* Step 1: Establish tag length and ct length bound *)
  gcm_encrypt_tag_length encrypt key nonce aad pt;
  let zero_block : (s:seq UInt8.t{Seq.length s = 16}) = Seq.create 16 0uy in
  let h_bytes = encrypt zero_block in
  let h = Spec.GaloisField.bs_to_gf h_bytes in
  let j0 = make_j0 nonce in
  let icb = incr32 j0 in
  let ct = gctr encrypt icb pt in
  gctr_length encrypt icb pt;
  (* ct has same length as pt, so ct * 8 < pow2 64 *)
  assert (Seq.length ct = Seq.length pt);
  assert (Seq.length ct * 8 < pow2 64);
  (* Step 2: Show decrypt recomputes same tag *)
  let len_a = UInt64.uint_to_t (Seq.length aad * 8) in
  let len_c = UInt64.uint_to_t (Seq.length ct * 8) in
  ghash_input_mod16 (pad_to_16 aad) (pad_to_16 ct)
                    (uint64_to_be_bytes len_a) (uint64_to_be_bytes len_c);
  let ghash_input = Seq.append (pad_to_16 aad)
                      (Seq.append (pad_to_16 ct)
                        (Seq.append (uint64_to_be_bytes len_a)
                                    (uint64_to_be_bytes len_c))) in
  let s = ghash h ghash_input in
  let s_bytes = Spec.GaloisField.gf_to_bs s in
  let encrypted_s : (r:seq UInt8.t{Seq.length r = 16}) = encrypt j0 in
  let full_tag = xor_bytes s_bytes encrypted_s in
  let tag = Seq.slice full_tag 0 tag_size in
  (* The tag computed by decrypt is identical — same h, j0, ghash_input, s, encrypted_s *)
  let computed_tag' = Seq.slice (xor_bytes s_bytes encrypted_s) 0 tag_size in
  assert (computed_tag' == tag);
  (* Step 3: constant_eq tag tag = true *)
  constant_eq_correct tag tag;
  assert (constant_eq tag computed_tag' = true);
  (* Step 4: decrypt returns Some (gctr encrypt icb ct) = Some pt *)
  gctr_involutive encrypt icb pt;
  assert (gctr encrypt icb ct == pt)
#pop-options

(** Sub-lemma: constant_eq returns false on distinct inputs of equal length.
    Directly dual to constant_eq_correct. *)
val constant_eq_neq :
    a:seq UInt8.t
    -> b:seq UInt8.t{Seq.length b = Seq.length a}
    -> Lemma (requires a =!= b)
             (ensures not (constant_eq a b))
let constant_eq_neq a b =
  (* From constant_eq_correct: constant_eq a b = true <==> a == b.
     Contrapositive: a =!= b ==> constant_eq a b <> true.
     Since constant_eq returns bool, <> true is = false, i.e. not (constant_eq a b). *)
  constant_eq_correct a b

(** Tag tampering is detected: modifying any byte of the tag causes
    decryption to fail.
    Proof: gcm_decrypt recomputes the tag identically to gcm_encrypt (structural
    identity of the computation), then checks constant_eq. Since bad_tag =!= good_tag,
    constant_eq returns false (by constant_eq_neq), and decrypt returns None. *)
#push-options "--z3rlimit 300 --fuel 1 --ifuel 0"
val gcm_tag_integrity :
    encrypt:aes_encrypt_fn_full
    -> key:seq UInt8.t{Seq.length key = key_size}
    -> nonce:seq UInt8.t{Seq.length nonce = nonce_size}
    -> aad:seq UInt8.t
    -> pt:seq UInt8.t
    -> bad_tag:seq UInt8.t{Seq.length bad_tag = tag_size}
    -> Lemma (requires (
        Seq.length pt * 8 < pow2 64 /\ Seq.length aad * 8 < pow2 64 /\
        (let (_, good_tag) = gcm_encrypt encrypt key nonce aad pt in
         bad_tag =!= good_tag)))
      (ensures (
        let (ct, _) = gcm_encrypt encrypt key nonce aad pt in
        gcm_decrypt encrypt key nonce aad ct bad_tag == None))
let gcm_tag_integrity encrypt key nonce aad pt bad_tag =
  (* Inline the encrypt computation to expose ct and good_tag *)
  let zero_block : (s:seq UInt8.t{Seq.length s = 16}) = Seq.create 16 0uy in
  let h_bytes = encrypt zero_block in
  let h = Spec.GaloisField.bs_to_gf h_bytes in
  let j0 = make_j0 nonce in
  let icb = incr32 j0 in
  let ct = gctr encrypt icb pt in
  gctr_length encrypt icb pt;
  assert (Seq.length ct = Seq.length pt);
  assert (Seq.length ct * 8 < pow2 64);
  let len_a = UInt64.uint_to_t (Seq.length aad * 8) in
  let len_c = UInt64.uint_to_t (Seq.length ct * 8) in
  ghash_input_mod16 (pad_to_16 aad) (pad_to_16 ct)
                    (uint64_to_be_bytes len_a) (uint64_to_be_bytes len_c);
  let ghash_input = Seq.append (pad_to_16 aad)
                      (Seq.append (pad_to_16 ct)
                        (Seq.append (uint64_to_be_bytes len_a)
                                    (uint64_to_be_bytes len_c))) in
  let s = ghash h ghash_input in
  let s_bytes = Spec.GaloisField.gf_to_bs s in
  let encrypted_s : (r:seq UInt8.t{Seq.length r = 16}) = encrypt j0 in
  let full_tag = xor_bytes s_bytes encrypted_s in
  let good_tag = Seq.slice full_tag 0 tag_size in
  (* gcm_encrypt returns (ct, good_tag) *)
  assert (Seq.length good_tag = tag_size);
  (* gcm_decrypt recomputes the same tag (computed_tag' == good_tag) *)
  (* Since bad_tag =!= good_tag, constant_eq bad_tag good_tag = false *)
  constant_eq_neq bad_tag good_tag;
  assert (not (constant_eq bad_tag good_tag))
  (* gcm_decrypt branches on constant_eq bad_tag computed_tag' where
     computed_tag' == good_tag, so it returns None *)
#pop-options

(** PLACEHOLDER: proves True, not actual universal hash property.
    GHASH universality (Pr[collision] <= ceil(L/128)/2^128) requires
    probabilistic reasoning over the choice of H, not provable in F*. *)
val ghash_universal_hash_placeholder : unit -> Lemma (True)
let ghash_universal_hash_placeholder () = ()

(** GHASH linearity: GHASH_H(X XOR Y) = GHASH_H(X) XOR GHASH_H(Y)
    when X and Y have the same length.
    Proof by induction over 16-byte blocks, using:
      - gf_mul_distributive: gf_mul (gf_xor a b) h == gf_xor (gf_mul a h) (gf_mul b h)
      - gf_xor_assoc: associativity of XOR in GF(2^128)
    The inner GHASH loop computes Y_i = (Y_{i-1} XOR X_i) * H.
    With XOR inputs: Y_i^{xy} = (Y_{i-1}^{xy} XOR (X_i XOR Y_i)) * H
    By IH: Y_{i-1}^{xy} = gf_xor Y_{i-1}^x Y_{i-1}^y
    Then: (gf_xor (gf_xor Y^x Y^y) (gf_xor X_i Y_i)) * H
        = (gf_xor (gf_xor Y^x X_i) (gf_xor Y^y Y_i)) * H   [by XOR assoc/comm]
        = gf_xor ((gf_xor Y^x X_i) * H) ((gf_xor Y^y Y_i) * H)  [by distributivity]
        = gf_xor Y_i^x Y_i^y *)

(** Helper: bs_to_gf of byte-XOR equals gf_xor of bs_to_gf.
    This is the byte-level homomorphism: XORing bytes then interpreting as
    a GF(2^128) element equals interpreting individually then XORing.
    Proof relies on the fact that be_bytes_to_uint64 is a linear map over XOR:
    assembling bytes via shift/or commutes with XOR. *)
private val bs_to_gf_xor :
    a:seq UInt8.t{Seq.length a = 16}
    -> b:seq UInt8.t{Seq.length b = 16}
    -> Lemma (
        let ab = Seq.init 16 (fun i -> UInt8.logxor (Seq.index a i) (Seq.index b i)) in
        Spec.GaloisField.bs_to_gf ab ==
        Spec.GaloisField.gf_xor (Spec.GaloisField.bs_to_gf a) (Spec.GaloisField.bs_to_gf b))
#push-options "--z3rlimit 600 --fuel 0 --ifuel 0"
private let bs_to_gf_xor a b =
  (* bs_to_gf builds (be_bytes_to_uint64 s 0, be_bytes_to_uint64 s 8) from 16 bytes.
     be_bytes_to_uint64 assembles 8 bytes via shift/or.
     XOR distributes over shift/or because:
       logxor (logor (shift_left a_i k) ...) (logor (shift_left b_i k) ...)
       == logor (shift_left (logxor a_i b_i) k) ...
     Z3's bitvector solver handles this with the index facts established. *)
  let ab = Seq.init 16 (fun i -> UInt8.logxor (Seq.index a i) (Seq.index b i)) in
  assert (Seq.length ab = 16);
  assert (forall (i:nat{i < 16}). Seq.index ab i == UInt8.logxor (Seq.index a i) (Seq.index b i))
#pop-options

(** GHASH loop linearity: ghash_loop on XOR input = gf_xor of individual loops *)
#push-options "--z3rlimit 200 --fuel 1 --ifuel 0"
private let rec ghash_loop_linear
    (h : Spec.GaloisField.gf128)
    (x y : seq UInt8.t{Seq.length x % 16 = 0 /\ Seq.length y = Seq.length x})
    (off : nat{off <= Seq.length x /\ off % 16 = 0})
    (yx yy : Spec.GaloisField.gf128)
    : Lemma
      (requires (
        let xy = Seq.init (Seq.length x) (fun i -> UInt8.logxor (Seq.index x i) (Seq.index y i)) in
        Seq.length xy % 16 = 0))
      (ensures (
        let xy = Seq.init (Seq.length x) (fun i -> UInt8.logxor (Seq.index x i) (Seq.index y i)) in
        let yxy = Spec.GaloisField.gf_xor yx yy in
        ghash_loop h xy off yxy ==
          Spec.GaloisField.gf_xor (ghash_loop h x off yx) (ghash_loop h y off yy)))
      (decreases (Seq.length x - off)) =
  let n = Seq.length x in
  let xy = Seq.init n (fun i -> UInt8.logxor (Seq.index x i) (Seq.index y i)) in
  if off = n then ()
  else begin
    let xi_bytes : (s:seq UInt8.t{Seq.length s = 16}) = Seq.slice x off (off + 16) in
    let yi_bytes : (s:seq UInt8.t{Seq.length s = 16}) = Seq.slice y off (off + 16) in
    let xyi_bytes : (s:seq UInt8.t{Seq.length s = 16}) = Seq.slice xy off (off + 16) in
    let xi = Spec.GaloisField.bs_to_gf xi_bytes in
    let yi = Spec.GaloisField.bs_to_gf yi_bytes in
    let xyi = Spec.GaloisField.bs_to_gf xyi_bytes in
    (* xyi == gf_xor xi yi: byte-XOR then interpret = interpret then XOR *)
    (* Need: slice of Seq.init = init of slice indices *)
    assert (forall (i:nat{i < 16}).
      Seq.index xyi_bytes i == UInt8.logxor (Seq.index xi_bytes i) (Seq.index yi_bytes i));
    (* Establish xyi_bytes == init 16 (fun i -> logxor (index xi_bytes i) (index yi_bytes i))
       so bs_to_gf_xor can connect *)
    let ab = Seq.init 16 (fun i -> UInt8.logxor (Seq.index xi_bytes i) (Seq.index yi_bytes i)) in
    Seq.lemma_eq_intro xyi_bytes ab;
    bs_to_gf_xor xi_bytes yi_bytes;
    (* gf_xor (gf_xor yx yy) xyi == gf_xor (gf_xor yx yy) (gf_xor xi yi)
       Rearrange to: gf_xor (gf_xor yx xi) (gf_xor yy yi)
       by associativity and commutativity *)
    let yxy = Spec.GaloisField.gf_xor yx yy in
    let yx_xi = Spec.GaloisField.gf_xor yx xi in
    let yy_yi = Spec.GaloisField.gf_xor yy yi in
    Spec.GaloisField.gf_xor_assoc yx yy (Spec.GaloisField.gf_xor xi yi);
    Spec.GaloisField.gf_xor_assoc yy xi yi;
    Spec.GaloisField.gf_xor_comm yy xi;
    Spec.GaloisField.gf_xor_assoc xi yy yi;
    Spec.GaloisField.gf_xor_assoc yx xi (Spec.GaloisField.gf_xor yy yi);
    (* Now: gf_xor yxy xyi == gf_xor yx_xi yy_yi *)
    (* Apply distributivity: gf_mul (gf_xor yx_xi yy_yi) h == gf_xor (gf_mul yx_xi h) (gf_mul yy_yi h) *)
    Spec.GaloisField.gf_mul_distributive yx_xi yy_yi h;
    (* Next accumulators *)
    let next_x = Spec.GaloisField.gf_mul (Spec.GaloisField.gf_xor yx xi) h in
    let next_y = Spec.GaloisField.gf_mul (Spec.GaloisField.gf_xor yy yi) h in
    (* Recurse *)
    ghash_loop_linear h x y (off + 16) next_x next_y
  end
#pop-options

#push-options "--z3rlimit 80 --fuel 1 --ifuel 0"
val ghash_linearity :
    h:Spec.GaloisField.gf128
    -> x:seq UInt8.t{Seq.length x % 16 = 0}
    -> y:seq UInt8.t{Seq.length y % 16 = 0 /\ Seq.length y = Seq.length x}
    -> Lemma (requires True)
      (ensures (
        let xy = Seq.init (Seq.length x) (fun i ->
          UInt8.logxor (Seq.index x i) (Seq.index y i)) in
        ghash h xy == Spec.GaloisField.gf_xor (ghash h x) (ghash h y)))
let ghash_linearity h x y =
  let xy = Seq.init (Seq.length x) (fun i -> UInt8.logxor (Seq.index x i) (Seq.index y i)) in
  (* xy has length = length x, which is % 16 = 0 *)
  assert (Seq.length xy = Seq.length x);
  assert (Seq.length xy % 16 = 0);
  (* ghash starts with gf_zero.  gf_xor gf_zero gf_zero == gf_zero *)
  Spec.GaloisField.gf_xor_zero_identity Spec.GaloisField.gf_zero;
  ghash_loop_linear h x y 0 Spec.GaloisField.gf_zero Spec.GaloisField.gf_zero
#pop-options

(** -------------------------------------------------------------------- **)
(** KAT Test Vectors (from NIST SP 800-38D / McGrew-Viega)              **)
(** -------------------------------------------------------------------- **)

let of_byte_list (l : list UInt8.t) : seq UInt8.t = Seq.seq_of_list l

(** NIST GCM Test Case 14 (AES-256, 96-bit IV):
    Key:   00000000000000000000000000000000
           00000000000000000000000000000000
    IV:    000000000000000000000000
    PT:    (empty)
    AAD:   (empty)
    CT:    (empty)
    Tag:   530f8afbc74536b9a963b4f1c4cb738b *)
let kat_tc14_key : seq UInt8.t = Seq.create 32 0x00uy
let kat_tc14_nonce : seq UInt8.t = Seq.create 12 0x00uy

let kat_tc14_tag : seq UInt8.t =
  of_byte_list [
    0x53uy; 0x0fuy; 0x8auy; 0xfbuy; 0xc7uy; 0x45uy; 0x36uy; 0xb9uy;
    0xa9uy; 0x63uy; 0xb4uy; 0xf1uy; 0xc4uy; 0xcbuy; 0x73uy; 0x8buy
  ]

(** Concrete AES-256 encrypt function for KAT verification.
    Spec.AES256.aes_encrypt is a fully proved, assert_norm-reducible
    AES-256 implementation with zero admits.  Partial application with
    a concrete key yields an aes_encrypt_fn_full suitable for GCM. *)
let aes256_encrypt_tc14 : aes_encrypt_fn_full =
  Spec.AES256.aes_encrypt kat_tc14_key

(** KAT TC14: fully concrete evaluation via the F* normalizer.
    The assert_norm forces the kernel to evaluate gcm_encrypt with the
    concrete Spec.AES256.aes_encrypt on NIST SP 800-38D Test Case 14.
    High fuel is needed to traverse the 256-element S-box lists during
    each AES SubBytes application across all 14 rounds, and for the
    GHASH Galois field arithmetic. *)
#push-options "--fuel 2000 --ifuel 2000 --z3rlimit 600000"
val gcm_kat_tc14 : unit
    -> Lemma (
        let (ct, tag) = gcm_encrypt aes256_encrypt_tc14 kat_tc14_key
                                    kat_tc14_nonce Seq.empty Seq.empty in
        tag == kat_tc14_tag)
let gcm_kat_tc14 () =
  assert_norm (
    let (ct, tag) = gcm_encrypt aes256_encrypt_tc14 kat_tc14_key
                                kat_tc14_nonce Seq.empty Seq.empty in
    tag == kat_tc14_tag)
#pop-options

(** NIST GCM Test Case 16 (AES-256, 96-bit IV with data):
    Key:   feffe9928665731c6d6a8f9467308308
           feffe9928665731c6d6a8f9467308308
    IV:    cafebabefacedbaddecaf888
    PT:    d9313225f88406e5a55909c5aff5269a
           86a7a9531534f7da2e4c303d8a318a72
           1c3c0c95956809532fcf0e2449a6b525
           b16aedf5aa0de657ba637b391aafd255
    AAD:   (empty)
    CT:    522dc1f099567d07f47f37a32a84427d
           643a8cdcbfe5c0c97598a2bd2555d1aa
           8cb08e48590dbb3da7b08b1056828838
           c5f61e6393ba7a0abcc9f662898015ad
    Tag:   b094dac5d93471bdec1a502270e3cc6c *)
let kat_tc16_key : seq UInt8.t =
  of_byte_list [
    0xfeuy; 0xffuy; 0xe9uy; 0x92uy; 0x86uy; 0x65uy; 0x73uy; 0x1cuy;
    0x6duy; 0x6auy; 0x8fuy; 0x94uy; 0x67uy; 0x30uy; 0x83uy; 0x08uy;
    0xfeuy; 0xffuy; 0xe9uy; 0x92uy; 0x86uy; 0x65uy; 0x73uy; 0x1cuy;
    0x6duy; 0x6auy; 0x8fuy; 0x94uy; 0x67uy; 0x30uy; 0x83uy; 0x08uy
  ]

let kat_tc16_nonce : seq UInt8.t =
  of_byte_list [
    0xcauy; 0xfeuy; 0xbauy; 0xbeuy; 0xfauy; 0xceuy; 0xdbuy; 0xaduy;
    0xdeuy; 0xcauy; 0xf8uy; 0x88uy
  ]

let kat_tc16_plaintext : seq UInt8.t =
  of_byte_list [
    0xd9uy; 0x31uy; 0x32uy; 0x25uy; 0xf8uy; 0x84uy; 0x06uy; 0xe5uy;
    0xa5uy; 0x59uy; 0x09uy; 0xc5uy; 0xafuy; 0xf5uy; 0x26uy; 0x9auy;
    0x86uy; 0xa7uy; 0xa9uy; 0x53uy; 0x15uy; 0x34uy; 0xf7uy; 0xdauy;
    0x2euy; 0x4cuy; 0x30uy; 0x3duy; 0x8auy; 0x31uy; 0x8auy; 0x72uy;
    0x1cuy; 0x3cuy; 0x0cuy; 0x95uy; 0x95uy; 0x68uy; 0x09uy; 0x53uy;
    0x2fuy; 0xcfuy; 0x0euy; 0x24uy; 0x49uy; 0xa6uy; 0xb5uy; 0x25uy;
    0xb1uy; 0x6auy; 0xeduy; 0xf5uy; 0xaauy; 0x0duy; 0xe6uy; 0x57uy;
    0xbauy; 0x63uy; 0x7buy; 0x39uy; 0x1auy; 0xafuy; 0xd2uy; 0x55uy
  ]

let kat_tc16_ciphertext : seq UInt8.t =
  of_byte_list [
    0x52uy; 0x2duy; 0xc1uy; 0xf0uy; 0x99uy; 0x56uy; 0x7duy; 0x07uy;
    0xf4uy; 0x7fuy; 0x37uy; 0xa3uy; 0x2auy; 0x84uy; 0x42uy; 0x7duy;
    0x64uy; 0x3auy; 0x8cuy; 0xdcuy; 0xbfuy; 0xe5uy; 0xc0uy; 0xc9uy;
    0x75uy; 0x98uy; 0xa2uy; 0xbduy; 0x25uy; 0x55uy; 0xd1uy; 0xaauy;
    0x8cuy; 0xb0uy; 0x8euy; 0x48uy; 0x59uy; 0x0duy; 0xbbuy; 0x3duy;
    0xa7uy; 0xb0uy; 0x8buy; 0x10uy; 0x56uy; 0x82uy; 0x88uy; 0x38uy;
    0xc5uy; 0xf6uy; 0x1euy; 0x63uy; 0x93uy; 0xbauy; 0x7auy; 0x0auy;
    0xbcuy; 0xc9uy; 0xf6uy; 0x62uy; 0x89uy; 0x80uy; 0x15uy; 0xaduy
  ]

let kat_tc16_tag : seq UInt8.t =
  of_byte_list [
    0xb0uy; 0x94uy; 0xdauy; 0xc5uy; 0xd9uy; 0x34uy; 0x71uy; 0xbduy;
    0xecuy; 0x1auy; 0x50uy; 0x22uy; 0x70uy; 0xe3uy; 0xccuy; 0x6cuy
  ]

(** Concrete AES-256 encrypt function for TC16, partially applied with the
    TC16 key. *)
let aes256_encrypt_tc16 : aes_encrypt_fn_full =
  Spec.AES256.aes_encrypt kat_tc16_key

(** KAT TC16: fully concrete evaluation via the F* normalizer.
    Same approach as TC14 but with 64 bytes of plaintext (4 AES-block GCTR)
    and non-trivial ciphertext comparison.  Requires high fuel for the
    multiple AES invocations and GHASH over the padded ciphertext. *)
#push-options "--fuel 2000 --ifuel 2000 --z3rlimit 600000"
val gcm_kat_tc16 : unit
    -> Lemma (
        let (ct, tag) = gcm_encrypt aes256_encrypt_tc16 kat_tc16_key
                                    kat_tc16_nonce Seq.empty kat_tc16_plaintext in
        ct == kat_tc16_ciphertext /\ tag == kat_tc16_tag)
let gcm_kat_tc16 () =
  assert_norm (
    let (ct, tag) = gcm_encrypt aes256_encrypt_tc16 kat_tc16_key
                                kat_tc16_nonce Seq.empty kat_tc16_plaintext in
    ct == kat_tc16_ciphertext /\ tag == kat_tc16_tag)
#pop-options
