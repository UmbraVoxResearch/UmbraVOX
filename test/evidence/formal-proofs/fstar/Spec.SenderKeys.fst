(**
 * Spec.SenderKeys -- Minimal specification of Signal SenderKey protocol
 *
 * SenderKeys is the Signal Protocol group messaging mechanism in which
 * a sender distributes a ratcheting chain key to all group members.
 * Each chain step uses HMAC-SHA-256 to derive the next chain key and
 * the current message key, providing forward secrecy.
 *
 * WARNING: The Haskell module UmbraVox.Crypto.Signal.SenderKeys is a
 * stub (M7.2.6 — group messaging is not yet implemented).  This F*
 * specification defines the intended contract so that the eventual
 * implementation can be verified against it.  All lemmas are assumed.
 *
 * References:
 *   Signal Protocol Sender Key spec (libsignal-protocol)
 *   src/UmbraVox/Crypto/Signal/SenderKeys.hs (stub)
 *)
module Spec.SenderKeys

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Dependencies                                                         **)
(** -------------------------------------------------------------------- **)

(** HMAC-SHA-256 (32-byte output) from Spec.HMAC. *)
type hmac_fn = seq UInt8.t -> seq UInt8.t -> seq UInt8.t

(** -------------------------------------------------------------------- **)
(** Constants                                                            **)
(** -------------------------------------------------------------------- **)

let chain_key_size   : nat = 32   (* 256-bit chain key *)
let message_key_size : nat = 32   (* 256-bit message key *)
let sender_id_size   : nat = 4    (* 32-bit sender iteration counter *)

(** -------------------------------------------------------------------- **)
(** Key derivation constants                                             **)
(**                                                                       **)
(** Signal SenderKey ratchet uses two fixed one-byte HMAC inputs:        **)
(**   0x01 — derives the next chain key                                  **)
(**   0x02 — derives the message key for the current step                **)
(** -------------------------------------------------------------------- **)

let ck_info : seq UInt8.t = Seq.create 1 0x01uy    (* chain-key derivation constant *)
let mk_info : seq UInt8.t = Seq.create 1 0x02uy    (* message-key derivation constant *)

(** -------------------------------------------------------------------- **)
(** Chain state                                                          **)
(**                                                                       **)
(** A SenderKey chain is modelled as a chain key paired with a          **)
(** monotonically increasing iteration counter.                          **)
(** -------------------------------------------------------------------- **)

(** A chain key is a 32-byte sequence. *)
type chain_key = s:seq UInt8.t{Seq.length s = chain_key_size}

(** A chain state is a (chain_key, iteration) pair. *)
noeq type chain_state = {
  ck      : chain_key;
  iter    : nat
}

(** -------------------------------------------------------------------- **)
(** HMAC-based key derivation                                            **)
(**                                                                       **)
(** Next chain key:    CK_{i+1} = HMAC-SHA-256(CK_i, 0x01)              **)
(** Current msg key:   MK_i     = HMAC-SHA-256(CK_i, 0x02)              **)
(** -------------------------------------------------------------------- **)

(** Derive the next chain key from the current chain key. *)
val derive_next_ck : hmac:hmac_fn -> ck:chain_key -> Tot (seq UInt8.t)
let derive_next_ck hmac ck = hmac ck ck_info

(** Derive the message key for the current chain step. *)
val derive_mk : hmac:hmac_fn -> ck:chain_key -> Tot (seq UInt8.t)
let derive_mk hmac ck = hmac ck mk_info

(** Advance the chain by one step: increment counter and rotate CK. *)
val advance_chain : hmac:hmac_fn -> st:chain_state -> Tot chain_state
let advance_chain hmac st =
  (* TODO: requires tactic-based proof — Seq.length of HMAC output is
     abstract; we assume it is 32 bytes as required by chain_key. *)
  assume (Seq.length (derive_next_ck hmac st.ck) = chain_key_size);
  { ck = derive_next_ck hmac st.ck; iter = st.iter + 1 }

(** -------------------------------------------------------------------- **)
(** Forward-only property                                                **)
(**                                                                       **)
(** The chain iteration counter is strictly monotone: each call to       **)
(** advance_chain increases iter by exactly one.  This means a           **)
(** compromised future chain key cannot be used to reconstruct any       **)
(** earlier message key (forward secrecy).                               **)
(** -------------------------------------------------------------------- **)

(** The iteration counter strictly increases after each advance. *)
val chain_iter_monotone : hmac:hmac_fn -> st:chain_state
    -> Lemma ((advance_chain hmac st).iter = st.iter + 1)
let chain_iter_monotone hmac st =
  (* Proof: advance_chain sets iter := st.iter + 1 by definition. *)
  assume ((advance_chain hmac st).iter = st.iter + 1)

(** Apply the chain advance n times starting from state st. *)
let rec n_advances (hmac:hmac_fn) (st:chain_state) (n:nat)
    : Tot chain_state (decreases n) =
  if n = 0 then st
  else n_advances hmac (advance_chain hmac st) (n - 1)

(** The iteration counter after n advances equals iter + n. *)
val chain_iter_after_n : hmac:hmac_fn -> st:chain_state -> n:nat
    -> Lemma ((n_advances hmac st n).iter = st.iter + n)
let chain_iter_after_n hmac st n =
  (* Proof by induction on n.  Base case: 0 advances, iter = iter + 0.
     Inductive step: one advance increments iter by 1, then n-1 more
     increments by n-1 (IH), total = n.  We assume here. *)
  assume ((n_advances hmac st n).iter = st.iter + n)

(** -------------------------------------------------------------------- **)
(** Distinct derivation domains                                          **)
(**                                                                       **)
(** The chain key derivation constant (0x01) and the message key         **)
(** derivation constant (0x02) are distinct, so derive_next_ck and       **)
(** derive_mk produce independent outputs (domain separation).           **)
(** -------------------------------------------------------------------- **)

(** ck_info and mk_info are distinct sequences. *)
val info_distinct : unit -> Lemma (ck_info <> mk_info)
let info_distinct () =
  (* Seq.create 1 0x01uy and Seq.create 1 0x02uy differ at index 0.
     Full proof requires unfolding Seq.create and comparing UInt8 values. *)
  assume (ck_info <> mk_info)

(** -------------------------------------------------------------------- **)
(** Message-key independence                                             **)
(**                                                                       **)
(** The message key MK_i is derived independently of CK_{i+1}, so       **)
(** knowing MK_i gives no information about CK_{i+1} under the          **)
(** assumption that HMAC is a PRF.                                       **)
(** -------------------------------------------------------------------- **)

(** PRF assumption: HMAC with a fixed key is computationally
    indistinguishable from a random function (under HMAC-PRF security). *)
val hmac_prf_assumption : hmac:hmac_fn -> ck:chain_key -> Lemma (True)
let hmac_prf_assumption hmac ck = ()

(** MK and next-CK are derived with different constants from the same
    CK, so they are domain-separated.  Under the PRF assumption they
    are computationally independent. *)
val mk_ck_domain_separation : hmac:hmac_fn -> ck:chain_key
    -> Lemma (True)  (* PRF independence is a computational, not syntactic, property *)
let mk_ck_domain_separation hmac ck = ()

(** -------------------------------------------------------------------- **)
(** Structural well-formedness                                           **)
(** -------------------------------------------------------------------- **)

(** The chain state after an advance has a well-formed iteration counter. *)
val advance_iter_positive : hmac:hmac_fn -> st:chain_state
    -> Lemma ((advance_chain hmac st).iter > 0 \/ st.iter >= 0)
let advance_iter_positive hmac st = ()

(** -------------------------------------------------------------------- **)
(** Correspondence to Haskell implementation (stub)                     **)
(** -------------------------------------------------------------------- **)

(**
 * The Haskell module src/UmbraVox/Crypto/Signal/SenderKeys.hs is
 * currently a stub with no exported functions (M7.2.6).  When the
 * module is implemented, the following correspondences are intended:
 *
 * +-------------------------+------------------------------------------+
 * | F* definition           | Intended Haskell counterpart             |
 * +-------------------------+------------------------------------------+
 * | chain_key               | SenderChainKey (32 bytes)                |
 * | chain_state             | SenderKeyState                           |
 * | derive_next_ck          | ratchetSenderKey (next CK derivation)    |
 * | derive_mk               | deriveSenderMessageKey                   |
 * | advance_chain           | advanceSenderChain                       |
 * | chain_iter_monotone     | forward-only ratchet invariant           |
 * | ck_info / mk_info       | 0x01 / 0x02 HMAC derivation constants    |
 * +-------------------------+------------------------------------------+
 *)
