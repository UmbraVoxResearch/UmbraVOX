(**
 * Spec.Dandelion -- Specification of Dandelion++ routing decision logic
 *
 * This module specifies the Dandelion++ routing decision for UmbraVOX
 * P2P message propagation.  Dandelion++ provides sender anonymity by
 * splitting propagation into:
 *
 *   1. Stem phase: message forwarded along a single relay chain
 *   2. Fluff phase: message broadcast to all peers (gossip)
 *
 * Each relay independently transitions from stem to fluff with
 * probability p (default 0.1 per hop).  Epoch rotation selects a
 * new stem peer every epoch_len seconds.
 *
 * All routing decisions use constant-time operations to prevent
 * timing leaks.
 *
 * Reference:
 *   Fanti et al., "Dandelion++: Lightweight Cryptocurrency Networking
 *   with Formal Anonymity Guarantees" (ACM SIGMETRICS 2018)
 *   UmbraVox.Network.Dandelion
 *)
module Spec.Dandelion

#set-options "--z3rlimit 300 --fuel 4 --ifuel 2"

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Constants                                                             **)
(** -------------------------------------------------------------------- **)

(** Routing modes *)
let mode_stem  : UInt8.t = 0x00uy
let mode_fluff : UInt8.t = 0x01uy

(** Route decision results *)
let decision_stem  : UInt8.t = 0x00uy
let decision_fluff : UInt8.t = 0x01uy

(** Default epoch length in seconds *)
let default_epoch_len : nat = 600

(** Maximum byte value for threshold comparison *)
let byte_max : nat = 256

(** -------------------------------------------------------------------- **)
(** Type aliases                                                         **)
(** -------------------------------------------------------------------- **)

(** Routing mode: stem or fluff *)
type routing_mode = m:UInt8.t{m = mode_stem \/ m = mode_fluff}

(** Fluff probability as a natural number threshold [0..256] *)
type fluff_threshold = n:nat{n <= byte_max}

(** -------------------------------------------------------------------- **)
(** Route decision                                                       **)
(** -------------------------------------------------------------------- **)

(** Compute the fluff threshold from a probability in [0..256] range.
    threshold = floor(fluff_prob * 256)
    A CSPRNG byte < threshold triggers stem-to-fluff transition. *)
val compute_threshold : prob_scaled:nat{prob_scaled <= byte_max} -> Tot fluff_threshold
let compute_threshold prob_scaled = prob_scaled

(** Route decision: given a CSPRNG byte, threshold, and current mode,
    determine whether to stem-forward or fluff-broadcast.

    Once in fluff mode, always remain in fluff (no return to stem).
    In stem mode, transition to fluff if csprng_byte < threshold. *)
val route_decision : csprng_byte:UInt8.t -> threshold:fluff_threshold
    -> current:routing_mode -> Tot routing_mode
let route_decision csprng_byte threshold current =
  if current = mode_fluff then mode_fluff
  else if UInt8.v csprng_byte < threshold then mode_fluff
  else mode_stem

(** -------------------------------------------------------------------- **)
(** Epoch rotation                                                       **)
(** -------------------------------------------------------------------- **)

(** Check whether the current epoch has expired.
    An epoch expires when now >= epoch_start + epoch_len,
    or when epoch_start = 0 (first epoch). *)
val epoch_expired : epoch_start:nat -> epoch_len:nat{epoch_len > 0} -> now:nat -> Tot bool
let epoch_expired epoch_start epoch_len now =
  epoch_start = 0 || now >= epoch_start + epoch_len

(** Compute new epoch start after rotation check.
    If expired, new_epoch_start = now; otherwise unchanged. *)
val new_epoch_start : epoch_start:nat -> epoch_len:nat{epoch_len > 0} -> now:nat -> Tot nat
let new_epoch_start epoch_start epoch_len now =
  if epoch_expired epoch_start epoch_len now then now
  else epoch_start

(** Compute post-rotation mode.
    On epoch expiry, reset to stem mode for new stem peer selection. *)
val post_rotation_mode : epoch_start:nat -> epoch_len:nat{epoch_len > 0}
    -> now:nat -> current:routing_mode -> Tot routing_mode
let post_rotation_mode epoch_start epoch_len now current =
  if epoch_expired epoch_start epoch_len now then mode_stem
  else current

(** -------------------------------------------------------------------- **)
(** Key invariants                                                       **)
(** -------------------------------------------------------------------- **)

(** Fluff mode is absorbing: once in fluff, always fluff (within epoch). *)
val fluff_absorbing : csprng_byte:UInt8.t -> threshold:fluff_threshold
    -> Lemma (route_decision csprng_byte threshold mode_fluff = mode_fluff)
let fluff_absorbing csprng_byte threshold = ()

(** Stem can only transition to fluff, never the reverse (within epoch). *)
val stem_monotonicity : b1:UInt8.t -> b2:UInt8.t -> t:fluff_threshold
    -> Lemma (requires route_decision b1 t mode_stem = mode_fluff)
             (ensures route_decision b2 t mode_fluff = mode_fluff)
let stem_monotonicity b1 b2 t = ()

(** First epoch always triggers rotation. *)
val first_epoch_rotates : epoch_len:nat{epoch_len > 0} -> now:nat
    -> Lemma (epoch_expired 0 epoch_len now = true)
let first_epoch_rotates epoch_len now = ()

(** Epoch rotation resets to stem mode. *)
val rotation_resets_stem : epoch_len:nat{epoch_len > 0} -> now:nat -> current:routing_mode
    -> Lemma (requires epoch_expired 0 epoch_len now)
             (ensures post_rotation_mode 0 epoch_len now current = mode_stem)
let rotation_resets_stem epoch_len now current = ()

(** Zero threshold means stem never transitions to fluff probabilistically. *)
val zero_threshold_no_fluff : b:UInt8.t
    -> Lemma (route_decision b 0 mode_stem = mode_stem)
let zero_threshold_no_fluff b = ()

(** -------------------------------------------------------------------- **)
(** Correspondence to Haskell implementation                             **)
(** -------------------------------------------------------------------- **)

(**
 * +-------------------------+--------------------------------------------+
 * | F* definition           | Haskell counterpart                        |
 * +-------------------------+--------------------------------------------+
 * | route_decision          | UmbraVox.Network.Dandelion.routeDecision   |
 * | epoch_expired           | UmbraVox.Network.Dandelion.epochExpired    |
 * | post_rotation_mode      | UmbraVox.Network.Dandelion.rotateEpoch     |
 * | mode_stem / mode_fluff  | Stem / Fluff constructors                  |
 * | default_epoch_len       | defaultEpochLen                            |
 * +-------------------------+--------------------------------------------+
 *)
