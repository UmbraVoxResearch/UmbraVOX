(**
 * DHTRoutingTable -- Formal specification of Kademlia routing table invariants
 *
 * SPECIFICATION ONLY: This module defines the structural invariants that
 * the Haskell routing table implementation (src/UmbraVox/Network/DHT/RoutingTable.hs)
 * must maintain.  It is not intended to be verified end-to-end but serves
 * as a machine-readable reference for the four core k-bucket invariants:
 *
 *   1. Bucket bounded: each bucket has at most k entries and k replacements.
 *   2. XOR distance ordering: nodes in bucket i have the correct XOR prefix.
 *   3. Replacement cache bounded: replacement cache has at most k entries.
 *   4. Self-exclusion: the local node ID never appears in any bucket.
 *
 * Reference: Petar Maymounkov & David Mazieres, "Kademlia: A Peer-to-peer
 * Information System Based on the XOR Metric", IPTPS 2002.
 *)
module DHTRoutingTable

open FStar.Seq
open FStar.UInt8
open FStar.Mul

(** -------------------------------------------------------------------- **)
(** Core types                                                            **)
(** -------------------------------------------------------------------- **)

(** A 256-bit node identifier (SHA-256 of identity public key). *)
type node_id = s:seq UInt8.t{length s = 32}

(** A k-bucket containing active entries and a replacement cache. *)
type bucket = {
  entries: seq node_id;
  replacements: seq node_id;
}

(** A routing table: 256 buckets indexed by XOR distance bit position,
    a self node ID, and the bucket capacity k. *)
type routing_table = {
  self_id: node_id;
  buckets: s:seq bucket{length s = 256};
  k: nat;
}

(** -------------------------------------------------------------------- **)
(** XOR distance                                                          **)
(** -------------------------------------------------------------------- **)

(** Bytewise XOR of two node IDs. *)
val xor_distance : a:node_id -> b:node_id -> Tot (s:seq UInt8.t{length s = 32})
let xor_distance a b =
  Seq.init 32 (fun i -> UInt8.logxor (index a i) (index b i))

(** Count leading zero bits in a byte. *)
val clz_byte : UInt8.t -> Tot nat
let clz_byte b =
  if UInt8.v b = 0 then 8
  else if UInt8.v b >= 128 then 0
  else if UInt8.v b >= 64 then 1
  else if UInt8.v b >= 32 then 2
  else if UInt8.v b >= 16 then 3
  else if UInt8.v b >= 8 then 4
  else if UInt8.v b >= 4 then 5
  else if UInt8.v b >= 2 then 6
  else 7

(** Count leading zero bits in a 32-byte distance value.
    Returns 256 if all bytes are zero (self == target). *)
val count_leading_zeros : d:seq UInt8.t{length d = 32} -> Tot nat
let rec count_leading_zeros d =
  (* Iterate bytes; first non-zero byte determines the count. *)
  if UInt8.v (index d 0) <> 0 then clz_byte (index d 0)
  else if length d = 1 then 8
  else 8 + count_leading_zeros (slice d 1 (length d))

(** Bucket index: 255 - leading_zero_bits(XOR(self, target)).
    Returns -1 when distance is zero (self == target). *)
val bucket_index : self:node_id -> target:node_id -> Tot int
let bucket_index self target =
  let d = xor_distance self target in
  let lz = count_leading_zeros d in
  if lz >= 256 then -1
  else 255 - lz

(** -------------------------------------------------------------------- **)
(** Invariant 1: Bucket size bounded                                      **)
(** -------------------------------------------------------------------- **)

(** Every bucket's entry list and replacement list have at most k elements. *)
val bucket_bounded : k:nat -> b:bucket -> Type0
let bucket_bounded k b =
  length b.entries <= k /\ length b.replacements <= k

(** All 256 buckets in a routing table satisfy the bound. *)
val all_buckets_bounded : rt:routing_table -> Type0
let all_buckets_bounded rt =
  forall (i:nat{i < 256}).
    bucket_bounded rt.k (index rt.buckets i)

(** -------------------------------------------------------------------- **)
(** Invariant 2: XOR distance ordering                                    **)
(** -------------------------------------------------------------------- **)

(** Every node in bucket i must have bucket_index(self, node) == i.
    This ensures nodes are placed in the correct XOR-distance bucket. *)
val xor_placement_correct : rt:routing_table -> Type0
let xor_placement_correct rt =
  forall (i:nat{i < 256}).
    let b = index rt.buckets i in
    forall (j:nat{j < length b.entries}).
      bucket_index rt.self_id (index b.entries j) = i

(** -------------------------------------------------------------------- **)
(** Invariant 3: Replacement cache bounded                                **)
(** -------------------------------------------------------------------- **)

(** The replacement cache for each bucket has at most k entries.
    (This is a component of bucket_bounded but called out explicitly
    per the Kademlia spec requirement.) *)
val replacement_cache_bounded : k:nat -> b:bucket -> Type0
let replacement_cache_bounded k b =
  length b.replacements <= k

val all_replacements_bounded : rt:routing_table -> Type0
let all_replacements_bounded rt =
  forall (i:nat{i < 256}).
    replacement_cache_bounded rt.k (index rt.buckets i)

(** -------------------------------------------------------------------- **)
(** Invariant 4: Self-exclusion                                           **)
(** -------------------------------------------------------------------- **)

(** The local node's own ID must never appear in any bucket's entry list
    or replacement list.  Inserting self is a no-op (SelfIgnored). *)
val self_excluded : self:node_id -> buckets:seq bucket -> Type0
let self_excluded self buckets =
  forall (i:nat{i < length buckets}).
    let b = index buckets i in
    (forall (j:nat{j < length b.entries}). index b.entries j <> self) /\
    (forall (j:nat{j < length b.replacements}). index b.replacements j <> self)

(** Combined self-exclusion for the routing table. *)
val rt_self_excluded : rt:routing_table -> Type0
let rt_self_excluded rt = self_excluded rt.self_id rt.buckets

(** -------------------------------------------------------------------- **)
(** Combined well-formedness                                              **)
(** -------------------------------------------------------------------- **)

(** A routing table is well-formed if all four invariants hold. *)
val well_formed : rt:routing_table -> Type0
let well_formed rt =
  all_buckets_bounded rt /\
  xor_placement_correct rt /\
  all_replacements_bounded rt /\
  rt_self_excluded rt

(** -------------------------------------------------------------------- **)
(** Empty routing table is well-formed                                    **)
(** -------------------------------------------------------------------- **)

(** An empty bucket trivially satisfies all per-bucket invariants. *)
val empty_bucket : bucket
let empty_bucket = { entries = Seq.empty; replacements = Seq.empty }

(** Lemma: empty_bucket is bounded for any k. *)
val empty_bucket_bounded_lemma : k:nat
    -> Lemma (bucket_bounded k empty_bucket)
let empty_bucket_bounded_lemma k = ()
