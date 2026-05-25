# Cryptographic Assumptions

12 properties in the UmbraVOX F* specification are **unprovable in any proof
assistant**. They are computational hardness assumptions — the security of the
entire field of modern cryptography rests on the belief that these problems are
hard. If any were false, the affected cryptographic primitive would be broken
globally, not just in UmbraVOX.

Each assumption is documented in the corresponding F* spec file with an
`(* ASSUME JUSTIFICATION: ... *)` block.

## 1. HMAC-SHA-256 PRF Security

**F* assume val:** `hmac_non_fixpoint` in `Spec.DoubleRatchet.fst`

**What it says:** HMAC-SHA-256(ck, 0x01) ≠ ck — the HMAC output is not a
fixpoint of its key.

**Why it's unprovable:** This is a consequence of HMAC being a pseudorandom
function (PRF). A PRF's output is computationally indistinguishable from
random, which means it cannot equal its input with non-negligible probability.
Proving this requires showing SHA-256's compression function is a PRF, which
reduces to showing the underlying block cipher (SHA-256's internal structure)
is secure — an open problem in complexity theory related to P ≠ NP.

**What would break if false:** The Double Ratchet's chain key derivation would
cycle, producing repeated message keys. An attacker could predict future
message keys from a single compromised chain key.

**Reference:** RFC 2104 (HMAC), Bellare 2006 "New Proofs for NMAC and HMAC"

## 2. HMAC-SHA-256 Collision Resistance

**F* assume val:** `hmac_collision_resistance` in `Spec.DoubleRatchet.fst`

**What it says:** HMAC-SHA-256(ck, 0x01) ≠ HMAC-SHA-256(ck, 0x02) — distinct
inputs produce distinct outputs.

**Why it's unprovable:** Collision resistance of hash functions is a
computational assumption. SHA-256 maps a 512-bit block to 256 bits, so
collisions must exist by the pigeonhole principle. The assumption is that
finding one is computationally infeasible. Proving this would require proving
a lower bound on the circuit complexity of inverting SHA-256 — an open problem
equivalent to P ≠ NP.

**What would break if false:** The Double Ratchet's chain key and message key
would collide, breaking the separation between sending and receiving chains.

**Reference:** FIPS 180-4 (SHA-256), Rogaway-Shrimpton 2004 "Cryptographic
Hash-Function Basics"

## 3. Poly1305 UF-CMA (Tag Forgery Resistance)

**F* assume val:** `tag_forgery_ct_axiom` in `Spec.ChaChaPoly.fst`

**What it says:** Flipping any byte of a ChaChaPoly ciphertext causes
decryption to fail (the authentication tag rejects).

**Why it's unprovable:** Poly1305 is a universal hash function. Its security
(UF-CMA: unforgeability under chosen-message attack) relies on the
ε-almost-delta-universal property: for any two distinct messages, the
probability of a tag collision is at most ε = 8⌈L/16⌉ / 2^106 where L is
the message length. Proving this requires showing the polynomial evaluation
over GF(2^130-5) has the claimed collision bound — which depends on the
algebraic structure being computationally hidden by the one-time key.

**What would break if false:** An attacker could modify ciphertexts without
detection, breaking authenticated encryption entirely.

**Reference:** RFC 8439 (ChaCha20-Poly1305), Bernstein 2005 "The Poly1305-AES
message-authentication code"

## 4. ML-KEM IND-CCA2 Security

**F* assume val:** `ind_cca2_security` in `Spec.PQWrapper.fst`

**What it says:** ML-KEM-768 ciphertexts are indistinguishable from random
under adaptive chosen-ciphertext attack.

**Why it's unprovable:** IND-CCA2 security of ML-KEM reduces to the hardness
of the Module Learning With Errors (MLWE) problem, which in turn reduces to
worst-case lattice problems (approximate SVP on module lattices). No one has
proved that these lattice problems are hard — it's the fundamental assumption
of post-quantum cryptography. A proof would imply breakthrough results in
computational complexity.

**What would break if false:** The post-quantum key encapsulation would be
breakable, allowing decryption of the PQXDH shared secret by a quantum
adversary.

**Reference:** FIPS 203 (ML-KEM), Regev 2005 "On Lattices, Learning with
Errors, Random Linear Codes, and Cryptography"

## 5. ML-KEM Implicit Rejection

**F* assume val:** `mlkem_implicit_rejection` in `Spec.PQWrapper.fst`

**What it says:** When ML-KEM decapsulation receives an invalid ciphertext,
the output is pseudorandom (indistinguishable from a valid shared secret).

**Why it's unprovable:** Implicit rejection is a design property of ML-KEM
that depends on the PRF security of SHAKE-256. The decapsulation algorithm
re-encrypts and compares; on mismatch, it derives output from a secret seed
via SHAKE-256. Proving the output is pseudorandom requires proving SHAKE-256
is a PRF, which reduces to the sponge construction's security assumption.

**What would break if false:** An active attacker submitting crafted
ciphertexts could distinguish rejection from acceptance, enabling a
Bleichenbacher-style adaptive attack to recover the secret key.

**Reference:** FIPS 203 Section 7.3, Hofheinz-Hövelmanns-Kiltz 2017
"A Modular Analysis of the Fujisaki-Okamoto Transformation"

## 6. HMAC-SHA-256 Integrity

**F* assume val:** `hmac_integrity` in `Spec.SessionState.fst`

**What it says:** An attacker cannot forge a valid HMAC tag without knowing
the key.

**Why it's unprovable:** This is the standard HMAC unforgeability assumption
(UF-CMA). It reduces to the PRF security of the underlying hash function.
Same complexity-theoretic barrier as assumptions 1 and 2.

**What would break if false:** Session state could be tampered with — an
attacker could modify serialized session data and produce a valid MAC,
allowing session hijacking.

**Reference:** RFC 2104, FIPS 198-1 (HMAC)

## 7. HMAC-SHA-256 Unforgeability (Wire Format)

**F* assume val:** `hmac_unforgeability` in `Spec.WireFormat.fst`

**What it says:** Same as #6 but applied to wire format message authentication.

**Why it's unprovable:** Same as #6 — HMAC UF-CMA assumption.

**What would break if false:** Wire protocol messages could be forged,
allowing message injection and impersonation.

**Reference:** RFC 2104

## 8. Stealth Address Unlinkability

**F* assume val:** `unlinkability` in `Spec.StealthAddress.fst`

**What it says:** Two stealth addresses derived from different ephemeral keys
for the same recipient are computationally unlinkable.

**Why it's unprovable:** Unlinkability reduces to the Decisional Diffie-Hellman
(DDH) assumption on Curve25519. DDH states that given (G, aG, bG), the value
abG is indistinguishable from a random group element. This is a standard
elliptic curve hardness assumption. Proving it would require proving that
discrete logarithm on Curve25519 is hard — an open problem.

**What would break if false:** An observer could link stealth addresses to the
same recipient, breaking the privacy of the stealth address scheme.

**Reference:** RFC 7748 (X25519), Boneh-Shoup "A Graduate Course in Applied
Cryptography" Chapter 10

## 9. VRF Strong Uniqueness

**F* assume val:** `vrf_strong_uniqueness` in `Spec.VRF.fst`

**What it says:** For a given secret key and message, all valid VRF proofs
produce the same output hash.

**Why it's unprovable:** Uniqueness of ECVRF-ED25519-SHA512-ELL2 relies on
the binding property of the discrete logarithm: given Gamma = [x]H (where x
is the secret key and H = hash_to_curve(pk, msg)), Gamma is uniquely
determined. A second valid proof with a different Gamma would imply knowledge
of two discrete logs for the same base — violating the DL assumption.

**What would break if false:** A VRF prover could produce different outputs
for the same input, breaking verifiable randomness (e.g., leader election
would be manipulable).

**Reference:** RFC 9381 (ECVRF), Goldberg-Naor-Papadopoulos-Reyzin 2017
"Verifiable Random Functions from Standard Assumptions"

## 10. VRF Collision Resistance

**F* assume val:** `vrf_collision_resistance` in `Spec.VRF.fst`

**What it says:** Distinct messages produce distinct VRF outputs.

**Why it's unprovable:** VRF output is SHA-512(encode(Gamma)) where
Gamma = [x]hash_to_curve(pk, msg). Collision resistance requires both
hash_to_curve injective (reduces to Elligator2 + SHA-512 collision
resistance) and SHA-512 itself collision-resistant. Same barrier as
assumption #2.

**What would break if false:** Two different messages would map to the same
VRF output, breaking the randomness guarantee.

**Reference:** RFC 9381, FIPS 180-4 (SHA-512)

## 11. Ed25519 Distinct-Message Distinct-Signature

**F* assume val:** `distinct_messages_distinct_sigs` in `Spec.Ed25519.fst`

**What it says:** If msg1 ≠ msg2, then SHA-512(prefix || msg1) ≠
SHA-512(prefix || msg2) implies the Ed25519 signatures are distinct.

**Why it's unprovable:** The antecedent (SHA-512 collision resistance on
prefix-extended inputs) is a standard hash assumption. The consequent follows
algebraically if the antecedent holds, since the nonce r = SHA-512(prefix ||
msg) determines the signature's R component. But proving the antecedent
requires proving SHA-512 collision resistance.

**What would break if false:** Two different messages could produce the same
signature, enabling signature reuse attacks.

**Reference:** RFC 8032 (Ed25519), FIPS 180-4 (SHA-512)

## 12. SHA-256 Refinement Axiom

**F* assume val:** `sha256_refinement_axiom` in `Spec.SHA256.Refinement.fst`

**What it says:** The Haskell SHA-256 implementation produces the same output
as the F* specification for all inputs.

**Why it's unprovable in a proof assistant:** This bridges two different
programming languages (Haskell and F*). The F* spec defines SHA-256 purely;
the Haskell implementation uses ByteString, foreign calls, and GHC's runtime.
Proving equivalence would require a verified compiler for both languages or a
formal model of GHC's semantics — neither exists.

**Mitigation:** Differential testing against libsodium, HACL*, and OpenSSL
reference implementations (`./uv test differential`). Test vectors from
NIST CAVP. The Haskell implementation passes all 2048 NIST test vectors.

**What would break if false:** The F* proofs about SHA-256 properties would
not apply to the actual running code.

**Reference:** FIPS 180-4, NIST CAVP test vectors

## Summary

| # | Assumption | Primitive | Hardness basis |
|---|-----------|-----------|----------------|
| 1 | PRF security | HMAC-SHA-256 | SHA-256 compression function |
| 2 | Collision resistance | HMAC-SHA-256 | SHA-256 pigeonhole |
| 3 | UF-CMA | Poly1305 | GF(2^130-5) polynomial evaluation |
| 4 | IND-CCA2 | ML-KEM-768 | Module-LWE / lattice SVP |
| 5 | Implicit rejection | ML-KEM-768 | SHAKE-256 PRF |
| 6 | Integrity | HMAC-SHA-256 | Same as #1 |
| 7 | Unforgeability | HMAC-SHA-256 | Same as #1 |
| 8 | Unlinkability | Stealth addresses | Curve25519 DDH |
| 9 | Strong uniqueness | ECVRF | Ed25519 discrete log |
| 10 | Collision resistance | ECVRF | SHA-512 + Elligator2 |
| 11 | Distinct signatures | Ed25519 | SHA-512 collision resistance |
| 12 | Refinement | SHA-256 | Cross-language equivalence |

These 12 assumptions are the **trust boundary** of UmbraVOX's formal
verification. Everything inside this boundary is proved (or provable in Coq).
Everything outside is a standard cryptographic assumption shared by every
implementation of these primitives worldwide.
