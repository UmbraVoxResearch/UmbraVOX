# Export Control Compliance Process

This document describes the steps required to comply with U.S. Export Administration Regulations (EAR) for open-source cryptographic software, using the same exemption relied upon by the Signal project and other widely distributed encryption tools.

## Applicable Exemption

Open-source encryption software that is publicly available may qualify for the License Exception ENC unrestricted (formerly TSU) under **15 CFR 742.15(b)**. This provision permits export and re-export of publicly available encryption source code provided that the Bureau of Industry and Security (BIS) is notified before or contemporaneously with public release.

This is a **notification-only** requirement. It is not a license application, and no approval or response from BIS is required before publication.

## Filing the BIS Notification

### When to File

The notification must be sent **before or at the time of public release** of the source code. Do not delay public release waiting for a response; no response is required.

### Where to Send

Send the notification by email to **both** of the following addresses:

- `crypt@bis.doc.gov` (Bureau of Industry and Security, Department of Commerce)
- `enc@nsa.gov` (National Security Agency, Encryption Review)

### Email Template

```
Subject: EAR Section 742.15(b) Notification — UmbraVOX

To Whom It May Concern:

Pursuant to 15 CFR 742.15(b), this email serves as notification that
the following publicly available encryption source code is being made
available on the Internet.

Project Name:    UmbraVOX
Repository URL:  https://github.com/[organization]/UmbraVOX
License:         [License name, e.g., BSD-3-Clause]

Algorithms and Cryptographic Functions Implemented:
- X25519 (ECDH key agreement, Curve25519)
- Ed25519 (EdDSA digital signatures)
- AES-256-GCM (authenticated encryption)
- ChaCha20-Poly1305 (authenticated encryption)
- SHA-256, SHA-512 (hash functions)
- SHA-3 / Keccak / SHAKE (hash and extendable-output functions)
- HMAC-SHA-256, HMAC-SHA-512 (message authentication)
- HKDF (key derivation, RFC 5869)
- ML-KEM-768 (post-quantum key encapsulation, FIPS 203)
- Signal Protocol (X3DH key agreement, Double Ratchet)

The source code is publicly available and accessible without
restriction. This notification is provided in accordance with the
requirements of EAR Section 742.15(b).

Respectfully,
[Name]
[Title / Role]
[Contact Information]
```

Adjust the algorithm list as the project evolves. Include any new cryptographic primitives added after initial filing.

### What to Include

The notification should contain:

1. **Project name** — UmbraVOX
2. **Repository URL** — the public URL where the source code is hosted
3. **License** — the open-source license under which the code is published
4. **Algorithms listed** — every cryptographic algorithm implemented or invoked by the software
5. **Brief description** — a short statement that the source code is publicly available

## Record Keeping

Retain evidence of filing, including:

- A copy of the email as sent (with headers and timestamps)
- Any delivery receipts or confirmations
- A record of the date of public release relative to the date of notification

Store these records for a minimum of five years, consistent with general EAR record-keeping requirements (15 CFR 762).

## Derivative Lineage Consideration

If UmbraVOX incorporates or derives from existing notified open-source cryptographic projects (such as libsignal), consider structuring the repository history to establish clear derivative lineage. One approach is to fork from a project that has already completed its own BIS notification as the initial commit, making the chain of derivation explicit in the version control history. This may simplify compliance review and reduce ambiguity about the origin of cryptographic components.

## Future: FIPS 140-3 Validation

For deployment within the Defense Industrial Base (DIB) or other environments requiring validated cryptographic modules, future development may pursue FIPS 140-3 validation of core encryption modules. FIPS validation is a separate process from EAR notification and involves testing by an accredited Cryptographic and Security Testing (CST) laboratory, followed by review by the Cryptographic Module Validation Program (CMVP).

FIPS 140-3 validation is not required for the EAR notification described above, but may be necessary for certain institutional or government-adjacent deployments.

## References

- [15 CFR Part 742 — Control Policy: CCL-Based Controls](https://www.ecfr.gov/current/title-15/subtitle-B/chapter-VII/subchapter-C/part-742)
- [15 CFR 742.15 — Encryption items](https://www.ecfr.gov/current/title-15/section-742.15)
- [15 CFR Part 762 — Recordkeeping](https://www.ecfr.gov/current/title-15/subtitle-B/chapter-VII/subchapter-C/part-762)
- [BIS Encryption Page](https://www.bis.doc.gov/index.php/policy-guidance/encryption)
- [Signal's approach to open-source encryption compliance](https://signal.org/)
