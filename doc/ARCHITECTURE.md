# UmbraVox: Module Map

This document describes the source layout and module organization.

## Source Tree

```
src/UmbraVox/
  Crypto/                 Cryptographic primitives
    SHA256.hs               FIPS 180-4 SHA-256
    SHA512.hs               FIPS 180-4 SHA-512
    HMAC.hs                 RFC 2104 HMAC-SHA256/SHA512
    HKDF.hs                 RFC 5869 HKDF-Extract/Expand
    AES.hs                  FIPS 197 AES-256 block cipher
    GCM.hs                  SP 800-38D AES-GCM authenticated encryption
    Curve25519.hs           RFC 7748 X25519 Diffie-Hellman
    Ed25519.hs              RFC 8032 Ed25519 signatures
    Keccak.hs               FIPS 202 SHA-3 and SHAKE
    Poly1305.hs             RFC 8439 Poly1305 MAC
    MLKEM.hs                FIPS 203 ML-KEM-768 post-quantum KEM
    VRF.hs                  RFC 9381 ECVRF (partial)
    Random.hs               ChaCha20-based CSPRNG
    ConstantTime.hs         Constant-time comparison utilities
    BIP39.hs                BIP39 mnemonic wordlist (2048 words)
    Export.hs               Encrypted identity export/import
    StealthAddress.hs       Dual-Key Stealth Address Protocol (DKSAP)
    PQWrapper.hs            Post-quantum outer encryption layer
    KeyStore.hs             Encrypted-at-rest key storage
    Signal/                 Signal protocol implementation
      X3DH.hs                 X3DH key agreement
      PQXDH.hs                Post-quantum extended X3DH (X3DH + ML-KEM-768)
      DoubleRatchet.hs        Signal Double Ratchet
      SenderKeys.hs           Sender Keys for group messaging
      Session.hs              Signal session management

  Network/                Networking and transport
    TransportClass.hs       Pluggable transport abstraction (typeclass)
    Transport.hs            TCP transport implementation
    Transport/              Transport backend stubs
      Loopback.hs             In-process loopback (testing)
      Intercept.hs            Message interception (testing)
      UDP.hs                  UDP transport (stub)
      Blockchain.hs           On-chain transport (stub)
      SignalServer.hs         Signal server relay (stub)
      XMPP.hs                XMPP relay (stub)
      Discord.hs              Discord relay (stub)
      Matrix.hs               Matrix relay (stub)
    Noise.hs                Noise_IK handshake (X25519 + ChaChaPoly1305)
    Noise/
      State.hs                Noise handshake state machine
      Handshake.hs            Noise handshake message processing
    MDNS.hs                 mDNS/DNS-SD LAN peer discovery
    PeerExchange.hs         Peer exchange on first connection
    PeerManager.hs          Peer tracking and scoring
    Gossip.hs               Block/transaction gossip (stub)
    Dandelion.hs            Dandelion++ IP obfuscation (stub)
    Protocol.hs             Network protocol messages
    Sync.hs                 Chain synchronization (stub)

  Chat/                   Chat application layer
    Session.hs              Chat session (wraps Signal encrypt/decrypt)
    Wire.hs                 Wire format for chat messages
    Message.hs              Message types and construction
    Transaction.hs          Chat-to-blockchain transaction (stub)
    Contacts.hs             Contact list management
    API.hs                  JSON-RPC chat API (stub)

  Storage/                Persistence
    Anthony.hs              Anthony DB interface (peers, settings, conversations)
    Schema.hs               Database schema definitions
    ChainDB.hs              Append-only block storage (stub)
    StateDB.hs              Ledger state storage (stub)
    Index.hs                Block/transaction indexes (stub)
    Checkpoint.hs           Truncation checkpoints (stub)

  Protocol/               Serialization and encoding
    Encoding.hs             Shared encoding utilities (port defaults, byte helpers)
    CBOR.hs                 Length-prefixed binary framing
    MessageFormat.hs        1024-byte message block layout
    WireFormat.hs           Network envelope format
    QRCode.hs               QR code generation for safety numbers

  TUI/                    Terminal user interface
    Types.hs                Core types (AppState, AppConfig, Layout, InputEvent)
    Constants.hs            UI constants (colors, dimensions)
    Terminal.hs             Terminal control (raw mode, cursor, escape sequences)
    Layout.hs               Pane layout calculation with resize support
    Render.hs               Screen rendering (split panes, borders, menus)
    Dialog.hs               Modal dialog rendering (help, settings, keys, etc.)
    Menu.hs                 F1-F5 dropdown menu system
    Input.hs                Keyboard input handling and event loop
    Actions.hs              User action handlers (connect, settings, export, etc.)
    Actions/
      Session.hs              Session-related actions
      Export.hs               Identity export actions
    Handshake.hs            Identity generation and Noise_IK handshake flow

  Economics/              Token economics (stubs)
    Token.hs                Token supply and distribution types
    Fees.hs                 Message fee calculation
    Rewards.hs              Block producer rewards
    Penalty.hs              Validator penalty tiers
    Cycle.hs                11-day cycle management
    Onboarding.hs           New user token bootstrap

  Consensus/              Consensus engine (stubs)
    Types.hs                SlotNo, EpochNo, CycleNo, BlockNo
    Block.hs                Block and BlockHeader types
    Ledger.hs               Ledger state (balances, stakes)
    Protocol.hs             Ouroboros Praos adaptation
    LeaderElection.hs       VRF-based slot leader election
    Validation.hs           Block and transaction validation
    ForkChoice.hs           Longest-chain fork selection
    Nonce.hs                Epoch nonce evolution
    Truncation.hs           Chain truncation logic
    Mempool.hs              Transaction mempool

  Tools/                  Development tools
    Complexity.hs           Cyclomatic complexity checker
    FStarVerify.hs          F* formal verification runner
    FetchReferences.hs      Reference document fetcher
```

## Application Entry Points

```
app/Main.hs               Main TUI application (cabal run umbravox)
codegen/Main.hs            Code generator (cabal run codegen)
scripts/check-complexity/  Complexity checker (cabal run check-complexity)
scripts/fstar-verify/      F* verifier (cabal run fstar-verify)
scripts/fetch-references/  Reference fetcher (cabal run fetch-references)
```

## Test Organization

```
test/
  Main.hs                  Test runner (1,082 tests)
  Test/
    Util.hs                  Test utilities
    Harness.hs               Test harness framework
    EndToEnd.hs              End-to-end encrypted messaging tests
    EndToEnd2.hs             Additional E2E tests
    Crypto/                  Cryptographic primitive tests (NIST/RFC KAT vectors)
    Network/                 Transport, Noise, MDNS, PeerExchange tests
    Protocol/                CBOR, QRCode, MessageFormat, WireFormat tests
    Chat/                    Session, Message, Transaction, Contacts, API tests
    Storage/                 Anthony DB, ChainDB, StateDB, Index, Checkpoint tests
    TUI/                     TUI unit tests and simulation tests
      Sim/                     Full TUI simulation (contacts, chat, menus, dialogs)
    Consensus/               Consensus type and stub tests
    Economics/               Economics type and stub tests
    Tools/                   Tool tests (complexity, F* verify, fetch-references)
    Codegen.hs               Code generator tests
    Security.hs              Security property tests
    Fuzz.hs                  Fuzz tests (malformed input handling)
    Integration.hs           Integration tests
    Equivalence.hs           Pure Haskell vs FFI equivalence tests
```

## Formal Verification

```
test/evidence/formal-proofs/fstar/
  Spec.SHA256.fst            FIPS 180-4
  Spec.SHA512.fst            FIPS 180-4
  Spec.HMAC.fst              RFC 2104/4231
  Spec.HKDF.fst              RFC 5869
  Spec.AES256.fst            FIPS 197
  Spec.GaloisField.fst       SP 800-38D
  Spec.GCM.fst               SP 800-38D
  Spec.ChaCha20.fst          RFC 8439
  Spec.X25519.fst            RFC 7748
  Spec.Ed25519.fst           RFC 8032
  Spec.Keccak.fst            FIPS 202
  Spec.MLKEM768.fst          FIPS 203
  Spec.Poly1305.fst          RFC 8439
  Spec.DoubleRatchet.fst     Signal Double Ratchet
  Spec.X3DH.fst              X3DH key agreement
  Spec.PQXDH.fst             Post-quantum extended X3DH
  Spec.NoiseIK.fst           Noise_IK transport handshake
```

## Code Generation

```
codegen/
  Main.hs                  Generator entry point
  CryptoGen.hs             .spec -> Haskell/C/FFI code generator
  CBORGen.hs               .schema -> CBOR encoder/decoder generator
  FSMGen.hs                .fsm -> state machine generator
  TestGen.hs               Test harness generator
  Specs/                   10 specification files
    SHA256.spec, SHA512.spec, AES256.spec, ChaCha20.spec,
    Poly1305.spec, X25519.spec, HMAC.spec, HKDF.spec,
    Keccak.spec, MLKEM768.spec
```
