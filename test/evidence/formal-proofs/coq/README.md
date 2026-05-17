# Coq External Evidence for F* Trust Boundary

External verification artifacts backing F* `assume val` declarations.

## Build

```
nix-shell --run "make -C test/evidence/formal-proofs/coq"
```

## Status

| File | Status | Backs F* Declaration |
|------|--------|---------------------|
| Ed25519Constants.v | Checked | Constants only (no assumptions) |

## Rules

- No `Admitted.` in checked evidence files
- No `Axiom` or `Parameter` unless explicitly marked as draft
- Every theorem must map to a specific F* `assume val`
