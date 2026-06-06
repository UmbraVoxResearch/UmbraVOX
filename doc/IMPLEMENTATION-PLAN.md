# UmbraVOX Implementation Plan

This document turns the current highest-value backlog into a concrete
execution plan. It is intentionally narrow: the goal is to close the
largest user-visible gaps first, then tighten assurance and docs so the
repo state remains truthful.

## Scope

This plan covers the next implementation slice:

1. Make the top-level readiness pipeline and docs agree on what `./uv`
   does.
2. Finish the Signal bridge registration path end-to-end.
3. Close the associated verification and evidence gaps.
4. Keep the plan reflected in `doc/TODO.txt` so execution remains visible.

## Step 1: Align the top-level pipeline truthfully

Goal: ensure the build/test/verify/check story is represented consistently
in code and docs.

Sub-tasks:

- Confirm the authoritative `./uv` aggregate command semantics.
- Update `doc/QUICKSTART.md` and any related readiness docs to match the
  actual command behavior.
- If the aggregate gate changes, update the Go orchestration entrypoint and
  any CI/readiness references in the same change.
- Add or adjust documentation so `verify` is clearly either part of the
  aggregate gate or a separate explicit gate, but not both.

## Step 2: Complete the Signal bridge registration path

Goal: make linked-device registration a real end-to-end flow instead of a
stubbed crypto-only path.

Sub-tasks:

- Implement the provisioning WebSocket transport in
  `src/UmbraVox/Bridge/Signal/Registration.hs`.
- Parse the provisioning payload and complete the linked-device state
  transition.
- Implement prekey upload and the server health verification path.
- Add a dedicated test suite for the registration flow so this path is
  exercised directly rather than only indirectly.

## Step 3: Close the assurance evidence gaps

Goal: make the assurance story repeatable, not just described.

Sub-tasks:

- Stabilize the F* verification environment in the VM.
- Run the full verification suite and capture the evidence artifact.
- Update any assurance docs that still describe the verification story as
  partial once the run is repeatable.

## Step 4: Keep the backlog visible and consistent

Goal: ensure this plan stays the source of truth for the work it covers.

Sub-tasks:

- Mirror each execution step in `doc/TODO.txt` with a sub-task that
  references this document.
- Move completed items to `doc/CHANGELOG.txt` when they are done.
- Keep any new scope that appears during execution in a separate doc or
  TODO section rather than expanding this plan silently.

## Execution notes

- This plan is intentionally implementation-oriented and should be used as
  the working reference for the next few iterations.
- If a sub-task materially changes scope, update this document first, then
  reflect the change in `doc/TODO.txt`.
