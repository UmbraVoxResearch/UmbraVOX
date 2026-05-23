#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/release-lane-readiness-lib.sh"

lane_header \
  "Linux arm64 lane readiness" \
  "Checks native Linux arm64 host-validation prerequisites for the planned lane without claiming a finished arm64 release artifact path."

require_repo_path "uv" "build system entry point" "restore the uv script"
require_repo_path "scripts/release-package.sh" "release packager script" "restore scripts/release-package.sh before using this lane"
require_uname_s '^Linux$' "Linux host kernel" "rerun this script on a Linux host or schedule the lane on a Linux runner"
require_uname_m '^(aarch64|arm64)$' "arm64 userspace architecture" "rerun this script on an arm64 Linux host for native validation"
require_command git "git metadata tooling" "install git so build and release metadata checks can run"
require_command cabal "Cabal build entrypoint" "install cabal on the Linux arm64 lane host"
require_command ghc "GHC toolchain" "install GHC on the Linux arm64 lane host"
require_command tar "archive tooling" "install tar for source and evidence packaging on arm64"
require_command sha256sum "SHA-256 digest tooling" "install sha256sum for artifact and evidence digests on arm64"

lane_finish \
  "ready for the current Linux arm64 host-validation scaffold" \
  "not ready for the current Linux arm64 host-validation scaffold" \
  "run ./uv build, ./uv test, and ./uv verify on the native arm64 host to establish parity evidence" \
  "capture dedicated native Linux arm64 evidence for M3.1.2 with the build, test, verify, and release logs from the intended runner" \
  "define a native release-linux-arm64 packaging target before promoting this lane to artifact production" \
  "add an arm64 smoke step once the native packaging target exists"
