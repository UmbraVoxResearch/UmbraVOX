#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/release-lane-readiness-lib.sh"

lane_header \
  "macOS lane readiness" \
  "Checks native macOS source-release and native-build prerequisites without attempting to produce a macOS binary artifact."

require_repo_path "Makefile" "repo make entrypoint" "restore Makefile in the repo root before using this lane"
require_repo_path "scripts/release-package.sh" "release packager script" "restore scripts/release-package.sh before using this lane"
require_uname_s '^Darwin$' "macOS host kernel" "rerun this script on a macOS host or schedule the lane on a macOS runner"
require_gnu_make "make" "GNU Make entrypoint" "install GNU Make as make on the macOS lane host"
require_command git "git metadata tooling" "install git so release metadata and cleanliness checks can run"
require_command cabal "Cabal build entrypoint" "install cabal on the macOS lane host"
require_command ghc "GHC toolchain" "install GHC on the macOS lane host"
require_command clang "Apple C toolchain" "install the Xcode command line tools so native Haskell dependencies can compile"
require_command xcrun "Apple SDK locator" "install the Xcode command line tools so SDK lookup works on the macOS lane"
require_command tar "archive tooling" "install tar so the macOS source release can be assembled"
require_command sha256sum "SHA-256 digest tooling" "install GNU coreutils or another package that provides sha256sum on macOS"
require_make_target "release-macos-terminal" "macOS source release target" "add or restore the make release-macos-terminal target before promoting this lane"

lane_finish \
  "ready for the current macOS source-lane scaffold" \
  "not ready for the current macOS source-lane scaffold" \
  "run make release-macos-terminal on the native macOS host for the current source artifact" \
  "run make build and make test on the same host if you want native runtime evidence alongside the source release" \
  "capture dedicated native macOS evidence for M3.1.3.b with build, test, and release logs from the intended runner" \
  "add native macOS artifact packaging and smoke coverage before calling this an authoritative release lane"
