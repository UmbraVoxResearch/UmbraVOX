#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/release-lane-readiness-lib.sh"

lane_header \
  "Linux x86_64 lane readiness" \
  "Checks the current native Linux x86_64 release plus isolated smoke prerequisites without building or modifying artifacts."

require_repo_path "Makefile" "repo make entrypoint" "restore Makefile in the repo root before using this lane"
require_repo_path "scripts/release-package.sh" "release packager script" "restore scripts/release-package.sh before using this lane"
require_repo_path "scripts/release-smoke-linux.sh" "Linux smoke script" "restore scripts/release-smoke-linux.sh before using this lane"
require_uname_s '^Linux$' "Linux host kernel" "rerun this script on a Linux host or schedule the lane on a Linux runner"
require_uname_m '^(x86_64|amd64)$' "x86_64 userspace architecture" "rerun this script on an x86_64 Linux host for the current portable bundle path"
require_gnu_make "make" "GNU Make entrypoint" "install GNU Make as make on the Linux lane host"
require_command git "git metadata tooling" "install git so release metadata and cleanliness checks can run"
require_command cabal "Cabal build entrypoint" "install cabal on the Linux x86_64 lane host"
require_command ghc "GHC toolchain" "install GHC on the Linux x86_64 lane host"
require_command patchelf "ELF interpreter patch tooling" "install patchelf for the Linux bundle release lane"
require_command file "ELF/file inspection tooling" "install file for Linux bundle manifest checks"
require_command ldd "dynamic linker dependency inspection" "install ldd or glibc userland tools for Linux bundle closure checks"
require_command tar "archive tooling" "install tar so the release bundle can be assembled"
require_command sha256sum "SHA-256 digest tooling" "install sha256sum for release digests"
require_any_command "container smoke runtime" "install podman or docker for the isolated Linux smoke step" podman docker
require_make_target "release-linux" "Linux release target" "add or restore the make release-linux target before promoting this lane"
require_make_target "release-smoke-linux" "Linux smoke target" "add or restore the make release-smoke-linux target before promoting this lane"

lane_finish \
  "ready for the current Linux x86_64 lane scaffold" \
  "not ready for the current Linux x86_64 lane scaffold" \
  "enter nix-shell if you want the pinned developer environment, then run make release-linux" \
  "run make release-smoke-linux after the bundle build to verify the artifact in isolation" \
  "capture dedicated native Linux x86_64 evidence for M3.1.1.b by running make build, make test, and make verify on the intended runner" \
  "retain the build, test, verify, and release logs as runner evidence before promoting this lane beyond host packaging" \
  "wire this host class into CI only after native lane promotion rules are documented"
