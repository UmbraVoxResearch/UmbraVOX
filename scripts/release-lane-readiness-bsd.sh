#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/release-lane-readiness-lib.sh"

lane_header \
  "BSD lane readiness" \
  "Checks the current BSD source-release prerequisites and keeps the lane explicitly source-only until native builders exist."

require_repo_path "uv" "build system entry point" "restore the uv script"
require_repo_path "scripts/release-package.sh" "release packager script" "restore scripts/release-package.sh before using this lane"
require_uname_s '^(FreeBSD|OpenBSD|NetBSD|DragonFly)$' "BSD host kernel" "rerun this script on a supported BSD host"
require_command git "git metadata tooling" "install git on the BSD lane host"
require_command cabal "Cabal build entrypoint" "install cabal on the BSD lane host"
require_command ghc "GHC toolchain" "install GHC on the BSD lane host"
require_command tar "archive tooling" "install tar so the BSD source release can be assembled"
require_command sha256sum "SHA-256 digest tooling" "install GNU coreutils or another package that provides sha256sum on the BSD lane host"

lane_finish \
  "ready for the current BSD source-lane scaffold" \
  "not ready for the current BSD source-lane scaffold" \
  "run ./uv release bsd-terminal for the current source artifact" \
  "run ./uv build and ./uv test on the same host if you want BSD-native parity evidence" \
  "capture BSD-native evidence only after a repo-owned BSD runner exists, then record build/test/release logs for future parity review" \
  "keep BSD source-only until native builder and smoke policy are defined"
