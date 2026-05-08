#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/release-lane-readiness-lib.sh"

lane_header \
  "BSD lane readiness" \
  "Checks the current BSD source-release prerequisites and keeps the lane explicitly source-only until native builders exist."

require_repo_path "Makefile" "repo make entrypoint" "restore Makefile in the repo root before using this lane"
require_repo_path "scripts/release-package.sh" "release packager script" "restore scripts/release-package.sh before using this lane"
require_uname_s '^(FreeBSD|OpenBSD|NetBSD|DragonFly)$' "BSD host kernel" "rerun this script on a supported BSD host"
require_any_gnu_make "GNU Make entrypoint" "install GNU Make and expose it as gmake or GNU-compatible make on the BSD lane host" gmake make
require_command git "git metadata tooling" "install git on the BSD lane host"
require_command cabal "Cabal build entrypoint" "install cabal on the BSD lane host"
require_command ghc "GHC toolchain" "install GHC on the BSD lane host"
require_command tar "archive tooling" "install tar so the BSD source release can be assembled"
require_command sha256sum "SHA-256 digest tooling" "install GNU coreutils or another package that provides sha256sum on the BSD lane host"
require_make_target "release-bsd-terminal" "BSD source release target" "add or restore the make release-bsd-terminal target before promoting this lane"

lane_finish \
  "ready for the current BSD source-lane scaffold" \
  "not ready for the current BSD source-lane scaffold" \
  "run the GNU Make release target for the current source artifact, for example gmake release-bsd-terminal" \
  "run GNU Make build and test targets on the same host if you want BSD-native parity evidence" \
  "keep BSD source-only until native builder and smoke policy are defined"
