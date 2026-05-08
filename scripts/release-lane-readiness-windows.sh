#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/release-lane-readiness-lib.sh"

lane_header \
  "Windows lane readiness" \
  "Checks the current Windows CLI source-release prerequisites from a Bash-capable Windows shell without attempting a destructive native build."

require_repo_path "Makefile" "repo make entrypoint" "restore Makefile in the repo root before using this lane"
require_repo_path "scripts/release-package.sh" "release packager script" "restore scripts/release-package.sh before using this lane"
require_uname_s '^(MINGW|MSYS|CYGWIN)' "Windows Bash host shell" "rerun this script from Git Bash, MSYS2, or Cygwin on a Windows host"
require_gnu_make "make" "GNU Make entrypoint" "install GNU Make in the Windows Bash environment"
require_command git "git metadata tooling" "install git in the Windows Bash environment"
require_command cabal "Cabal build entrypoint" "install cabal on the Windows lane host"
require_command ghc "GHC toolchain" "install GHC on the Windows lane host"
require_command tar "archive tooling" "install tar so the source release staging path can run"
require_command sha256sum "SHA-256 digest tooling" "install sha256sum in the Windows Bash environment"
require_command zip "zip packaging tooling" "install zip so the Windows CLI source release can be assembled"
require_command powershell.exe "PowerShell entrypoint" "ensure powershell.exe is available for future Windows smoke and launcher checks"
require_command cmd.exe "cmd entrypoint" "ensure cmd.exe is available for future Windows launcher checks"
require_make_target "release-windows-cli" "Windows CLI source release target" "add or restore the make release-windows-cli target before promoting this lane"

lane_finish \
  "ready for the current Windows source-lane scaffold" \
  "not ready for the current Windows source-lane scaffold" \
  "run make release-windows-cli from the Windows Bash environment for the current source artifact" \
  "run make build and make test on the same host if you want native CLI parity evidence" \
  "capture dedicated native Windows CLI evidence for M3.1.4.b with build, test, and release logs from the intended runner" \
  "add native Windows packaging and PowerShell smoke coverage before promoting this lane beyond source release"
