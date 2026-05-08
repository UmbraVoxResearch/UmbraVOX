#!/usr/bin/env bash
set -euo pipefail

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  echo "source scripts/release-lane-readiness-lib.sh from a lane script" >&2
  exit 1
fi

ROOT="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
READINESS_FAILURES=()

lane_header() {
  local title="$1"
  local summary="$2"
  printf '%s\n' "$title"
  printf '%s\n' "$summary"
  printf 'repo root: %s\n' "$ROOT"
}

check_ok() {
  printf '[ok] %s\n' "$1"
}

check_fail() {
  local detail="$1"
  local next_action="$2"
  printf '[missing] %s\n' "$detail"
  READINESS_FAILURES+=("$next_action")
}

require_command() {
  local cmd="$1"
  local detail="$2"
  local next_action="$3"
  if command -v "$cmd" >/dev/null 2>&1; then
    check_ok "$detail ($cmd)"
  else
    check_fail "$detail ($cmd)" "$next_action"
  fi
}

require_any_command() {
  local detail="$1"
  local next_action="$2"
  shift 2

  local cmd=""
  for cmd in "$@"; do
    if command -v "$cmd" >/dev/null 2>&1; then
      check_ok "$detail ($cmd)"
      return
    fi
  done

  check_fail "$detail ($*)" "$next_action"
}

require_repo_path() {
  local relpath="$1"
  local detail="$2"
  local next_action="$3"
  if [[ -e "$ROOT/$relpath" ]]; then
    check_ok "$detail ($relpath)"
  else
    check_fail "$detail ($relpath)" "$next_action"
  fi
}

require_make_target() {
  local target="$1"
  local detail="$2"
  local next_action="$3"
  if grep -Eq "^[[:space:]]*${target}:" "$ROOT/Makefile"; then
    check_ok "$detail ($target)"
  else
    check_fail "$detail ($target)" "$next_action"
  fi
}

require_uname_s() {
  local pattern="$1"
  local detail="$2"
  local next_action="$3"
  local actual
  actual="$(uname -s 2>/dev/null || echo unknown)"
  if [[ "$actual" =~ $pattern ]]; then
    check_ok "$detail ($actual)"
  else
    check_fail "$detail (found $actual)" "$next_action"
  fi
}

require_uname_m() {
  local pattern="$1"
  local detail="$2"
  local next_action="$3"
  local actual
  actual="$(uname -m 2>/dev/null || echo unknown)"
  if [[ "$actual" =~ $pattern ]]; then
    check_ok "$detail ($actual)"
  else
    check_fail "$detail (found $actual)" "$next_action"
  fi
}

require_gnu_make() {
  local cmd="$1"
  local detail="$2"
  local next_action="$3"
  local version

  if ! command -v "$cmd" >/dev/null 2>&1; then
    check_fail "$detail ($cmd)" "$next_action"
    return
  fi

  version="$("$cmd" --version 2>/dev/null | head -n1 || true)"
  if [[ "$version" == GNU\ Make* ]]; then
    check_ok "$detail ($cmd)"
  else
    check_fail "$detail ($cmd is not GNU Make)" "$next_action"
  fi
}

require_any_gnu_make() {
  local detail="$1"
  local next_action="$2"
  shift 2

  local cmd=""
  local version=""
  for cmd in "$@"; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
      continue
    fi
    version="$("$cmd" --version 2>/dev/null | head -n1 || true)"
    if [[ "$version" == GNU\ Make* ]]; then
      check_ok "$detail ($cmd)"
      return
    fi
  done

  check_fail "$detail ($*)" "$next_action"
}

lane_finish() {
  local ready_message="$1"
  local blocked_message="$2"
  shift 2

  local action=""
  printf '\n'
  if [[ "${#READINESS_FAILURES[@]}" -gt 0 ]]; then
    printf 'Result: %s\n' "$blocked_message"
    printf 'Next actions:\n'
    for action in "${READINESS_FAILURES[@]}"; do
      printf -- '- %s\n' "$action"
    done
    for action in "$@"; do
      printf -- '- %s\n' "$action"
    done
    exit 1
  fi

  printf 'Result: %s\n' "$ready_message"
  printf 'Next actions:\n'
  for action in "$@"; do
    printf -- '- %s\n' "$action"
  done
}
