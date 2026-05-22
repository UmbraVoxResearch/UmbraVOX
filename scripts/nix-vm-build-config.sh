#!/usr/bin/env bash
# Resolve VM-local nix build configuration with fail-closed guards.
#
# Usage:
#   eval "$("./scripts/nix-vm-build-config.sh" shell)"
#   ./scripts/nix-vm-build-config.sh print

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DEFAULT_CONFIG_FILE="$REPO_ROOT/nix/vm-build.env"
CONFIG_FILE="${UMBRAVOX_NIX_CONFIG_FILE:-$DEFAULT_CONFIG_FILE}"

die() {
  echo "[NIX-VM-CONFIG] ERROR: $*" >&2
  exit 1
}

load_file() {
  local file="$1"
  while IFS= read -r line || [ -n "$line" ]; do
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    [[ -z "$line" || "${line:0:1}" == "#" ]] && continue
    case "$line" in
      *=*)
        local k="${line%%=*}"
        local v="${line#*=}"
        k="${k%"${k##*[![:space:]]}"}"
        v="${v#"${v%%[![:space:]]*}"}"
        v="${v%"${v##*[![:space:]]}"}"
        if [[ -z "${!k+x}" ]]; then
          export "$k=$v"
        fi
        ;;
      *)
        die "Invalid line in config file '$file': $line"
        ;;
    esac
  done <"$file"
}

resolve_config() {
  local source="env-only"
  local file_effective="<none>"
  local env_override=0

  for k in UMBRAVOX_NIX_BUILD_DIR UMBRAVOX_NIX_SANDBOX_BUILD_DIR UMBRAVOX_NIX_LOCAL_ONLY UMBRAVOX_NIX_REQUIRE_CONFIG; do
    if [[ -n "${!k+x}" ]]; then
      env_override=1
    fi
  done

  if [[ -f "$CONFIG_FILE" ]]; then
    load_file "$CONFIG_FILE"
    source="file"
    file_effective="$CONFIG_FILE"
  else
    if [[ "${UMBRAVOX_NIX_REQUIRE_CONFIG:-0}" == "1" ]]; then
      die "Required config file not found: $CONFIG_FILE"
    fi
  fi

  if [[ "$source" == "file" && "$env_override" -eq 1 ]]; then
    source="file+env"
  fi

  local build_dir sandbox_build_dir local_only require_config
  if [[ -n "${UMBRAVOX_NIX_BUILD_DIR+x}" ]]; then
    build_dir="${UMBRAVOX_NIX_BUILD_DIR}"
  else
    build_dir="build/vm/tmp"
  fi
  if [[ -n "${UMBRAVOX_NIX_SANDBOX_BUILD_DIR+x}" ]]; then
    sandbox_build_dir="${UMBRAVOX_NIX_SANDBOX_BUILD_DIR}"
  else
    sandbox_build_dir="/build"
  fi
  if [[ -n "${UMBRAVOX_NIX_LOCAL_ONLY+x}" ]]; then
    local_only="${UMBRAVOX_NIX_LOCAL_ONLY}"
  else
    local_only="1"
  fi
  if [[ -n "${UMBRAVOX_NIX_REQUIRE_CONFIG+x}" ]]; then
    require_config="${UMBRAVOX_NIX_REQUIRE_CONFIG}"
  else
    require_config="0"
  fi

  [[ "$local_only" == "1" ]] || die "UMBRAVOX_NIX_LOCAL_ONLY must be 1 (received '$local_only')"
  [[ -n "$build_dir" ]] || die "UMBRAVOX_NIX_BUILD_DIR must be non-empty"
  [[ -n "$sandbox_build_dir" ]] || die "UMBRAVOX_NIX_SANDBOX_BUILD_DIR must be non-empty"

  # Hard guard: forbid remote-builder knobs in local-only mode.
  [[ -z "${UMBRAVOX_NIX_BUILDER:-}" ]] || die "UMBRAVOX_NIX_BUILDER is set but local-only mode is enforced"
  [[ -z "${NIX_REMOTE:-}" ]] || die "NIX_REMOTE is set but local-only mode is enforced"

  cat <<EOF
export UMBRAVOX_NIX_CONFIG_SOURCE='${source}'
export UMBRAVOX_NIX_CONFIG_FILE_EFFECTIVE='${file_effective}'
export UMBRAVOX_NIX_BUILD_DIR='${build_dir}'
export UMBRAVOX_NIX_SANDBOX_BUILD_DIR='${sandbox_build_dir}'
export UMBRAVOX_NIX_LOCAL_ONLY='${local_only}'
export UMBRAVOX_NIX_REQUIRE_CONFIG='${require_config}'
EOF
}

print_config() {
  eval "$(resolve_config)"
  echo "UMBRAVOX_NIX_CONFIG_SOURCE=$UMBRAVOX_NIX_CONFIG_SOURCE"
  echo "UMBRAVOX_NIX_CONFIG_FILE_EFFECTIVE=$UMBRAVOX_NIX_CONFIG_FILE_EFFECTIVE"
  echo "UMBRAVOX_NIX_BUILD_DIR=$UMBRAVOX_NIX_BUILD_DIR"
  echo "UMBRAVOX_NIX_SANDBOX_BUILD_DIR=$UMBRAVOX_NIX_SANDBOX_BUILD_DIR"
  echo "UMBRAVOX_NIX_LOCAL_ONLY=$UMBRAVOX_NIX_LOCAL_ONLY"
  echo "UMBRAVOX_NIX_REQUIRE_CONFIG=$UMBRAVOX_NIX_REQUIRE_CONFIG"
}

cmd="${1:-shell}"
case "$cmd" in
  shell)
    resolve_config
    ;;
  print)
    print_config
    ;;
  *)
    die "Unknown command '$cmd'. Use: shell|print"
    ;;
esac
