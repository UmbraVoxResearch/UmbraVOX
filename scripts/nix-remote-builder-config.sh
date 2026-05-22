#!/usr/bin/env bash
# Load/validate UmbraVOX remote Nix builder configuration.
# Intended use:
#   eval "$("./scripts/nix-remote-builder-config.sh" shell)"
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DEFAULT_CONFIG_FILE="$REPO_ROOT/nix/remote-builder.env"

usage() {
    cat <<'EOF'
Usage:
  nix-remote-builder-config.sh shell

Emits shell exports for the effective remote-builder configuration.
Precedence: environment variables override config-file values.
EOF
}

is_true() {
    case "${1,,}" in
        1|true|yes|on) return 0 ;;
        *) return 1 ;;
    esac
}

normalize_bool() {
    case "${1,,}" in
        1|true|yes|on) echo "true" ;;
        0|false|no|off) echo "false" ;;
        *)
            echo "[NIX-REMOTE] Invalid boolean value: '$1'" >&2
            exit 1
            ;;
    esac
}

emit_export() {
    local key="$1"
    local value="$2"
    printf 'export %s=%q\n' "$key" "$value"
}

COMMAND="${1:-shell}"
if [ "$COMMAND" != "shell" ]; then
    usage >&2
    exit 1
fi

CONFIG_FILE="${UMBRAVOX_NIX_CONFIG_FILE:-$DEFAULT_CONFIG_FILE}"

ENV_HAS_BUILDER=0
ENV_HAS_SUBSTITUTES=0
ENV_HAS_REMOTE_REQUIRED=0
if [ "${UMBRAVOX_NIX_BUILDER+x}" = "x" ]; then ENV_HAS_BUILDER=1; fi
if [ "${UMBRAVOX_NIX_BUILDERS_USE_SUBSTITUTES+x}" = "x" ]; then ENV_HAS_SUBSTITUTES=1; fi
if [ "${UMBRAVOX_NIX_REMOTE_REQUIRED+x}" = "x" ]; then ENV_HAS_REMOTE_REQUIRED=1; fi

ENV_BUILDER="${UMBRAVOX_NIX_BUILDER-}"
ENV_SUBSTITUTES="${UMBRAVOX_NIX_BUILDERS_USE_SUBSTITUTES-}"
ENV_REMOTE_REQUIRED="${UMBRAVOX_NIX_REMOTE_REQUIRED-}"

CONFIG_FILE_USED=""
if [ -f "$CONFIG_FILE" ]; then
    # shellcheck disable=SC1090
    source "$CONFIG_FILE"
    CONFIG_FILE_USED="$CONFIG_FILE"
fi

# Environment always wins over file values.
if [ "$ENV_HAS_BUILDER" -eq 1 ]; then UMBRAVOX_NIX_BUILDER="$ENV_BUILDER"; fi
if [ "$ENV_HAS_SUBSTITUTES" -eq 1 ]; then UMBRAVOX_NIX_BUILDERS_USE_SUBSTITUTES="$ENV_SUBSTITUTES"; fi
if [ "$ENV_HAS_REMOTE_REQUIRED" -eq 1 ]; then UMBRAVOX_NIX_REMOTE_REQUIRED="$ENV_REMOTE_REQUIRED"; fi

: "${UMBRAVOX_NIX_BUILDERS_USE_SUBSTITUTES:=true}"
: "${UMBRAVOX_NIX_REMOTE_REQUIRED:=1}"

if [ -n "$CONFIG_FILE_USED" ]; then
    if [ "$ENV_HAS_BUILDER" -eq 1 ] || [ "$ENV_HAS_SUBSTITUTES" -eq 1 ] || [ "$ENV_HAS_REMOTE_REQUIRED" -eq 1 ]; then
        UMBRAVOX_NIX_CONFIG_SOURCE="file+env"
    else
        UMBRAVOX_NIX_CONFIG_SOURCE="file"
    fi
else
    if [ "$ENV_HAS_BUILDER" -eq 1 ] || [ "$ENV_HAS_SUBSTITUTES" -eq 1 ] || [ "$ENV_HAS_REMOTE_REQUIRED" -eq 1 ]; then
        UMBRAVOX_NIX_CONFIG_SOURCE="env-only"
    else
        UMBRAVOX_NIX_CONFIG_SOURCE="none"
    fi
fi

UMBRAVOX_NIX_BUILDERS_USE_SUBSTITUTES="$(normalize_bool "$UMBRAVOX_NIX_BUILDERS_USE_SUBSTITUTES")"

if ! is_true "$UMBRAVOX_NIX_REMOTE_REQUIRED"; then
    echo "[NIX-REMOTE] UMBRAVOX_NIX_REMOTE_REQUIRED must remain enabled (1/true)." >&2
    echo "[NIX-REMOTE] Local fallback is intentionally disabled for VM image builds." >&2
    exit 1
fi
UMBRAVOX_NIX_REMOTE_REQUIRED="1"

if [ -z "${UMBRAVOX_NIX_BUILDER-}" ]; then
    echo "[NIX-REMOTE] Missing required UMBRAVOX_NIX_BUILDER." >&2
    if [ -n "$CONFIG_FILE_USED" ]; then
        echo "[NIX-REMOTE] Set it in $CONFIG_FILE_USED or via environment variable." >&2
    else
        echo "[NIX-REMOTE] Set it via environment variable or create $DEFAULT_CONFIG_FILE." >&2
    fi
    exit 1
fi

emit_export "UMBRAVOX_NIX_BUILDER" "$UMBRAVOX_NIX_BUILDER"
emit_export "UMBRAVOX_NIX_BUILDERS_USE_SUBSTITUTES" "$UMBRAVOX_NIX_BUILDERS_USE_SUBSTITUTES"
emit_export "UMBRAVOX_NIX_REMOTE_REQUIRED" "$UMBRAVOX_NIX_REMOTE_REQUIRED"
emit_export "UMBRAVOX_NIX_CONFIG_SOURCE" "$UMBRAVOX_NIX_CONFIG_SOURCE"
emit_export "UMBRAVOX_NIX_CONFIG_FILE_EFFECTIVE" "${CONFIG_FILE_USED:-<none>}"
