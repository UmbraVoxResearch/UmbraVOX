#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"

declare -a REQUIRED_FAILURES=()
declare -a REQUIRED_LANES=(
  "Linux x86_64 native runner lane"
)
declare -a INFORMATIONAL_LANES=(
  "Linux arm64 native runner lane"
  "macOS native runner lane"
  "Windows native runner lane"
  "BSD native runner lane"
)

run_lane() {
  local label="$1"
  local mode="$2"
  local script_name="$3"
  local script_path="$SCRIPT_DIR/$script_name"
  local status="ready"
  local exit_code=0

  printf '=== %s ===\n' "$label"
  printf 'mode: %s\n' "$mode"

  if [[ ! -f "$script_path" ]]; then
    printf '[missing] aggregate entrypoint script (%s)\n' "$script_name"
    if [[ "$mode" == "required" ]]; then
      REQUIRED_FAILURES+=("$label: missing $script_name")
      status="blocked (missing script)"
    fi
    printf '\n'
    return 0
  fi

  if bash "$script_path"; then
    status="ready"
  else
    exit_code=$?
    status="blocked (exit ${exit_code})"
    if [[ "$mode" == "required" ]]; then
      REQUIRED_FAILURES+=("$label: ${status}")
    fi
  fi

  printf 'aggregate status: %s\n' "$status"
  printf '\n'
}

run_lane "Linux x86_64 native runner lane" "required" "release-lane-readiness-linux-x86_64.sh"
run_lane "Linux arm64 native runner lane" "informational" "release-lane-readiness-linux-arm64.sh"
run_lane "macOS native runner lane" "informational" "release-lane-readiness-macos.sh"
run_lane "Windows native runner lane" "informational" "release-lane-readiness-windows.sh"
run_lane "BSD native runner lane" "informational" "release-lane-readiness-bsd.sh"

printf 'Aggregate summary\n'
printf 'required lanes:\n'
for lane in "${REQUIRED_LANES[@]}"; do
  printf -- '- %s\n' "$lane"
done
printf 'informational lanes:\n'
for lane in "${INFORMATIONAL_LANES[@]}"; do
  printf -- '- %s\n' "$lane"
done

if ((${#REQUIRED_FAILURES[@]} > 0)); then
  printf 'result: blocked\n'
  printf 'required core Linux lane failures:\n'
  for failure in "${REQUIRED_FAILURES[@]}"; do
    printf -- '- %s\n' "$failure"
  done
  printf 'note: informational lanes do not affect the exit status\n'
  exit 1
fi

printf 'result: ready\n'
printf 'note: informational lanes do not affect the exit status\n'
