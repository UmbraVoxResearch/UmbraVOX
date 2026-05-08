#!/usr/bin/env bash
set -euo pipefail

ROOT="${UMBRAVOX_ROOT:-$(pwd)}"
mode="${1:-qemu}"
FIRECRACKER_TEMP_CONFIG=""

die() {
  echo "error: $*" >&2
  exit 1
}

require_dir() {
  local path="$1"
  local label="$2"
  [[ -d "$path" ]] || die "$label not found: $path"
}

require_file() {
  local path="$1"
  local label="$2"
  [[ -f "$path" ]] || die "$label not found: $path"
}

require_readable_file() {
  local path="$1"
  local label="$2"
  require_file "$path" "$label"
  [[ -r "$path" ]] || die "$label not readable: $path"
}

require_command() {
  local command_name="$1"
  local error_message="$2"
  command -v "$command_name" >/dev/null 2>&1 || die "$error_message"
}

cleanup_temp_files() {
  if [[ -n "${FIRECRACKER_TEMP_CONFIG:-}" ]] && [[ -e "$FIRECRACKER_TEMP_CONFIG" ]]; then
    rm -f -- "$FIRECRACKER_TEMP_CONFIG"
  fi
}

trap cleanup_temp_files EXIT

require_dir "$ROOT" "UMBRAVOX_ROOT"
cd "$ROOT"

check_artifact() {
  local latest=""
  local latest_mtime=-1
  local mtime
  local artifact
  local artifacts=()

  shopt -s nullglob
  artifacts=(build/releases/umbravox-*-linux-x86_64.tar.gz)
  shopt -u nullglob

  ((${#artifacts[@]} > 0)) || die "no linux release artifact found under build/releases; run make release-linux first"

  for artifact in "${artifacts[@]}"; do
    mtime="$(stat -c '%Y' "$artifact")" || die "unable to stat release artifact: $artifact"
    if ((mtime > latest_mtime)); then
      latest="$artifact"
      latest_mtime="$mtime"
    fi
  done

  [[ -n "$latest" ]] || die "no linux release artifact found under build/releases; run make release-linux first"
  echo "artifact: $latest"
}

if (($# > 1)); then
  echo "usage: $0 <qemu|firecracker>" >&2
  exit 2
fi

qemu_boot_smoke() {
  : "${UMBRAVOX_QEMU_KERNEL:?set UMBRAVOX_QEMU_KERNEL to a Linux kernel image path}"
  : "${UMBRAVOX_QEMU_INITRD:?set UMBRAVOX_QEMU_INITRD to an initrd path}"
  : "${UMBRAVOX_QEMU_ROOTFS:?set UMBRAVOX_QEMU_ROOTFS to a rootfs image path}"
  require_file "$UMBRAVOX_QEMU_KERNEL" "QEMU kernel image"
  require_file "$UMBRAVOX_QEMU_INITRD" "QEMU initrd"
  require_file "$UMBRAVOX_QEMU_ROOTFS" "QEMU rootfs image"
  if [[ -z "${UMBRAVOX_QEMU_APPEND:-}" ]]; then
    if [[ -n "${UMBRAVOX_QEMU_PROFILE:-}" ]]; then
      require_file "$ROOT/scripts/release-smoke-qemu-profile.sh" "QEMU profile helper"
      UMBRAVOX_QEMU_APPEND="$(bash "$ROOT/scripts/release-smoke-qemu-profile.sh" "$UMBRAVOX_QEMU_PROFILE")"
    else
      : "${UMBRAVOX_QEMU_APPEND:?set UMBRAVOX_QEMU_APPEND, or set UMBRAVOX_QEMU_PROFILE to a deterministic profile}"
    fi
  fi
  [[ -n "$UMBRAVOX_QEMU_APPEND" ]] || die "QEMU kernel command line is empty"

  qemu-system-x86_64 \
    -machine q35,accel=kvm \
    -cpu max \
    -m "${UMBRAVOX_QEMU_MEM_MB:-1024}" \
    -smp "${UMBRAVOX_QEMU_CPUS:-2}" \
    -nographic \
    -nodefaults \
    -no-reboot \
    -kernel "$UMBRAVOX_QEMU_KERNEL" \
    -initrd "$UMBRAVOX_QEMU_INITRD" \
    -append "$UMBRAVOX_QEMU_APPEND" \
    -drive "if=virtio,format=raw,file=${UMBRAVOX_QEMU_ROOTFS},readonly=on"
}

firecracker_boot_smoke() {
  local missing=()
  local root_drive_count=""
  local root_drive_index=""

  [[ -n "${UMBRAVOX_FIRECRACKER_KERNEL:-}" ]] || missing+=("UMBRAVOX_FIRECRACKER_KERNEL")
  [[ -n "${UMBRAVOX_FIRECRACKER_ROOTFS:-}" ]] || missing+=("UMBRAVOX_FIRECRACKER_ROOTFS")
  [[ -n "${UMBRAVOX_FIRECRACKER_CONFIG:-}" ]] || missing+=("UMBRAVOX_FIRECRACKER_CONFIG")
  if ((${#missing[@]} > 0)); then
    die "incomplete Firecracker pinned-boot inputs: missing ${missing[*]}; set all of UMBRAVOX_FIRECRACKER_KERNEL, UMBRAVOX_FIRECRACKER_ROOTFS, and UMBRAVOX_FIRECRACKER_CONFIG, or unset them all to keep scaffold behavior"
  fi

  require_readable_file "$UMBRAVOX_FIRECRACKER_KERNEL" "Firecracker kernel image"
  require_readable_file "$UMBRAVOX_FIRECRACKER_ROOTFS" "Firecracker rootfs image"
  require_readable_file "$UMBRAVOX_FIRECRACKER_CONFIG" "Firecracker config"
  require_command jq "jq not available; install jq to validate and pin Firecracker config inputs"

  if ! jq -e 'type == "object"' "$UMBRAVOX_FIRECRACKER_CONFIG" >/dev/null; then
    die "Firecracker config is not a valid JSON object: $UMBRAVOX_FIRECRACKER_CONFIG"
  fi
  if ! jq -e 'has("boot-source") and (.["boot-source"] | type == "object")' "$UMBRAVOX_FIRECRACKER_CONFIG" >/dev/null; then
    die "Firecracker config must contain an object-valued \"boot-source\" entry: $UMBRAVOX_FIRECRACKER_CONFIG"
  fi
  if ! jq -e 'has("drives") and (.drives | type == "array") and (.drives | length > 0)' "$UMBRAVOX_FIRECRACKER_CONFIG" >/dev/null; then
    die "Firecracker config must contain a non-empty \"drives\" array: $UMBRAVOX_FIRECRACKER_CONFIG"
  fi

  root_drive_count="$(jq -r '[.drives[] | select(.is_root_device == true)] | length' "$UMBRAVOX_FIRECRACKER_CONFIG")" || die "unable to inspect Firecracker root drive entries: $UMBRAVOX_FIRECRACKER_CONFIG"
  if [[ "$root_drive_count" != "1" ]]; then
    die "Firecracker config must mark exactly one drive with is_root_device=true; found $root_drive_count in $UMBRAVOX_FIRECRACKER_CONFIG"
  fi

  root_drive_index="$(jq -er '.drives | to_entries | map(select(.value.is_root_device == true)) | .[0].key' "$UMBRAVOX_FIRECRACKER_CONFIG")" || die "unable to resolve Firecracker root drive index: $UMBRAVOX_FIRECRACKER_CONFIG"
  if ! jq -e --argjson root_drive_index "$root_drive_index" '.drives[$root_drive_index].drive_id | type == "string" and length > 0' "$UMBRAVOX_FIRECRACKER_CONFIG" >/dev/null; then
    die "Firecracker root drive entry must define a non-empty drive_id: $UMBRAVOX_FIRECRACKER_CONFIG"
  fi

  FIRECRACKER_TEMP_CONFIG="$(mktemp "${TMPDIR:-/tmp}/umbravox-firecracker-config.XXXXXX.json")" || die "unable to create temporary Firecracker config"
  if ! jq \
    --arg kernel "$UMBRAVOX_FIRECRACKER_KERNEL" \
    --arg rootfs "$UMBRAVOX_FIRECRACKER_ROOTFS" \
    --argjson root_drive_index "$root_drive_index" \
    '."boot-source".kernel_image_path = $kernel
     | .drives[$root_drive_index].path_on_host = $rootfs' \
    "$UMBRAVOX_FIRECRACKER_CONFIG" >"$FIRECRACKER_TEMP_CONFIG"; then
    die "unable to render pinned Firecracker config from $UMBRAVOX_FIRECRACKER_CONFIG"
  fi

  echo "using Firecracker base config: $UMBRAVOX_FIRECRACKER_CONFIG"
  echo "pinned Firecracker kernel: $UMBRAVOX_FIRECRACKER_KERNEL"
  echo "pinned Firecracker rootfs: $UMBRAVOX_FIRECRACKER_ROOTFS"
  firecracker --config-file "$FIRECRACKER_TEMP_CONFIG"
}

case "$mode" in
  qemu)
    if ! command -v qemu-system-x86_64 >/dev/null 2>&1; then
      die "qemu-system-x86_64 not available; install QEMU for microVM smoke lane"
    fi
    if [[ ! -e /dev/kvm ]]; then
      die "/dev/kvm not present; QEMU smoke lane requires KVM-capable host"
    fi
    check_artifact
    if [[ -n "${UMBRAVOX_QEMU_SMOKE_RUNNER:-}" ]]; then
      echo "running QEMU smoke runner command from UMBRAVOX_QEMU_SMOKE_RUNNER"
      # Intended usage: provide a host-specific command that boots a prepared
      # guest image and performs in-guest bundle checks.
      bash -lc "$UMBRAVOX_QEMU_SMOKE_RUNNER"
      exit 0
    fi
    if [[ -n "${UMBRAVOX_QEMU_KERNEL:-}" ]] || [[ -n "${UMBRAVOX_QEMU_INITRD:-}" ]] || [[ -n "${UMBRAVOX_QEMU_ROOTFS:-}" ]] || [[ -n "${UMBRAVOX_QEMU_APPEND:-}" ]] || [[ -n "${UMBRAVOX_QEMU_PROFILE:-}" ]]; then
      echo "running QEMU pinned-boot smoke path from UMBRAVOX_QEMU_* inputs"
      qemu_boot_smoke
      exit 0
    fi
    cat <<'EOF'
QEMU microVM smoke scaffold
- prerequisites satisfied
- to execute in-guest checks now, set UMBRAVOX_QEMU_SMOKE_RUNNER to a host-specific boot-and-check command
- or set UMBRAVOX_QEMU_KERNEL, UMBRAVOX_QEMU_INITRD, UMBRAVOX_QEMU_ROOTFS, and UMBRAVOX_QEMU_APPEND for pinned-boot execution
- optional deterministic profile path: set UMBRAVOX_QEMU_PROFILE (uses scripts/release-smoke-qemu-profile.sh)
- default behavior remains scaffold-only until pinned guest boot wiring is configured
EOF
    ;;
  firecracker)
    if ! command -v firecracker >/dev/null 2>&1; then
      die "firecracker not available; install Firecracker for microVM smoke lane"
    fi
    if [[ ! -e /dev/kvm ]]; then
      die "/dev/kvm not present; Firecracker smoke lane requires KVM-capable host"
    fi
    check_artifact
    if [[ -n "${UMBRAVOX_FIRECRACKER_SMOKE_RUNNER:-}" ]]; then
      echo "running Firecracker smoke runner command from UMBRAVOX_FIRECRACKER_SMOKE_RUNNER"
      bash -lc "$UMBRAVOX_FIRECRACKER_SMOKE_RUNNER"
      exit 0
    fi
    if [[ -n "${UMBRAVOX_FIRECRACKER_KERNEL:-}" ]] || [[ -n "${UMBRAVOX_FIRECRACKER_ROOTFS:-}" ]] || [[ -n "${UMBRAVOX_FIRECRACKER_CONFIG:-}" ]]; then
      echo "running Firecracker pinned-boot smoke path from UMBRAVOX_FIRECRACKER_* inputs"
      firecracker_boot_smoke
      exit 0
    fi
    cat <<'EOF'
Firecracker microVM smoke scaffold
- prerequisites satisfied
- to execute in-guest checks now, set UMBRAVOX_FIRECRACKER_SMOKE_RUNNER to a host-specific boot-and-check command
- or set UMBRAVOX_FIRECRACKER_KERNEL, UMBRAVOX_FIRECRACKER_ROOTFS, and UMBRAVOX_FIRECRACKER_CONFIG for pinned-boot execution
- default behavior remains scaffold-only until pinned microVM boot wiring is configured
EOF
    ;;
  *)
    echo "usage: $0 <qemu|firecracker>" >&2
    exit 2
    ;;
esac
