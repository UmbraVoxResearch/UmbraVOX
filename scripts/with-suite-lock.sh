#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -lt 2 ]; then
  echo "usage: $0 <lock-name> <command> [args...]" >&2
  exit 2
fi

lock_name="$1"
shift

lock_root="build/test-artifacts/.locks"
lock_dir="$lock_root/${lock_name}.lock"
pid_file="$lock_dir/pid"
cmd_file="$lock_dir/cmd"
started_file="$lock_dir/started_at"

mkdir -p "$lock_root"

cleanup() {
  if [ -d "$lock_dir" ] && [ -f "$pid_file" ] && [ "$(cat "$pid_file" 2>/dev/null || true)" = "$$" ]; then
    rm -rf "$lock_dir"
  fi
}

trap cleanup EXIT INT TERM

acquire_lock() {
  if mkdir "$lock_dir" 2>/dev/null; then
    printf '%s\n' "$$" > "$pid_file"
    printf '%s\n' "$*" > "$cmd_file"
    date -u +"%Y-%m-%dT%H:%M:%SZ" > "$started_file"
    return 0
  fi

  existing_pid="$(cat "$pid_file" 2>/dev/null || true)"
  existing_cmd="$(cat "$cmd_file" 2>/dev/null || true)"

  if [ -n "$existing_pid" ] && kill -0 "$existing_pid" 2>/dev/null; then
    echo "[LOCK] ${lock_name} already running under pid ${existing_pid}" >&2
    if [ -n "$existing_cmd" ]; then
      echo "[LOCK] active command: ${existing_cmd}" >&2
    fi
    return 1
  fi

  echo "[LOCK] removing stale ${lock_name} lock" >&2
  rm -rf "$lock_dir"
  mkdir "$lock_dir"
  printf '%s\n' "$$" > "$pid_file"
  printf '%s\n' "$*" > "$cmd_file"
  date -u +"%Y-%m-%dT%H:%M:%SZ" > "$started_file"
}

acquire_lock "$@" || exit 99

"$@"
