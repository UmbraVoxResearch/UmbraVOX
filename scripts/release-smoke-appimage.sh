#!/usr/bin/env bash
set -euo pipefail

ROOT="${UMBRAVOX_ROOT:-$(pwd)}"
cd "$ROOT"

latest="$(ls -1t build/releases/umbravox-*-linux-x86_64-appimage-scaffold.tar.gz 2>/dev/null | head -n1 || true)"
if [[ -z "${latest:-}" ]]; then
  echo "no AppImage scaffold artifact found under build/releases; run ./uv release appimage first" >&2
  exit 1
fi

echo "AppImage smoke placeholder"
echo "Artifact: ${latest}"
echo "Status: non-authoritative scaffold-only check"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
tar -xzf "$latest" -C "$tmp"
stage_dir="$(find "$tmp" -maxdepth 1 -mindepth 1 -type d | head -n1)"
if [[ -z "${stage_dir:-}" ]]; then
  echo "scaffold archive did not unpack to a stage directory: $latest" >&2
  exit 1
fi

test -f "$stage_dir/APPIMAGE-PLACEHOLDER.txt"
test -f "$stage_dir/AppDir/AppRun"
test -f "$stage_dir/AppDir/umbravox.desktop"

echo "placeholder smoke ok: scaffold layout present, no authoritative AppImage claim made"
