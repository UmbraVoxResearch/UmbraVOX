#!/usr/bin/env bash
# Local TUI screenshot capture — no VM required.
# Builds the umbravox binary locally, launches it in tmux with a fixed
# terminal size (120x40), runs the 8 standard TUI scenarios, and captures
# each pane to ANSI files (with optional HTML conversion via aha).
#
# Usage:
#   bash scripts/tui-screenshot-local.sh
#   nix-shell --run "bash scripts/tui-screenshot-local.sh"
#
# Output: build/screenshots/*.ansi (and *.html if aha is available)
set -euo pipefail

OUTDIR="${1:-build/screenshots}"
SESSION="umbravox-cap"

# --------------------------------------------------------------------
# Preflight checks
# --------------------------------------------------------------------

if ! command -v tmux >/dev/null 2>&1; then
    echo "ERROR: tmux is required but not found."
    echo "  Run inside nix-shell, or install tmux."
    exit 1
fi

# --------------------------------------------------------------------
# Build
# --------------------------------------------------------------------

echo "[screenshot-local] Building umbravox..."
cabal build umbravox 2>&1 | tail -5

# Locate the built binary
UMBRAVOX=$(find -L dist-newstyle -name umbravox -type f -path '*/x/umbravox/build/*' 2>/dev/null | head -1)
if [ -z "$UMBRAVOX" ]; then
    echo "ERROR: umbravox binary not found in dist-newstyle"
    exit 1
fi
echo "[screenshot-local] Binary: $UMBRAVOX"

# --------------------------------------------------------------------
# Prepare output directory
# --------------------------------------------------------------------

mkdir -p "$OUTDIR"
export TERM=xterm-256color

# Kill any leftover session from a previous run
tmux kill-session -t "$SESSION" 2>/dev/null || true

# --------------------------------------------------------------------
# Launch TUI in tmux (120x40, ephemeral + no-config)
# --------------------------------------------------------------------

echo "[screenshot-local] Launching TUI in tmux (120x40)..."
tmux new-session -d -s "$SESSION" -x 120 -y 40 "$UMBRAVOX --ephemeral --no-config"
sleep 3  # wait for TUI startup (local builds may be slower)

capture() {
    sleep 0.3
    tmux capture-pane -t "$SESSION" -p -e > "$OUTDIR/$1.ansi"
    echo "  captured: $1"
}

# --------------------------------------------------------------------
# Run the 8 standard TUI scenarios
# (mirrors scripts/vm-tui-scenario.sh exactly)
# --------------------------------------------------------------------

# 01: Initial screen — full-screen parity baseline (R1.6)
capture "01-initial-screen"

# 02: Help overlay (F1)
tmux send-keys -t "$SESSION" F1; capture "02-help-overlay"
tmux send-keys -t "$SESSION" Escape; sleep 0.2

# 03: Prefs dialog (F2)
tmux send-keys -t "$SESSION" F2; sleep 0.3
tmux send-keys -t "$SESSION" Enter; capture "03-prefs-dialog"
tmux send-keys -t "$SESSION" Escape; sleep 0.2

# 04: Identity panel visible (toggle key info on)
tmux send-keys -t "$SESSION" F3; sleep 0.3
tmux send-keys -t "$SESSION" Down; sleep 0.1
tmux send-keys -t "$SESSION" Down; sleep 0.1
tmux send-keys -t "$SESSION" Down; sleep 0.1
tmux send-keys -t "$SESSION" Enter; sleep 0.5
capture "04-identity-panel"

# 05: Key info hidden (toggle off)
tmux send-keys -t "$SESSION" F3; sleep 0.3
tmux send-keys -t "$SESSION" Down; sleep 0.1
tmux send-keys -t "$SESSION" Down; sleep 0.1
tmux send-keys -t "$SESSION" Down; sleep 0.1
tmux send-keys -t "$SESSION" Enter; sleep 0.5
capture "05-key-info-hidden"

# 06: Contacts toolbar visible
capture "06-contacts-toolbar"

# 07: Chat pane focused (Tab — R1.4.a.4)
tmux send-keys -t "$SESSION" Tab; sleep 0.3
capture "07-chat-focused"

# 08: Rich text toolbar
tmux send-keys -t "$SESSION" 'Hello world'; sleep 0.3
capture "08-rich-toolbar"

# --------------------------------------------------------------------
# Tear down
# --------------------------------------------------------------------

tmux send-keys -t "$SESSION" C-q; sleep 0.5
tmux kill-session -t "$SESSION" 2>/dev/null || true

# --------------------------------------------------------------------
# Convert ANSI to HTML (optional)
# --------------------------------------------------------------------

if command -v aha >/dev/null 2>&1; then
    echo "[screenshot-local] Converting to HTML via aha..."
    for f in "$OUTDIR"/*.ansi; do
        aha -f "$f" > "${f%.ansi}.html" 2>/dev/null || true
    done
else
    echo "[screenshot-local] aha not found — skipping HTML conversion."
fi

# --------------------------------------------------------------------
# Summary
# --------------------------------------------------------------------

echo ""
echo "[screenshot-local] Screenshots saved to $OUTDIR/"
ls -la "$OUTDIR/"
count=$(ls "$OUTDIR"/*.ansi 2>/dev/null | wc -l)
echo ""
echo "SCREENSHOT_LOCAL_COMPLETE=YES ($count ANSI files)"
