#!/usr/bin/env bash
# Shared TUI scenario: launch umbravox in tmux, navigate key states, capture frames.
# Called by vm-screenshot-capture.sh and vm-record-session.sh.
set -euo pipefail
OUTDIR="${1:-/work/screenshots}"
mkdir -p "$OUTDIR"
export TERM=xterm-256color
tmux new-session -d -s cap -x 120 -y 40 '/work/umbravox/dist-newstyle/build/*/ghc-*/UmbraVox-*/x/umbravox/build/umbravox/umbravox --ephemeral --no-config'
sleep 2  # wait for TUI startup
capture() { tmux capture-pane -t cap -p -e > "$OUTDIR/$1.ansi"; }
capture "01-initial-screen"
tmux send-keys -t cap F1; sleep 0.5; capture "02-help-overlay"
tmux send-keys -t cap Escape; sleep 0.3
tmux send-keys -t cap F4; sleep 0.5; capture "03-prefs-dialog"
tmux send-keys -t cap Escape; sleep 0.3
tmux send-keys -t cap F5; sleep 0.5; capture "04-identity-menu"
tmux send-keys -t cap Escape; sleep 0.3
# Quit
tmux send-keys -t cap C-q; sleep 0.5
tmux kill-session -t cap 2>/dev/null || true
echo "SCENARIO_COMPLETE=YES"
