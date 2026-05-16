#!/usr/bin/env bash
# Shared TUI scenario: launch umbravox in tmux, navigate key states, capture frames.
# Called by vm-screenshot-capture.sh, vm-record-session.sh, and vm-visual-regression.sh.
#
# Captures:
#   01-initial-screen      — R1.6: full-screen parity baseline
#   02-help-overlay        — modal rendering
#   03-prefs-dialog        — F2 Prefs menu + dialog
#   04-identity-panel      — key info visible (QR, safety number, fingerprints)
#   05-key-info-hidden     — toggle key info off (contacts fill the pane)
#   06-contacts-toolbar    — contacts toolbar visible (New/Rename/Browse/Verify)
#   07-chat-focused        — R1.4.a.4: chat pane focus, scrollbar, input area
#   08-rich-toolbar        — rich text toolbar (Bold/Italic/Color/Link/Emoji)
set -euo pipefail
OUTDIR="${1:-/work/screenshots}"
mkdir -p "$OUTDIR"
export TERM=xterm-256color

# Find the built TUI binary
UMBRAVOX=$(find /work/umbravox/dist-newstyle -name umbravox -type f -path '*/x/umbravox/build/*' 2>/dev/null | head -1)
if [ -z "$UMBRAVOX" ]; then
    echo "ERROR: umbravox binary not found in dist-newstyle"
    echo "SCENARIO_COMPLETE=NO"
    exit 1
fi

tmux new-session -d -s cap -x 120 -y 40 "$UMBRAVOX --ephemeral --no-config"
sleep 2  # wait for TUI startup

capture() {
    sleep 0.3
    tmux capture-pane -t cap -p -e > "$OUTDIR/$1.ansi"
    echo "  captured: $1"
}

# 01: Initial screen — full-screen parity baseline (R1.6)
capture "01-initial-screen"

# 02: Help overlay
tmux send-keys -t cap F1; capture "02-help-overlay"
tmux send-keys -t cap Escape; sleep 0.2

# 03: Prefs dialog (F2)
tmux send-keys -t cap F2; sleep 0.3
tmux send-keys -t cap Enter; capture "03-prefs-dialog"
tmux send-keys -t cap Escape; sleep 0.2

# 04: Identity panel visible (toggle key info on if not already)
# Press the Toggle Key Info item in the F3 Identity menu
tmux send-keys -t cap F3; sleep 0.3
# Navigate to "Toggle Key Info" (item 3, index 3)
tmux send-keys -t cap Down; sleep 0.1
tmux send-keys -t cap Down; sleep 0.1
tmux send-keys -t cap Down; sleep 0.1
tmux send-keys -t cap Enter; sleep 0.5
capture "04-identity-panel"

# 05: Key info hidden (toggle off)
tmux send-keys -t cap F3; sleep 0.3
tmux send-keys -t cap Down; sleep 0.1
tmux send-keys -t cap Down; sleep 0.1
tmux send-keys -t cap Down; sleep 0.1
tmux send-keys -t cap Enter; sleep 0.5
capture "05-key-info-hidden"

# 06: Contacts toolbar visible (with key info hidden, toolbar is at bottom)
capture "06-contacts-toolbar"

# 07: Chat pane focused (Tab to switch panes — R1.4.a.4)
tmux send-keys -t cap Tab; sleep 0.3
capture "07-chat-focused"

# 08: Rich text toolbar (type something to show input area activity)
tmux send-keys -t cap 'Hello world'; sleep 0.3
capture "08-rich-toolbar"

# Quit
tmux send-keys -t cap C-q; sleep 0.5
tmux kill-session -t cap 2>/dev/null || true
echo "SCENARIO_COMPLETE=YES"
