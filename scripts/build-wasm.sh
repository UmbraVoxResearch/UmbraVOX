#!/usr/bin/env bash
# Build the UmbraVox pure-crypto WASM module.
#
# This script documents the required steps for a WASM build using the GHC
# WASM backend (wasm32-wasi-ghc) provided by ghc-wasm-meta.
#
# SCAFFOLD: wasm32-wasi-ghc is not included in the project nix-shell yet.
# The cabal flag and module split in UmbraVox.cabal are ready; this script
# will work once ghc-wasm-meta is added to shell.nix.
# See doc/WASM-TARGET.md for the full design and module eligibility details.
#
# Prerequisites (not in current nix-shell):
#   wasm32-wasi-ghc    -- GHC cross-compiler targeting wasm32-wasi
#   wasm32-wasi-cabal  -- cabal-install wrapper for wasm32-wasi-ghc
#   wasm-opt           -- binaryen optimizer (optional, for size budget check)
#
# Usage (run from project root, inside nix-shell):
#   nix-shell --run "bash scripts/build-wasm.sh"

set -euo pipefail

ROOT="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
cd "$ROOT"

# ---------------------------------------------------------------------------
# Toolchain check
# ---------------------------------------------------------------------------

if ! command -v wasm32-wasi-cabal &>/dev/null; then
  cat >&2 <<'EOF'
ERROR: wasm32-wasi-cabal not found.

The GHC WASM backend is provided by ghc-wasm-meta and is not yet included in
the UmbraVox nix-shell.  To install it manually:

  nix profile install gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org#all_9_10

Or follow https://gitlab.haskell.org/ghc/ghc-wasm-meta for the latest
instructions.  Once installed, re-run this script.
EOF
  exit 1
fi

# ---------------------------------------------------------------------------
# Build: pure crypto core only (flag wasm=True excludes Network/TUI/Storage)
# ---------------------------------------------------------------------------

echo "==> Building UmbraVox pure-crypto WASM module (flag: wasm)"
# wasm32-wasi-cabal build UmbraVox -f wasm
#
# The above command is commented out because it requires wasm32-wasi-cabal.
# Uncomment and run once the toolchain is available.

# ---------------------------------------------------------------------------
# Locate the compiled WASM artifact
# ---------------------------------------------------------------------------

# WASM_OBJ=$(find dist-newstyle -name "*.wasm" 2>/dev/null | head -1)
# if [[ -z "$WASM_OBJ" ]]; then
#   echo "ERROR: no .wasm artifact found in dist-newstyle" >&2
#   exit 1
# fi

# ---------------------------------------------------------------------------
# Optional: size optimization via wasm-opt (binaryen)
# ---------------------------------------------------------------------------

# TARGET="build/umbravox-core.wasm"
# mkdir -p build
#
# if command -v wasm-opt &>/dev/null; then
#   echo "==> Optimizing with wasm-opt -Oz"
#   wasm-opt -Oz "$WASM_OBJ" -o "$TARGET"
# else
#   cp "$WASM_OBJ" "$TARGET"
#   echo "NOTE: wasm-opt not found; skipping size optimization"
# fi

# ---------------------------------------------------------------------------
# Size budget check: target < 2 MB
# ---------------------------------------------------------------------------

# SIZE=$(stat -c%s "$TARGET" 2>/dev/null || stat -f%z "$TARGET")
# SIZE_KB=$(( SIZE / 1024 ))
# BUDGET_KB=$(( 2 * 1024 ))
#
# echo "==> WASM module size: ${SIZE_KB} KB (budget: ${BUDGET_KB} KB)"
# if (( SIZE_KB > BUDGET_KB )); then
#   echo "WARNING: WASM module exceeds 2 MB size budget (${SIZE_KB} KB)" >&2
# fi
# echo "    Artifact: $TARGET"

echo "NOTE: build-wasm.sh is a scaffold."
echo "      Uncomment the build steps above once wasm32-wasi-cabal is available."
echo "      See doc/WASM-TARGET.md for details."
