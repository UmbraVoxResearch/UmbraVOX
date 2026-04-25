# UmbraVOX Build System
# =====================
# DO-178C DAL A compliant build pipeline with quality gates.
#
# Usage:
#   make              - Build all (library + executables)
#   make test         - Run full test suite (179 tests)
#   make verify       - Run F* formal verification (11 modules)
#   make quality      - Run all quality gates (tests + verify + complexity)
#   make complexity   - Check cyclomatic complexity (<= 8 for all functions)
#   make clean        - Remove build artifacts
#
# Prerequisites: nix-shell (provides GHC, Cabal, F*, Z3)

.PHONY: all build run test verify complexity quality lint codegen clean help
.DEFAULT_GOAL := all

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

SHELL := /bin/bash
MAX_COMPLEXITY := 8
FSTAR_DIR := test/evidence/formal-proofs/fstar

# Unset LD_LIBRARY_PATH to prevent curl segfaults in nix-shell
# (nix glibc conflicts with system curl used by cabal)
unexport LD_LIBRARY_PATH

# Source directories
SRC_DIRS := src/UmbraVox test/Test codegen
SRC_FILES := $(shell find $(SRC_DIRS) -name '*.hs' 2>/dev/null)

# --------------------------------------------------------------------------
# Colors
# --------------------------------------------------------------------------

RED    := \033[0;31m
GREEN  := \033[0;32m
YELLOW := \033[1;33m
BLUE   := \033[0;34m
NC     := \033[0m

# --------------------------------------------------------------------------
# Targets
# --------------------------------------------------------------------------

all: build

help:
	@echo ""
	@echo -e "$(BLUE)  UmbraVOX Build System$(NC)"
	@echo -e "$(BLUE)  =====================$(NC)"
	@echo ""
	@echo "  Build & Run:"
	@echo "    make build       Build library + executables"
	@echo "    make run         Run UmbraVOX TUI application"
	@echo "    make test        Run Haskell test suite (216 tests)"
	@echo "    make codegen     Generate Haskell + C + FFI from .spec files"
	@echo ""
	@echo "  Quality Gates (DO-178C DAL A):"
	@echo "    make verify      Run F* formal verification (all modules)"
	@echo "    make complexity  Check cyclomatic complexity (<= $(MAX_COMPLEXITY))"
	@echo "    make lint        Check code formatting and style"
	@echo "    make quality     Run ALL quality gates"
	@echo ""
	@echo "  Maintenance:"
	@echo "    make clean       Remove build artifacts"
	@echo "    make help        Show this help"
	@echo ""
	@echo "  Quality Gate Requirements:"
	@echo "    - All 216 Haskell tests pass (KAT, fuzz, security, integration)"
	@echo "    - All F* specifications verify (auto-discovered)"
	@echo "    - Cyclomatic complexity <= $(MAX_COMPLEXITY) for all functions"
	@echo "    - 10 .spec files generate 30 Haskell + C + FFI outputs"
	@echo ""
	@echo "  Keyboard Shortcuts (TUI):"
	@echo "    Tab     Switch focus (contacts <-> chat)"
	@echo "    N       New connection    S   Secure notes"
	@echo "    R       Rename contact    K   Identity & keys"
	@echo "    Ctrl+N  New connection    Ctrl+R  Verify keys"
	@echo "    Ctrl+X  Export (encrypted) Ctrl+P  Preferences"
	@echo "    Ctrl+Q  Quit             ?       Help"
	@echo ""

run: build
	@cabal run umbravox

# --------------------------------------------------------------------------
# Build
# --------------------------------------------------------------------------

build:
	@echo -e "$(BLUE)[BUILD]$(NC) Building UmbraVOX..."
	@cabal build all 2>&1 | tail -5
	@echo -e "$(GREEN)[BUILD]$(NC) Build complete."

# --------------------------------------------------------------------------
# Test
# --------------------------------------------------------------------------

test: build
	@echo -e "$(BLUE)[TEST]$(NC) Running Haskell test suite..."
	@cabal test 2>&1 | tee /tmp/umbravox-test-output.txt | grep -E "passed|FAILED|All tests"
	@if grep -q "All tests passed" /tmp/umbravox-test-output.txt; then \
		echo -e "$(GREEN)[TEST]$(NC) All tests passed."; \
	else \
		echo -e "$(RED)[TEST]$(NC) TESTS FAILED."; \
		exit 1; \
	fi

# --------------------------------------------------------------------------
# F* Formal Verification
# --------------------------------------------------------------------------

verify:
	@echo -e "$(BLUE)[VERIFY]$(NC) Running F* formal verification (all modules)..."
	@cd $(FSTAR_DIR) && bash verify.sh 2>&1 | tee /tmp/umbravox-verify-output.txt | grep -E "PASS|FAIL|Summary" --color=never
	@if grep -q "All modules verified" /tmp/umbravox-verify-output.txt; then \
		echo -e "$(GREEN)[VERIFY]$(NC) All F* modules verified."; \
	else \
		echo -e "$(RED)[VERIFY]$(NC) F* VERIFICATION FAILED."; \
		exit 1; \
	fi

# --------------------------------------------------------------------------
# Cyclomatic Complexity
# --------------------------------------------------------------------------

# Cyclomatic complexity checker for Haskell
# Measures: number of independent paths through a function
# Threshold: <= 8 (DO-178C DAL A requirement)
#
# Heuristic: count decision points (case, if, guard, pattern match alternatives)
# per top-level function definition.

complexity:
	@echo -e "$(BLUE)[COMPLEXITY]$(NC) Checking cyclomatic complexity (<= $(MAX_COMPLEXITY))..."
	@violations=0; total=0; \
	for f in $(SRC_FILES); do \
		result=$$(python3 scripts/check_complexity.py "$$f" $(MAX_COMPLEXITY) 2>/dev/null); \
		if [ $$? -ne 0 ]; then \
			echo "$$result"; \
			violations=$$((violations + 1)); \
		fi; \
		total=$$((total + 1)); \
	done; \
	if [ $$violations -gt 0 ]; then \
		echo -e "$(RED)[COMPLEXITY]$(NC) $$violations file(s) exceed complexity threshold."; \
		exit 1; \
	else \
		echo -e "$(GREEN)[COMPLEXITY]$(NC) All $$total files pass complexity check (<= $(MAX_COMPLEXITY))."; \
	fi

# --------------------------------------------------------------------------
# Lint / Style
# --------------------------------------------------------------------------

lint:
	@echo -e "$(BLUE)[LINT]$(NC) Checking code style..."
	@violations=0; \
	for f in $(SRC_FILES); do \
		if grep -Pn '\t' "$$f" > /dev/null 2>&1; then \
			echo -e "$(YELLOW)[LINT]$(NC) Tabs found in $$f"; \
			violations=$$((violations + 1)); \
		fi; \
		if grep -Pn '.{120,}' "$$f" > /dev/null 2>&1; then \
			lines=$$(grep -Pcn '.{120,}' "$$f"); \
			echo -e "$(YELLOW)[LINT]$(NC) $$lines line(s) > 120 chars in $$f"; \
			violations=$$((violations + 1)); \
		fi; \
		if [ -n "$$(tail -c 1 "$$f" 2>/dev/null)" ]; then \
			echo -e "$(YELLOW)[LINT]$(NC) Missing trailing newline in $$f"; \
			violations=$$((violations + 1)); \
		fi; \
	done; \
	if [ $$violations -gt 0 ]; then \
		echo -e "$(YELLOW)[LINT]$(NC) $$violations style issue(s) found (non-blocking)."; \
	else \
		echo -e "$(GREEN)[LINT]$(NC) All files pass style check."; \
	fi

# --------------------------------------------------------------------------
# Code Generation
# --------------------------------------------------------------------------

codegen: build
	@echo -e "$(BLUE)[CODEGEN]$(NC) Generating Haskell + C + FFI from .spec files..."
	@cabal run codegen 2>&1 | tail -20
	@echo -e "$(GREEN)[CODEGEN]$(NC) Code generation complete."

# --------------------------------------------------------------------------
# Quality Gate (all checks)
# --------------------------------------------------------------------------

quality: test verify complexity lint
	@echo ""
	@echo -e "$(GREEN)========================================$(NC)"
	@echo -e "$(GREEN)  ALL QUALITY GATES PASSED$(NC)"
	@echo -e "$(GREEN)========================================$(NC)"

# --------------------------------------------------------------------------
# Clean
# --------------------------------------------------------------------------

clean:
	@echo -e "$(BLUE)[CLEAN]$(NC) Removing build artifacts..."
	@cabal clean 2>/dev/null || true
	@rm -rf $(FSTAR_DIR)/_cache $(FSTAR_DIR)/_output
	@rm -f /tmp/umbravox-test-output.txt /tmp/umbravox-verify-output.txt
	@echo -e "$(GREEN)[CLEAN]$(NC) Done."

cleandb:
	@echo -e "$(BLUE)[CLEANDB]$(NC) Removing local database..."
	@rm -f ~/.umbravox/umbravox.db
	@echo -e "$(GREEN)[CLEANDB]$(NC) Database removed. Will be recreated on next launch."

cleanall: clean cleandb
	@echo -e "$(BLUE)[CLEANALL]$(NC) Removing all local data..."
	@rm -rf ~/.umbravox/tools
	@echo -e "$(GREEN)[CLEANALL]$(NC) All cleaned (build + DB + tools)."
