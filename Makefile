# UmbraVOX Build System
# =====================
# DO-178C DAL A compliant build pipeline with quality gates.
#
# Usage:
#   make              - Build + run the full pipeline (build, test, verify, complexity, lint, license, format-check)
#   make build        - Build library + executables only
#   make run          - Run UmbraVOX TUI application
#   make test         - Run the fast messaging-MVP hardening gate
#   make test-core    - Run the core deterministic messaging suite
#   make test-core-crypto - Run deterministic crypto/unit coverage
#   make test-core-network - Run deterministic network/discovery coverage
#   make test-core-chat - Run deterministic chat/protocol coverage
#   make test-core-tui - Run deterministic non-simulated TUI coverage
#   make test-core-tools - Run deterministic codegen/tools/fuzz coverage
#   make test-tcp     - Run real TCP hardening scenarios
#   make test-fault   - Run adversarial fault-injection scenarios
#   make test-recovery - Run persistence and recovery scenarios
#   make test-tui-sim - Run TUI simulation coverage
#   make test-integrity - Run wire/integrity coverage
#   make test-mdns    - Run the exact mDNS/discovery suite
#   make test-deferred - Run preserved deferred blockchain/economics suites
#   make soak         - Run the long soak suite and write an artifact report
#   make verify       - Run F* formal verification (17 modules)
#   make complexity   - Check cyclomatic complexity (<= 8 for all functions)
#   make lint         - Check code formatting and style
#   make license      - Check SPDX license headers in source files
#   make license-fix  - Add missing SPDX headers automatically
#   make format-check - Check for tabs and trailing whitespace
#   make codegen      - Generate Haskell + C + FFI from .spec files
#   make quality      - Run the full pipeline (same as make)
#   make release-linux - Build a portable Linux x86_64 terminal bundle
#   make release-windows-cli - Build a Windows CLI source release zip
#   make release-macos-terminal - Build a macOS terminal source release tarball
#   make release-bsd-terminal - Build a BSD terminal source release tarball
#   make release-freedos - Build a FreeDOS research/source release zip
#   make release      - Build all release artifacts
#   make evidence     - Run quality and write a publication evidence bundle
#   make clean        - Remove build artifacts, build/, and dist-newstyle
#   make cleandb      - Remove local database
#   make cleanall     - Remove everything (build + DB + tools)
#   make help         - Show help
#
# Prerequisites: nix-shell (provides GHC, Cabal, F*, Z3)

.PHONY: all build run test test-core test-core-crypto test-core-network test-core-chat test-core-tui test-core-tools test-tcp test-fault test-recovery test-tui-sim test-integrity test-mdns test-deferred soak verify complexity quality evidence lint license license-fix format-check codegen release release-linux release-windows-cli release-macos-terminal release-bsd-terminal release-freedos release-source clean cleandb cleanall help
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

TEST_ARTIFACT_DIR := build/test-artifacts
EVIDENCE_DIR := build/evidence
RELEASE_DIR := build/releases
SUITE_LOCK := ./scripts/with-suite-lock.sh suite-gate

# --------------------------------------------------------------------------
# Targets
# --------------------------------------------------------------------------

all: build test verify complexity lint license format-check
	@echo ""
	@echo -e "$(GREEN)========================================$(NC)"
	@echo -e "$(GREEN)  ALL CHECKS PASSED$(NC)"
	@echo -e "$(GREEN)========================================$(NC)"

help:
	@echo ""
	@echo -e "$(BLUE)  UmbraVOX Build System$(NC)"
	@echo -e "$(BLUE)  =====================$(NC)"
	@echo ""
	@echo "  Build & Run:"
	@echo "    make             Build + run the full pipeline (build, test, verify, complexity, lint, license, format-check)"
	@echo "    make build       Build library + executables only"
	@echo "    make run         Run UmbraVOX TUI application"
	@echo "    make test        Run fast messaging-MVP hardening gate"
	@echo "    make test-core   Run core deterministic messaging suite"
	@echo "    make test-core-crypto Run deterministic crypto/unit coverage"
	@echo "    make test-core-network Run deterministic network/discovery coverage"
	@echo "    make test-core-chat Run deterministic chat/protocol coverage"
	@echo "    make test-core-tui Run deterministic non-simulated TUI coverage"
	@echo "    make test-core-tools Run deterministic codegen/tools/fuzz coverage"
	@echo "    make test-tcp    Run real TCP hardening suite"
	@echo "    make test-fault  Run adversarial fault-injection suite"
	@echo "    make test-recovery Run persistence and recovery suite"
	@echo "    make test-tui-sim Run TUI simulation suite"
	@echo "    make test-integrity Run wire/integrity suite"
	@echo "    make test-mdns   Run exact mDNS/discovery suite"
	@echo "    make test-deferred Run preserved deferred blockchain/economics suites"
	@echo "    make soak        Run long soak suite and write artifact report"
	@echo "    make codegen     Generate Haskell + C + FFI from .spec files"
	@echo "    make evidence    Run quality and write a publication evidence bundle"
	@echo "    make release-linux Build portable Linux x86_64 terminal bundle"
	@echo "    make release-windows-cli Build Windows CLI source release zip"
	@echo "    make release-macos-terminal Build macOS terminal source release tarball"
	@echo "    make release-bsd-terminal Build BSD terminal source release tarball"
	@echo "    make release-freedos Build FreeDOS research/source release zip"
	@echo "    make release     Build all release artifacts"
	@echo ""
	@echo "  Quality Gates:"
	@echo "    make verify      Run F* formal verification (17 modules)"
	@echo "    make complexity  Check cyclomatic complexity (<= $(MAX_COMPLEXITY))"
	@echo "    make lint        Check code formatting and style"
	@echo "    make license     Check SPDX license headers in source files"
	@echo "    make license-fix Add missing SPDX headers automatically"
	@echo "    make format-check Check for tabs and trailing whitespace"
	@echo "    make quality     Same as make (lint/format-check are non-blocking)"
	@echo ""
	@echo "  Maintenance:"
	@echo "    make clean       Remove build artifacts + build/ + dist-newstyle"
	@echo "    make cleandb     Remove local database"
	@echo "    make cleanall    Remove everything (build + DB + tools)"
	@echo "    make help        Show this help"
	@echo ""
	@echo "  Quality Gate Requirements:"
	@echo "    - Fast gate passes: core + TCP + fault + recovery suites"
	@echo "    - Deferred blockchain/economics suites stay explicit and separate"
	@echo "    - Soak suite available as a separate target with artifacts"
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
	@cabal run umbravox; stty sane echo 2>/dev/null; true

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
	@echo -e "$(BLUE)[TEST]$(NC) Running fast messaging-MVP hardening gate..."
	@$(SUITE_LOCK) bash -c 'mkdir -p $(TEST_ARTIFACT_DIR); \
	log_file=$$(mktemp "$(TEST_ARTIFACT_DIR)/test-required.XXXXXX.log"); \
	echo -e "$(BLUE)[TEST]$(NC) Log: $$log_file"; \
	set -o pipefail; \
	cabal test umbravox-test --test-options="required" 2>&1 | tee "$$log_file"; \
	status=$${PIPESTATUS[0]}; \
	if [ $$status -ne 0 ]; then \
		echo -e "$(RED)[TEST]$(NC) Gate command failed. See $$log_file"; \
		exit $$status; \
	fi; \
	if grep -q "All tests passed\\." "$$log_file"; then \
		echo -e "$(GREEN)[TEST]$(NC) All tests passed."; \
	else \
		echo -e "$(RED)[TEST]$(NC) Success marker missing. See $$log_file"; \
		exit 1; \
	fi'

test-core: build
	@echo -e "$(BLUE)[TEST-CORE]$(NC) Running core deterministic suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="core"

test-core-crypto: build
	@echo -e "$(BLUE)[TEST-CORE-CRYPTO]$(NC) Running core crypto suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="core-crypto"

test-core-network: build
	@echo -e "$(BLUE)[TEST-CORE-NETWORK]$(NC) Running core network suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="core-network"

test-core-chat: build
	@echo -e "$(BLUE)[TEST-CORE-CHAT]$(NC) Running core chat/protocol suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="core-chat"

test-core-tui: build
	@echo -e "$(BLUE)[TEST-CORE-TUI]$(NC) Running core TUI suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="core-tui"

test-core-tools: build
	@echo -e "$(BLUE)[TEST-CORE-TOOLS]$(NC) Running core tools/codegen suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="core-tools"

test-tcp: build
	@echo -e "$(BLUE)[TEST-TCP]$(NC) Running real TCP suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="tcp"

test-fault: build
	@echo -e "$(BLUE)[TEST-FAULT]$(NC) Running fault-injection suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="fault"

test-recovery: build
	@echo -e "$(BLUE)[TEST-RECOVERY]$(NC) Running recovery suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="recovery"

test-tui-sim: build
	@echo -e "$(BLUE)[TEST-TUI-SIM]$(NC) Running TUI simulation suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="tui-sim"

test-integrity: build
	@echo -e "$(BLUE)[TEST-INTEGRITY]$(NC) Running wire/integrity suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="integrity"

test-mdns: build
	@echo -e "$(BLUE)[TEST-MDNS]$(NC) Running exact mDNS/discovery suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="mdns"

test-deferred: build
	@echo -e "$(BLUE)[TEST-DEFERRED]$(NC) Running preserved deferred suite..."
	@$(SUITE_LOCK) cabal test umbravox-test --test-options="deferred"

soak: build
	@echo -e "$(BLUE)[SOAK]$(NC) Running soak suite..."
	@$(SUITE_LOCK) bash -c 'mkdir -p $(TEST_ARTIFACT_DIR); \
	cabal test umbravox-test --test-options="soak" 2>&1 | tee $(TEST_ARTIFACT_DIR)/soak-report.txt'

# --------------------------------------------------------------------------
# F* Formal Verification
# --------------------------------------------------------------------------

verify:
	@echo -e "$(BLUE)[VERIFY]$(NC) Running F* formal verification (all modules)..."
	@$(SUITE_LOCK) bash -c 'mkdir -p $(TEST_ARTIFACT_DIR); \
	log_file=$$(mktemp "$(TEST_ARTIFACT_DIR)/verify.XXXXXX.log"); \
	echo -e "$(BLUE)[VERIFY]$(NC) Log: $$log_file"; \
	set -o pipefail; \
	cabal run fstar-verify 2>&1 | tee "$$log_file"; \
	status=$${PIPESTATUS[0]}; \
	if [ $$status -ne 0 ]; then \
		echo -e "$(RED)[VERIFY]$(NC) Verification command failed. See $$log_file"; \
		exit $$status; \
	fi; \
	if grep -q "All modules verified" "$$log_file"; then \
		echo -e "$(GREEN)[VERIFY]$(NC) All F* modules verified."; \
	else \
		echo -e "$(RED)[VERIFY]$(NC) Success marker missing. See $$log_file"; \
		exit 1; \
	fi'

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
		result=$$(cabal run check-complexity -- "$$f" $(MAX_COMPLEXITY) 2>/dev/null); \
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
# SPDX License Headers
# --------------------------------------------------------------------------

SPDX_ID := SPDX-License-Identifier: Apache-2.0

license:
	@echo -e "$(BLUE)[LICENSE]$(NC) Checking SPDX headers in source files..."
	@missing=0; total=0; \
	for f in $(SRC_FILES); do \
		total=$$((total + 1)); \
		if ! head -5 "$$f" | grep -q "SPDX-License-Identifier" 2>/dev/null; then \
			echo -e "$(RED)[LICENSE]$(NC) Missing SPDX header: $$f"; \
			missing=$$((missing + 1)); \
		fi; \
	done; \
	if [ $$missing -gt 0 ]; then \
		echo -e "$(RED)[LICENSE]$(NC) $$missing of $$total files missing SPDX header."; \
		echo "  Run 'make license-fix' to add headers automatically."; \
		exit 1; \
	else \
		echo -e "$(GREEN)[LICENSE]$(NC) All $$total files have SPDX headers."; \
	fi

license-fix:
	@echo -e "$(BLUE)[LICENSE]$(NC) Adding SPDX headers to source files..."
	@fixed=0; \
	for f in $(SRC_FILES); do \
		if ! head -5 "$$f" | grep -q "SPDX-License-Identifier" 2>/dev/null; then \
			sed -i '1s/^/-- $(SPDX_ID)\n/' "$$f"; \
			echo "  Added header: $$f"; \
			fixed=$$((fixed + 1)); \
		fi; \
	done; \
	echo -e "$(GREEN)[LICENSE]$(NC) Fixed $$fixed file(s)."

# --------------------------------------------------------------------------
# Code Formatting Check
# --------------------------------------------------------------------------

format-check:
	@echo -e "$(BLUE)[FORMAT]$(NC) Checking code formatting..."
	@violations=0; \
	for f in $(SRC_FILES); do \
		if grep -Pn '\t' "$$f" > /dev/null 2>&1; then \
			echo -e "$(YELLOW)[FORMAT]$(NC) Tabs found: $$f"; \
			violations=$$((violations + 1)); \
		fi; \
		if grep -Pn '\s$$' "$$f" > /dev/null 2>&1; then \
			echo -e "$(YELLOW)[FORMAT]$(NC) Trailing whitespace: $$f"; \
			violations=$$((violations + 1)); \
		fi; \
	done; \
	if [ $$violations -gt 0 ]; then \
		echo -e "$(YELLOW)[FORMAT]$(NC) $$violations formatting issue(s) found (non-blocking)."; \
	else \
		echo -e "$(GREEN)[FORMAT]$(NC) All files pass formatting check."; \
	fi

# --------------------------------------------------------------------------
# Code Generation
# --------------------------------------------------------------------------

codegen: build
	@echo -e "$(BLUE)[CODEGEN]$(NC) Generating Haskell + C + FFI from .spec files..."
	@cabal run codegen 2>&1 | tail -20
	@echo -e "$(GREEN)[CODEGEN]$(NC) Code generation complete."

# --------------------------------------------------------------------------
# Release Packaging
# --------------------------------------------------------------------------

release: release-linux release-windows-cli release-macos-terminal release-bsd-terminal release-freedos
	@echo -e "$(GREEN)[RELEASE]$(NC) All release artifacts written under $(RELEASE_DIR)"

release-source:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building generic source release..."
	@./scripts/release-package.sh source

release-linux:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building Linux x86_64 portable bundle..."
	@./scripts/release-package.sh linux

release-windows-cli:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building Windows CLI source release..."
	@./scripts/release-package.sh windows-cli

release-macos-terminal:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building macOS terminal source release..."
	@./scripts/release-package.sh macos-terminal

release-bsd-terminal:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building BSD terminal source release..."
	@./scripts/release-package.sh bsd-terminal

release-freedos:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building FreeDOS research/source release..."
	@./scripts/release-package.sh freedos

# --------------------------------------------------------------------------
# Quality Gate (all checks)
# --------------------------------------------------------------------------

quality: all

evidence:
	@echo -e "$(BLUE)[EVIDENCE]$(NC) Building publication evidence bundle..."
	@mkdir -p $(EVIDENCE_DIR)
	@ts=$$(date -u +%Y%m%dT%H%M%SZ); \
	out="$(EVIDENCE_DIR)/$$ts"; \
	mkdir -p "$$out"; \
	echo -e "$(BLUE)[EVIDENCE]$(NC) Output: $$out"; \
	set -o pipefail; \
	$(MAKE) quality 2>&1 | tee "$$out/quality.log"; \
	cp -a $(TEST_ARTIFACT_DIR) "$$out/test-artifacts" 2>/dev/null || true; \
	git rev-parse HEAD > "$$out/git-head.txt"; \
	git status -sb > "$$out/git-status.txt"; \
	{ \
		echo "timestamp_utc=$$ts"; \
		echo "quality_log=$$out/quality.log"; \
		echo "test_artifacts=$$out/test-artifacts"; \
		echo "git_head=$$(cat $$out/git-head.txt)"; \
	} > "$$out/manifest.txt"; \
	ln -sfn "$$ts" "$(EVIDENCE_DIR)/latest"; \
	echo -e "$(GREEN)[EVIDENCE]$(NC) Bundle complete: $$out"

# --------------------------------------------------------------------------
# Clean
# --------------------------------------------------------------------------

clean:
	@echo -e "$(BLUE)[CLEAN]$(NC) Removing build artifacts..."
	@cabal clean 2>/dev/null || true
	@rm -rf build
	@rm -rf dist-newstyle
	@rm -rf $(FSTAR_DIR)/_cache $(FSTAR_DIR)/_output
	@echo -e "$(GREEN)[CLEAN]$(NC) Done."

cleandb:
	@echo -e "$(BLUE)[CLEANDB]$(NC) Removing local database..."
	@rm -f ~/.umbravox/umbravox.db
	@echo -e "$(GREEN)[CLEANDB]$(NC) Database removed. Will be recreated on next launch."

cleanall: clean cleandb
	@echo -e "$(BLUE)[CLEANALL]$(NC) Removing all local data..."
	@rm -rf ~/.umbravox/tools
	@echo -e "$(GREEN)[CLEANALL]$(NC) All cleaned (build + DB + tools)."
