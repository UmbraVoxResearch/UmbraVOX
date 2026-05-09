# UmbraVOX Build System
# =====================
# DO-178C DAL A compliant build pipeline with quality gates.
#
# Usage:
#   make              - Build + run the full pipeline (build, test, verify, complexity, lint, license, format-check)
#   make build        - Build library + executables only
#   make build-haskell - Opt-in bridge wrapper for Haskell build orchestration
#   make run          - Run UmbraVOX TUI application
#   make test         - Run the fast messaging-MVP hardening gate
#   make test-haskell  - Opt-in bridge wrapper for Haskell test orchestration
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
#   make verify-haskell - Opt-in bridge wrapper for Haskell verification orchestration
#   make complexity   - Check cyclomatic complexity (<= 8 for all functions)
#   make lint         - Check code formatting and style
#   make license      - Check SPDX license headers in source files
#   make license-fix  - Add missing SPDX headers automatically
#   make release-compliance - Run placeholder SBOM/license compliance gates
#   make release-sbom - Check SBOM tooling presence for future artifact generation
#   make release-license-bundle - Check license-bundle tooling presence for future artifact generation
#   make format-check - Check for tabs and trailing whitespace
#   make codegen      - Generate Haskell + C + FFI from .spec files
#   make quality      - Run the full pipeline (same as make)
#   make release-linux - Build a portable Linux x86_64 terminal bundle
#   make release-appimage - Build an experimental Linux AppImage scaffold
#   make release-smoke-linux - Run isolated Linux bundle smoke check (podman/docker if available)
#   make release-smoke-appimage - Run non-authoritative AppImage scaffold smoke placeholder
#   make release-smoke-qemu - Run QEMU microVM Linux bundle smoke scaffold
#   make release-smoke-qemu-profile - Run QEMU microVM smoke using UMBRAVOX_QEMU_PROFILE (default: bundle-basic)
#   make release-smoke-firecracker - Run Firecracker microVM Linux bundle smoke scaffold
#   make release-smoke-firecracker-pinned - Run Firecracker microVM smoke using pinned-input variables
#   make release-windows-cli - Build a Windows CLI source release zip
#   make release-macos-terminal - Build a macOS terminal source release tarball
#   make release-bsd-terminal - Build a BSD terminal source release tarball
#   make release-freedos - Build a FreeDOS research/source release zip
#   make release      - Build all release artifacts
#   make platform-lane-qemu - Run the QEMU release-lane host prerequisite script
#   make platform-lane-firecracker - Run the Firecracker release-lane host prerequisite script
#   make platform-smoke-qemu-profile - Print the deterministic QEMU smoke append profile
#   make platform-sanity - Check Makefile wiring for platform lane scripts/helpers
#   make release-lane-qemu - Validate QEMU/KVM release-lane host prerequisites
#   make release-lane-firecracker - Validate Firecracker release-lane host prerequisites
#   make release-lane-readiness - Run aggregate native release-lane readiness checks
#   make release-lane-readiness-haskell - Run the Haskell bridge for readiness checks
#   make sanity       - Check Makefile wiring for release smoke/microVM helpers
#   make evidence     - Run quality and write a publication evidence bundle
#   make clean        - Remove build artifacts, build/, and dist-newstyle
#   make cleandb      - Remove local database
#   make cleanall     - Remove everything (build + DB + tools)
#   make help         - Show help
#
# Prerequisites: nix-shell (provides GHC, Cabal, F*, Z3)

.PHONY: all build build-haskell run test test-haskell test-core test-core-crypto test-core-network test-core-chat test-core-tui test-core-tools test-tcp test-fault test-recovery test-tui-sim test-integrity test-mdns test-deferred soak verify verify-haskell complexity quality evidence lint license license-fix release-compliance release-sbom release-license-bundle format-check codegen release release-linux release-appimage release-smoke-linux release-smoke-appimage release-smoke-qemu release-smoke-qemu-profile release-smoke-firecracker release-smoke-firecracker-pinned release-smoke-qemu-nix platform-lane-qemu platform-lane-firecracker platform-smoke-qemu-profile platform-sanity release-lane-qemu release-lane-firecracker release-lane-readiness release-lane-readiness-haskell release-gate-assurance release-windows-cli release-macos-terminal release-bsd-terminal release-freedos release-source sanity vm-smoke vm-image-build vm-image-clean image-clean firecracker-smoke firecracker-image-build release-sbom-generate release-license-bundle-generate release-license-check release-linking release-manifest release-checksums clean cleandb cleanall help
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
TEST_REQUIRED_TIMEOUT ?= 25m
UMBRAVOX_USE_HASKELL_ORCH ?= 0
QEMU_SMOKE_PROFILE ?= bundle-basic
FIRECRACKER_SMOKE_KERNEL ?=
FIRECRACKER_SMOKE_ROOTFS ?=
FIRECRACKER_SMOKE_CONFIG ?=

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
	@echo "    make release-appimage Build experimental Linux AppImage scaffold"
	@echo "    make release-smoke-linux Run isolated Linux bundle smoke check"
	@echo "    make release-smoke-appimage Run non-authoritative AppImage scaffold smoke placeholder"
	@echo "    make release-smoke-qemu Run QEMU microVM Linux bundle smoke scaffold"
	@echo "    make release-smoke-qemu-profile Run QEMU microVM smoke with deterministic profile (QEMU_SMOKE_PROFILE=$(QEMU_SMOKE_PROFILE))"
	@echo "    make release-smoke-firecracker Run Firecracker microVM Linux bundle smoke scaffold"
	@echo "    make release-smoke-firecracker-pinned Run Firecracker microVM smoke with pinned inputs"
	@echo "    make platform-lane-qemu Run QEMU release-lane prerequisite script"
	@echo "    make platform-lane-firecracker Run Firecracker release-lane prerequisite script"
	@echo "    make platform-smoke-qemu-profile Print deterministic QEMU smoke profile append"
	@echo "    make release-lane-qemu Validate QEMU/KVM release-lane prerequisites"
	@echo "    make release-lane-firecracker Validate Firecracker release-lane prerequisites"
	@echo "    make release-lane-readiness Run aggregate native release-lane readiness checks"
	@echo "    make release-lane-readiness-haskell Run the Haskell bridge for readiness checks"
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
	@echo "    make release-compliance Run placeholder SBOM/license compliance gates"
	@echo "    make release-sbom Check SBOM tooling presence for future generation"
	@echo "    make release-license-bundle Check license-bundle tooling presence for future generation"
	@echo "    make release-sbom-generate Generate SBOM for release artifacts"
	@echo "    make release-license-bundle-generate Generate third-party license bundle"
	@echo "    make release-license-check Check dependency license policy"
	@echo "    make release-linking Analyze static/dynamic linking obligations"
	@echo "    make release-manifest Generate release provenance manifest"
	@echo "    make release-checksums Emit SHA-256 checksums for release artifacts"
	@echo "    make format-check Check for tabs and trailing whitespace"
	@echo "    make release-gate-assurance Run assurance matrix freshness gate"
	@echo "    make quality     Same as make (lint/format-check are non-blocking)"
	@echo ""
	@echo "  VM Smoke (isolated build/test):"
	@echo "    make vm-smoke       Run full pipeline inside isolated QEMU VM"
	@echo "    make vm-image-build Build and cache the NixOS VM image"
	@echo "    make vm-image-clean Remove the cached VM image"
	@echo "    make image-clean    Alias for vm-image-clean"
	@echo "    make firecracker-smoke  Run pipeline inside Firecracker VM"
	@echo "    make firecracker-image-build Build Firecracker image"
	@echo ""
	@echo "  Maintenance:"
	@echo "    make clean       Remove build artifacts + build/ + dist-newstyle"
	@echo "    make cleandb     Remove local database"
	@echo "    make cleanall    Remove everything (build + DB + tools)"
	@echo "    make platform-sanity Check Makefile wiring for platform lane scripts/helpers"
	@echo "    make sanity      Check Makefile wiring for release smoke/microVM helpers"
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

build-haskell:
	@echo -e "$(BLUE)[BUILD-HASKELL]$(NC) Opt-in bridge wrapper: legacy Make build by default; set UMBRAVOX_USE_HASKELL_ORCH=1 to try the Haskell subcommand."
	@if [ "$(UMBRAVOX_USE_HASKELL_ORCH)" = "1" ]; then \
		cabal run umbravox -- build --orchestrated; \
	else \
		$(MAKE) build; \
	fi

# --------------------------------------------------------------------------
# Test
# --------------------------------------------------------------------------

test: build
	@echo -e "$(BLUE)[TEST]$(NC) Running fast messaging-MVP hardening gate..."
	@$(SUITE_LOCK) bash -c 'mkdir -p $(TEST_ARTIFACT_DIR); \
	log_file=$$(mktemp "$(TEST_ARTIFACT_DIR)/test-required.XXXXXX.log"); \
	echo -e "$(BLUE)[TEST]$(NC) Log: $$log_file"; \
	echo -e "$(BLUE)[TEST]$(NC) Timeout: $(TEST_REQUIRED_TIMEOUT)"; \
	set -o pipefail; \
	timeout --foreground $(TEST_REQUIRED_TIMEOUT) cabal test umbravox-test --test-options="required" 2>&1 | tee "$$log_file"; \
	status=$${PIPESTATUS[0]}; \
	if [ $$status -eq 124 ]; then \
		echo -e "$(RED)[TEST]$(NC) Gate timed out after $(TEST_REQUIRED_TIMEOUT). See $$log_file"; \
		exit 124; \
	fi; \
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

test-haskell:
	@echo -e "$(BLUE)[TEST-HASKELL]$(NC) Opt-in bridge wrapper: legacy Make test by default; set UMBRAVOX_USE_HASKELL_ORCH=1 to try the Haskell subcommand."
	@if [ "$(UMBRAVOX_USE_HASKELL_ORCH)" = "1" ]; then \
		cabal run umbravox -- test --orchestrated; \
	else \
		$(MAKE) test; \
	fi

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

verify-haskell:
	@echo -e "$(BLUE)[VERIFY-HASKELL]$(NC) Opt-in bridge wrapper: legacy Make verify by default; set UMBRAVOX_USE_HASKELL_ORCH=1 to try the Haskell subcommand."
	@if [ "$(UMBRAVOX_USE_HASKELL_ORCH)" = "1" ]; then \
		cabal run umbravox -- verify --orchestrated; \
	else \
		$(MAKE) verify; \
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
# SBOM / License Bundle Placeholder Gates
# --------------------------------------------------------------------------

SBOM_TOOL ?= syft
LICENSE_BUNDLE_TOOL ?= reuse

release-compliance: release-sbom release-license-bundle
	@echo -e "$(GREEN)[COMPLIANCE]$(NC) Placeholder compliance gates passed; no SBOM or license bundle artifact was generated."

release-sbom:
	@echo -e "$(BLUE)[COMPLIANCE]$(NC) Checking SBOM gate prerequisites..."
	@if ! command -v $(SBOM_TOOL) >/dev/null 2>&1; then \
		echo -e "$(RED)[COMPLIANCE]$(NC) Missing required tool: $(SBOM_TOOL)"; \
		echo "  Add $(SBOM_TOOL) to the nix-shell or dev environment before enabling SBOM generation."; \
		exit 1; \
	fi
	@echo -e "$(GREEN)[COMPLIANCE]$(NC) SBOM gate is placeholder-only for now; no artifact emitted."

release-license-bundle:
	@echo -e "$(BLUE)[COMPLIANCE]$(NC) Checking license-bundle gate prerequisites..."
	@if ! command -v $(LICENSE_BUNDLE_TOOL) >/dev/null 2>&1; then \
		echo -e "$(RED)[COMPLIANCE]$(NC) Missing required tool: $(LICENSE_BUNDLE_TOOL)"; \
		echo "  Add $(LICENSE_BUNDLE_TOOL) to the nix-shell or dev environment before enabling license-bundle generation."; \
		exit 1; \
	fi
	@echo -e "$(GREEN)[COMPLIANCE]$(NC) License-bundle gate is placeholder-only for now; no artifact emitted."

release-sbom-generate:
	@echo -e "$(BLUE)[COMPLIANCE]$(NC) Generating SBOM..."
	@cabal run umbravox -- release-sbom-generate

release-license-bundle-generate:
	@echo -e "$(BLUE)[COMPLIANCE]$(NC) Generating third-party license bundle..."
	@cabal run umbravox -- release-license-bundle-generate

release-license-check:
	@echo -e "$(BLUE)[COMPLIANCE]$(NC) Checking license policy..."
	@cabal run umbravox -- release-license-check

release-linking:
	@echo -e "$(BLUE)[COMPLIANCE]$(NC) Analyzing linking obligations..."
	@cabal run umbravox -- release-linking

release-manifest:
	@echo -e "$(BLUE)[RELEASE]$(NC) Generating release provenance manifest..."
	@cabal run umbravox -- release-manifest

release-checksums:
	@echo -e "$(BLUE)[RELEASE]$(NC) Emitting release artifact checksums..."
	@cabal run umbravox -- release-checksums

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
# Release Smoke and Lane Checks
# --------------------------------------------------------------------------
# Haskell parity status:
#   DONE: vm-smoke, firecracker-smoke, release-sbom-generate,
#         release-license-bundle-generate, release-license-check,
#         release-linking, release-manifest, release-checksums,
#         build, test, verify, release-lane-readiness
#   PENDING: release-smoke-linux, release-smoke-appimage,
#            release-smoke-qemu, release-lane-qemu,
#            release-lane-firecracker, release-gate-assurance
# Shell targets below will be retired when Haskell parity is proven
# for the remaining items (M4.3.4).

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

release-appimage:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building experimental Linux AppImage scaffold..."
	@./scripts/release-package.sh appimage

release-smoke-linux:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running isolated Linux release smoke check..."
	@./scripts/release-smoke-linux.sh

release-smoke-appimage:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running non-authoritative AppImage scaffold smoke placeholder..."
	@./scripts/release-smoke-appimage.sh

release-smoke-qemu:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running QEMU microVM release smoke check..."
	@./scripts/release-smoke-microvm.sh qemu

release-smoke-qemu-profile:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running QEMU microVM release smoke check with deterministic profile '$(QEMU_SMOKE_PROFILE)'..."
	@UMBRAVOX_QEMU_PROFILE="$(QEMU_SMOKE_PROFILE)" ./scripts/release-smoke-microvm.sh qemu

release-smoke-firecracker:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running Firecracker microVM release smoke check..."
	@./scripts/release-smoke-microvm.sh firecracker

release-smoke-firecracker-pinned:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running Firecracker microVM release smoke check with pinned inputs..."
	@UMBRAVOX_FIRECRACKER_KERNEL="$(FIRECRACKER_SMOKE_KERNEL)" \
	UMBRAVOX_FIRECRACKER_ROOTFS="$(FIRECRACKER_SMOKE_ROOTFS)" \
	UMBRAVOX_FIRECRACKER_CONFIG="$(FIRECRACKER_SMOKE_CONFIG)" \
	./scripts/release-smoke-microvm.sh firecracker

release-smoke-qemu-nix:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running Nix-managed QEMU microVM release smoke check..."
	@./scripts/release-smoke-qemu-nix.sh

platform-lane-qemu:
	@echo -e "$(BLUE)[PLATFORM]$(NC) Running QEMU release-lane prerequisite script..."
	@./scripts/release-lane-qemu.sh

platform-lane-firecracker:
	@echo -e "$(BLUE)[PLATFORM]$(NC) Running Firecracker release-lane prerequisite script..."
	@./scripts/release-lane-firecracker.sh

platform-smoke-qemu-profile:
	@echo -e "$(BLUE)[PLATFORM]$(NC) Printing deterministic QEMU smoke profile '$(QEMU_SMOKE_PROFILE)'..."
	@./scripts/release-smoke-qemu-profile.sh "$(QEMU_SMOKE_PROFILE)"

release-lane-qemu:
	@echo -e "$(BLUE)[RELEASE]$(NC) Checking QEMU/KVM release-lane prerequisites..."
	@./scripts/release-lane-qemu.sh

release-lane-firecracker:
	@echo -e "$(BLUE)[RELEASE]$(NC) Checking Firecracker release-lane prerequisites..."
	@./scripts/release-lane-firecracker.sh

release-lane-readiness:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running aggregate native release-lane readiness checks..."
	@status=0; \
	for script in \
		./scripts/release-lane-readiness-linux-x86_64.sh \
		./scripts/release-lane-readiness-linux-arm64.sh \
		./scripts/release-lane-readiness-macos.sh \
		./scripts/release-lane-readiness-windows.sh \
		./scripts/release-lane-readiness-bsd.sh; do \
		echo ""; \
		echo -e "$(BLUE)[READINESS]$(NC) $$script"; \
		if ! "$$script"; then \
			status=1; \
		fi; \
	done; \
	exit $$status

release-lane-readiness-haskell:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running Haskell bridge for release-lane readiness..."
	@cabal run umbravox -- release-lane-readiness

release-gate-assurance:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running assurance matrix release gate..."
	@./scripts/release-gate-assurance.sh

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

platform-sanity:
	@echo -e "$(BLUE)[SANITY]$(NC) Checking Makefile platform lane wiring..."
	@test -f ./scripts/release-smoke-linux.sh
	@test -f ./scripts/release-smoke-appimage.sh
	@test -f ./scripts/release-smoke-microvm.sh
	@test -f ./scripts/release-smoke-qemu-profile.sh
	@test -f ./scripts/release-lane-qemu.sh
	@test -f ./scripts/release-lane-firecracker.sh
	@test -f ./scripts/release-lane-readiness-lib.sh
	@test -f ./scripts/release-lane-readiness-linux-x86_64.sh
	@test -f ./scripts/release-lane-readiness-linux-arm64.sh
	@test -f ./scripts/release-lane-readiness-macos.sh
	@test -f ./scripts/release-lane-readiness-windows.sh
	@test -f ./scripts/release-lane-readiness-bsd.sh
	@echo "QEMU_SMOKE_PROFILE=$(QEMU_SMOKE_PROFILE)"
	@echo "FIRECRACKER_SMOKE_KERNEL=$(if $(strip $(FIRECRACKER_SMOKE_KERNEL)),$(FIRECRACKER_SMOKE_KERNEL),<unset>)"
	@echo "FIRECRACKER_SMOKE_ROOTFS=$(if $(strip $(FIRECRACKER_SMOKE_ROOTFS)),$(FIRECRACKER_SMOKE_ROOTFS),<unset>)"
	@echo "FIRECRACKER_SMOKE_CONFIG=$(if $(strip $(FIRECRACKER_SMOKE_CONFIG)),$(FIRECRACKER_SMOKE_CONFIG),<unset>)"
	@echo -e "$(GREEN)[SANITY]$(NC) Platform lane targets/helpers are wired."

sanity:
	@echo -e "$(BLUE)[SANITY]$(NC) Checking Makefile release smoke/microVM wiring..."
	@test -f ./scripts/release-smoke-linux.sh
	@test -f ./scripts/release-smoke-appimage.sh
	@test -f ./scripts/release-smoke-microvm.sh
	@test -f ./scripts/release-lane-qemu.sh
	@test -f ./scripts/release-lane-firecracker.sh
	@test -f ./scripts/release-lane-readiness-lib.sh
	@test -f ./scripts/release-lane-readiness-linux-x86_64.sh
	@test -f ./scripts/release-lane-readiness-linux-arm64.sh
	@test -f ./scripts/release-lane-readiness-macos.sh
	@test -f ./scripts/release-lane-readiness-windows.sh
	@test -f ./scripts/release-lane-readiness-bsd.sh
	@echo "QEMU_SMOKE_PROFILE=$(QEMU_SMOKE_PROFILE)"
	@echo "FIRECRACKER_SMOKE_KERNEL=$(if $(strip $(FIRECRACKER_SMOKE_KERNEL)),$(FIRECRACKER_SMOKE_KERNEL),<unset>)"
	@echo "FIRECRACKER_SMOKE_ROOTFS=$(if $(strip $(FIRECRACKER_SMOKE_ROOTFS)),$(FIRECRACKER_SMOKE_ROOTFS),<unset>)"
	@echo "FIRECRACKER_SMOKE_CONFIG=$(if $(strip $(FIRECRACKER_SMOKE_CONFIG)),$(FIRECRACKER_SMOKE_CONFIG),<unset>)"
	@echo -e "$(GREEN)[SANITY]$(NC) Release smoke/microVM targets are wired."

# --------------------------------------------------------------------------
# VM Isolated Smoke Testing
# --------------------------------------------------------------------------

vm-smoke:
	@echo -e "$(BLUE)[VM-SMOKE]$(NC) Running isolated VM build/test/release pipeline..."
	@cabal run umbravox -- vm-smoke

vm-image-build:
	@echo -e "$(BLUE)[VM-IMAGE]$(NC) Building/caching NixOS VM image..."
	@cabal run umbravox -- vm-image-build

vm-image-clean:
	@echo -e "$(BLUE)[VM-IMAGE]$(NC) Removing cached VM image..."
	@cabal run umbravox -- vm-image-clean

image-clean: vm-image-clean

# --------------------------------------------------------------------------
# Firecracker Isolated Smoke Testing
# --------------------------------------------------------------------------

firecracker-smoke:
	@echo -e "$(BLUE)[FC-SMOKE]$(NC) Running Firecracker isolated pipeline..."
	@cabal run umbravox -- firecracker-smoke

firecracker-image-build:
	@echo -e "$(BLUE)[FC-IMAGE]$(NC) Building/caching Firecracker image..."
	@cabal run umbravox -- firecracker-image-build

# --------------------------------------------------------------------------
# Clean
# --------------------------------------------------------------------------

clean:
	@echo -e "$(BLUE)[CLEAN]$(NC) Removing build artifacts..."
	@cabal clean 2>/dev/null || true
	@rm -rf build
	@rm -rf dist-newstyle
	@rm -rf $(FSTAR_DIR)/_cache $(FSTAR_DIR)/_output
	@echo -e "$(GREEN)[CLEAN]$(NC) Done. (VM image not removed; use make image-clean)"

cleandb:
	@echo -e "$(BLUE)[CLEANDB]$(NC) Removing local database..."
	@rm -f ~/.umbravox/umbravox.db
	@echo -e "$(GREEN)[CLEANDB]$(NC) Database removed. Will be recreated on next launch."

cleanall: clean cleandb
	@echo -e "$(BLUE)[CLEANALL]$(NC) Removing all local data..."
	@rm -rf ~/.umbravox/tools
	@echo -e "$(GREEN)[CLEANALL]$(NC) All cleaned (build + DB + tools)."
