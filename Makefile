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
#   make test-differential - Run differential C vs Haskell tests
#   make soak         - Run the long soak suite and write an artifact report
#   make mcdc-report  - Build with HPC coverage and emit per-module expression report
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
#   make check-evidence - Run external evidence verification checks
#   make quality      - Run the full pipeline (same as make)
#   make vm-dev       - Interactive dev shell inside NixOS QEMU VM (M13.13)
#   make vm-build     - Build inside NixOS VM (cabal build all)
#   make vm-test      - Test inside NixOS VM (cabal test required)
#   make vm-verify    - F* verification inside NixOS VM
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
#   make release-freebsd - Build a FreeBSD platform release tarball (M14.4.1)
#   make release-openbsd - Build an OpenBSD platform release tarball (M14.4.2)
#   make release-netbsd  - Build a NetBSD platform release tarball (M14.4.3)
#   make release-illumos - Build an illumos/OmniOS platform release tarball (M14.4.4)
#   make release-linux-arm64 - Build a Linux arm64 platform release tarball (M14.4.5)
#   make release      - Build all release artifacts
#   make platform-lane-qemu - Run the QEMU release-lane host prerequisite script
#   make platform-lane-firecracker - Run the Firecracker release-lane host prerequisite script
#   make platform-smoke-qemu-profile - Print the deterministic QEMU smoke append profile
#   make platform-sanity - Check Makefile wiring for platform lane scripts/helpers
#   make release-lane-qemu - Validate QEMU/KVM release-lane host prerequisites
#   make release-lane-firecracker - Validate Firecracker release-lane host prerequisites
#   make release-lane-readiness - Run aggregate native release-lane readiness checks
#   make release-lane-readiness-haskell - Run the Haskell bridge for readiness checks
#   make test-shells  - Test nix-shell environments (banner + tools)
#   make test-vm      - Test NixOS development VM (boot + toolchain + source mount)
#   make sanity       - Check Makefile wiring for release smoke/microVM helpers
#   make evidence     - Run quality and write a publication evidence bundle
#   make clean        - Remove build artifacts, build/, and dist-newstyle
#   make cleandb      - Remove local database
#   make cleanall     - Remove everything (build + DB + tools)
#   make help         - Show help
#
# Prerequisites: nix-shell (provides GHC, Cabal, F*, Z3)

.PHONY: all build build-haskell run test test-haskell test-core test-core-crypto test-core-network test-core-chat test-core-tui test-core-tools test-tcp test-fault test-recovery test-tui-sim test-integrity test-mdns test-deferred test-differential soak mcdc-report verify verify-haskell complexity quality evidence check-evidence lint license license-fix release-compliance release-sbom release-license-bundle format-check codegen release release-linux release-appimage release-smoke-linux release-smoke-appimage release-smoke-qemu release-smoke-qemu-profile release-smoke-firecracker release-smoke-firecracker-pinned release-smoke-qemu-nix platform-lane-qemu platform-lane-firecracker platform-smoke-qemu-profile platform-sanity release-lane-qemu release-lane-firecracker release-lane-readiness release-lane-readiness-haskell release-gate-assurance release-windows-cli release-macos-terminal release-bsd-terminal release-freedos release-source release-freebsd release-openbsd release-netbsd release-illumos release-linux-arm64 test-shells test-vm sanity vm-smoke vm-image-build vm-image-clean vm-cache-clean vm-extract image-clean vm-dev vm-build vm-test vm-verify firecracker-smoke firecracker-image-build release-sbom-generate release-license-bundle-generate release-license-check release-linking release-manifest release-checksums test-offline-parity vm-integration-test vm-integration-test-dual-lan verify-traffic vm-forensics vm-smoke-freebsd vm-smoke-illumos vm-smoke-openbsd vm-smoke-netbsd vm-smoke-dragonfly vm-smoke-arm64 vm-socks5-test vm-screenshot vm-record vm-visual-regression visual-reference-update clean cleandb cleanall help
.DEFAULT_GOAL := all

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

SHELL := /bin/bash
MAX_COMPLEXITY := 8
FSTAR_DIR := test/evidence/formal-proofs/fstar

# VM-first development: all commands run in VM by default.
# Set UMBRAVOX_LOCAL=1 to run locally (requires full toolchain in nix-shell).
UMBRAVOX_LOCAL ?= 0

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

# Offline mode: when UMBRAVOX_OFFLINE=1, use pre-built binaries from
# dist-newstyle directly instead of going through cabal run/test.
# This is required for VM smoke testing where cabal cannot resolve
# the package index.
UMBRAVOX_OFFLINE ?= 0
# Offline/online binary resolution.
# In offline mode (UMBRAVOX_OFFLINE=1), recipes use 'find' at shell time
# to locate pre-built binaries in dist-newstyle.  In online mode, recipes
# delegate to cabal run/test which resolves the package index.
FIND_EXE = find dist-newstyle -path '*/build/$(1)/$(1)' -type f 2>/dev/null | head -1

ifeq ($(UMBRAVOX_OFFLINE),1)
  run_test_suite = $$($(call FIND_EXE,umbravox-test)) $(1)
  RUN_FSTAR = $$($(call FIND_EXE,fstar-verify))
  RUN_COMPLEXITY = $$($(call FIND_EXE,check-complexity))
  RUN_CODEGEN = $$($(call FIND_EXE,codegen))
else
  run_test_suite = cabal test umbravox-test --test-options="$(1)"
  RUN_FSTAR = cabal run fstar-verify --
  RUN_COMPLEXITY = cabal run check-complexity --
  RUN_CODEGEN = cabal run codegen --
endif
INTEGRATION_AGENTS ?= 3
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
	@echo "    make test-differential Run differential C vs Haskell tests"
	@echo "    make soak        Run long soak suite and write artifact report"
	@echo "    make mcdc-report Build with HPC coverage and emit per-module expression report"
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
	@echo "    make release-freebsd Build FreeBSD platform release tarball (M14.4.1)"
	@echo "    make release-openbsd Build OpenBSD platform release tarball (M14.4.2)"
	@echo "    make release-netbsd  Build NetBSD platform release tarball (M14.4.3)"
	@echo "    make release-illumos Build illumos/OmniOS platform release tarball (M14.4.4)"
	@echo "    make release-linux-arm64 Build Linux arm64 platform release tarball (M14.4.5)"
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
	@echo "    make check-evidence Run external evidence verification checks"
	@echo "    make release-gate-assurance Run assurance matrix freshness gate"
	@echo "    make verify-traffic Verify no plaintext in captured traffic"
	@echo "    make quality     Same as make (lint/format-check are non-blocking)"
	@echo ""
	@echo "  VM Development (M13.13 — full toolchain inside VM):"
	@echo "    make vm-dev         Interactive dev shell inside NixOS VM"
	@echo "    make vm-build       Build inside VM (cabal build all)"
	@echo "    make vm-test        Test inside VM (cabal test required)"
	@echo "    make vm-verify      F* verification inside VM"
	@echo ""
	@echo "  VM Smoke (isolated build/test):"
	@echo "    make vm-smoke       Run full pipeline inside isolated QEMU VM"
	@echo "    make vm-image-build Build and cache the NixOS VM image"
	@echo "    make vm-image-clean Remove the cached VM image"
	@echo "    make image-clean    Alias for vm-image-clean"
	@echo "    make firecracker-smoke  Run pipeline inside Firecracker VM"
	@echo "    make firecracker-image-build Build Firecracker image"
	@echo "    make vm-integration-test Run multi-VM integration test (INTEGRATION_AGENTS=3)"
	@echo "    make vm-integration-test-dual-lan Run dual-LAN integration test (6 agents)"
	@echo "    make vm-forensics   Run VM forensics verification (pcap, disk, log)"
	@echo "    make vm-smoke-freebsd Run FreeBSD 14.x QEMU VM smoke (M14.2.1 / M14.5.6)"
	@echo "    make vm-smoke-illumos Run OmniOS/illumos QEMU VM smoke (M14.2.5 / M14.5.8)"
	@echo "    make vm-smoke-openbsd Run OpenBSD 7.x QEMU VM smoke (M14.2.2)"
	@echo "    make vm-smoke-netbsd Run NetBSD 10.x QEMU VM smoke (M14.2.3)"
	@echo "    make vm-smoke-dragonfly Run DragonFlyBSD 6.x QEMU VM smoke (M14.2.4, INFO if GHC unavailable)"
	@echo "    make vm-smoke-arm64     Run Linux arm64 QEMU VM smoke via qemu-system-aarch64 (M14.5.9)"
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
	@if [ "$(UMBRAVOX_LOCAL)" != "1" ]; then $(MAKE) vm-build; exit $$?; fi
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

test:
	@if [ "$(UMBRAVOX_LOCAL)" != "1" ]; then $(MAKE) vm-test; exit $$?; fi
	@$(MAKE) UMBRAVOX_LOCAL=1 build
	@echo -e "$(BLUE)[TEST]$(NC) Running fast messaging-MVP hardening gate..."
	@$(SUITE_LOCK) bash -c 'mkdir -p $(TEST_ARTIFACT_DIR); \
	log_file=$$(mktemp "$(TEST_ARTIFACT_DIR)/test-required.XXXXXX.log"); \
	echo -e "$(BLUE)[TEST]$(NC) Log: $$log_file"; \
	echo -e "$(BLUE)[TEST]$(NC) Timeout: $(TEST_REQUIRED_TIMEOUT)"; \
	set -o pipefail; \
	timeout --foreground $(TEST_REQUIRED_TIMEOUT) $(call run_test_suite,required) 2>&1 | tee "$$log_file"; \
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
	@$(SUITE_LOCK) $(call run_test_suite,core)

test-core-crypto: build
	@echo -e "$(BLUE)[TEST-CORE-CRYPTO]$(NC) Running core crypto suite..."
	@$(SUITE_LOCK) $(call run_test_suite,core-crypto)

test-core-network: build
	@echo -e "$(BLUE)[TEST-CORE-NETWORK]$(NC) Running core network suite..."
	@$(SUITE_LOCK) $(call run_test_suite,core-network)

test-core-chat: build
	@echo -e "$(BLUE)[TEST-CORE-CHAT]$(NC) Running core chat/protocol suite..."
	@$(SUITE_LOCK) $(call run_test_suite,core-chat)

test-core-tui: build
	@echo -e "$(BLUE)[TEST-CORE-TUI]$(NC) Running core TUI suite..."
	@$(SUITE_LOCK) $(call run_test_suite,core-tui)

test-core-tools: build
	@echo -e "$(BLUE)[TEST-CORE-TOOLS]$(NC) Running core tools/codegen suite..."
	@$(SUITE_LOCK) $(call run_test_suite,core-tools)

test-tcp: build
	@echo -e "$(BLUE)[TEST-TCP]$(NC) Running real TCP suite..."
	@$(SUITE_LOCK) $(call run_test_suite,tcp)

test-fault: build
	@echo -e "$(BLUE)[TEST-FAULT]$(NC) Running fault-injection suite..."
	@$(SUITE_LOCK) $(call run_test_suite,fault)

test-recovery: build
	@echo -e "$(BLUE)[TEST-RECOVERY]$(NC) Running recovery suite..."
	@$(SUITE_LOCK) $(call run_test_suite,recovery)

test-tui-sim: build
	@echo -e "$(BLUE)[TEST-TUI-SIM]$(NC) Running TUI simulation suite..."
	@$(SUITE_LOCK) $(call run_test_suite,tui-sim)

test-integrity: build
	@echo -e "$(BLUE)[TEST-INTEGRITY]$(NC) Running wire/integrity suite..."
	@$(SUITE_LOCK) $(call run_test_suite,integrity)

test-mdns: build
	@echo -e "$(BLUE)[TEST-MDNS]$(NC) Running exact mDNS/discovery suite..."
	@$(SUITE_LOCK) $(call run_test_suite,mdns)

test-deferred: build
	@echo -e "$(BLUE)[TEST-DEFERRED]$(NC) Running preserved deferred suite..."
	@$(SUITE_LOCK) $(call run_test_suite,deferred)

test-differential: build
	@echo -e "$(BLUE)[TEST-DIFF]$(NC) Running differential C vs Haskell tests..."
	@$(SUITE_LOCK) $(call run_test_suite,differential)

soak: build
	@echo -e "$(BLUE)[SOAK]$(NC) Running soak suite..."
	@$(SUITE_LOCK) bash -c 'mkdir -p $(TEST_ARTIFACT_DIR); \
	$(call run_test_suite,soak) 2>&1 | tee $(TEST_ARTIFACT_DIR)/soak-report.txt'

# --------------------------------------------------------------------------
# MC/DC Coverage (HPC)
# --------------------------------------------------------------------------
#
# DO-178C DAL A requires Modified Condition/Decision Coverage (MC/DC).
# GHC's HPC tool provides expression-level and branch coverage, which is
# the closest structural coverage available for Haskell source code.
#
# See doc/DO-178C-COVERAGE.md for the gap analysis and plan.
#
# Build workaround: --enable-coverage triggers a full library rebuild.
# The configure step pre-creates the autogen/ directory so that parallel
# builds find cabal_macros.h before compiling C FFI sources.  No -j1 needed.

mcdc-report:
	@echo -e "$(BLUE)[COVERAGE]$(NC) Building with HPC coverage (configure + build + test)..."
	@cabal configure --enable-coverage
	@cabal build all --enable-coverage 2>&1 | tail -5
	@echo -e "$(BLUE)[COVERAGE]$(NC) Running required test suite with HPC instrumentation..."
	@cabal test umbravox-test --enable-coverage --test-options='required' 2>&1 | tail -10
	@echo -e "$(BLUE)[COVERAGE]$(NC) Generating per-module expression coverage report..."
	@tix=$$(find dist-newstyle -name '*.tix' -path '*/umbravox-test*' | head -1); \
	mix_lib=$$(find dist-newstyle -path '*/extra-compilation-artifacts/hpc/vanilla/mix' -not -path '*/umbravox-test*' | head -1); \
	mix_test=$$(find dist-newstyle -path '*/umbravox-test*/extra-compilation-artifacts/hpc/vanilla/mix' | head -1); \
	if [ -z "$$tix" ]; then \
		echo -e "$(RED)[COVERAGE]$(NC) No .tix file found — test run may have failed."; \
		exit 1; \
	fi; \
	echo -e "$(GREEN)[COVERAGE]$(NC) Tix: $$tix"; \
	hpc report "$$tix" --per-module \
		$$([ -n "$$mix_lib" ] && echo "--hpcdir=$$mix_lib") \
		$$([ -n "$$mix_test" ] && echo "--hpcdir=$$mix_test"); \
	echo -e "$(GREEN)[COVERAGE]$(NC) HTML report: $$(find dist-newstyle -name 'hpc_index.html' | head -1)"

# --------------------------------------------------------------------------
# F* Formal Verification
# --------------------------------------------------------------------------

verify:
	@if [ "$(UMBRAVOX_LOCAL)" != "1" ]; then $(MAKE) vm-verify; exit $$?; fi
	@echo -e "$(BLUE)[VERIFY]$(NC) Running F* formal verification (all modules)..."
	@$(SUITE_LOCK) bash -c 'mkdir -p $(TEST_ARTIFACT_DIR); \
	log_file=$$(mktemp "$(TEST_ARTIFACT_DIR)/verify.XXXXXX.log"); \
	echo -e "$(BLUE)[VERIFY]$(NC) Log: $$log_file"; \
	set -o pipefail; \
	$(RUN_FSTAR) 2>&1 | tee "$$log_file"; \
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
		result=$$($(RUN_COMPLEXITY) "$$f" $(MAX_COMPLEXITY) 2>/dev/null); \
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
	@$(RUN_CODEGEN) 2>&1 | tail -20
	@echo -e "$(GREEN)[CODEGEN]$(NC) Code generation complete."

# --------------------------------------------------------------------------
# Release Smoke and Lane Checks
# --------------------------------------------------------------------------
# Haskell parity status:
#   ALL targets now route through Haskell entrypoints.
#   Shell scripts in scripts/ are preserved for reference only.

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
	@cabal run umbravox -- smoke-linux

release-smoke-appimage:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running non-authoritative AppImage scaffold smoke placeholder..."
	@cabal run umbravox -- smoke-appimage

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
	@cabal run umbravox -- lane-qemu

release-lane-firecracker:
	@echo -e "$(BLUE)[RELEASE]$(NC) Checking Firecracker release-lane prerequisites..."
	@cabal run umbravox -- lane-firecracker

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
	@cabal run umbravox -- gate-assurance

verify-traffic:
	@echo -e "$(BLUE)[TRAFFIC]$(NC) Verifying no plaintext in captured traffic..."
	@cabal run umbravox -- verify-traffic

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
# Platform Release Packaging (M14.4.1-5)
# --------------------------------------------------------------------------

release-freebsd:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building FreeBSD platform release tarball (M14.4.1)..."
	@chmod +x ./scripts/release-package-platform.sh
	@nix-shell --run "./scripts/release-package-platform.sh freebsd"

release-openbsd:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building OpenBSD platform release tarball (M14.4.2)..."
	@chmod +x ./scripts/release-package-platform.sh
	@nix-shell --run "./scripts/release-package-platform.sh openbsd"

release-netbsd:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building NetBSD platform release tarball (M14.4.3)..."
	@chmod +x ./scripts/release-package-platform.sh
	@nix-shell --run "./scripts/release-package-platform.sh netbsd"

release-illumos:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building illumos/OmniOS platform release tarball (M14.4.4)..."
	@chmod +x ./scripts/release-package-platform.sh
	@nix-shell --run "./scripts/release-package-platform.sh illumos"

release-linux-arm64:
	@echo -e "$(BLUE)[RELEASE]$(NC) Building Linux arm64 platform release tarball (M14.4.5)..."
	@chmod +x ./scripts/release-package-platform.sh
	@nix-shell --run "./scripts/release-package-platform.sh linux-arm64"

# --------------------------------------------------------------------------
# Linux arm64 VM Smoke (M14.5.9)
# --------------------------------------------------------------------------

vm-smoke-arm64:
	@echo -e "$(BLUE)[VM-ARM64]$(NC) Running Linux arm64 QEMU VM smoke (M14.5.9)..."
	@echo -e "$(YELLOW)[VM-ARM64]$(NC) NOTE: requires qemu-system-aarch64 and an aarch64 NixOS image."
	@echo -e "$(YELLOW)[VM-ARM64]$(NC) Image: nix build .#vm-image-aarch64 (requires aarch64 builder or binfmt_misc)."
	@chmod +x ./scripts/vm-build-test.sh
	@nix-shell --run "\
		ARCH_IMAGE=\$$(find build/vm-cache/arm64 -name '*.img' -o -name '*.qcow2' 2>/dev/null | head -1); \
		if [ -z \"\$$ARCH_IMAGE\" ]; then \
			echo '[VM-ARM64] No arm64 image found under build/vm-cache/arm64/'; \
			echo '[VM-ARM64] Build with: nix build .#vm-image-aarch64'; \
			echo '[VM-ARM64] Then copy the result to build/vm-cache/arm64/'; \
			exit 1; \
		fi; \
		echo '[VM-ARM64] Image: '\"\$$ARCH_IMAGE\"; \
		qemu-system-aarch64 \
			-machine virt \
			-cpu cortex-a72 \
			-m 2048 \
			-smp 2 \
			-nographic \
			-drive if=virtio,format=raw,file=\"\$$ARCH_IMAGE\" \
			-netdev user,id=net0 \
			-device virtio-net-pci,netdev=net0 \
			-kernel \"\$$(find build/vm-cache/arm64 -name 'bzImage' -o -name 'Image' 2>/dev/null | head -1)\" \
			-append 'console=ttyAMA0 root=/dev/vda panic=1' \
			-serial mon:stdio \
		2>&1 | tee build/vm-cache/arm64/smoke.log; \
		if grep -q 'VM_BUILD_TEST=PASS' build/vm-cache/arm64/smoke.log 2>/dev/null; then \
			echo '[VM-ARM64] arm64 smoke: PASS'; \
		else \
			echo '[VM-ARM64] arm64 smoke: FAIL'; \
			exit 1; \
		fi"

# --------------------------------------------------------------------------
# Environment Tests
# --------------------------------------------------------------------------

test-shells:
	@echo -e "$(BLUE)[TEST-SHELLS]$(NC) Testing nix-shell environments..."
	@bash scripts/test-shells.sh

test-vm:
	@echo -e "$(BLUE)[TEST-VM]$(NC) Testing NixOS development VM..."
	@bash scripts/test-vm.sh

# --------------------------------------------------------------------------
# Quality Gate (all checks)
# --------------------------------------------------------------------------

quality: all check-evidence

check-evidence:
	@echo "Running external evidence checks..."
	@bash test/evidence/formal-proofs/check-external-evidence.sh

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

test-offline-parity: build
	@echo -e "$(BLUE)[PARITY]$(NC) Verifying online/offline test parity..."
	@mkdir -p $(TEST_ARTIFACT_DIR); \
	echo -e "$(BLUE)[PARITY]$(NC) Running core-crypto in normal (online) mode..."; \
	$(call run_test_suite,core-crypto) > $(TEST_ARTIFACT_DIR)/parity-online.log 2>&1; \
	online_exit=$$?; \
	echo -e "$(BLUE)[PARITY]$(NC) Running core-crypto in offline mode..."; \
	UMBRAVOX_OFFLINE=1 $(MAKE) test-core-crypto > $(TEST_ARTIFACT_DIR)/parity-offline.log 2>&1; \
	offline_exit=$$?; \
	online_pass=$$(grep -c "PASS:" $(TEST_ARTIFACT_DIR)/parity-online.log 2>/dev/null || echo 0); \
	offline_pass=$$(grep -c "PASS:" $(TEST_ARTIFACT_DIR)/parity-offline.log 2>/dev/null || echo 0); \
	if [ "$$online_exit" != "$$offline_exit" ]; then \
		echo -e "$(RED)[PARITY]$(NC) Exit code mismatch: online=$$online_exit offline=$$offline_exit"; \
		exit 1; \
	fi; \
	if [ "$$online_pass" != "$$offline_pass" ]; then \
		echo -e "$(RED)[PARITY]$(NC) Pass count mismatch: online=$$online_pass offline=$$offline_pass"; \
		exit 1; \
	fi; \
	echo -e "$(GREEN)[PARITY]$(NC) Both modes: exit=$$online_exit, $$online_pass assertions passed."

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
# VM-Based Development (M13.13)
# --------------------------------------------------------------------------
# These targets run the full dev toolchain inside the NixOS QEMU VM.
# The host only needs QEMU, git, make, and genext2fs (from shell-minimal.nix).
# See doc/VM-DEVELOPMENT.md for the migration plan.

vm-dev:
	@echo -e "$(BLUE)[VM-DEV]$(NC) Booting interactive NixOS development VM..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh interactive

vm-build:
	@echo -e "$(BLUE)[VM-BUILD]$(NC) Building inside NixOS VM..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh exec "cabal build all --enable-tests 2>&1; echo VM_DEV_RESULT=\$$?"

vm-test:
	@echo -e "$(BLUE)[VM-TEST]$(NC) Testing inside NixOS VM..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh exec "cabal build all --enable-tests 2>&1 && cabal test umbravox-test --test-options='required' 2>&1; echo VM_DEV_RESULT=\$$?"

vm-verify:
	@echo -e "$(BLUE)[VM-VERIFY]$(NC) Running F* verification inside NixOS VM..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh exec "cabal build all 2>&1 && cabal run fstar-verify 2>&1; echo VM_DEV_RESULT=\$$?"

# --------------------------------------------------------------------------
# VM Isolated Smoke Testing
# --------------------------------------------------------------------------

vm-smoke:
	@echo -e "$(BLUE)[VM-SMOKE]$(NC) Running isolated VM build/test/release pipeline..."
	@cabal run umbravox -- vm-smoke

vm-image-build:
	@echo -e "$(BLUE)[VM-IMAGE]$(NC) Building/caching NixOS VM image..."
	@mkdir -p build/vm
	@if [ -L build/vm/image ] && [ -e build/vm/image ]; then \
		echo -e "$(GREEN)[VM-IMAGE]$(NC) Image already cached at build/vm/image"; \
	else \
		echo -e "$(BLUE)[VM-IMAGE]$(NC) Building via nix (this may take several minutes)..."; \
		nix build .#vm-image -o build/vm/image 2>/dev/null || \
		nix-build nix/vm-image.nix -A qemu -o build/vm/image 2>/dev/null || \
		(echo -e "$(RED)[VM-IMAGE]$(NC) nix build failed — falling back to cabal bridge"; \
		 cabal run umbravox -- vm-image-build); \
		echo -e "$(GREEN)[VM-IMAGE]$(NC) Image cached at build/vm/image"; \
	fi

vm-image-clean:
	@echo -e "$(BLUE)[VM-IMAGE]$(NC) Removing cached VM image..."
	@rm -f build/vm/image
	@echo -e "$(GREEN)[VM-IMAGE]$(NC) Image cache cleared."

vm-cache-clean:
	@echo -e "$(BLUE)[VM-CACHE]$(NC) Removing persistent build cache disk..."
	@rm -f build/vm/build-cache.qcow2
	@echo -e "$(GREEN)[VM-CACHE]$(NC) Build cache cleared. Next VM build will start fresh."

vm-extract:
	@echo -e "$(BLUE)[VM-EXTRACT]$(NC) VM build output is at build/vm-output/"
	@if [ -d build/vm-output ] && [ "$$(ls -A build/vm-output 2>/dev/null)" ]; then \
		echo "Files:"; \
		ls -la build/vm-output/; \
	else \
		echo "No output files yet."; \
		echo "Inside vm-dev, copy files to /output/ — they appear at build/vm-output/ on the host."; \
	fi

image-clean: vm-image-clean

vm-socks5-test:
	@echo "Running SOCKS5 transport test in VM..."
	@nix-shell --run "cabal run umbravox -- vm-socks5-test"

vm-screenshot:
	@echo "Capturing TUI screenshots in VM..."
	@nix-shell --run "cabal run umbravox -- vm-screenshot"

vm-record:
	@echo "Recording TUI session in VM..."
	@nix-shell --run "cabal run umbravox -- vm-record"

vm-visual-regression:
	@echo "Running visual regression check in VM..."
	@nix-shell --run "cabal run umbravox -- vm-visual-regression"

visual-reference-update:
	@echo "Updating visual reference baselines..."
	@cp -v build/evidence/screenshots/*.ansi test/evidence/visual-reference/ 2>/dev/null || echo "No screenshots to copy — run 'make vm-screenshot' first"

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
# Multi-VM Integration Testing
# --------------------------------------------------------------------------

vm-integration-test:
	@echo -e "$(BLUE)[INTEGRATION]$(NC) Running multi-VM integration test ($(INTEGRATION_AGENTS) agents)..."
	@cabal run umbravox -- vm-integration-test --agents=$(INTEGRATION_AGENTS)

vm-integration-test-dual-lan:
	@echo -e "$(BLUE)[INTEGRATION]$(NC) Running dual-LAN integration test (6 agents)..."
	@cabal run umbravox -- vm-integration-test --agents=6 --dual-lan

vm-forensics:
	@echo -e "$(BLUE)[FORENSICS]$(NC) Running VM forensics verification..."
	@cabal run umbravox -- vm-forensics

# --------------------------------------------------------------------------
# FreeBSD / illumos VM Smoke Testing (M14.5.6, M14.5.8)
# --------------------------------------------------------------------------

vm-smoke-freebsd:
	@echo -e "$(BLUE)[VM-FREEBSD]$(NC) Running FreeBSD QEMU VM smoke (M14.2.1)..."
	@chmod +x ./scripts/vm-freebsd-setup.sh
	@./scripts/vm-freebsd-setup.sh

vm-smoke-illumos:
	@echo -e "$(BLUE)[VM-ILLUMOS]$(NC) Running OmniOS/illumos QEMU VM smoke (M14.2.5)..."
	@chmod +x ./scripts/vm-illumos-setup.sh
	@./scripts/vm-illumos-setup.sh

vm-smoke-openbsd:
	@echo -e "$(BLUE)[VM-OPENBSD]$(NC) Running OpenBSD QEMU VM smoke (M14.2.2)..."
	@chmod +x ./scripts/vm-openbsd-setup.sh
	@nix-shell nix/vm-openbsd.nix --run ./scripts/vm-openbsd-setup.sh

vm-smoke-netbsd:
	@echo -e "$(BLUE)[VM-NETBSD]$(NC) Running NetBSD QEMU VM smoke (M14.2.3)..."
	@chmod +x ./scripts/vm-netbsd-setup.sh
	@nix-shell nix/vm-netbsd.nix --run ./scripts/vm-netbsd-setup.sh

vm-smoke-dragonfly:
	@echo -e "$(BLUE)[VM-DRAGONFLY]$(NC) Running DragonFlyBSD QEMU VM smoke (M14.2.4)..."
	@echo -e "$(YELLOW)[VM-DRAGONFLY]$(NC) INFO: exits 0 with a notice if GHC binary package is unavailable."
	@chmod +x ./scripts/vm-dragonfly-setup.sh
	@nix-shell nix/vm-dragonfly.nix --run ./scripts/vm-dragonfly-setup.sh

# --------------------------------------------------------------------------
# Clean
# --------------------------------------------------------------------------

clean:
	@echo -e "$(BLUE)[CLEAN]$(NC) Removing build artifacts..."
	@cabal clean 2>/dev/null || true
	@rm -rf build
	@rm -rf dist-newstyle
	@rm -f ./*.tix
	@find . -maxdepth 2 -type f -name "*.tix" -delete 2>/dev/null || true
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
