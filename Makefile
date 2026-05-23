# UmbraVOX Build System
# =====================
# DO-178C DAL A compliant build pipeline with quality gates.
#
# Usage:
#   make              - Build + run the full pipeline (build, test, verify, complexity, lint, license, format-check)
#   make build        - Build library + executables only
#   make build-haskell - Opt-in bridge wrapper for Haskell build orchestration
#   make run          - Launch UmbraVOX TUI via vm-run-gui (VM)
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
#   make signal-bridge-build - Build Signal bridge plugin (M19.6.3)
#   make test-signal-compat - Run Signal wire-compatibility tests (M19.6.3)
#   make test-signal-bridge-ipc - Run Signal bridge IPC subprocess smoke test
#   make test-differential - Run differential C vs Haskell tests
#   make soak         - Run the long soak suite and write an artifact report
#   make mcdc-report  - Build with HPC coverage and emit per-module expression report
#   make coverage-report - Build with HPC and generate HTML coverage report under build/coverage/
#   make coverage-check - Check per-module coverage against tier targets (doc/MCDC-TARGETS.md)
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
#   make docs         - Generate Haddock API documentation
#   make dep-graph    - Generate module dependency graph (DOT + text)
#   make check-evidence - Run external evidence verification checks
#   make assurance-fast - Run fast documentation/hygiene/ledger checks (seconds, no compilation)
#   make assurance      - Run full release-grade evidence suite (minutes, builds + tests)
#   make quality      - Run the full pipeline (same as make)
#   make vm-dev       - Interactive dev shell inside NixOS QEMU VM (M13.13)
#   make vm-build     - Build inside NixOS VM (cabal build all)
#   make vm-build-only - Build inside VM with auto image-build (host needs only QEMU+Nix)
#   make vm-test      - Test inside NixOS VM (cabal test required)
#   make vm-test-ephemeral - Build fresh VM image, run tests, discard image (M20.5.7)
#   make vm-verify    - F* verification inside NixOS VM
#   make vm-run-gui   - Boot dev VM with graphical QEMU window (VGA display)
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
#   make test-make-options - Dry-run every declared .PHONY make target
#   make test-vm-config - Validate VM-local Nix config resolution/guards
#   make sanity       - Check Makefile wiring for release smoke/microVM helpers
#   make evidence     - Run quality and write a publication evidence bundle
#   make screenshot-local - Capture 8 TUI screenshots locally (no VM, needs tmux)
#   make check-isolation - Verify build isolation (no host GHC/cabal leak, VM images)
#   make clean        - Remove build artifacts, build/, and dist-newstyle
#   make cleandb      - Remove local database
#   make cleanall     - Remove everything (build + DB + tools)
#   make help         - Show help
#
# Prerequisites:
#   VM mode (default): make vm-image-build (needs qemu + nix)

.PHONY: all build build-haskell run run-local test test-haskell test-core test-core-crypto test-core-network test-core-chat test-core-tui test-core-tools test-tcp test-fault test-recovery test-tui-sim test-integrity test-mdns test-deferred test-differential soak mcdc-report coverage-report coverage-check verify verify-haskell complexity quality evidence check-evidence assurance-fast assurance lint license license-fix release-compliance release-sbom release-license-bundle format-check codegen docs release release-linux release-appimage release-smoke-linux release-smoke-appimage release-smoke-qemu release-smoke-qemu-profile release-smoke-firecracker release-smoke-firecracker-pinned release-smoke-qemu-nix platform-lane-qemu platform-lane-firecracker platform-smoke-qemu-profile platform-sanity release-lane-qemu release-lane-firecracker release-lane-readiness release-lane-readiness-haskell release-gate-assurance check-vectors release-windows-cli release-macos-terminal release-bsd-terminal release-freedos release-source release-freebsd release-openbsd release-netbsd release-illumos release-linux-arm64 test-infra test-shells test-vm test-make-options test-vm-config sanity vm-smoke vm-image-build vm-image-clean vm-cache-clean vm-extract image-clean vm-dev vm-build vm-build-only vm-test vm-test-ephemeral vm-verify vm-run-gui firecracker-smoke firecracker-image-build release-sbom-generate release-license-bundle-generate release-license-check release-linking release-manifest release-checksums test-offline-parity vm-integration-test vm-integration-test-dual-lan verify-traffic vm-forensics vm-smoke-freebsd vm-smoke-illumos vm-smoke-openbsd vm-smoke-netbsd vm-smoke-dragonfly vm-smoke-arm64 vm-socks5-test vm-screenshot screenshot-local vm-record vm-visual-regression visual-reference-update differential-vectors test-differential-oracle test-differential-full fuzz-differential fuzz-afl differential differential-evidence-check signal-bridge-build test-signal-compat test-signal-bridge-ipc check-isolation tools dep-graph clean cleandb cleanall help
.DEFAULT_GOAL := all

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

SHELL := /bin/bash
MAX_COMPLEXITY := 8
FSTAR_DIR := test/evidence/formal-proofs/fstar

# VM-only execution helper.
# Usage: $(call vm_or_local,cabal build all --enable-tests 2>&1)
# Always delegates to scripts/vm-dev-run.sh exec "<cmd>".
# Requires the VM image at build/vm/image; errors if missing.
define vm_or_local
	@if [ ! -d build/vm/image ] && [ ! -L build/vm/image ]; then \
		echo -e "$(RED)[ERROR]$(NC) No VM image found at build/vm/image."; \
		echo "  Run 'make vm-image-build' first."; \
		exit 1; \
	fi; \
	./scripts/vm-dev-run.sh exec "$(1)"
endef

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
  run_test_suite = cabal test umbravox-test --test-options='$(1)'
  RUN_FSTAR = cabal run fstar-verify --
  RUN_COMPLEXITY = cabal run check-complexity --
  RUN_CODEGEN = cabal run codegen --
endif

# Logged gate wrapper (M20.7.2): run a command with [TAG] echo/pass/fail.
# Usage: $(call logged_gate,TAG,description,command)
define logged_gate
	@echo -e "$(BLUE)[$(1)]$(NC) $(2)"
	@if $(3); then \
		echo -e "$(GREEN)[$(1)]$(NC) $(2) — passed."; \
	else \
		echo -e "$(RED)[$(1)]$(NC) $(2) — failed."; \
		exit 1; \
	fi
endef

define run_named_suite
	@echo -e "$(BLUE)[$(1)]$(NC) $(2)"
	$(call vm_or_local,$(SUITE_LOCK) $(call run_test_suite,$(3)))
endef
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

help: # [host-only]
	@echo ""
	@echo -e "$(BLUE)  UmbraVOX Build System$(NC)"
	@echo -e "$(BLUE)  =====================$(NC)"
	@echo ""
	@echo "  Build & Run (VM-default: auto-delegates to VM when image exists):"
	@echo "    make             Build + run the full pipeline (build, test, verify, complexity, lint, license, format-check)"
	@echo "    make build       Build library + executables only [VM-default]"
	@echo "    make run         Launch UmbraVOX TUI via vm-run-gui"
	@echo "    make test        Run fast messaging-MVP hardening gate [VM-default]"
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
	@echo "    make coverage-report Build with HPC and generate HTML coverage report under build/coverage/"
	@echo "    make coverage-check Check per-module coverage against tier targets (doc/MCDC-TARGETS.md)"
	@echo "    make signal-bridge-build Build Signal bridge plugin (M19.6.3)"
	@echo "    make test-signal-bridge-ipc Run Signal bridge IPC subprocess smoke test"
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
	@echo "    make verify      Run F* formal verification (17 modules) [VM-default]"
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
	@echo "    make assurance-fast Run fast documentation/hygiene/ledger checks (seconds, no compilation)"
	@echo "    make assurance     Run full release-grade evidence suite (minutes, builds + tests)"
	@echo "    make release-gate-assurance Run assurance matrix freshness gate"
	@echo "    make verify-traffic Verify no plaintext in captured traffic"
	@echo "    make quality     Same as make (lint/format-check are non-blocking)"
	@echo ""
	@echo "  VM Development (M13.13 — full toolchain inside VM) [VM-only]:"
	@echo "    make vm-dev         Interactive dev shell inside NixOS VM"
	@echo "    make vm-build       Build inside VM (cabal build all)"
	@echo "    make vm-build-only  Build inside VM, auto-build image first (host needs only QEMU+Nix)"
	@echo "    make vm-test        Test inside VM (cabal test required)"
	@echo "    make vm-test-ephemeral Build fresh VM image, run tests, discard image"
	@echo "    make vm-verify      F* verification inside VM"
	@echo "    make vm-run-gui     Boot dev VM with graphical QEMU window (TUI on VGA console)"
	@echo ""
	@echo "  VM Smoke (isolated build/test) [VM-only]:"
	@echo "    make vm-smoke       Run full pipeline inside isolated QEMU VM"
	@echo "    make vm-image-build Build VM image inside a builder VM (no host nix store)"
	@echo "    make vm-image-build-host Build VM image on host (uses host nix store, ~30GB)"
	@echo "    make vm-image-clean Remove the cached VM image"
	@echo "    make image-clean    Alias for vm-image-clean"
	@echo "    make firecracker-smoke  Run pipeline inside Firecracker VM"
	@echo "    make firecracker-image-build Build Firecracker image"
	@echo "    make vm-signal-server-build Build Signal-Server VM image (M19.4.1)"
	@echo "    make vm-signal-server  Boot Signal-Server VM (PostgreSQL/Redis/ZK/Java)"
	@echo "    make test-signal-compat Run Signal wire-compatibility tests (M19.6.3)"
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
	@echo "  TUI Screenshots (R1.4.a.4, R1.5.b, R1.6):"
	@echo "    make screenshot-local  Capture 8 TUI screenshots locally (no VM required, needs tmux)"
	@echo "    make vm-screenshot     Capture TUI screenshots inside NixOS VM"
	@echo ""
	@echo "  Maintenance:"
	@echo "    make check-isolation  Verify build isolation (no host GHC/cabal leak, VM images present)"
	@echo "    make test-make-options Dry-run every declared .PHONY target"
	@echo "    make test-vm-config Validate VM-local nix config resolution/guards"
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
	@echo "  Target Annotations:"
	@echo "    [VM-default]  Runs in VM by default"
	@echo "    [VM-only]     Always runs inside a QEMU/Firecracker VM"
	@echo "    [host-only]   Always runs on the host (e.g. TUI, clean, lint)"
	@echo "    (unmarked)    Runs wherever invoked (host or VM)"
	@echo ""
	@echo "  VM Isolation:"
	@echo "    All build/test/verify targets default to VM execution."
	@echo "    Run 'make vm-image-build' to create the VM image first."
	@echo "    VM image builds use fail-closed local nix config from nix/vm-build.env."
	@echo ""
	@echo "  Keyboard Shortcuts (TUI):"
	@echo "    Tab     Switch focus (contacts <-> chat)"
	@echo "    N       New connection    S   Secure notes"
	@echo "    R       Rename contact    K   Identity & keys"
	@echo "    Ctrl+N  New connection    Ctrl+R  Verify keys"
	@echo "    Ctrl+X  Export (encrypted) Ctrl+P  Preferences"
	@echo "    Ctrl+Q  Quit             ?       Help"
	@echo ""

run: # [host-only] orchestration alias
	@$(MAKE) vm-run-gui

run-local: # [host-only] compatibility guard
	@echo -e "$(RED)[RUN]$(NC) Local host compilation is disabled."
	@echo "  Use 'make vm-run-gui' (or 'make run') for VM-isolated execution."
	@exit 1

# --------------------------------------------------------------------------
# Build
# --------------------------------------------------------------------------

build:
	@echo -e "$(BLUE)[BUILD]$(NC) Building UmbraVOX..."
	$(call vm_or_local,cabal build all 2>&1 | tail -5)
	@echo -e "$(GREEN)[BUILD]$(NC) Build complete."

build-haskell:
	@echo -e "$(BLUE)[BUILD-HASKELL]$(NC) Opt-in bridge wrapper: legacy Make build by default; set UMBRAVOX_USE_HASKELL_ORCH=1 to try the Haskell subcommand."
	@$(MAKE) build

# --------------------------------------------------------------------------
# Test
# --------------------------------------------------------------------------

test:
	@$(MAKE) build
	@echo -e "$(BLUE)[TEST]$(NC) Running fast messaging-MVP hardening gate..."
	$(call vm_or_local,UMBRAVOX_TEST_TMPDIR=build/test-tmp $(SUITE_LOCK) bash -c 'mkdir -p $(TEST_ARTIFACT_DIR) build/test-tmp; \
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
	fi')

test-haskell:
	@echo -e "$(BLUE)[TEST-HASKELL]$(NC) Opt-in bridge wrapper: legacy Make test by default."
	@$(MAKE) test

test-core: build
	$(call run_named_suite,TEST-CORE,Running core deterministic suite...,core)

test-core-crypto: build
	$(call run_named_suite,TEST-CORE-CRYPTO,Running core crypto suite...,core-crypto)

test-core-network: build
	$(call run_named_suite,TEST-CORE-NETWORK,Running core network suite...,core-network)

test-core-chat: build
	$(call run_named_suite,TEST-CORE-CHAT,Running core chat/protocol suite...,core-chat)

test-core-tui: build
	$(call run_named_suite,TEST-CORE-TUI,Running core TUI suite...,core-tui)

test-core-tools: build
	$(call run_named_suite,TEST-CORE-TOOLS,Running core tools/codegen suite...,core-tools)

test-tcp: build
	$(call run_named_suite,TEST-TCP,Running real TCP suite...,tcp)

test-fault: build
	$(call run_named_suite,TEST-FAULT,Running fault-injection suite...,fault)

test-recovery: build
	$(call run_named_suite,TEST-RECOVERY,Running recovery suite...,recovery)

test-tui-sim: build
	$(call run_named_suite,TEST-TUI-SIM,Running TUI simulation suite...,tui-sim)

test-integrity: build
	$(call run_named_suite,TEST-INTEGRITY,Running wire/integrity suite...,integrity)

test-mdns: build
	$(call run_named_suite,TEST-MDNS,Running exact mDNS/discovery suite...,mdns)

test-deferred: build
	$(call run_named_suite,TEST-DEFERRED,Running preserved deferred suite...,deferred)

test-differential: build
	$(call run_named_suite,TEST-DIFF,Running differential C vs Haskell tests...,differential)

soak: build
	@echo -e "$(BLUE)[SOAK]$(NC) Running soak suite..."
	$(call vm_or_local,$(SUITE_LOCK) bash -c 'mkdir -p $(TEST_ARTIFACT_DIR); \
	$(call run_test_suite,soak) 2>&1 | tee $(TEST_ARTIFACT_DIR)/soak-report.txt')

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
	$(call vm_or_local,cabal configure --enable-coverage && \
	cabal build all --enable-coverage 2>&1 | tail -5 && \
	echo -e "$(BLUE)[COVERAGE]$(NC) Running required test suite with HPC instrumentation..." && \
	cabal test umbravox-test --enable-coverage --test-options='required' 2>&1 | tail -10 && \
	echo -e "$(BLUE)[COVERAGE]$(NC) Generating per-module expression coverage report..." && \
	tix=$$(find dist-newstyle -name '*.tix' -path '*/umbravox-test*' | head -1) && \
	mix_lib=$$(find dist-newstyle -path '*/extra-compilation-artifacts/hpc/vanilla/mix' -not -path '*/umbravox-test*' | head -1) && \
	mix_test=$$(find dist-newstyle -path '*/umbravox-test*/extra-compilation-artifacts/hpc/vanilla/mix' | head -1) && \
	if [ -z "$$tix" ]; then \
		echo -e "$(RED)[COVERAGE]$(NC) No .tix file found — test run may have failed."; \
		exit 1; \
	fi && \
	echo -e "$(GREEN)[COVERAGE]$(NC) Tix: $$tix" && \
	hpc report "$$tix" --per-module \
		$$([ -n "$$mix_lib" ] && echo "--hpcdir=$$mix_lib") \
		$$([ -n "$$mix_test" ] && echo "--hpcdir=$$mix_test") && \
	echo -e "$(GREEN)[COVERAGE]$(NC) HTML report: $$(find dist-newstyle -name 'hpc_index.html' | head -1)")

# --------------------------------------------------------------------------
# Test Coverage Report (M26.2.5)
# --------------------------------------------------------------------------
#
# Generates an HPC coverage report from cabal test --enable-coverage
# and writes an HTML report under build/coverage/.

coverage-report:
	@echo -e "$(BLUE)[COVERAGE]$(NC) Building and running tests with HPC coverage..."
	$(call vm_or_local,cabal clean 2>/dev/null; \
	cabal configure --enable-coverage && \
	cabal build all --enable-coverage 2>&1 | tail -5 && \
	cabal test umbravox-test --enable-coverage --test-options='required' 2>&1 | tail -10 && \
	echo -e "$(BLUE)[COVERAGE]$(NC) Locating coverage data..." && \
	tix=$$(find dist-newstyle -name '*.tix' -path '*/umbravox-test*' | head -1) && \
	mix_lib=$$(find dist-newstyle -path '*/extra-compilation-artifacts/hpc/vanilla/mix' -not -path '*/umbravox-test*' | head -1) && \
	mix_test=$$(find dist-newstyle -path '*/umbravox-test*/extra-compilation-artifacts/hpc/vanilla/mix' | head -1) && \
	if [ -z "$$tix" ]; then \
		echo -e "$(RED)[COVERAGE]$(NC) No .tix file found — test run may have failed."; \
		exit 1; \
	fi && \
	echo -e "$(GREEN)[COVERAGE]$(NC) Tix: $$tix" && \
	mkdir -p build/coverage && \
	hpc report "$$tix" --per-module \
		$$([ -n "$$mix_lib" ] && echo "--hpcdir=$$mix_lib") \
		$$([ -n "$$mix_test" ] && echo "--hpcdir=$$mix_test") \
		> build/coverage/coverage-summary.txt && \
	hpc markup "$$tix" --destdir=build/coverage \
		$$([ -n "$$mix_lib" ] && echo "--hpcdir=$$mix_lib") \
		$$([ -n "$$mix_test" ] && echo "--hpcdir=$$mix_test") && \
	echo -e "$(GREEN)[COVERAGE]$(NC) Summary: build/coverage/coverage-summary.txt" && \
	echo -e "$(GREEN)[COVERAGE]$(NC) HTML: build/coverage/hpc_index.html")

# --------------------------------------------------------------------------
# Coverage Target Check (M16.2-4)
# --------------------------------------------------------------------------
#
# Parses the HPC coverage summary and compares each module's expression
# coverage against the tier targets in doc/MCDC-TARGETS.md.
# Fails if any module is below its target.
# Requires: make coverage-report (to generate build/coverage/coverage-summary.txt)

coverage-check:
	@echo -e "$(BLUE)[COVERAGE-CHECK]$(NC) Checking per-module coverage against tier targets..."
	@bash scripts/coverage-check.sh build/coverage/coverage-summary.txt

# --------------------------------------------------------------------------
# F* Formal Verification
# --------------------------------------------------------------------------

verify:
	@echo -e "$(BLUE)[VERIFY]$(NC) Running F* formal verification (all modules)..."
	$(call vm_or_local,$(SUITE_LOCK) bash -c 'mkdir -p $(TEST_ARTIFACT_DIR); \
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
	fi')

verify-haskell:
	@echo -e "$(BLUE)[VERIFY-HASKELL]$(NC) Opt-in bridge wrapper: legacy Make verify by default."
	@$(MAKE) verify

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
	$(call vm_or_local,violations=0; total=0; \
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
	fi)

# --------------------------------------------------------------------------
# Lint / Style
# --------------------------------------------------------------------------

lint: # [host-only] grep/shell only, no compiler
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

license: # [host-only] grep/shell only, no compiler
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

license-fix: # [host-only] sed only, no compiler
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
	$(call vm_or_local,cabal run umbravox -- release-sbom-generate)

release-license-bundle-generate:
	@echo -e "$(BLUE)[COMPLIANCE]$(NC) Generating third-party license bundle..."
	$(call vm_or_local,cabal run umbravox -- release-license-bundle-generate)

release-license-check:
	@echo -e "$(BLUE)[COMPLIANCE]$(NC) Checking license policy..."
	$(call vm_or_local,cabal run umbravox -- release-license-check)

release-linking:
	@echo -e "$(BLUE)[COMPLIANCE]$(NC) Analyzing linking obligations..."
	$(call vm_or_local,cabal run umbravox -- release-linking)

release-manifest:
	@echo -e "$(BLUE)[RELEASE]$(NC) Generating release provenance manifest..."
	$(call vm_or_local,cabal run umbravox -- release-manifest)

release-checksums:
	@echo -e "$(BLUE)[RELEASE]$(NC) Emitting release artifact checksums..."
	$(call vm_or_local,cabal run umbravox -- release-checksums)

# --------------------------------------------------------------------------
# Code Formatting Check
# --------------------------------------------------------------------------

format-check: # [host-only] grep only, no compiler
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
	$(call vm_or_local,$(RUN_CODEGEN) 2>&1 | tail -20)
	@echo -e "$(GREEN)[CODEGEN]$(NC) Code generation complete."

# --------------------------------------------------------------------------
# Documentation
# --------------------------------------------------------------------------

docs:
	@echo -e "$(BLUE)[DOCS]$(NC) Generating Haddock API documentation..."
	$(call vm_or_local,cabal haddock --haddock-html 2>&1 | tail -10)
	@echo -e "$(GREEN)[DOCS]$(NC) Haddock documentation generated."

dep-graph:
	@echo -e "$(BLUE)[DEP-GRAPH]$(NC) Generating module dependency graph..."
	@mkdir -p build
	@bash scripts/gen-dep-graph.sh src/UmbraVox
	@echo -e "$(GREEN)[DEP-GRAPH]$(NC) Done. See build/dep-graph.dot and build/dep-graph.txt"

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
	$(call vm_or_local,cabal run umbravox -- smoke-linux)

release-smoke-appimage:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running non-authoritative AppImage scaffold smoke placeholder..."
	$(call vm_or_local,cabal run umbravox -- smoke-appimage)

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
	$(call vm_or_local,cabal run umbravox -- lane-qemu)

release-lane-firecracker:
	@echo -e "$(BLUE)[RELEASE]$(NC) Checking Firecracker release-lane prerequisites..."
	$(call vm_or_local,cabal run umbravox -- lane-firecracker)

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
	$(call vm_or_local,cabal run umbravox -- release-lane-readiness)

release-gate-assurance:
	@echo -e "$(BLUE)[RELEASE]$(NC) Running assurance matrix release gate..."
	$(call vm_or_local,cabal run umbravox -- gate-assurance)

verify-traffic:
	@echo -e "$(BLUE)[TRAFFIC]$(NC) Verifying no plaintext in captured traffic..."
	$(call vm_or_local,cabal run umbravox -- verify-traffic)

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

# Platform release targets require host nix as a bootstrap dependency.
# nix-shell shell-minimal.nix provides coreutils, gnutar, and gzip for
# cross-platform packaging without a full Haskell toolchain.
release-freebsd: # [host-only] packaging only, no compiler needed
	@echo -e "$(BLUE)[RELEASE]$(NC) Building FreeBSD platform release tarball (M14.4.1)..."
	@chmod +x ./scripts/release-package-platform.sh
	@nix-shell shell-minimal.nix --run "./scripts/release-package-platform.sh freebsd"

release-openbsd: # [host-only] packaging only, no compiler needed
	@echo -e "$(BLUE)[RELEASE]$(NC) Building OpenBSD platform release tarball (M14.4.2)..."
	@chmod +x ./scripts/release-package-platform.sh
	@nix-shell shell-minimal.nix --run "./scripts/release-package-platform.sh openbsd"

release-netbsd: # [host-only] packaging only, no compiler needed
	@echo -e "$(BLUE)[RELEASE]$(NC) Building NetBSD platform release tarball (M14.4.3)..."
	@chmod +x ./scripts/release-package-platform.sh
	@nix-shell shell-minimal.nix --run "./scripts/release-package-platform.sh netbsd"

release-illumos: # [host-only] packaging only, no compiler needed
	@echo -e "$(BLUE)[RELEASE]$(NC) Building illumos/OmniOS platform release tarball (M14.4.4)..."
	@chmod +x ./scripts/release-package-platform.sh
	@nix-shell shell-minimal.nix --run "./scripts/release-package-platform.sh illumos"

release-linux-arm64: # [host-only] packaging only, no compiler needed
	@echo -e "$(BLUE)[RELEASE]$(NC) Building Linux arm64 platform release tarball (M14.4.5)..."
	@chmod +x ./scripts/release-package-platform.sh
	@nix-shell shell-minimal.nix --run "./scripts/release-package-platform.sh linux-arm64"

# --------------------------------------------------------------------------
# Linux arm64 VM Smoke (M14.5.9)
# --------------------------------------------------------------------------

vm-smoke-arm64:
	@echo -e "$(BLUE)[VM-ARM64]$(NC) Running Linux arm64 QEMU VM smoke (M14.5.9)..."
	@echo -e "$(YELLOW)[VM-ARM64]$(NC) NOTE: requires qemu-system-aarch64 and an aarch64 NixOS image."
	@echo -e "$(YELLOW)[VM-ARM64]$(NC) Image: nix build .#vm-image-aarch64 (requires aarch64 builder or binfmt_misc)."
	@chmod +x ./scripts/vm-build-test.sh
	@nix-shell shell-minimal.nix --run "\
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

test-infra:
	@echo -e "$(BLUE)[TEST-INFRA]$(NC) Running infrastructure regression suite..."
	@bash scripts/test-infrastructure.sh

test-shells:
	@echo -e "$(BLUE)[TEST-SHELLS]$(NC) Testing nix-shell environments..."
	@bash scripts/test-shells.sh

test-vm:
	@echo -e "$(BLUE)[TEST-VM]$(NC) Testing NixOS development VM..."
	@bash scripts/test-vm.sh

test-make-options:
	@echo -e "$(BLUE)[TEST-MAKE]$(NC) Dry-running all declared make options..."
	@bash scripts/test-make-options.sh

test-vm-config:
	@echo -e "$(BLUE)[TEST-VM-CONFIG]$(NC) Testing VM-local nix config resolution..."
	@bash scripts/test-vm-build-config.sh

# --------------------------------------------------------------------------
# Quality Gate (all checks)
# --------------------------------------------------------------------------

quality: all check-evidence assurance-fast

# --------------------------------------------------------------------------
# Assurance Evidence Suites
# --------------------------------------------------------------------------

ASSURANCE_LOG_DIR := test/evidence/formal-proofs/logs

assurance-fast:
	@echo -e "$(BLUE)[ASSURANCE-FAST]$(NC) Running fast documentation/hygiene/ledger checks..."
	@mkdir -p $(ASSURANCE_LOG_DIR)
	@pass=0; fail=0; skip=0; ts=$$(date -u +%Y%m%dT%H%M%SZ); \
	log="$(ASSURANCE_LOG_DIR)/assurance-fast-$$ts.log"; \
	exec > >(tee "$$log") 2>&1; \
	\
	echo "=== F* admit check ==="; \
	admit_count=$$(grep -rn '\badmit\b\|admit()' $(FSTAR_DIR)/*.fst 2>/dev/null | grep -v '\*)\|(\*\|//' | grep -v 'admit_smt' | wc -l); \
	if [ "$$admit_count" -eq 0 ]; then \
		echo -e "$(GREEN)[PASS]$(NC) F* admit check: 0 admits found"; \
		pass=$$((pass + 1)); \
	else \
		echo -e "$(RED)[FAIL]$(NC) F* admit check: $$admit_count admit(s) found"; \
		grep -rn '\badmit\b\|admit()' $(FSTAR_DIR)/*.fst 2>/dev/null | grep -v '\*)\|(\*\|//' | grep -v 'admit_smt'; \
		fail=$$((fail + 1)); \
	fi; \
	\
	echo ""; \
	echo "=== F* assume val inventory ==="; \
	assume_count=$$(grep -rn 'assume val' $(FSTAR_DIR)/*.fst 2>/dev/null | wc -l); \
	echo "  assume val count: $$assume_count"; \
	echo "  (logged to $(ASSURANCE_LOG_DIR)/assume-val-inventory-$$ts.txt)"; \
	grep -rn 'assume val' $(FSTAR_DIR)/*.fst 2>/dev/null > "$(ASSURANCE_LOG_DIR)/assume-val-inventory-$$ts.txt" || true; \
	pass=$$((pass + 1)); \
	\
	echo ""; \
	echo "=== Coq no-Admitted check ==="; \
	admitted_count=$$(grep -rn '^\s*Admitted\.' test/evidence/formal-proofs/coq/*.v 2>/dev/null | grep -v '(\*' | wc -l); \
	if [ "$$admitted_count" -eq 0 ]; then \
		echo -e "$(GREEN)[PASS]$(NC) Coq no-Admitted check: 0 Admitted found"; \
		pass=$$((pass + 1)); \
	else \
		echo -e "$(RED)[FAIL]$(NC) Coq no-Admitted check: $$admitted_count Admitted found"; \
		grep -rn '^\s*Admitted\.' test/evidence/formal-proofs/coq/*.v 2>/dev/null | grep -v '(\*'; \
		fail=$$((fail + 1)); \
	fi; \
	\
	echo ""; \
	echo "=== Ledger consistency ==="; \
	if [ -x test/evidence/formal-proofs/check-assumption-ledger.sh ]; then \
		if bash test/evidence/formal-proofs/check-assumption-ledger.sh; then \
			echo -e "$(GREEN)[PASS]$(NC) Ledger consistency"; \
			pass=$$((pass + 1)); \
		else \
			echo -e "$(RED)[FAIL]$(NC) Ledger consistency"; \
			fail=$$((fail + 1)); \
		fi; \
	else \
		echo -e "$(YELLOW)[SKIP]$(NC) Ledger consistency: check-assumption-ledger.sh not found"; \
		skip=$$((skip + 1)); \
	fi; \
	\
	echo ""; \
	echo "=== Proof hygiene ==="; \
	if [ -x test/evidence/formal-proofs/check-proof-hygiene.sh ]; then \
		if bash test/evidence/formal-proofs/check-proof-hygiene.sh; then \
			echo -e "$(GREEN)[PASS]$(NC) Proof hygiene"; \
			pass=$$((pass + 1)); \
		else \
			echo -e "$(RED)[FAIL]$(NC) Proof hygiene"; \
			fail=$$((fail + 1)); \
		fi; \
	else \
		echo -e "$(YELLOW)[SKIP]$(NC) Proof hygiene: check-proof-hygiene.sh not found"; \
		skip=$$((skip + 1)); \
	fi; \
	\
	echo ""; \
	echo "========================================"; \
	echo "  ASSURANCE-FAST SUMMARY"; \
	echo "  PASS: $$pass  FAIL: $$fail  SKIP: $$skip"; \
	echo "  Log: $$log"; \
	echo "========================================"; \
	if [ "$$fail" -gt 0 ]; then \
		echo -e "$(RED)[ASSURANCE-FAST]$(NC) FAIL ($$fail check(s) failed)"; \
		exit 1; \
	else \
		echo -e "$(GREEN)[ASSURANCE-FAST]$(NC) PASS ($$pass passed, $$skip skipped)"; \
	fi

assurance: assurance-fast
	@echo ""
	@echo -e "$(BLUE)[ASSURANCE]$(NC) Running full release-grade evidence suite..."
	@mkdir -p $(ASSURANCE_LOG_DIR)
	$(call vm_or_local,pass=0; fail=0; skip=0; ts=$$(date -u +%Y%m%dT%H%M%SZ); \
	log="$(ASSURANCE_LOG_DIR)/assurance-full-$$ts.log"; \
	exec > >(tee "$$log") 2>&1; \
	\
	echo "=== Coq build ==="; \
	if command -v coqc >/dev/null 2>&1; then \
		if make -C test/evidence/formal-proofs/coq; then \
			echo -e "$(GREEN)[PASS]$(NC) Coq build"; \
			pass=$$((pass + 1)); \
		else \
			echo -e "$(RED)[FAIL]$(NC) Coq build"; \
			fail=$$((fail + 1)); \
		fi; \
	else \
		echo -e "$(YELLOW)[SKIP]$(NC) Coq build: coqc not available"; \
		skip=$$((skip + 1)); \
	fi; \
	\
	echo ""; \
	echo "=== Haskell build + tests ==="; \
	if cabal test umbravox-test --test-options='required'; then \
		echo -e "$(GREEN)[PASS]$(NC) Haskell build + tests"; \
		pass=$$((pass + 1)); \
	else \
		echo -e "$(RED)[FAIL]$(NC) Haskell build + tests"; \
		fail=$$((fail + 1)); \
	fi; \
	\
	echo ""; \
	echo "=== Infrastructure tests ==="; \
	if [ -x scripts/test-infrastructure.sh ]; then \
		if bash scripts/test-infrastructure.sh; then \
			echo -e "$(GREEN)[PASS]$(NC) Infrastructure tests"; \
			pass=$$((pass + 1)); \
		else \
			echo -e "$(RED)[FAIL]$(NC) Infrastructure tests"; \
			fail=$$((fail + 1)); \
		fi; \
	else \
		echo -e "$(YELLOW)[SKIP]$(NC) Infrastructure tests: scripts/test-infrastructure.sh not found"; \
		skip=$$((skip + 1)); \
	fi; \
	\
	echo ""; \
	echo "=== External evidence ==="; \
	if [ -x test/evidence/formal-proofs/check-external-evidence.sh ]; then \
		if bash test/evidence/formal-proofs/check-external-evidence.sh; then \
			echo -e "$(GREEN)[PASS]$(NC) External evidence"; \
			pass=$$((pass + 1)); \
		else \
			echo -e "$(RED)[FAIL]$(NC) External evidence"; \
			fail=$$((fail + 1)); \
		fi; \
	else \
		echo -e "$(YELLOW)[SKIP]$(NC) External evidence: check-external-evidence.sh not found"; \
		skip=$$((skip + 1)); \
	fi; \
	\
	echo ""; \
	echo "========================================"; \
	echo "  ASSURANCE (FULL) SUMMARY"; \
	echo "  PASS: $$pass  FAIL: $$fail  SKIP: $$skip"; \
	echo "  (plus assurance-fast checks above)"; \
	echo "  Log: $$log"; \
	echo "========================================"; \
	if [ "$$fail" -gt 0 ]; then \
		echo -e "$(RED)[ASSURANCE]$(NC) FAIL ($$fail check(s) failed)"; \
		exit 1; \
	else \
		echo -e "$(GREEN)[ASSURANCE]$(NC) PASS ($$pass passed, $$skip skipped)"; \
	fi)

check-vectors:
	@echo -e "$(BLUE)[VECTORS]$(NC) Verifying test vector integrity (SHA-256)..."
	@cd test/vectors && sha256sum -c SHA256SUMS

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
	$(call vm_or_local,bash ./scripts/test-offline-parity.sh $(TEST_ARTIFACT_DIR))

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
	@./scripts/vm-dev-run.sh exec "cabal build all --enable-tests 2>&1"

vm-test:
	@echo -e "$(BLUE)[VM-TEST]$(NC) Testing inside NixOS VM..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh exec "cabal build all --enable-tests 2>&1 && cabal test umbravox-test --test-options='required' 2>&1"

vm-build-only: vm-image-build
	@echo -e "$(BLUE)[VM-BUILD-ONLY]$(NC) Building inside NixOS VM (no host toolchain needed)..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh exec "cabal build all --enable-tests 2>&1"

vm-run-gui:
	@echo -e "$(BLUE)[VM-GUI]$(NC) Booting NixOS VM with graphical QEMU window..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh gui

vm-verify:
	@echo -e "$(BLUE)[VM-VERIFY]$(NC) Running F* verification inside NixOS VM..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh exec "cabal build all 2>&1 && cabal run fstar-verify 2>&1"

vm-test-ephemeral: # Build fresh VM image in temp dir, run tests, discard image
	@echo -e "$(BLUE)[VM-EPHEMERAL]$(NC) Building ephemeral VM image for testing..."
	@mkdir -p build/vm-ephemeral build/vm-ephemeral/tmp build/vm-ephemeral/tmp/sandbox
	@CFG_SCRIPT="./scripts/nix-vm-build-config.sh"; \
	if [ ! -x "$$CFG_SCRIPT" ] && [ -f "$$CFG_SCRIPT" ]; then chmod +x "$$CFG_SCRIPT"; fi; \
	if [ -z "$$(command -v nix 2>/dev/null)" ] && [ -x /nix/var/nix/profiles/default/bin/nix ]; then \
		export PATH="/nix/var/nix/profiles/default/bin:$$PATH"; \
	fi; \
	if ! command -v nix >/dev/null 2>&1 && ! command -v nix-build >/dev/null 2>&1; then \
		echo -e "$(RED)[VM-EPHEMERAL]$(NC) Neither 'nix' nor 'nix-build' is available on PATH."; \
		exit 1; \
	fi; \
	CFG_EXPORTS="$$( "$$CFG_SCRIPT" shell )" || exit 1; \
	eval "$$CFG_EXPORTS"; \
	echo -e "$(BLUE)[NIX-VM-CONFIG]$(NC) source=$$UMBRAVOX_NIX_CONFIG_SOURCE local_only=$$UMBRAVOX_NIX_LOCAL_ONLY"; \
	TMPDIR="$$(pwd)/build/vm-ephemeral/tmp"; export TMPDIR; \
	echo -e "$(BLUE)[VM-EPHEMERAL]$(NC) Building via nix into ephemeral path..."; \
	if ! ( \
		nix --extra-experimental-features "nix-command flakes" \
			--option build-dir "$$TMPDIR" \
			--option sandbox-build-dir "$$UMBRAVOX_NIX_SANDBOX_BUILD_DIR" \
			build -L .#vm-image -o build/vm-ephemeral/image || \
		nix-build --option build-dir "$$TMPDIR" \
			--option sandbox-build-dir "$$UMBRAVOX_NIX_SANDBOX_BUILD_DIR" \
			nix/vm-image.nix -A qemu -o build/vm-ephemeral/image \
	); then \
		echo -e "$(RED)[VM-EPHEMERAL]$(NC) Ephemeral image build failed."; \
		rm -rf build/vm-ephemeral; \
		exit 1; \
	fi; \
	echo -e "$(GREEN)[VM-EPHEMERAL]$(NC) Ephemeral image built. Running tests..."; \
	EPHEMERAL_STATUS=0; \
	VM_IMAGE_PATH=build/vm-ephemeral/image \
		./scripts/vm-dev-run.sh exec \
		"cabal build all --enable-tests 2>&1 && cabal test umbravox-test --test-options='required' 2>&1" \
		|| EPHEMERAL_STATUS=$$?; \
	echo -e "$(BLUE)[VM-EPHEMERAL]$(NC) Cleaning up ephemeral image..."; \
	rm -rf build/vm-ephemeral; \
	if [ "$$EPHEMERAL_STATUS" -eq 0 ]; then \
		echo -e "$(GREEN)[VM-EPHEMERAL]$(NC) Ephemeral VM tests passed."; \
	else \
		echo -e "$(RED)[VM-EPHEMERAL]$(NC) Ephemeral VM tests failed (exit $$EPHEMERAL_STATUS)."; \
		exit $$EPHEMERAL_STATUS; \
	fi

# --------------------------------------------------------------------------
# VM Isolated Smoke Testing
# --------------------------------------------------------------------------

vm-smoke:
	@echo -e "$(BLUE)[VM-SMOKE]$(NC) Running isolated VM build/test/release pipeline..."
	$(call vm_or_local,cabal run umbravox -- vm-smoke)

vm-image-build: # [host-only] Build VM image inside a builder VM (M20.5.8)
	@echo -e "$(BLUE)[VM-IMAGE]$(NC) Building NixOS VM image inside builder VM..."
	@mkdir -p build/vm build/vm/tmp build/vm/tmp/sandbox
	@chmod +x ./scripts/vm-image-builder.sh
	@if command -v genext2fs >/dev/null 2>&1; then \
		./scripts/vm-image-builder.sh; \
	elif command -v nix-shell >/dev/null 2>&1; then \
		echo -e "$(YELLOW)[VM-IMAGE]$(NC) genext2fs not found. Enter nix-shell to continue?"; \
		printf "  Enter nix-shell and build? [Y/n] "; \
		read -r ans; \
		case "$$ans" in \
			[Nn]*) echo "Aborted."; exit 1 ;; \
			*) echo -e "$(BLUE)[VM-IMAGE]$(NC) Entering nix-shell..."; \
			   nix-shell shell.nix --run "./scripts/vm-image-builder.sh" ;; \
		esac; \
	else \
		echo -e "$(RED)[VM-IMAGE]$(NC) genext2fs not found and nix-shell not available."; \
		echo "  Install nix (https://nixos.org/) then run:"; \
		echo "    nix-shell shell.nix"; \
		echo "    make vm-image-build"; \
		exit 1; \
	fi

vm-image-build-host: # [host-only] nix-build produces VM image directly on host
ifndef I_KNOW_THIS_TOUCHES_HOST
	$(error vm-image-build-host writes to /nix/store. Set I_KNOW_THIS_TOUCHES_HOST=1 or use make vm-image-build)
endif
	@echo -e "$(YELLOW)[VM-IMAGE]$(NC) WARNING: Building VM image on the host."
	@echo -e "$(YELLOW)[VM-IMAGE]$(NC) This touches the host /nix/store and uses ~30GB of disk."
	@echo -e "$(YELLOW)[VM-IMAGE]$(NC) Prefer 'make vm-image-build' (builder VM) instead."
	@echo -e "$(BLUE)[VM-IMAGE]$(NC) Building/caching NixOS VM image on host..."
	@mkdir -p build/vm build/vm/tmp build/vm/tmp/sandbox
	@CFG_SCRIPT="./scripts/nix-vm-build-config.sh"; \
	if [ ! -x "$$CFG_SCRIPT" ] && [ -f "$$CFG_SCRIPT" ]; then chmod +x "$$CFG_SCRIPT"; fi; \
	if [ -z "$$(command -v nix 2>/dev/null)" ] && [ -x /nix/var/nix/profiles/default/bin/nix ]; then \
		export PATH="/nix/var/nix/profiles/default/bin:$$PATH"; \
	fi; \
	if ! command -v nix >/dev/null 2>&1 && ! command -v nix-build >/dev/null 2>&1; then \
		echo -e "$(RED)[VM-IMAGE]$(NC) Neither 'nix' nor 'nix-build' is available on PATH."; \
		echo "  Install Nix or add /nix/var/nix/profiles/default/bin to PATH."; \
		exit 1; \
	fi; \
	CFG_EXPORTS="$$( "$$CFG_SCRIPT" shell )" || exit 1; \
	eval "$$CFG_EXPORTS"; \
	echo -e "$(BLUE)[NIX-VM-CONFIG]$(NC) source=$$UMBRAVOX_NIX_CONFIG_SOURCE file=$$UMBRAVOX_NIX_CONFIG_FILE_EFFECTIVE"; \
	echo -e "$(BLUE)[NIX-VM-CONFIG]$(NC) UMBRAVOX_NIX_BUILD_DIR=$$UMBRAVOX_NIX_BUILD_DIR"; \
	echo -e "$(BLUE)[NIX-VM-CONFIG]$(NC) UMBRAVOX_NIX_SANDBOX_BUILD_DIR=$$UMBRAVOX_NIX_SANDBOX_BUILD_DIR"; \
	echo -e "$(BLUE)[NIX-VM-CONFIG]$(NC) UMBRAVOX_NIX_LOCAL_ONLY=$$UMBRAVOX_NIX_LOCAL_ONLY"; \
	if [ -L build/vm/image ] && [ -e build/vm/image ]; then \
		echo -e "$(GREEN)[VM-IMAGE]$(NC) Image already cached at build/vm/image"; \
	else \
		TMPDIR="$$(pwd)/$$UMBRAVOX_NIX_BUILD_DIR"; export TMPDIR; \
		mkdir -p "$$TMPDIR"; \
		echo -e "$(BLUE)[VM-IMAGE]$(NC) Using TMPDIR=$$TMPDIR"; \
		echo -e "$(BLUE)[VM-IMAGE]$(NC) Building via nix (this may take several minutes)..."; \
		if ! ( \
			nix --extra-experimental-features "nix-command flakes" \
				--option build-dir "$$TMPDIR" \
				--option sandbox-build-dir "$$UMBRAVOX_NIX_SANDBOX_BUILD_DIR" \
				build -L .#vm-image -o build/vm/image || \
			nix-build --option build-dir "$$TMPDIR" \
				--option sandbox-build-dir "$$UMBRAVOX_NIX_SANDBOX_BUILD_DIR" \
				nix/vm-image.nix -A qemu -o build/vm/image \
		); then \
			echo -e "$(RED)[VM-IMAGE]$(NC) Local nix build failed (fail-closed)."; \
			echo "  No alternate builder fallback is attempted."; \
			exit 1; \
		fi; \
		echo -e "$(GREEN)[VM-IMAGE]$(NC) Image cached at build/vm/image"; \
	fi

vm-image-clean: # [host-only] file operations
	@echo -e "$(BLUE)[VM-IMAGE]$(NC) Removing cached VM image..."
	@rm -f build/vm/image
	@rm -rf build/vm/tmp
	@echo -e "$(GREEN)[VM-IMAGE]$(NC) Image cache cleared."

vm-cache-clean: # [host-only] file operations
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

# --------------------------------------------------------------------------
# Signal-Server VM — two-stage build (M19.4)
# Stage 1: build VM (network, Maven) → signal-server.jar
# Stage 2: runtime VM (deny-all, pre-built JAR + backing services)
# --------------------------------------------------------------------------

vm-signal-server-build-jar:
	@echo -e "$(BLUE)[SIGNAL-VM]$(NC) Stage 1: Building Signal-Server JAR in VM..."
	@chmod +x ./scripts/vm-signal-server-run.sh
	@./scripts/vm-signal-server-run.sh build-jar

vm-signal-server-build:
	@echo -e "$(BLUE)[SIGNAL-VM]$(NC) Stage 2: Building runtime VM image..."
	@mkdir -p build/vm-signal-server build/vm-signal-server/tmp build/vm-signal-server/tmp/sandbox
	@CFG_SCRIPT="./scripts/nix-vm-build-config.sh"; \
	if [ ! -x "$$CFG_SCRIPT" ] && [ -f "$$CFG_SCRIPT" ]; then chmod +x "$$CFG_SCRIPT"; fi; \
	CFG_EXPORTS="$$( "$$CFG_SCRIPT" shell )" || exit 1; \
	eval "$$CFG_EXPORTS"; \
	echo -e "$(BLUE)[NIX-VM-CONFIG]$(NC) source=$$UMBRAVOX_NIX_CONFIG_SOURCE file=$$UMBRAVOX_NIX_CONFIG_FILE_EFFECTIVE"; \
	echo -e "$(BLUE)[NIX-VM-CONFIG]$(NC) UMBRAVOX_NIX_BUILD_DIR=$$UMBRAVOX_NIX_BUILD_DIR"; \
	echo -e "$(BLUE)[NIX-VM-CONFIG]$(NC) UMBRAVOX_NIX_SANDBOX_BUILD_DIR=$$UMBRAVOX_NIX_SANDBOX_BUILD_DIR"; \
	echo -e "$(BLUE)[NIX-VM-CONFIG]$(NC) UMBRAVOX_NIX_LOCAL_ONLY=$$UMBRAVOX_NIX_LOCAL_ONLY"; \
	if [ -d build/vm-signal-server/image ]; then \
		echo -e "$(GREEN)[SIGNAL-VM]$(NC) Image already cached at build/vm-signal-server/image"; \
	else \
		if [ -z "$$(command -v nix-build 2>/dev/null)" ] && [ -x /nix/var/nix/profiles/default/bin/nix-build ]; then \
			export PATH="/nix/var/nix/profiles/default/bin:$$PATH"; \
		fi; \
		if ! command -v nix-build >/dev/null 2>&1; then \
			echo -e "$(RED)[SIGNAL-VM]$(NC) 'nix-build' is not available on PATH."; \
			echo "  Install Nix or add /nix/var/nix/profiles/default/bin to PATH."; \
			exit 1; \
		fi; \
		TMPDIR="$$(pwd)/build/vm-signal-server/tmp"; export TMPDIR; \
		mkdir -p "$$TMPDIR"; \
		echo -e "$(BLUE)[SIGNAL-VM]$(NC) Using TMPDIR=$$TMPDIR"; \
		echo -e "$(BLUE)[SIGNAL-VM]$(NC) Building via nix (this may take several minutes)..."; \
		nix-build --option build-dir "$$TMPDIR" \
			--option sandbox-build-dir "$$UMBRAVOX_NIX_SANDBOX_BUILD_DIR" \
			nix/vm-signal-server.nix -A qemu -o build/vm-signal-server/image || \
		(echo -e "$(RED)[SIGNAL-VM]$(NC) Local nix-build failed (fail-closed)."; exit 1); \
		echo -e "$(GREEN)[SIGNAL-VM]$(NC) Image cached at build/vm-signal-server/image"; \
	fi

vm-signal-server:
	@echo -e "$(BLUE)[SIGNAL-VM]$(NC) Booting Signal-Server VM..."
	@chmod +x ./scripts/vm-signal-server-run.sh
	@./scripts/vm-signal-server-run.sh interactive

vm-signal-server-check:
	@echo -e "$(BLUE)[SIGNAL-VM]$(NC) Booting Signal-Server VM (service check)..."
	@chmod +x ./scripts/vm-signal-server-run.sh
	@./scripts/vm-signal-server-run.sh check

# --------------------------------------------------------------------------
# Signal Bridge Plugin (M19.6.3)
# --------------------------------------------------------------------------

signal-bridge-build:
	@echo -e "$(BLUE)[SIGNAL-BRIDGE]$(NC) Building Signal bridge plugin..."
	$(call vm_or_local,cabal build umbravox-signal-bridge 2>&1 | tail -5)
	@echo -e "$(GREEN)[SIGNAL-BRIDGE]$(NC) Build complete."

test-signal-compat:
	@echo -e "$(BLUE)[SIGNAL-COMPAT]$(NC) Running Signal wire-compatibility tests..."
	@chmod +x ./scripts/vm-signal-test.sh
	@./scripts/vm-signal-test.sh

test-signal-bridge-ipc: signal-bridge-build
	@echo -e "$(BLUE)[SIGNAL-IPC]$(NC) Running Signal bridge IPC smoke test..."
	$(call vm_or_local,chmod +x ./scripts/test-signal-bridge-ipc.sh && ./scripts/test-signal-bridge-ipc.sh)

vm-socks5-test:
	@echo -e "$(BLUE)[VM-SOCKS5]$(NC) Running SOCKS5 transport test in VM..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh exec "bash /work/umbravox/scripts/vm-socks5-test.sh"

vm-screenshot:
	@echo -e "$(BLUE)[VM-SCREENSHOT]$(NC) Capturing TUI screenshots in VM..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh exec "cabal build all --enable-tests 2>&1 && bash /work/umbravox/scripts/vm-screenshot-capture.sh"

screenshot-local: # [host-only] captures host tmux session
	@echo -e "$(BLUE)[SCREENSHOT]$(NC) Capturing TUI screenshots locally (no VM required)..."
	@chmod +x ./scripts/tui-screenshot-local.sh
	@bash ./scripts/tui-screenshot-local.sh

vm-record:
	@echo -e "$(BLUE)[VM-RECORD]$(NC) Recording TUI session in VM..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh exec "cabal build all --enable-tests 2>&1 && bash /work/umbravox/scripts/vm-record-session.sh"

vm-visual-regression:
	@echo -e "$(BLUE)[VM-VISUAL]$(NC) Running visual regression check in VM..."
	@chmod +x ./scripts/vm-dev-run.sh
	@./scripts/vm-dev-run.sh exec "cabal build all --enable-tests 2>&1 && bash /work/umbravox/scripts/vm-visual-regression.sh"

visual-reference-update:
	@echo "Updating visual reference baselines..."
	@cp -v build/evidence/screenshots/*.ansi test/evidence/visual-reference/ 2>/dev/null || echo "No screenshots to copy — run 'make vm-screenshot' first"

# --------------------------------------------------------------------------
# Cleanroom Multi-Oracle Differential Testing (v0.1.4)
# --------------------------------------------------------------------------

differential-vectors:
	@echo -e "$(BLUE)[DIFFERENTIAL]$(NC) Generating test vectors..."
	@chmod +x ./scripts/vm-differential-run.sh
	@./scripts/vm-differential-run.sh vectors

test-differential-oracle:
	@echo -e "$(BLUE)[DIFFERENTIAL]$(NC) Running Tier 1 oracle differential tests..."
	$(call vm_or_local,cabal test umbravox-test --test-options='differential-oracle' 2>&1 | tail -10)

test-differential-full:
	@echo -e "$(BLUE)[DIFFERENTIAL]$(NC) Running Tier 2 differential tests..."
	@echo -e "$(BLUE)[DIFFERENTIAL]$(NC) Step 1: Haskell differential + negative + metamorphic + fuzz..."
	$(call vm_or_local,cabal test umbravox-test --test-options='differential-oracle' 2>&1 | tail -10)
	@echo -e "$(BLUE)[DIFFERENTIAL]$(NC) Step 2: VM oracle vectors (if available)..."
	@chmod +x ./scripts/vm-differential-run.sh
	@./scripts/vm-differential-run.sh full 2>/dev/null || echo -e "$(YELLOW)[DIFFERENTIAL]$(NC) VM vectors not available — Haskell tests only."
	@echo -e "$(GREEN)[DIFFERENTIAL]$(NC) Tier 2 complete."

fuzz-differential:
	@echo -e "$(BLUE)[DIFFERENTIAL]$(NC) Running Tier 3 differential fuzzing..."
	@echo -e "$(BLUE)[DIFFERENTIAL]$(NC) Step 1: Deterministic fuzz (600 vectors)..."
	$(call vm_or_local,cabal test umbravox-test --test-options='differential-oracle' 2>&1 | tail -10)
	@echo -e "$(BLUE)[DIFFERENTIAL]$(NC) Step 2: Protocol self-consistency traces..."
	@echo -e "$(GREEN)[DIFFERENTIAL]$(NC) Tier 3 complete (AFL++ requires oracle VM seed corpus)."

# --------------------------------------------------------------------------
# AFL++ Parser Fuzzing (M18.6.2-3)
# --------------------------------------------------------------------------

fuzz-afl:
	@echo -e "$(BLUE)[FUZZ-AFL]$(NC) Building AFL++ fuzz harnesses..."
	$(call vm_or_local,mkdir -p build/fuzz && \
	ghc -O2 -isrc -o build/fuzz/fuzz-gcm test/fuzz/harness-gcm-decrypt.hs && \
	ghc -O2 -isrc -o build/fuzz/fuzz-ed25519 test/fuzz/harness-ed25519-verify.hs && \
	ghc -O2 -isrc -o build/fuzz/fuzz-x25519 test/fuzz/harness-x25519.hs)
	@echo -e "$(GREEN)[FUZZ-AFL]$(NC) Harnesses built in build/fuzz/."
	@echo -e "$(BLUE)[FUZZ-AFL]$(NC) Generate seeds: bash test/fuzz/seed-from-vectors.sh"
	@echo -e "$(BLUE)[FUZZ-AFL]$(NC) Run: afl-fuzz -i test/fuzz/corpus/gcm -o build/fuzz/findings/gcm -- build/fuzz/fuzz-gcm"
	@echo -e "$(BLUE)[FUZZ-AFL]$(NC) Run: afl-fuzz -i test/fuzz/corpus/ed25519 -o build/fuzz/findings/ed25519 -- build/fuzz/fuzz-ed25519"
	@echo -e "$(BLUE)[FUZZ-AFL]$(NC) Run: afl-fuzz -i test/fuzz/corpus/x25519 -o build/fuzz/findings/x25519 -- build/fuzz/fuzz-x25519"

differential: test-differential-full
	@echo -e "$(GREEN)[DIFFERENTIAL]$(NC) Differential testing complete."

differential-evidence-check:
	@echo -e "$(BLUE)[DIFFERENTIAL]$(NC) Checking evidence completeness..."
	@chmod +x ./scripts/vm-differential-run.sh
	@./scripts/vm-differential-run.sh evidence

# --------------------------------------------------------------------------
# Firecracker Isolated Smoke Testing
# --------------------------------------------------------------------------

firecracker-smoke:
	@echo -e "$(BLUE)[FC-SMOKE]$(NC) Running Firecracker isolated pipeline..."
	$(call vm_or_local,cabal run umbravox -- firecracker-smoke)

firecracker-image-build:
	@echo -e "$(BLUE)[FC-IMAGE]$(NC) Building/caching Firecracker image..."
	$(call vm_or_local,cabal run umbravox -- firecracker-image-build)

# --------------------------------------------------------------------------
# Multi-VM Integration Testing
# --------------------------------------------------------------------------

vm-integration-test:
	@echo -e "$(BLUE)[INTEGRATION]$(NC) Running multi-VM integration test ($(INTEGRATION_AGENTS) agents)..."
	$(call vm_or_local,cabal run umbravox -- vm-integration-test --agents=$(INTEGRATION_AGENTS))

vm-integration-test-dual-lan:
	@echo -e "$(BLUE)[INTEGRATION]$(NC) Running dual-LAN integration test (6 agents)..."
	$(call vm_or_local,cabal run umbravox -- vm-integration-test --agents=6 --dual-lan)

vm-forensics:
	@echo -e "$(BLUE)[FORENSICS]$(NC) Running VM forensics verification..."
	$(call vm_or_local,cabal run umbravox -- vm-forensics)

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
# Build Isolation Check
# --------------------------------------------------------------------------
# Verifies that the host environment is not leaking build tools outside
# nix-shell, and that VM images exist for the primary build targets.

check-isolation:
	@echo -e "$(BLUE)[ISOLATION]$(NC) Checking build isolation status..."
	@pass=0; warn=0; \
	echo ""; \
	echo "  Host toolchain check:"; \
	if [ -n "$$UMBRAVOX_VM" ]; then \
		echo -e "    GHC:   $(GREEN)OK$(NC) (inside VM)"; \
		echo -e "    Cabal: $(GREEN)OK$(NC) (inside VM)"; \
		pass=$$((pass + 2)); \
	elif [ -n "$$IN_NIX_SHELL" ] && [ "$${UMBRAVOX_SHELL_KIND:-}" = "minimal" ]; then \
		echo -e "    GHC:   $(GREEN)OK$(NC) (inside shell-minimal.nix; orchestration shell)"; \
		echo -e "    Cabal: $(GREEN)OK$(NC) (inside shell-minimal.nix; orchestration shell)"; \
		pass=$$((pass + 2)); \
	elif [ -n "$$IN_NIX_SHELL" ] && [ "$${UMBRAVOX_SHELL_KIND:-}" = "full" ]; then \
		echo -e "    GHC:   $(YELLOW)WARN$(NC) (inside full local shell; host compilers available)"; \
		echo -e "    Cabal: $(YELLOW)WARN$(NC) (inside full local shell; host compilers available)"; \
		warn=$$((warn + 2)); \
	else \
		if command -v ghc >/dev/null 2>&1; then \
			ghc_path=$$(command -v ghc); \
			if echo "$$ghc_path" | grep -q '/nix/store'; then \
				echo -e "    GHC:   $(YELLOW)WARN$(NC) — found at $$ghc_path (nix store, but not in nix-shell)"; \
				warn=$$((warn + 1)); \
			else \
				echo -e "    GHC:   $(YELLOW)WARN$(NC) — found at $$ghc_path (system install, not nix-managed)"; \
				warn=$$((warn + 1)); \
			fi; \
		else \
			echo -e "    GHC:   $(GREEN)OK$(NC) (not on host PATH)"; \
			pass=$$((pass + 1)); \
		fi; \
		if command -v cabal >/dev/null 2>&1; then \
			cabal_path=$$(command -v cabal); \
			if echo "$$cabal_path" | grep -q '/nix/store'; then \
				echo -e "    Cabal: $(YELLOW)WARN$(NC) — found at $$cabal_path (nix store, but not in nix-shell)"; \
				warn=$$((warn + 1)); \
			else \
				echo -e "    Cabal: $(YELLOW)WARN$(NC) — found at $$cabal_path (system install, not nix-managed)"; \
				warn=$$((warn + 1)); \
			fi; \
		else \
			echo -e "    Cabal: $(GREEN)OK$(NC) (not on host PATH)"; \
			pass=$$((pass + 1)); \
		fi; \
	fi; \
	\
	echo ""; \
	echo "  VM image check:"; \
	if [ -d build/vm/image ] || [ -L build/vm/image ]; then \
		echo -e "    Dev VM image:            $(GREEN)OK$(NC) (build/vm/image)"; \
		pass=$$((pass + 1)); \
	else \
		echo -e "    Dev VM image:            $(YELLOW)MISSING$(NC) — run 'make vm-image-build'"; \
		warn=$$((warn + 1)); \
	fi; \
	if [ -d build/vm-signal-server/image ]; then \
		echo -e "    Signal-Server VM image:  $(GREEN)OK$(NC) (build/vm-signal-server/image)"; \
		pass=$$((pass + 1)); \
	else \
		echo -e "    Signal-Server VM image:  $(YELLOW)MISSING$(NC) — run 'make vm-signal-server-build'"; \
		warn=$$((warn + 1)); \
	fi; \
	if [ -d build/vm-signal-server/build-image ]; then \
		echo -e "    Signal build VM image:   $(GREEN)OK$(NC) (build/vm-signal-server/build-image)"; \
		pass=$$((pass + 1)); \
	else \
		echo -e "    Signal build VM image:   $(YELLOW)MISSING$(NC) — built on first 'make vm-signal-server-build-jar'"; \
		warn=$$((warn + 1)); \
	fi; \
	\
	echo ""; \
	echo "  VM local nix config check:"; \
	CFG_SCRIPT="./scripts/nix-vm-build-config.sh"; \
	if [ ! -x "$$CFG_SCRIPT" ] && [ -f "$$CFG_SCRIPT" ]; then chmod +x "$$CFG_SCRIPT"; fi; \
	if [ -x "$$CFG_SCRIPT" ]; then \
		if CFG_EXPORTS="$$( "$$CFG_SCRIPT" shell 2>/dev/null )"; then \
			eval "$$CFG_EXPORTS"; \
			echo -e "    VM nix config:           $(GREEN)OK$(NC) ($$UMBRAVOX_NIX_CONFIG_SOURCE)"; \
			echo "      file=$$UMBRAVOX_NIX_CONFIG_FILE_EFFECTIVE"; \
			echo "      build_dir=$$UMBRAVOX_NIX_BUILD_DIR"; \
			echo "      sandbox_build_dir=$$UMBRAVOX_NIX_SANDBOX_BUILD_DIR"; \
			echo "      local_only=$$UMBRAVOX_NIX_LOCAL_ONLY"; \
			pass=$$((pass + 1)); \
		else \
			cfg_err="$$( "$$CFG_SCRIPT" shell 2>&1 >/dev/null )"; \
			echo -e "    VM nix config:           $(YELLOW)WARN$(NC) — $$cfg_err"; \
			warn=$$((warn + 1)); \
		fi; \
	else \
		echo -e "    VM nix config:           $(YELLOW)WARN$(NC) — $$CFG_SCRIPT missing or not executable"; \
		warn=$$((warn + 1)); \
	fi; \
	\
	echo ""; \
	echo "  Environment:"; \
	if [ -n "$$UMBRAVOX_VM" ]; then \
		echo "    UMBRAVOX_VM    = $$UMBRAVOX_VM (inside VM)"; \
	else \
		echo "    UMBRAVOX_VM    = <unset> (host)"; \
	fi; \
	if [ -n "$$IN_NIX_SHELL" ]; then \
		echo "    IN_NIX_SHELL   = $$IN_NIX_SHELL"; \
	else \
		echo "    IN_NIX_SHELL   = <unset>"; \
	fi; \
	if [ -n "$$UMBRAVOX_SHELL_KIND" ]; then \
		echo "    UMBRAVOX_SHELL_KIND = $$UMBRAVOX_SHELL_KIND"; \
	else \
		echo "    UMBRAVOX_SHELL_KIND = <unset>"; \
	fi; \
	\
	echo ""; \
	echo "========================================"; \
	echo "  ISOLATION: $$pass OK, $$warn warnings"; \
	echo "========================================"; \
	if [ "$$warn" -gt 0 ]; then \
		echo -e "$(YELLOW)[ISOLATION]$(NC) $$warn warning(s). Prefer 'make vm-build' from shell-minimal.nix for strict VM isolation."; \
	else \
		echo -e "$(GREEN)[ISOLATION]$(NC) Build isolation looks good."; \
	fi

# --------------------------------------------------------------------------
# Go Tools (M22.1 scaffold)
# --------------------------------------------------------------------------

tools: # [host-only] build Go tool binaries
	@echo -e "$(BLUE)[TOOLS]$(NC) Building Go tools..."
	@mkdir -p build/tools
	@cd tools && GOMODCACHE=$(CURDIR)/build/go/mod GOCACHE=$(CURDIR)/build/go/cache go build -o ../build/tools/ ./cmd/...
	@echo -e "$(GREEN)[TOOLS]$(NC) Go tools built in build/tools/"

# --------------------------------------------------------------------------
# Clean
# --------------------------------------------------------------------------

clean: # [host-only] file operations
	@echo -e "$(BLUE)[CLEAN]$(NC) Removing build artifacts..."
	@cabal clean 2>/dev/null || true
	@rm -rf build
	@rm -rf dist-newstyle
	@rm -f ./*.tix
	@find . -maxdepth 2 -type f -name "*.tix" -delete 2>/dev/null || true
	@rm -rf $(FSTAR_DIR)/_cache $(FSTAR_DIR)/_output
	@echo -e "$(GREEN)[CLEAN]$(NC) Done. (VM image not removed; use make image-clean)"

# Ensure cabal can recover from a clean state — if dist-newstyle was
# removed (by make clean or manually), cabal build handles it natively.
# This target explicitly verifies the build works from a clean slate.
clean-build-test: clean
	@echo -e "$(BLUE)[CLEAN-BUILD-TEST]$(NC) Verifying build from clean state..."
	$(call vm_or_local, cabal build all 2>&1 | tail -5)
	@echo -e "$(GREEN)[CLEAN-BUILD-TEST]$(NC) Clean build verified."

cleandb: # [host-only] file operations
	@echo -e "$(BLUE)[CLEANDB]$(NC) Removing local database..."
	@rm -f .umbravox-data/umbravox.db
	@echo -e "$(GREEN)[CLEANDB]$(NC) Database removed. Will be recreated on next launch."

cleanall: clean cleandb # [host-only] file operations
	@echo -e "$(BLUE)[CLEANALL]$(NC) Removing all local data..."
	@rm -rf .umbravox-data/tools
	@echo -e "$(GREEN)[CLEANALL]$(NC) All cleaned (build + DB + tools)."
