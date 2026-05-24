// SPDX-License-Identifier: Apache-2.0
// Command umbravox-vm is the unified build/test/VM orchestration tool for UmbraVOX.
//
// Usage:
//
//	umbravox-vm                      build + test + check (fast gate)
//	umbravox-vm build                build in VM
//	umbravox-vm test [SUITE]         run test suite in VM
//	umbravox-vm dev [--gui]          interactive VM dev shell
//	umbravox-vm verify               F* formal verification
//	umbravox-vm check [GATE]         static quality gates
//	umbravox-vm coverage [FLAGS]     HPC coverage report
//	umbravox-vm release [PLATFORM]   build release artifacts
//	umbravox-vm vm <action>          VM infrastructure management
//	umbravox-vm evidence [FLAGS]     evidence & assurance bundles
//	umbravox-vm fuzz [MODE]          fuzzing
//	umbravox-vm clean [--all]        remove build artifacts
//	umbravox-vm exec -- <cmd>        run arbitrary command in VM
//	umbravox-vm help                 show help
package main

import (
	"fmt"
	"os"
)

func main() {
	os.Exit(run(os.Args[1:]))
}

func run(args []string) int {
	if len(args) == 0 {
		// No args = fast gate: build + test + check
		return runAll()
	}

	cmd := args[0]
	rest := args[1:]

	switch cmd {
	case "build":
		return runBuild(rest)
	case "test":
		return runTest(rest)
	case "dev":
		return runDev(rest)
	case "verify":
		return runVerify()
	case "check":
		return runCheck(rest)
	case "coverage":
		return runCoverage(rest)
	case "release":
		return runRelease(rest)
	case "vm":
		return runVM(rest)
	case "evidence":
		return runEvidence(rest)
	case "fuzz":
		return runFuzz(rest)
	case "clean":
		return runClean(rest)
	case "exec":
		return runExec(rest)
	case "help", "-h", "--help":
		printHelp()
		return 0
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\nRun 'uv help' for usage.\n", cmd)
		return 2
	}
}

func printHelp() {
	fmt.Print(`UmbraVOX Build System

Usage: ./uv <command> [args]

Commands:
  (no args)           Build + test + check (fast gate)
  build               Build library and executables in VM
  test [SUITE]        Run test suite (default: required fast gate)
  dev [--gui]         Interactive VM development shell
  verify              F* formal verification (17 modules)
  check [GATE]        Static quality gates (lint, format, license, complexity)
  coverage [FLAGS]    HPC coverage report (--check, --mcdc)
  release [PLATFORM]  Build release artifacts
  vm <action>         VM infrastructure (build-image, smoke, signal, seed)
  evidence [FLAGS]    Evidence & assurance bundles (--fast, --full)
  fuzz [MODE]         Fuzzing (differential, afl)
  clean [--all|--nix-gc] Remove build artifacts (--nix-gc frees /nix/store)
  exec -- <cmd>       Run arbitrary command in VM (power user)
  help                Show this help

Test suites:
  core, core-crypto, core-network, core-chat, core-tui, core-tools,
  tcp, fault, recovery, tui-sim, integrity, mdns, deferred,
  differential, soak, ephemeral

VM actions:
  build-image [--on-host]    Build NixOS VM image
  clean-image                Remove cached VM image
  smoke [TARGET]             Platform smoke test
  seed build|clean           Seed image management
  signal build-jar|extract-hash|run|health Signal Server VM
  integration [--dual-lan]   Multi-VM integration test
  info                       VM config diagnostics

Examples:
  ./uv                       Quick gate: build + test + check
  ./uv build                 Just build
  ./uv test tcp              Run TCP hardening suite
  ./uv dev                   Interactive VM shell
  ./uv vm build-image        Build the VM image
  ./uv exec -- cabal repl    Start a REPL in the VM
`)
}
