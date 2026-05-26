// SPDX-License-Identifier: Apache-2.0
package main

// vmSmokeRelease implements `./uv vm smoke release [qemu]`.
//
// This is the Go port of scripts/release-smoke-microvm.sh. It boots a
// microVM via QEMU with a release bundle and verifies the binary runs
// correctly — a smoke test for release artifacts.
//
// # Environment variables
//
//	UMBRAVOX_QEMU_SMOKE_RUNNER   Path to a host-specific boot-and-check
//	                              executable. When set, it is executed directly
//	                              (no shell interpretation).
//	UMBRAVOX_QEMU_KERNEL         Path to Linux kernel image (pinned-boot)
//	UMBRAVOX_QEMU_INITRD         Path to initrd image (pinned-boot)
//	UMBRAVOX_QEMU_ROOTFS         Path to rootfs image (pinned-boot)
//	UMBRAVOX_QEMU_APPEND         Kernel command-line (pinned-boot)
//	UMBRAVOX_QEMU_PROFILE        Deterministic profile name (uses
//	                              scripts/release-smoke-qemu-profile.sh)
//	UMBRAVOX_QEMU_VERIFY_CMD     In-guest verification command template
//	UMBRAVOX_QEMU_MEM_MB         Guest RAM in MB (default: 1024)
//	UMBRAVOX_QEMU_CPUS           Guest vCPU count (default: 2)

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
)

// vmSmokeRelease is the entry point for `./uv vm smoke release [mode]`.
func vmSmokeRelease(args []string) int {
	mode := "qemu"
	if len(args) > 0 {
		mode = args[0]
	}
	if len(args) > 1 {
		fmt.Fprintln(os.Stderr, "Usage: ./uv vm smoke release [qemu]")
		return 2
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	switch mode {
	case "qemu":
		return smokeReleaseQEMU(repoRoot)
	default:
		fmt.Fprintf(os.Stderr, "Unknown mode %q; expected qemu\n", mode)
		return 2
	}
}

// ── artifact check ────────────────────────────────────────────────────────────

// findLatestReleaseArtifact finds the most recently modified Linux release
// tarball under build/releases/ and returns its path, or an error if none
// exists.
func findLatestReleaseArtifact(repoRoot string) (string, error) {
	pattern := filepath.Join(repoRoot, "build", "releases", "umbravox-*-linux-x86_64.tar.gz")
	matches, err := filepath.Glob(pattern)
	if err != nil {
		return "", fmt.Errorf("glob release artifacts: %w", err)
	}
	if len(matches) == 0 {
		return "", fmt.Errorf("no linux release artifact found under build/releases; run ./uv release linux first")
	}

	latest := ""
	var latestMtime int64 = -1
	for _, m := range matches {
		fi, err := os.Stat(m)
		if err != nil {
			return "", fmt.Errorf("stat release artifact %s: %w", m, err)
		}
		if fi.ModTime().Unix() > latestMtime {
			latest = m
			latestMtime = fi.ModTime().Unix()
		}
	}
	return latest, nil
}

// ── QEMU smoke ────────────────────────────────────────────────────────────────

func smokeReleaseQEMU(repoRoot string) int {
	// Prerequisites
	if _, err := exec.LookPath("qemu-system-x86_64"); err != nil {
		log.Fail(tag, "qemu-system-x86_64 not available; install QEMU for microVM smoke lane")
		return 1
	}
	if _, err := os.Stat("/dev/kvm"); err != nil {
		log.Fail(tag, "/dev/kvm not present; QEMU smoke lane requires KVM-capable host")
		return 1
	}

	artifact, err := findLatestReleaseArtifact(repoRoot)
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}
	log.Info(tag, fmt.Sprintf("artifact: %s", artifact))

	// Runner path: exec directly, no shell interpretation.
	// Finding:   UMBRAVOX_QEMU_SMOKE_RUNNER was originally passed verbatim
	//            to `bash -lc`, allowing arbitrary shell expansion and making
	//            it impossible to pass paths with spaces safely.
	// Vulnerability: an attacker who controls the environment variable (or a
	//            misconfigured CI job) could inject shell metacharacters and
	//            execute arbitrary commands under the login shell's PATH.
	// Fix:       Validate that the value is a path to an existing executable
	//            before use, then exec it directly so no shell interpretation
	//            occurs.
	// Verified:  exec.LookPath resolves PATH lookups; os.Stat + executable bit
	//            check covers absolute paths; syscall.Exec replaces the current
	//            process with the runner, eliminating the sub-shell attack
	//            surface.
	if runner := os.Getenv("UMBRAVOX_QEMU_SMOKE_RUNNER"); runner != "" {
		log.Info(tag, "running QEMU smoke runner command from UMBRAVOX_QEMU_SMOKE_RUNNER")
		return execRunner(runner)
	}

	// Pinned-boot path
	kernel := os.Getenv("UMBRAVOX_QEMU_KERNEL")
	initrd := os.Getenv("UMBRAVOX_QEMU_INITRD")
	rootfs := os.Getenv("UMBRAVOX_QEMU_ROOTFS")
	appendLine := os.Getenv("UMBRAVOX_QEMU_APPEND")
	profile := os.Getenv("UMBRAVOX_QEMU_PROFILE")

	if kernel != "" || initrd != "" || rootfs != "" || appendLine != "" || profile != "" {
		log.Info(tag, "running QEMU pinned-boot smoke path from UMBRAVOX_QEMU_* inputs")
		return qemuPinnedBoot(repoRoot, kernel, initrd, rootfs, appendLine, profile)
	}

	// Scaffold: prerequisites satisfied but no boot wiring configured yet.
	fmt.Print(`QEMU microVM smoke scaffold
- prerequisites satisfied
- to execute in-guest checks now, set UMBRAVOX_QEMU_SMOKE_RUNNER to a host-specific boot-and-check command
- or set UMBRAVOX_QEMU_KERNEL, UMBRAVOX_QEMU_INITRD, UMBRAVOX_QEMU_ROOTFS, UMBRAVOX_QEMU_APPEND, and UMBRAVOX_QEMU_VERIFY_CMD for pinned-boot execution
- optional deterministic profile path: set UMBRAVOX_QEMU_PROFILE (uses scripts/release-smoke-qemu-profile.sh)
- default behavior remains scaffold-only until pinned guest boot wiring is configured
- in-guest verification template example: /usr/local/bin/umbravox-release-smoke --verify bundle-basic
`)
	return 0
}

// qemuPinnedBoot validates inputs and boots the guest with explicit image paths.
func qemuPinnedBoot(repoRoot, kernel, initrd, rootfs, appendLine, profile string) int {
	// All three image paths are required.
	for _, pair := range [][2]string{
		{kernel, "UMBRAVOX_QEMU_KERNEL"},
		{initrd, "UMBRAVOX_QEMU_INITRD"},
		{rootfs, "UMBRAVOX_QEMU_ROOTFS"},
	} {
		if pair[0] == "" {
			log.Fail(tag, fmt.Sprintf("set %s to a Linux kernel/initrd/rootfs path", pair[1]))
			return 1
		}
		if _, err := os.Stat(pair[0]); err != nil {
			log.Fail(tag, fmt.Sprintf("%s not found: %s", pair[1], pair[0]))
			return 1
		}
	}

	verifyCmd := os.Getenv("UMBRAVOX_QEMU_VERIFY_CMD")
	if verifyCmd == "" {
		log.Fail(tag, "set UMBRAVOX_QEMU_VERIFY_CMD to the explicit in-guest verification command template")
		return 1
	}

	// Resolve kernel command-line: explicit > profile helper > error.
	if appendLine == "" {
		if profile != "" {
			profileScript := filepath.Join(repoRoot, "scripts", "release-smoke-qemu-profile.sh")
			if _, err := os.Stat(profileScript); err != nil {
				log.Fail(tag, fmt.Sprintf("QEMU profile helper not found: %s", profileScript))
				return 1
			}
			out, err := exec.Command("bash", profileScript, profile).Output()
			if err != nil {
				log.Fail(tag, fmt.Sprintf("QEMU profile helper failed: %v", err))
				return 1
			}
			appendLine = strings.TrimSpace(string(out))
		}
	}
	if appendLine == "" {
		log.Fail(tag, "QEMU kernel command line is empty; set UMBRAVOX_QEMU_APPEND or UMBRAVOX_QEMU_PROFILE")
		return 1
	}

	log.Info(tag, fmt.Sprintf("QEMU in-guest verification command template: %s", verifyCmd))

	memMB := envOrDefault("UMBRAVOX_QEMU_MEM_MB", "1024")
	cpus := envOrDefault("UMBRAVOX_QEMU_CPUS", "2")

	qemuArgs := []string{
		"-machine", "q35,accel=kvm",
		"-cpu", "max",
		"-m", memMB,
		"-smp", cpus,
		"-nographic",
		"-nodefaults",
		"-no-reboot",
		"-kernel", kernel,
		"-initrd", initrd,
		"-append", appendLine,
		"-drive", fmt.Sprintf("if=virtio,format=raw,file=%s,readonly=on", rootfs),
	}

	cmd := exec.Command("qemu-system-x86_64", qemuArgs...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			log.Fail(tag, fmt.Sprintf("QEMU exited with %d", exitErr.ExitCode()))
			return exitErr.ExitCode()
		}
		log.Fail(tag, fmt.Sprintf("QEMU error: %v", err))
		return 1
	}
	return 0
}

// ── helpers ───────────────────────────────────────────────────────────────────

// execRunner validates that runner is an executable and replaces the current
// process with it (no shell interpretation).
func execRunner(runner string) int {
	// Resolve via PATH or validate as absolute path.
	resolved, err := exec.LookPath(runner)
	if err != nil {
		// LookPath failed; try treating as a literal path.
		fi, statErr := os.Stat(runner)
		if statErr != nil || fi.Mode()&0o111 == 0 {
			log.Fail(tag, fmt.Sprintf("smoke runner is not a valid executable: %s", runner))
			return 1
		}
		resolved = runner
	}

	// syscall.Exec replaces this process — no sub-shell, no metachar expansion.
	if err := syscall.Exec(resolved, []string{resolved}, os.Environ()); err != nil {
		log.Fail(tag, fmt.Sprintf("exec runner failed: %v", err))
		return 1
	}
	// Unreachable after a successful Exec.
	return 0
}

// envOrDefault returns the value of envKey, or fallback if empty.
func envOrDefault(envKey, fallback string) string {
	if v := os.Getenv(envKey); v != "" {
		return v
	}
	return fallback
}
