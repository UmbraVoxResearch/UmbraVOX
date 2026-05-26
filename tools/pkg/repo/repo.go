// SPDX-License-Identifier: Apache-2.0
// Package repo finds the UmbraVOX repository root and runs preflight checks.
package repo

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/vmctl"
)

// Root walks up from the executable path (then cwd) to find the repo root.
// It looks for the uv bootstrap script alongside tools/, or UmbraVox.cabal.
func Root() (string, error) {
	isRoot := func(dir string) bool {
		// Primary: uv wrapper + tools/ directory
		if _, err := os.Stat(filepath.Join(dir, "uv")); err == nil {
			if _, err := os.Stat(filepath.Join(dir, "tools")); err == nil {
				return true
			}
		}
		// Fallback: UmbraVox.cabal
		if _, err := os.Stat(filepath.Join(dir, "UmbraVox.cabal")); err == nil {
			return true
		}
		return false
	}

	// Try from executable location first
	if exe, err := os.Executable(); err == nil {
		dir := filepath.Dir(exe)
		for i := 0; i < 10; i++ {
			if isRoot(dir) {
				return dir, nil
			}
			dir = filepath.Dir(dir)
		}
	}

	// Fallback: walk up from cwd
	cwd, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("cannot determine working directory: %w", err)
	}
	dir := cwd
	for i := 0; i < 10; i++ {
		if isRoot(dir) {
			return dir, nil
		}
		dir = filepath.Dir(dir)
	}

	return "", fmt.Errorf("cannot find repo root (looked for uv + tools/ or UmbraVox.cabal)")
}

// Preflight checks tools and VM image availability in order:
// 1. Check tools (qemu, genext2fs) → offer nix-shell if missing
// 2. Check VM image → offer to build if missing
func Preflight(vmImagePath string, requireImage bool) error {
	if _, err := os.Stat("/dev/kvm"); os.IsNotExist(err) {
		log.Warn("UV", "/dev/kvm not found; falling back to TCG (slow)")
	}

	// Step 1: Check tools — offer nix-shell if missing
	missingTools := false
	if _, err := exec.LookPath("qemu-system-x86_64"); err != nil {
		missingTools = true
	}
	if _, err := exec.LookPath("genext2fs"); err != nil {
		missingTools = true
	}

	if missingTools && os.Getenv("IN_NIX_SHELL") == "" {
		repoRoot, _ := Root()
		shellNix := filepath.Join(repoRoot, "shell-minimal.nix")
		if _, err := os.Stat(shellNix); err != nil {
			shellNix = filepath.Join(repoRoot, "shell.nix")
		}

		log.Warn("UV", "Required tools (qemu, genext2fs) not on PATH.")
		fmt.Fprintf(os.Stderr, "\n  Enter nix-shell to get them? [y/N] ")
		var answer string
		fmt.Scanln(&answer)
		if answer == "y" || answer == "Y" || answer == "yes" {
			nixShell, err := exec.LookPath("nix-shell")
			if err != nil {
				return fmt.Errorf("nix-shell not found on PATH")
			}
			// Re-exec via ./uv wrapper (not the Go binary directly)
			// so the bootstrap can recompile if needed
			repoRoot2, _ := Root()
			uvPath := filepath.Join(repoRoot2, "uv")
			innerCmd := uvPath
			for _, a := range os.Args[1:] {
				innerCmd += " " + a
			}
			cmd := exec.Command(nixShell, shellNix, "--run", innerCmd)
			cmd.Stdin = os.Stdin
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				if exitErr, ok := err.(*exec.ExitError); ok {
					os.Exit(exitErr.ExitCode())
				}
				os.Exit(1)
			}
			os.Exit(0)
		}
		return fmt.Errorf("preflight checks failed — missing tools")
	} else if missingTools {
		return fmt.Errorf("preflight checks failed — qemu/genext2fs not on PATH")
	}

	// Step 2: Check VM image — offer to build if missing
	if requireImage {
		if fi, err := os.Stat(vmImagePath); err != nil || !fi.IsDir() {
			log.Warn("UV", "VM image not found.")
			fmt.Fprintf(os.Stderr, "\n  Build the VM image now? [y/N] ")
			var answer string
			fmt.Scanln(&answer)
			if answer == "y" || answer == "Y" || answer == "yes" {
				repoRoot, _ := Root()
				buildCmd := exec.Command(filepath.Join(repoRoot, "uv"), "vm", "build-image")
				buildCmd.Stdin = os.Stdin
				buildCmd.Stdout = os.Stdout
				buildCmd.Stderr = os.Stderr
				if err := buildCmd.Run(); err != nil {
					return fmt.Errorf("VM image build failed")
				}
				if fi, err := os.Stat(vmImagePath); err != nil || !fi.IsDir() {
					return fmt.Errorf("VM image still not found after build")
				}
			} else {
				log.Info("UV", "Run './uv vm build-image' to build the VM image.")
				return fmt.Errorf("VM image not found")
			}
		}
	}

	return nil
}

// PreflightFirecracker checks that Firecracker and KVM are available.
// Delegates to vmctl.PreflightFirecracker.
func PreflightFirecracker() error {
	return vmctl.PreflightFirecracker()
}
