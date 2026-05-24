// SPDX-License-Identifier: Apache-2.0
// Package repo finds the UmbraVOX repository root and runs preflight checks.
package repo

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
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

// Preflight checks KVM, QEMU, and genext2fs availability.
// If requireImage is true, also checks that vmImagePath exists.
func Preflight(vmImagePath string, requireImage bool) error {
	ok := true

	if _, err := os.Stat("/dev/kvm"); os.IsNotExist(err) {
		log.Warn("UV", "/dev/kvm not found; falling back to TCG (slow)")
	}
	if _, err := exec.LookPath("qemu-system-x86_64"); err != nil {
		log.Fail("UV", "qemu-system-x86_64 not on PATH")
		ok = false
	}
	if _, err := exec.LookPath("genext2fs"); err != nil {
		log.Fail("UV", "genext2fs not on PATH")
		ok = false
	}
	if requireImage {
		if fi, err := os.Stat(vmImagePath); err != nil || !fi.IsDir() {
			log.Fail("UV", fmt.Sprintf("VM image not found at %s", vmImagePath))
			log.Warn("UV", "Run './uv vm build-image' first.")
			ok = false
		}
	}

	if !ok {
		return fmt.Errorf("preflight checks failed")
	}
	return nil
}
