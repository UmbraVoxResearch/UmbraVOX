// SPDX-License-Identifier: Apache-2.0
// Package vmctl provides shared VM control helpers used by multiple commands.
package vmctl

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
)

// PreflightKVM checks that /dev/kvm is present.
func PreflightKVM() error {
	if _, err := os.Stat("/dev/kvm"); os.IsNotExist(err) {
		return fmt.Errorf("/dev/kvm not found (KVM required)")
	}
	return nil
}

// PreflightQEMU checks that qemu-system-x86_64 is on PATH and /dev/kvm exists.
func PreflightQEMU() error {
	var errs []string

	if _, err := os.Stat("/dev/kvm"); os.IsNotExist(err) {
		errs = append(errs, "/dev/kvm not found (KVM required)")
	}
	if _, err := exec.LookPath("qemu-system-x86_64"); err != nil {
		errs = append(errs, "qemu-system-x86_64 not on PATH")
	}

	if len(errs) > 0 {
		return fmt.Errorf("QEMU preflight failed:\n  %s", strings.Join(errs, "\n  "))
	}
	return nil
}

// PreflightFirecracker checks that firecracker is on PATH and /dev/kvm exists.
func PreflightFirecracker() error {
	var errs []string

	if _, err := os.Stat("/dev/kvm"); os.IsNotExist(err) {
		errs = append(errs, "/dev/kvm not found (KVM required for Firecracker)")
	}
	if _, err := exec.LookPath("firecracker"); err != nil {
		errs = append(errs, "firecracker not on PATH (install via nix or package manager)")
	}

	if len(errs) > 0 {
		return fmt.Errorf("Firecracker preflight failed:\n  %s", strings.Join(errs, "\n  "))
	}
	return nil
}

// PreflightDirect checks that nix-shell is on PATH (or at the default Nix
// profile location). No KVM device is required.
func PreflightDirect() error {
	if _, err := exec.LookPath("nix-shell"); err == nil {
		return nil
	}
	const defaultPath = "/nix/var/nix/profiles/default/bin/nix-shell"
	if _, err := os.Stat(defaultPath); err == nil {
		return nil
	}
	return fmt.Errorf("Direct preflight failed:\n  nix-shell not found on PATH or at %s", defaultPath)
}
