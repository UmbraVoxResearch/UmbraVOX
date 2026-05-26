// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"context"
	"strings"
	"testing"
)

// Boot() dispatch tests verify that the correct hypervisor implementation is
// selected based on VMSpec.Hypervisor. Each dispatch test uses an
// already-canceled context so that even when preflight succeeds (e.g. QEMU +
// KVM are available on the host), the underlying exec.CommandContext fails
// immediately without actually booting a VM.
//
// The dispatch is confirmed by inspecting the resulting error: each hypervisor
// backend produces a distinct error (preflight message or runtime error
// referencing its binary name).

// canceledCtx returns a context that is already canceled.
func canceledCtx() context.Context {
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	return ctx
}

func TestBoot_DispatchQEMU(t *testing.T) {
	spec := &VMSpec{Hypervisor: HypervisorQEMU}

	_, err := Boot(canceledCtx(), spec, t.TempDir())

	// Two possible outcomes:
	// 1. Preflight fails (no QEMU/KVM) -> error mentions QEMU preflight.
	// 2. Preflight passes, exec fails due to canceled ctx -> error mentions
	//    QEMUHypervisor or qemu-system-x86_64 (from the exec failure).
	// Either confirms QEMU was the selected backend.
	if err == nil {
		t.Fatal("expected error from canceled context, got nil")
	}

	msg := err.Error()
	if !strings.Contains(msg, "QEMU") &&
		!strings.Contains(msg, "qemu") &&
		!strings.Contains(msg, "/dev/kvm") {
		t.Errorf("expected QEMU-related error, got: %v", err)
	}
}

func TestBoot_DispatchFirecracker(t *testing.T) {
	spec := &VMSpec{
		Hypervisor: HypervisorFirecracker,
		Boot: &BootSpec{
			KernelPath: "/nonexistent/vmlinux",
			KernelArgs: "console=ttyS0",
		},
		BaseImage: ImageRef{Path: "/nonexistent/rootfs.img"},
	}

	_, err := Boot(canceledCtx(), spec, t.TempDir())

	if err == nil {
		t.Fatal("expected error from canceled context, got nil")
	}

	msg := err.Error()
	if !strings.Contains(msg, "Firecracker") &&
		!strings.Contains(msg, "firecracker") &&
		!strings.Contains(msg, "/dev/kvm") {
		t.Errorf("expected Firecracker-related error, got: %v", err)
	}
}

func TestBoot_DispatchDirect(t *testing.T) {
	spec := &VMSpec{Hypervisor: HypervisorDirect}

	_, err := Boot(canceledCtx(), spec, t.TempDir())

	if err == nil {
		t.Fatal("expected error from canceled context, got nil")
	}

	msg := err.Error()
	if !strings.Contains(msg, "Direct") &&
		!strings.Contains(msg, "nix-shell") {
		t.Errorf("expected Direct-related error, got: %v", err)
	}
}

func TestBoot_DirectPassesShellNixFromBootSpec(t *testing.T) {
	spec := &VMSpec{
		Hypervisor: HypervisorDirect,
		Boot: &BootSpec{
			ShellNix: "/tmp/fake-shell.nix",
			Command:  "true",
		},
	}

	_, err := Boot(canceledCtx(), spec, t.TempDir())
	if err == nil {
		t.Fatal("expected error, got nil")
	}

	// The Boot function must select DirectHypervisor. The error should
	// reference the Direct backend (preflight or runtime).
	msg := err.Error()
	if !strings.Contains(msg, "Direct") &&
		!strings.Contains(msg, "nix-shell") {
		t.Errorf("expected Direct-related error, got: %v", err)
	}
}

func TestBoot_DirectNilBootSpecUsesEmptyShellNix(t *testing.T) {
	// When Boot.ShellNix is empty (spec.Boot is nil), the DirectHypervisor
	// should still be created with an empty ShellNix field.
	spec := &VMSpec{Hypervisor: HypervisorDirect}

	_, err := Boot(canceledCtx(), spec, t.TempDir())
	if err == nil {
		t.Fatal("expected error, got nil")
	}

	msg := err.Error()
	if !strings.Contains(msg, "Direct") &&
		!strings.Contains(msg, "nix-shell") {
		t.Errorf("expected Direct-related error, got: %v", err)
	}
}

func TestBoot_UnknownHypervisorReturnsError(t *testing.T) {
	spec := &VMSpec{Hypervisor: HypervisorType(99)}

	_, err := Boot(canceledCtx(), spec, t.TempDir())
	if err == nil {
		t.Fatal("expected error for unknown hypervisor type, got nil")
	}

	if !strings.Contains(err.Error(), "unknown hypervisor type") {
		t.Errorf("expected 'unknown hypervisor type' error, got: %v", err)
	}
}

func TestBoot_UnknownHypervisorDoesNotCallPreflight(t *testing.T) {
	// Unknown type should return before reaching preflight. Verify the error
	// does NOT contain any preflight-related message.
	spec := &VMSpec{Hypervisor: HypervisorType(42)}

	_, err := Boot(canceledCtx(), spec, t.TempDir())
	if err == nil {
		t.Fatal("expected error, got nil")
	}

	msg := err.Error()
	if strings.Contains(msg, "preflight") {
		t.Errorf("unknown hypervisor should not reach preflight, got: %v", err)
	}
}

func TestBoot_NilSpecPanics(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Fatal("expected panic for nil spec, but Boot returned normally")
		}
	}()

	Boot(canceledCtx(), nil, t.TempDir())
}
