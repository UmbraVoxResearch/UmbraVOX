// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"context"
	"fmt"
	"os"
	"os/exec"
)

// DirectHypervisor runs commands on the host via nix-shell.
// No VM is booted — the command executes directly in the host's
// nix-shell environment. Used for CI runners without KVM access.
type DirectHypervisor struct {
	ShellNix string // path to shell.nix (default: <repoRoot>/shell.nix)
	Logger   Logger
}

// Preflight checks that nix-shell is on PATH.
func (d *DirectHypervisor) Preflight() error {
	return PreflightDirect()
}

// Boot runs nix-shell <ShellNix> --pure --run "<spec.Boot.Command>" with
// timeout derived from ctx. stdin/stdout/stderr are connected to the host
// process. The returned Result carries the command's exit code.
func (d *DirectHypervisor) Boot(ctx context.Context, spec *VMSpec, _ string) (*Result, error) {
	if spec.Boot == nil || spec.Boot.Command == "" {
		return nil, fmt.Errorf("DirectHypervisor: spec.Boot.Command must not be empty")
	}

	shellNix := d.ShellNix
	if shellNix == "" {
		return nil, fmt.Errorf("DirectHypervisor: ShellNix path is required")
	}

	if _, err := os.Stat(shellNix); err != nil {
		return nil, fmt.Errorf("DirectHypervisor: shell.nix not found at %s: %w", shellNix, err)
	}

	nixShell, err := exec.LookPath("nix-shell")
	if err != nil {
		// Fall back to the default Nix profile location.
		const defaultPath = "/nix/var/nix/profiles/default/bin/nix-shell"
		if _, statErr := os.Stat(defaultPath); statErr == nil {
			nixShell = defaultPath
		} else {
			return nil, fmt.Errorf("DirectHypervisor: nix-shell not found on PATH or at %s", defaultPath)
		}
	}

	cmd := exec.CommandContext(ctx, nixShell, shellNix, "--pure", "--run", spec.Boot.Command)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	runErr := cmd.Run()

	result := &Result{}
	if runErr != nil {
		if exitErr, ok := runErr.(*exec.ExitError); ok {
			result.ExitCode = exitErr.ExitCode()
		} else {
			return nil, fmt.Errorf("DirectHypervisor: nix-shell error: %w", runErr)
		}
	}

	return result, nil
}

// Supports reports which features the DirectHypervisor provides.
// Display (GUI), 9p shares, and overlay disks are not available since no VM
// is booted. UserNet (real host network) is always available.
func (d *DirectHypervisor) Supports(feature Feature) bool {
	switch feature {
	case FeatureGUI, Feature9P, FeatureResize:
		return false
	default:
		return true
	}
}
