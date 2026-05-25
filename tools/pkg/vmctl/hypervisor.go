// SPDX-License-Identifier: Apache-2.0

package vmctl

import "context"

// Hypervisor is the common interface implemented by each VM backend.
type Hypervisor interface {
	// Preflight checks that all runtime dependencies (binaries, devices)
	// are available. Returns a descriptive error on failure.
	Preflight() error

	// Boot launches the VM described by spec, using tmpDir for ephemeral
	// files (overlays, configs). It blocks until the VM exits or ctx is
	// cancelled.
	Boot(ctx context.Context, spec *VMSpec, tmpDir string) (*Result, error)

	// Supports reports whether this backend supports the given feature.
	Supports(feature Feature) bool
}
