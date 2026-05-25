// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"context"
	"fmt"
)

// Boot is the unified entry point. It selects the hypervisor based on
// spec.Hypervisor and delegates to the appropriate implementation.
func Boot(ctx context.Context, spec *VMSpec, tmpDir string) (*Result, error) {
	var h Hypervisor
	switch spec.Hypervisor {
	case HypervisorQEMU:
		h = &QEMUHypervisor{}
	case HypervisorFirecracker:
		h = &FirecrackerHypervisor{}
	case HypervisorDirect:
		shellNix := ""
		if spec.Boot != nil {
			shellNix = spec.Boot.ShellNix
		}
		h = &DirectHypervisor{ShellNix: shellNix}
	default:
		return nil, fmt.Errorf("unknown hypervisor type: %d", spec.Hypervisor)
	}
	if err := h.Preflight(); err != nil {
		return nil, fmt.Errorf("preflight: %w", err)
	}
	return h.Boot(ctx, spec, tmpDir)
}
