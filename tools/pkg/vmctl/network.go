// SPDX-License-Identifier: Apache-2.0

package vmctl

// NetworkMode constants are defined in vmctl.go (NetworkNone, NetworkUserMode, NetworkTAP).
// This file provides helper functions for network configuration.

// String returns a human-readable label for the network mode.
func (m NetworkMode) String() string {
	switch m {
	case NetworkNone:
		return "none"
	case NetworkUserMode:
		return "user"
	case NetworkTAP:
		return "tap"
	case NetworkSlirp:
		return "slirp"
	default:
		return "unknown"
	}
}

// QEMUNetArgs returns the QEMU command-line arguments for this network mode.
// For NetworkNone it returns "-nic none"; for user mode it returns a basic
// user-mode NIC; for TAP mode callers must supply RawArgs in NetworkSpec.
func (m NetworkMode) QEMUNetArgs() string {
	switch m {
	case NetworkUserMode:
		return "-nic user,model=virtio"
	case NetworkTAP:
		return "" // caller must supply raw args
	default:
		return "-nic none"
	}
}
