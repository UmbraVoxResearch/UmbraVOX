// SPDX-License-Identifier: Apache-2.0

// Package vmctl provides a unified VM management framework.
// It abstracts QEMU and Firecracker behind a common interface,
// handles disk management, resource scaling, and nix image building.
// The package has zero knowledge of any specific project.
package vmctl

import "time"

// HypervisorType selects the backend hypervisor.
type HypervisorType int

const (
	HypervisorQEMU        HypervisorType = iota // QEMU/KVM full-system emulation
	HypervisorFirecracker                        // Firecracker microVM
	HypervisorDirect                             // host nix-shell, no VM booted
)

// Feature enumerates optional hypervisor capabilities.
type Feature int

const (
	Feature9P     Feature = iota // 9p/virtfs host-guest file sharing
	FeatureGUI                   // graphical display output
	FeatureResize                // qcow2 overlay / COW disks
)

// DisplayMode selects VM display output.
type DisplayMode int

const (
	DisplayNone DisplayMode = iota // headless, serial on stdio
	DisplayGTK                     // GTK window with VGA
)

// NetworkMode selects the VM network backend.
type NetworkMode int

const (
	NetworkNone     NetworkMode = iota // no network
	NetworkUserMode                    // QEMU user-mode (SLIRP) networking
	NetworkTAP                         // TAP-based bridged networking
)

// DiskFormat enumerates supported disk image formats.
type DiskFormat string

const (
	DiskFormatQCOW2 DiskFormat = "qcow2"
	DiskFormatRaw   DiskFormat = "raw"
	DiskFormatExt2  DiskFormat = "ext2"
	DiskFormatExt4  DiskFormat = "ext4"
)

// Resources describes CPU and memory allocation for a VM.
type Resources struct {
	Cores    int // number of vCPUs (0 = auto-detect)
	MemoryMB int // RAM in megabytes (0 = auto-detect)
	// Fraction is the percentage of host resources to allocate when
	// Cores or MemoryMB are zero. Range 1-100; default is 50.
	Fraction int
	// MinCores is the minimum core count floor applied after scaling.
	// Zero means no floor beyond the default of 1.
	MinCores int
	// MinMemMB is the minimum memory floor in MB applied after scaling.
	// Zero means no floor beyond the default of 256.
	MinMemMB int
}

// ImageRef identifies a VM base image.
type ImageRef struct {
	Path string // local path to the image file
	// Format is the on-disk format (raw, qcow2, etc).
	Format DiskFormat
}

// DiskSpec describes a disk to attach to the VM.
type DiskSpec struct {
	Path      string     // host path to the disk image
	Format    DiskFormat // image format
	ReadOnly  bool
	Interface string // e.g. "virtio" (hypervisor-specific)
}

// ShareSpec describes a host-guest filesystem share (9p/virtfs).
type ShareSpec struct {
	HostPath      string // host directory to share
	MountTag      string // guest mount tag
	SecurityModel string // e.g. "mapped-xattr"
	ID            string // share identifier
}

// NetworkSpec describes the VM network configuration.
type NetworkSpec struct {
	Mode    NetworkMode
	RawArgs string // optional raw hypervisor-specific network args
}

// BootSpec describes kernel/initrd boot parameters for direct-kernel boot.
// When used with HypervisorDirect, only Command and ShellNix are relevant;
// the kernel fields are ignored.
type BootSpec struct {
	KernelPath string // path to kernel image
	InitrdPath string // path to initrd (optional)
	KernelArgs string // kernel command-line arguments
	Command    string // shell command for HypervisorDirect (passed to nix-shell --run)
	ShellNix   string // path to shell.nix for HypervisorDirect (default: <repoRoot>/shell.nix)
}

// VMSpec is the complete specification for launching a VM.
type VMSpec struct {
	Hypervisor HypervisorType
	Resources  Resources
	BaseImage  ImageRef
	Boot       *BootSpec    // nil for disk-boot (QEMU default)
	Disks      []DiskSpec   // additional drives beyond the base image
	Shares     []ShareSpec  // host-guest filesystem shares
	Network    NetworkSpec
	Display    DisplayMode
	NoReboot   bool          // power-off instead of reboot on exit
	Timeout    time.Duration // kill VM after this duration (0 = unlimited)

	// StatusFile is the guest-written file (inside an output share) that
	// contains the guest exit code. Empty means use the hypervisor exit code.
	StatusFile string
}

// Result holds the outcome of a VM invocation.
type Result struct {
	ExitCode   int    // guest exit code (from StatusFile) or hypervisor exit code
	StatusFile string // path to the status file that was read, if any
}

// Logger is the interface for VM lifecycle messages.
// Implementations typically write colored output to stderr.
type Logger interface {
	Info(tag, msg string)
	Warn(tag, msg string)
	Fail(tag, msg string)
	OK(tag, msg string)
}
