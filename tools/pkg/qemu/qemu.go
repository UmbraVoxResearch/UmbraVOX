// Package qemu builds QEMU command-line arguments for UmbraVOX development VMs.
//
// This is the Go equivalent of the QEMU argument construction in
// scripts/vm-dev-run.sh (lines ~310-330). It assembles the -machine,
// -cpu, -m, -smp, -drive, -virtfs, display, and network arguments
// into a structured representation that can be rendered to a string slice.
package qemu

import (
	"fmt"
	"runtime"
)

// DriveFormat enumerates supported QEMU disk image formats.
type DriveFormat string

const (
	FormatQCOW2 DriveFormat = "qcow2"
	FormatRaw   DriveFormat = "raw"
)

// Drive describes a single -drive argument.
type Drive struct {
	Interface string      // e.g. "virtio"
	Format    DriveFormat // e.g. FormatQCOW2, FormatRaw
	File      string      // path to the image
	ReadOnly  bool
}

// VirtFS describes a single -virtfs 9p share.
type VirtFS struct {
	LocalPath     string // host directory
	MountTag      string // guest mount tag
	SecurityModel string // e.g. "mapped-xattr"
	ID            string
}

// DisplayMode selects QEMU display output.
type DisplayMode int

const (
	DisplayNone    DisplayMode = iota // -nographic -nodefaults -serial stdio
	DisplayGTK                        // -display gtk -vga std
)

// Config holds all parameters needed to construct QEMU arguments.
type Config struct {
	Machine    string      // e.g. "q35,accel=kvm"
	CPUModel   string      // e.g. "max"
	MemoryMB   int         // RAM in megabytes
	SMP        int         // number of vCPUs
	Drives     []Drive
	VirtFS     []VirtFS
	NetArgs    string      // raw network args, e.g. "-nic none"
	Display    DisplayMode
	NoReboot   bool        // add -no-reboot (for exec mode)
}

// DefaultConfig returns a Config with sensible defaults for UmbraVOX dev VMs.
// Cores and memory are auto-scaled to 50% of the host (minimum 25%, floor 2 cores / 2048 MB).
func DefaultConfig() Config {
	cores := runtime.NumCPU()
	vmCores := cores / 2
	if min := cores / 4; vmCores < min {
		vmCores = min
	}
	if vmCores < 2 {
		vmCores = 2
	}

	// Memory detection is platform-specific; default to 4096 MB.
	// The caller should override MemoryMB after reading /proc/meminfo or equivalent.
	vmMem := 4096

	return Config{
		Machine:  "q35,accel=kvm",
		CPUModel: "max",
		MemoryMB: vmMem,
		SMP:      vmCores,
		NetArgs:  "-nic none",
		Display:  DisplayNone,
	}
}

// Args renders the Config into a QEMU command-line argument slice
// suitable for exec.Command("qemu-system-x86_64", args...).
func (c *Config) Args() []string {
	args := []string{
		"-machine", c.Machine,
		"-cpu", c.CPUModel,
		"-m", fmt.Sprintf("%d", c.MemoryMB),
		"-smp", fmt.Sprintf("%d", c.SMP),
	}

	for _, d := range c.Drives {
		spec := fmt.Sprintf("if=%s,format=%s,file=%s", d.Interface, d.Format, d.File)
		if d.ReadOnly {
			spec += ",readonly=on"
		}
		args = append(args, "-drive", spec)
	}

	for _, v := range c.VirtFS {
		spec := fmt.Sprintf("local,path=%s,mount_tag=%s,security_model=%s,id=%s",
			v.LocalPath, v.MountTag, v.SecurityModel, v.ID)
		args = append(args, "-virtfs", spec)
	}

	// Network args are passed as a raw string (split later or passed directly).
	// For now, append as-is.
	if c.NetArgs != "" {
		args = append(args, c.NetArgs)
	}

	switch c.Display {
	case DisplayGTK:
		args = append(args, "-display", "gtk", "-vga", "std")
	default:
		args = append(args, "-nographic", "-nodefaults", "-serial", "stdio")
	}

	if c.NoReboot {
		args = append(args, "-no-reboot")
	}

	return args
}
