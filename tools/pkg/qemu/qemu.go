// SPDX-License-Identifier: Apache-2.0
// Package qemu builds QEMU command-line arguments for UmbraVOX development VMs.
package qemu

import (
	"bufio"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"strings"
	"time"
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

// VMProfile selects resource scaling presets.
type VMProfile int

const (
	ProfileDev     VMProfile = iota // 50% host resources (interactive dev)
	ProfileBuild                    // 75% host resources (batch builds)
	ProfileRuntime                  // 25% host resources (signal server runtime)
)

// Config holds all parameters needed to construct QEMU arguments.
type Config struct {
	Machine    string        // e.g. "q35,accel=kvm"
	CPUModel   string        // e.g. "max"
	MemoryMB   int           // RAM in megabytes
	SMP        int           // number of vCPUs
	Drives     []Drive
	VirtFS     []VirtFS
	NetArgs    string        // raw network args, e.g. "-nic none"
	Display    DisplayMode
	NoReboot   bool          // add -no-reboot (for exec mode)
	Timeout    time.Duration // kill VM after this duration (0 = no timeout)
}

// ReadHostMemoryMB reads total host memory from /proc/meminfo.
// Returns 8192 as a fallback on non-Linux or read failure.
func ReadHostMemoryMB() int {
	f, err := os.Open("/proc/meminfo")
	if err != nil {
		return 8192
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "MemTotal:") {
			fields := strings.Fields(line)
			if len(fields) >= 2 {
				kb, err := strconv.Atoi(fields[1])
				if err == nil {
					return kb / 1024
				}
			}
		}
	}
	return 8192
}

// ScaleToHost returns (cores, memMB) for a given VMProfile based on
// the current host's resources.
func ScaleToHost(p VMProfile) (cores int, memMB int) {
	hostCores := runtime.NumCPU()
	hostMem := ReadHostMemoryMB()

	switch p {
	case ProfileBuild:
		cores = hostCores * 3 / 4
		memMB = hostMem * 3 / 4
	case ProfileRuntime:
		cores = hostCores / 4
		memMB = hostMem / 4
	default: // ProfileDev
		cores = hostCores / 2
		memMB = hostMem / 2
	}

	// Floor: 25% of host or absolute minimum
	if minC := hostCores / 4; cores < minC {
		cores = minC
	}
	if minM := hostMem / 4; memMB < minM {
		memMB = minM
	}
	if cores < 2 {
		cores = 2
	}
	if memMB < 2048 {
		memMB = 2048
	}
	return cores, memMB
}

// DefaultConfig returns a Config with sensible defaults for UmbraVOX dev VMs.
// Cores and memory are auto-scaled to 50% of the host.
func DefaultConfig() Config {
	cores, mem := ScaleToHost(ProfileDev)
	return Config{
		Machine:  "q35,accel=kvm",
		CPUModel: "max",
		MemoryMB: mem,
		SMP:      cores,
		NetArgs:  "-nic none",
		Display:  DisplayNone,
	}
}

// ProfileConfig returns a Config pre-configured for the given VMProfile.
func ProfileConfig(p VMProfile) Config {
	cores, mem := ScaleToHost(p)
	return Config{
		Machine:  "q35,accel=kvm",
		CPUModel: "max",
		MemoryMB: mem,
		SMP:      cores,
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
