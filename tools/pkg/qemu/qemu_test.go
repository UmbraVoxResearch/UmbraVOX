// SPDX-License-Identifier: Apache-2.0

package qemu

import (
	"fmt"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/UmbraVoxResearch/vmctl"
)

// hostCores and hostMem capture actual host values so assertions stay
// machine-independent.
var (
	hostCores = runtime.NumCPU()
	hostMem   = vmctl.ReadHostMemoryMB()
)

// ---------- DefaultConfig ----------

func TestDefaultConfig_Populated(t *testing.T) {
	c := DefaultConfig()

	if c.Machine == "" {
		t.Error("Machine must not be empty")
	}
	if c.CPUModel == "" {
		t.Error("CPUModel must not be empty")
	}
	if c.MemoryMB <= 0 {
		t.Errorf("MemoryMB must be positive, got %d", c.MemoryMB)
	}
	if c.SMP <= 0 {
		t.Errorf("SMP must be positive, got %d", c.SMP)
	}
	if c.NetArgs == "" {
		t.Error("NetArgs must not be empty in default config")
	}
}

func TestDefaultConfig_UsesKVM(t *testing.T) {
	c := DefaultConfig()
	if !strings.Contains(c.Machine, "accel=kvm") {
		t.Errorf("Machine should include KVM acceleration, got %q", c.Machine)
	}
}

func TestDefaultConfig_DisplayNone(t *testing.T) {
	c := DefaultConfig()
	if c.Display != DisplayNone {
		t.Errorf("Display: got %d, want DisplayNone (%d)", c.Display, DisplayNone)
	}
}

// ---------- ScaleToHost ----------

func TestScaleToHost_Dev(t *testing.T) {
	cores, mem := ScaleToHost(ProfileDev)

	if cores < 2 {
		t.Errorf("ProfileDev cores must be >= 2 (MinCores), got %d", cores)
	}
	if mem < 2048 {
		t.Errorf("ProfileDev memory must be >= 2048 (MinMemMB), got %d", mem)
	}
}

func TestScaleToHost_Build(t *testing.T) {
	cores, mem := ScaleToHost(ProfileBuild)

	if cores < 2 {
		t.Errorf("ProfileBuild cores must be >= 2 (MinCores), got %d", cores)
	}
	if mem < 2048 {
		t.Errorf("ProfileBuild memory must be >= 2048 (MinMemMB), got %d", mem)
	}

	// Build should allocate >= Dev on any machine with >4 cores.
	devCores, devMem := ScaleToHost(ProfileDev)
	if hostCores > 4 && cores < devCores {
		t.Errorf("ProfileBuild cores (%d) should be >= ProfileDev cores (%d)", cores, devCores)
	}
	if hostMem > 8192 && mem < devMem {
		t.Errorf("ProfileBuild mem (%d) should be >= ProfileDev mem (%d)", mem, devMem)
	}
}

func TestScaleToHost_Runtime(t *testing.T) {
	cores, mem := ScaleToHost(ProfileRuntime)

	if cores < 2 {
		t.Errorf("ProfileRuntime cores must be >= 2 (MinCores), got %d", cores)
	}
	if mem < 2048 {
		t.Errorf("ProfileRuntime memory must be >= 2048 (MinMemMB), got %d", mem)
	}

	// Runtime should allocate <= Dev on any machine with >8 cores.
	devCores, devMem := ScaleToHost(ProfileDev)
	if hostCores > 8 && cores > devCores {
		t.Errorf("ProfileRuntime cores (%d) should be <= ProfileDev cores (%d)", cores, devCores)
	}
	if hostMem > 16384 && mem > devMem {
		t.Errorf("ProfileRuntime mem (%d) should be <= ProfileDev mem (%d)", mem, devMem)
	}
}

// ---------- ProfileConfig ----------

func TestProfileConfig_AllProfiles(t *testing.T) {
	profiles := []struct {
		name string
		p    VMProfile
	}{
		{"Dev", ProfileDev},
		{"Build", ProfileBuild},
		{"Runtime", ProfileRuntime},
	}

	for _, tc := range profiles {
		t.Run(tc.name, func(t *testing.T) {
			c := ProfileConfig(tc.p)
			if c.Machine == "" {
				t.Error("Machine must not be empty")
			}
			if c.CPUModel == "" {
				t.Error("CPUModel must not be empty")
			}
			if c.MemoryMB <= 0 {
				t.Errorf("MemoryMB must be positive, got %d", c.MemoryMB)
			}
			if c.SMP <= 0 {
				t.Errorf("SMP must be positive, got %d", c.SMP)
			}
		})
	}
}

// ---------- Args ----------

func TestArgs_Minimal(t *testing.T) {
	c := Config{
		Machine:  "q35,accel=kvm",
		CPUModel: "max",
		MemoryMB: 4096,
		SMP:      4,
		NetArgs:  "-nic none",
		Display:  DisplayNone,
	}
	args := c.Args()

	expect := map[string]string{
		"-machine": "q35,accel=kvm",
		"-cpu":     "max",
		"-m":       "4096",
		"-smp":     "4",
	}
	for flag, want := range expect {
		idx := indexOf(args, flag)
		if idx < 0 || idx+1 >= len(args) {
			t.Errorf("missing flag %s in args", flag)
			continue
		}
		if args[idx+1] != want {
			t.Errorf("%s: got %q, want %q", flag, args[idx+1], want)
		}
	}
}

func TestArgs_DisplayNone(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
		Display:  DisplayNone,
	}
	args := c.Args()

	for _, flag := range []string{"-nographic", "-nodefaults"} {
		if indexOf(args, flag) < 0 {
			t.Errorf("DisplayNone: missing %s", flag)
		}
	}
	idx := indexOf(args, "-serial")
	if idx < 0 || idx+1 >= len(args) || args[idx+1] != "stdio" {
		t.Error("DisplayNone: expected -serial stdio")
	}
}

func TestArgs_DisplayGTK(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
		Display:  DisplayGTK,
	}
	args := c.Args()

	idx := indexOf(args, "-display")
	if idx < 0 || idx+1 >= len(args) || args[idx+1] != "gtk" {
		t.Error("DisplayGTK: expected -display gtk")
	}
	idx = indexOf(args, "-vga")
	if idx < 0 || idx+1 >= len(args) || args[idx+1] != "std" {
		t.Error("DisplayGTK: expected -vga std")
	}
	if indexOf(args, "-nographic") >= 0 {
		t.Error("DisplayGTK: should not include -nographic")
	}
}

func TestArgs_NoReboot(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
		NoReboot: true,
	}
	args := c.Args()

	if indexOf(args, "-no-reboot") < 0 {
		t.Error("NoReboot=true: missing -no-reboot flag")
	}
}

func TestArgs_NoReboot_Omitted(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
		NoReboot: false,
	}
	args := c.Args()

	if indexOf(args, "-no-reboot") >= 0 {
		t.Error("NoReboot=false: should not include -no-reboot flag")
	}
}

func TestArgs_Drive(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
		Drives: []Drive{
			{
				Interface: "virtio",
				Format:    FormatQCOW2,
				File:      "/tmp/disk.qcow2",
				ReadOnly:  false,
			},
		},
	}
	args := c.Args()

	idx := indexOf(args, "-drive")
	if idx < 0 || idx+1 >= len(args) {
		t.Fatal("missing -drive flag")
	}
	spec := args[idx+1]
	for _, want := range []string{"if=virtio", "format=qcow2", "file=/tmp/disk.qcow2"} {
		if !strings.Contains(spec, want) {
			t.Errorf("-drive spec missing %q: got %q", want, spec)
		}
	}
	if strings.Contains(spec, "readonly=on") {
		t.Error("-drive should not contain readonly=on when ReadOnly=false")
	}
}

func TestArgs_Drive_ReadOnly(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
		Drives: []Drive{
			{
				Interface: "virtio",
				Format:    FormatRaw,
				File:      "/tmp/disk.raw",
				ReadOnly:  true,
			},
		},
	}
	args := c.Args()

	idx := indexOf(args, "-drive")
	if idx < 0 || idx+1 >= len(args) {
		t.Fatal("missing -drive flag")
	}
	spec := args[idx+1]
	if !strings.Contains(spec, "readonly=on") {
		t.Errorf("-drive spec should contain readonly=on: got %q", spec)
	}
	if !strings.Contains(spec, "format=raw") {
		t.Errorf("-drive spec should contain format=raw: got %q", spec)
	}
}

func TestArgs_MultipleDrives(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
		Drives: []Drive{
			{Interface: "virtio", Format: FormatQCOW2, File: "/tmp/a.qcow2"},
			{Interface: "virtio", Format: FormatRaw, File: "/tmp/b.raw"},
		},
	}
	args := c.Args()

	count := 0
	for _, a := range args {
		if a == "-drive" {
			count++
		}
	}
	if count != 2 {
		t.Errorf("expected 2 -drive flags, got %d", count)
	}
}

func TestArgs_VirtFS(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
		VirtFS: []VirtFS{
			{
				LocalPath:     "/home/user/src",
				MountTag:      "src",
				SecurityModel: "mapped-xattr",
				ID:            "fsdev0",
			},
		},
	}
	args := c.Args()

	idx := indexOf(args, "-virtfs")
	if idx < 0 || idx+1 >= len(args) {
		t.Fatal("missing -virtfs flag")
	}
	spec := args[idx+1]
	for _, want := range []string{
		"local,path=/home/user/src",
		"mount_tag=src",
		"security_model=mapped-xattr",
		"id=fsdev0",
	} {
		if !strings.Contains(spec, want) {
			t.Errorf("-virtfs spec missing %q: got %q", want, spec)
		}
	}
}

func TestArgs_NetArgs_Splitting(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
		NetArgs:  "-nic user,model=virtio",
	}
	args := c.Args()

	idx := indexOf(args, "-nic")
	if idx < 0 || idx+1 >= len(args) {
		t.Fatal("NetArgs splitting: missing -nic")
	}
	if args[idx+1] != "user,model=virtio" {
		t.Errorf("NetArgs splitting: got %q, want %q", args[idx+1], "user,model=virtio")
	}
}

func TestArgs_NetArgs_Empty(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
		NetArgs:  "",
	}
	args := c.Args()

	if indexOf(args, "-nic") >= 0 {
		t.Error("empty NetArgs should not produce -nic in args")
	}
}

// ---------- Edge cases ----------

func TestArgs_ZeroMemory(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 0,
		SMP:      2,
	}
	args := c.Args()

	idx := indexOf(args, "-m")
	if idx < 0 || idx+1 >= len(args) {
		t.Fatal("missing -m flag")
	}
	if args[idx+1] != "0" {
		t.Errorf("-m: got %q, want %q", args[idx+1], "0")
	}
}

func TestArgs_ZeroCores(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      0,
	}
	args := c.Args()

	idx := indexOf(args, "-smp")
	if idx < 0 || idx+1 >= len(args) {
		t.Fatal("missing -smp flag")
	}
	if args[idx+1] != "0" {
		t.Errorf("-smp: got %q, want %q", args[idx+1], "0")
	}
}

func TestArgs_NoDrives(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
	}
	args := c.Args()

	if indexOf(args, "-drive") >= 0 {
		t.Error("config with no drives should not contain -drive flag")
	}
}

func TestArgs_NoVirtFS(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
	}
	args := c.Args()

	if indexOf(args, "-virtfs") >= 0 {
		t.Error("config with no VirtFS should not contain -virtfs flag")
	}
}

// ---------- DriveFormat / Profile constants ----------

func TestDriveFormat_Values(t *testing.T) {
	if FormatQCOW2 != "qcow2" {
		t.Errorf("FormatQCOW2: got %q, want %q", FormatQCOW2, "qcow2")
	}
	if FormatRaw != "raw" {
		t.Errorf("FormatRaw: got %q, want %q", FormatRaw, "raw")
	}
}

func TestProfileConstants_Distinct(t *testing.T) {
	profiles := []VMProfile{ProfileDev, ProfileBuild, ProfileRuntime}
	seen := make(map[VMProfile]bool)
	for _, p := range profiles {
		if seen[p] {
			t.Errorf("duplicate profile constant value: %d", p)
		}
		seen[p] = true
	}
}

func TestDisplayMode_Distinct(t *testing.T) {
	modes := []DisplayMode{DisplayNone, DisplayGTK}
	seen := make(map[DisplayMode]bool)
	for _, m := range modes {
		if seen[m] {
			t.Errorf("duplicate display mode value: %d", m)
		}
		seen[m] = true
	}
}

// ---------- Config.Timeout field ----------

func TestConfig_TimeoutField(t *testing.T) {
	c := Config{
		Machine:  "q35",
		CPUModel: "max",
		MemoryMB: 1024,
		SMP:      2,
		Timeout:  5 * time.Minute,
	}
	// Timeout is a field on Config but does not appear in Args output
	// (it is handled by the runner, not QEMU flags).
	// Verify it stores correctly.
	if c.Timeout != 5*time.Minute {
		t.Errorf("Timeout: got %v, want 5m", c.Timeout)
	}
}

// ---------- Full round-trip: DefaultConfig -> Args ----------

func TestDefaultConfig_ArgsRoundTrip(t *testing.T) {
	c := DefaultConfig()
	args := c.Args()

	// Basic sanity: args must contain the core flags.
	required := []string{"-machine", "-cpu", "-m", "-smp"}
	for _, flag := range required {
		if indexOf(args, flag) < 0 {
			t.Errorf("DefaultConfig().Args() missing required flag %s", flag)
		}
	}

	// Memory and SMP args must match the config.
	idx := indexOf(args, "-m")
	if idx >= 0 && idx+1 < len(args) {
		want := fmt.Sprintf("%d", c.MemoryMB)
		if args[idx+1] != want {
			t.Errorf("-m: got %q, want %q", args[idx+1], want)
		}
	}
	idx = indexOf(args, "-smp")
	if idx >= 0 && idx+1 < len(args) {
		want := fmt.Sprintf("%d", c.SMP)
		if args[idx+1] != want {
			t.Errorf("-smp: got %q, want %q", args[idx+1], want)
		}
	}
}

// ---------- helpers ----------

func indexOf(args []string, flag string) int {
	for i, a := range args {
		if a == flag {
			return i
		}
	}
	return -1
}
