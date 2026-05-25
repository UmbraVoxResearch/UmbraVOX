// SPDX-License-Identifier: Apache-2.0
// Package firecracker manages Firecracker microVM configuration and lifecycle.
package firecracker

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/UmbraVoxResearch/vmctl"
)

// Config holds parameters for a Firecracker microVM.
type Config struct {
	KernelPath    string
	InitrdPath    string // path to initrd; required for NixOS kernels with virtio as modules
	RootfsPath    string
	AppDiskPath   string // /dev/vdb in guest
	VcpuCount     int
	MemSizeMB     int
	KernelArgs    string
	SerialConsole bool
}

// firecrackerConfig mirrors the Firecracker JSON configuration format.
type firecrackerConfig struct {
	BootSource    bootSource    `json:"boot-source"`
	Drives        []drive       `json:"drives"`
	MachineConfig machineConfig `json:"machine-config"`
}

type bootSource struct {
	KernelImagePath string `json:"kernel_image_path"`
	InitrdPath      string `json:"initrd_path,omitempty"`
	BootArgs        string `json:"boot_args"`
}

type drive struct {
	DriveID      string `json:"drive_id"`
	PathOnHost   string `json:"path_on_host"`
	IsRootDevice bool   `json:"is_root_device"`
	IsReadOnly   bool   `json:"is_read_only"`
}

type machineConfig struct {
	VcpuCount int `json:"vcpu_count"`
	MemSizeMib int `json:"mem_size_mib"`
}

// ScaleToHost returns (cores, memMB) sized to 25% of host resources,
// with minimums of 1 core and 512 MB.
func ScaleToHost() (cores int, memMB int) {
	r := vmctl.ResolveResources(vmctl.Resources{
		Fraction: 25,
		MinCores: 1,
		MinMemMB: 512,
	})
	return r.Cores, r.MemoryMB
}

// WriteConfigFile writes a Firecracker JSON configuration file into dir
// and returns the path to the created file.
func (c *Config) WriteConfigFile(dir string) (string, error) {
	bootArgs := c.KernelArgs
	if c.SerialConsole && !strings.Contains(bootArgs, "console=") {
		if bootArgs != "" {
			bootArgs = "console=ttyS0 " + bootArgs
		} else {
			bootArgs = "console=ttyS0"
		}
	}

	cfg := firecrackerConfig{
		BootSource: bootSource{
			KernelImagePath: c.KernelPath,
			InitrdPath:      c.InitrdPath,
			BootArgs:        bootArgs,
		},
		Drives: []drive{
			{
				DriveID:      "rootfs",
				PathOnHost:   c.RootfsPath,
				IsRootDevice: true,
				IsReadOnly:   false,
			},
			{
				DriveID:      "app",
				PathOnHost:   c.AppDiskPath,
				IsRootDevice: false,
				IsReadOnly:   true,
			},
		},
		MachineConfig: machineConfig{
			VcpuCount:  c.VcpuCount,
			MemSizeMib: c.MemSizeMB,
		},
	}

	data, err := json.MarshalIndent(cfg, "", "  ")
	if err != nil {
		return "", fmt.Errorf("marshal firecracker config: %w", err)
	}

	p := filepath.Join(dir, "firecracker-config.json")
	if err := os.WriteFile(p, data, 0o644); err != nil {
		return "", fmt.Errorf("write firecracker config: %w", err)
	}
	return p, nil
}

// Boot starts a Firecracker microVM with the given Config, waits for it to
// exit, and returns the process exit code.
func Boot(cfg Config) (exitCode int, err error) {
	dir, err := os.MkdirTemp("", "firecracker-*")
	if err != nil {
		return -1, fmt.Errorf("create temp dir: %w", err)
	}
	defer os.RemoveAll(dir)

	cfgPath, err := cfg.WriteConfigFile(dir)
	if err != nil {
		return -1, err
	}

	cmd := exec.Command("firecracker", "--no-api", "--config-file", cfgPath)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			return exitErr.ExitCode(), nil
		}
		return -1, fmt.Errorf("firecracker: %w", err)
	}
	return 0, nil
}
