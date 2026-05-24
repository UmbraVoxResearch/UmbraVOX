// SPDX-License-Identifier: Apache-2.0
// Package vm provides a unified VM boot factory for UmbraVOX.
//
// All VM types (dev, builder, signal, etc.) share the same boot pattern:
// create overlay, ensure scratch disk, set up 9p shares, launch QEMU,
// wait for completion, extract exit status. This package encapsulates
// that pattern.
package vm

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/disk"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ninep"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
)

const tag = "VM"

// BootConfig describes how to boot a VM.
type BootConfig struct {
	// BaseImage is the path to the NixOS .img file (raw format).
	BaseImage string

	// Profile controls resource scaling (Dev=50%, Build=75%, Runtime=25%).
	Profile qemu.VMProfile

	// SourceDisk is an optional ext2 source disk (read-only, /dev/vdb).
	SourceDisk string

	// ScratchSize is the size of the persistent scratch/cache qcow2 disk.
	// Empty string means no scratch disk. Examples: "4G", "100G".
	ScratchSize string

	// ScratchPath is the path to the scratch disk. If empty and ScratchSize
	// is set, defaults to <TmpDir>/scratch.qcow2.
	ScratchPath string

	// OutputDir is the host directory exposed as a 9p share (mount_tag=output).
	// Empty string means no 9p share.
	OutputDir string

	// StatusFile is the filename within OutputDir to read for guest exit code.
	// Empty string means don't check (return QEMU exit code directly).
	StatusFile string

	// Network is the QEMU network argument string.
	// Examples: "-nic none", "-nic user,model=virtio"
	Network string

	// Display controls the QEMU display mode.
	Display qemu.DisplayMode

	// NoReboot adds -no-reboot (VM powers off instead of rebooting on exit).
	NoReboot bool

	// Timeout is the maximum time to wait for the VM. 0 means no timeout.
	Timeout time.Duration

	// TmpDir is where overlay and temp files are created.
	TmpDir string

	// Interactive connects stdin to the VM.
	Interactive bool
}

// BootResult contains the outcome of a VM boot.
type BootResult struct {
	// ExitCode is the guest exit code (from StatusFile) or QEMU exit code.
	ExitCode int

	// QEMUExitCode is the raw QEMU process exit code.
	QEMUExitCode int
}

// Boot launches a QEMU VM with the given configuration.
func Boot(cfg BootConfig) (*BootResult, error) {
	if cfg.TmpDir == "" {
		return nil, fmt.Errorf("TmpDir is required")
	}
	if err := os.MkdirAll(cfg.TmpDir, 0o755); err != nil { // #nosec G301 -- build directory, needs traversal
		return nil, fmt.Errorf("create tmp dir: %w", err)
	}

	// Resolve base image (follow symlinks)
	diskImg, err := filepath.EvalSymlinks(cfg.BaseImage)
	if err != nil {
		diskImg = cfg.BaseImage
	}

	// Create COW overlay
	overlay, err := disk.CreateOverlay(diskImg, cfg.TmpDir)
	if err != nil {
		return nil, fmt.Errorf("create overlay: %w", err)
	}
	defer overlay.Remove()

	// Build QEMU config
	qcfg := qemu.ProfileConfig(cfg.Profile)
	qcfg.Display = cfg.Display
	qcfg.NoReboot = cfg.NoReboot
	qcfg.Timeout = cfg.Timeout

	// Drives: overlay is always first (/dev/vda)
	qcfg.Drives = []qemu.Drive{
		{Interface: "virtio", Format: qemu.FormatQCOW2, File: overlay.Path},
	}

	// Source disk (/dev/vdb, read-only)
	if cfg.SourceDisk != "" {
		qcfg.Drives = append(qcfg.Drives, qemu.Drive{
			Interface: "virtio", Format: qemu.FormatRaw,
			File: cfg.SourceDisk, ReadOnly: true,
		})
	}

	// Scratch disk (/dev/vdc, persistent)
	if cfg.ScratchSize != "" {
		scratchPath := cfg.ScratchPath
		if scratchPath == "" {
			scratchPath = filepath.Join(cfg.TmpDir, "scratch.qcow2")
		}
		if err := disk.EnsureCacheDisk(scratchPath, cfg.ScratchSize); err != nil {
			return nil, fmt.Errorf("create scratch disk: %w", err)
		}
		qcfg.Drives = append(qcfg.Drives, qemu.Drive{
			Interface: "virtio", Format: qemu.FormatQCOW2, File: scratchPath,
		})
	}

	// 9p output share
	if cfg.OutputDir != "" {
		if err := os.MkdirAll(cfg.OutputDir, 0o755); err != nil { // #nosec G301 -- build directory, needs traversal
			return nil, fmt.Errorf("create output dir: %w", err)
		}
		share := ninep.DefaultOutputShare(cfg.OutputDir)
		qcfg.VirtFS = []qemu.VirtFS{{
			LocalPath:     share.LocalPath,
			MountTag:      share.MountTag,
			SecurityModel: string(share.SecurityModel),
			ID:            share.ID,
		}}
	}

	// Network
	if cfg.Network != "" {
		qcfg.NetArgs = cfg.Network
	}

	log.Info(tag, fmt.Sprintf("Boot: %d cores, %dMB RAM | net: %s",
		qcfg.SMP, qcfg.MemoryMB, qcfg.NetArgs))

	// Boot QEMU
	args := qcfg.Args()
	var cmd *exec.Cmd
	if cfg.Timeout > 0 {
		ctx, cancel := context.WithTimeout(context.Background(), cfg.Timeout)
		defer cancel()
		cmd = exec.CommandContext(ctx, "qemu-system-x86_64", args...)
	} else {
		cmd = exec.Command("qemu-system-x86_64", args...)
	}

	if cfg.Interactive {
		cmd.Stdin = os.Stdin
	}
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	qemuErr := cmd.Run()
	result := &BootResult{}

	if qemuErr != nil {
		if exitErr, ok := qemuErr.(*exec.ExitError); ok {
			result.QEMUExitCode = exitErr.ExitCode()
		} else {
			return nil, fmt.Errorf("qemu error: %w", qemuErr)
		}
	}

	// Read guest exit status from status file
	if cfg.StatusFile != "" && cfg.OutputDir != "" {
		statusPath := filepath.Join(cfg.OutputDir, cfg.StatusFile)
		data, err := os.ReadFile(statusPath)
		if err == nil {
			raw := strings.TrimSpace(strings.SplitN(string(data), "\n", 2)[0])
			if matched, _ := regexp.MatchString(`^[0-9]+$`, raw); matched {
				status, _ := strconv.Atoi(raw)
				result.ExitCode = status
				return result, nil
			}
		}
		result.ExitCode = result.QEMUExitCode
	} else {
		result.ExitCode = result.QEMUExitCode
	}

	return result, nil
}
