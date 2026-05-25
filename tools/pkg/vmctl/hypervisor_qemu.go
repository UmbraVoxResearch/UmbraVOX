// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
)

// QEMUHypervisor implements the Hypervisor interface using QEMU/KVM.
type QEMUHypervisor struct {
	Logger Logger
}

// Preflight checks that qemu-system-x86_64 is on PATH and /dev/kvm exists.
func (h *QEMUHypervisor) Preflight() error {
	return PreflightQEMU()
}

// Boot converts the VMSpec into QEMU arguments, launches
// qemu-system-x86_64, and blocks until the process exits or ctx is
// cancelled. The returned Result carries the exit code read from
// spec.StatusFile when set, or the hypervisor process exit code.
func (h *QEMUHypervisor) Boot(ctx context.Context, spec *VMSpec, tmpDir string) (*Result, error) {
	res := ResolveResources(spec.Resources)

	// Assemble QEMU argument list.
	args := []string{
		"-machine", "q35,accel=kvm",
		"-cpu", "max",
		"-m", strconv.Itoa(res.MemoryMB),
		"-smp", strconv.Itoa(res.Cores),
	}

	// Base image as the first drive.
	if spec.BaseImage.Path != "" {
		driveSpec := fmt.Sprintf("if=virtio,format=%s,file=%s",
			diskFormatQEMU(spec.BaseImage.Format), spec.BaseImage.Path)
		args = append(args, "-drive", driveSpec)
	}

	// Additional drives.
	for _, d := range spec.Disks {
		iface := d.Interface
		if iface == "" {
			iface = "virtio"
		}
		driveSpec := fmt.Sprintf("if=%s,format=%s,file=%s",
			iface, diskFormatQEMU(d.Format), d.Path)
		if d.ReadOnly {
			driveSpec += ",readonly=on"
		}
		args = append(args, "-drive", driveSpec)
	}

	// VirtFS / 9p shares.
	for _, s := range spec.Shares {
		secModel := s.SecurityModel
		if secModel == "" {
			secModel = "mapped-xattr"
		}
		fsSpec := fmt.Sprintf("local,path=%s,mount_tag=%s,security_model=%s,id=%s",
			s.HostPath, s.MountTag, secModel, s.ID)
		args = append(args, "-virtfs", fsSpec)
	}

	// Network.
	netArgs := spec.Network.RawArgs
	if netArgs == "" {
		netArgs = spec.Network.Mode.QEMUNetArgs()
	}
	if netArgs != "" {
		args = append(args, strings.Fields(netArgs)...)
	}

	// Direct-kernel boot.
	if spec.Boot != nil {
		if spec.Boot.KernelPath != "" {
			args = append(args, "-kernel", spec.Boot.KernelPath)
		}
		if spec.Boot.InitrdPath != "" {
			args = append(args, "-initrd", spec.Boot.InitrdPath)
		}
		if spec.Boot.KernelArgs != "" {
			args = append(args, "-append", spec.Boot.KernelArgs)
		}
	}

	// Display.
	switch spec.Display {
	case DisplayGTK:
		args = append(args, "-display", "gtk", "-vga", "std")
	default:
		args = append(args, "-nographic", "-nodefaults", "-serial", "stdio")
	}

	if spec.NoReboot {
		args = append(args, "-no-reboot")
	}

	// Launch QEMU.
	qemuBin := "qemu-system-x86_64"
	if h.Logger != nil {
		h.Logger.Info("qemu", fmt.Sprintf("launching %s (%d cores, %d MB RAM)", qemuBin, res.Cores, res.MemoryMB))
	}

	cmd := exec.CommandContext(ctx, qemuBin, args...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	runErr := cmd.Run()

	result := &Result{}

	// If a StatusFile is configured, read the guest exit code from it.
	if spec.StatusFile != "" {
		result.StatusFile = spec.StatusFile
		data, err := os.ReadFile(spec.StatusFile)
		if err == nil {
			code, parseErr := strconv.Atoi(strings.TrimSpace(string(data)))
			if parseErr == nil {
				result.ExitCode = code
				return result, nil
			}
		}
		// StatusFile missing or unparsable: fall through to process exit code.
	}

	if runErr != nil {
		if exitErr, ok := runErr.(*exec.ExitError); ok {
			result.ExitCode = exitErr.ExitCode()
		} else {
			return nil, fmt.Errorf("QEMUHypervisor: %w", runErr)
		}
	}

	return result, nil
}

// Supports reports whether the QEMU backend supports the given feature.
// QEMU/KVM supports all currently defined features.
func (h *QEMUHypervisor) Supports(feature Feature) bool {
	switch feature {
	case Feature9P, FeatureGUI, FeatureResize:
		return true
	default:
		return true
	}
}

// diskFormatQEMU maps DiskFormat to the string QEMU expects in -drive format=.
func diskFormatQEMU(f DiskFormat) string {
	switch f {
	case DiskFormatQCOW2:
		return "qcow2"
	case DiskFormatRaw, DiskFormatExt2, DiskFormatExt4:
		// ext2/ext4 images are raw block images from QEMU's perspective.
		return "raw"
	default:
		return "raw"
	}
}
