// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
)

// FirecrackerHypervisor implements the Hypervisor interface using Firecracker microVMs.
type FirecrackerHypervisor struct {
	Logger Logger
}

// Preflight checks that firecracker is on PATH and /dev/kvm exists.
func (h *FirecrackerHypervisor) Preflight() error {
	return PreflightFirecracker()
}

// Boot converts the VMSpec into a Firecracker configuration, launches the
// microVM, and blocks until it exits or ctx is cancelled. The returned Result
// carries the exit code read from spec.StatusFile when set, or the hypervisor
// process exit code.
func (h *FirecrackerHypervisor) Boot(ctx context.Context, spec *VMSpec, tmpDir string) (*Result, error) {
	if spec.Boot == nil {
		return nil, fmt.Errorf("FirecrackerHypervisor: spec.Boot is required (direct-kernel boot only)")
	}
	if spec.Boot.KernelPath == "" {
		return nil, fmt.Errorf("FirecrackerHypervisor: spec.Boot.KernelPath is required")
	}

	res := ResolveResources(spec.Resources)

	// Resolve rootfs path from the base image. Firecracker needs an
	// uncompressed, writable raw block device.
	rootfsPath, cleanup, err := h.resolveRootfs(spec.BaseImage.Path, tmpDir)
	if err != nil {
		return nil, fmt.Errorf("FirecrackerHypervisor: rootfs: %w", err)
	}
	if cleanup != nil {
		defer cleanup()
	}

	// Build Firecracker JSON config.
	bootArgs := spec.Boot.KernelArgs
	if !strings.Contains(bootArgs, "console=") {
		if bootArgs != "" {
			bootArgs = "console=ttyS0 " + bootArgs
		} else {
			bootArgs = "console=ttyS0"
		}
	}

	cfg := fcConfig{
		BootSource: fcBootSource{
			KernelImagePath: spec.Boot.KernelPath,
			InitrdPath:      spec.Boot.InitrdPath,
			BootArgs:        bootArgs,
		},
		MachineConfig: fcMachineConfig{
			VcpuCount:  res.Cores,
			MemSizeMib: res.MemoryMB,
		},
	}

	// Rootfs is always the first drive.
	cfg.Drives = append(cfg.Drives, fcDrive{
		DriveID:      "rootfs",
		PathOnHost:   rootfsPath,
		IsRootDevice: true,
		IsReadOnly:   false,
	})

	// Attach the first additional disk as the app disk (/dev/vdb).
	if len(spec.Disks) > 0 {
		cfg.Drives = append(cfg.Drives, fcDrive{
			DriveID:      "app",
			PathOnHost:   spec.Disks[0].Path,
			IsRootDevice: false,
			IsReadOnly:   spec.Disks[0].ReadOnly,
		})
	}

	if h.Logger != nil {
		h.Logger.Info("firecracker", fmt.Sprintf("launching microVM (%d cores, %d MB RAM)", res.Cores, res.MemoryMB))
	}

	// Write the JSON config file into tmpDir.
	cfgPath, err := writeFCConfig(cfg, tmpDir)
	if err != nil {
		return nil, fmt.Errorf("FirecrackerHypervisor: %w", err)
	}

	// Launch firecracker.
	cmd := exec.CommandContext(ctx, "firecracker", "--no-api", "--config-file", cfgPath)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	runErr := cmd.Run()

	result := &Result{}

	// If a StatusFile is configured, read the guest exit code from it.
	if spec.StatusFile != "" {
		result.StatusFile = spec.StatusFile
		data, readErr := os.ReadFile(spec.StatusFile)
		if readErr == nil {
			code, parseErr := strconv.Atoi(strings.TrimSpace(string(data)))
			if parseErr == nil {
				result.ExitCode = code
				return result, nil
			}
		}
		// StatusFile missing or unparsable: fall through to process exit code.
	}

	if runErr != nil {
		var exitErr *exec.ExitError
		if errors.As(runErr, &exitErr) {
			result.ExitCode = exitErr.ExitCode()
		} else {
			return nil, fmt.Errorf("FirecrackerHypervisor: %w", runErr)
		}
	}

	return result, nil
}

// Supports reports whether the Firecracker backend supports the given feature.
// Firecracker does not support 9p shares, graphical display, or qcow2 overlays.
func (h *FirecrackerHypervisor) Supports(feature Feature) bool {
	switch feature {
	case Feature9P, FeatureGUI, FeatureResize:
		return false
	default:
		return false
	}
}

// resolveRootfs returns a path to an uncompressed, writable rootfs image.
// If the source path ends in .zst, it is decompressed into tmpDir and a
// cleanup function is returned. Otherwise the original path is copied to
// tmpDir so Firecracker has a writable image (nix store paths are read-only).
func (h *FirecrackerHypervisor) resolveRootfs(src, tmpDir string) (path string, cleanup func(), err error) {
	if src == "" {
		return "", nil, fmt.Errorf("BaseImage.Path is required")
	}

	if !strings.HasSuffix(src, ".zst") {
		// Not compressed -- copy to tmpDir so Firecracker has a writable image.
		dst := filepath.Join(tmpDir, "rootfs.img")
		if copyErr := copyFileWritable(src, dst); copyErr != nil {
			return "", nil, fmt.Errorf("copy rootfs: %w", copyErr)
		}
		return dst, func() { os.Remove(dst) }, nil
	}

	// Decompress zstd-compressed rootfs into tmpDir.
	base := strings.TrimSuffix(filepath.Base(src), ".zst")
	dst := filepath.Join(tmpDir, base)

	if h.Logger != nil {
		h.Logger.Info("firecracker", "decompressing rootfs (zstd)...")
	}

	zstdCmd := exec.Command("zstd", "-d", "-f", src, "-o", dst)
	if out, zstdErr := zstdCmd.CombinedOutput(); zstdErr != nil {
		return "", nil, fmt.Errorf("zstd decompress: %w\n%s", zstdErr, out)
	}

	return dst, func() { os.Remove(dst) }, nil
}

// --- Firecracker JSON config types (local to avoid import cycle with
// the firecracker package which imports vmctl). ---

type fcConfig struct {
	BootSource    fcBootSource    `json:"boot-source"`
	Drives        []fcDrive       `json:"drives"`
	MachineConfig fcMachineConfig `json:"machine-config"`
}

type fcBootSource struct {
	KernelImagePath string `json:"kernel_image_path"`
	InitrdPath      string `json:"initrd_path,omitempty"`
	BootArgs        string `json:"boot_args"`
}

type fcDrive struct {
	DriveID      string `json:"drive_id"`
	PathOnHost   string `json:"path_on_host"`
	IsRootDevice bool   `json:"is_root_device"`
	IsReadOnly   bool   `json:"is_read_only"`
}

type fcMachineConfig struct {
	VcpuCount  int `json:"vcpu_count"`
	MemSizeMib int `json:"mem_size_mib"`
}

// writeFCConfig marshals cfg to JSON and writes it to dir/firecracker-config.json.
func writeFCConfig(cfg fcConfig, dir string) (string, error) {
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

// copyFileWritable copies src to dst with 0644 permissions.
func copyFileWritable(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()

	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()

	if _, err := io.Copy(out, in); err != nil {
		return err
	}
	return out.Chmod(0o644)
}
