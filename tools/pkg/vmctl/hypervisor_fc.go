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
	"time"
)

// FirecrackerHypervisor implements the Hypervisor interface using Firecracker microVMs.
type FirecrackerHypervisor struct {
	Logger Logger
}

// Preflight checks that firecracker is on PATH and /dev/kvm exists.
// It also warns (but does not fail) if slirp4netns is absent.
func (h *FirecrackerHypervisor) Preflight() error {
	return PreflightFirecrackerWithLogger(h.Logger)
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

	// Set up slirp4netns networking when requested.
	if spec.Network.Mode == NetworkSlirp {
		tapName, slirpCleanup, slirpErr := h.setupSlirpNetwork(ctx, tmpDir)
		if slirpErr != nil {
			// Graceful degradation: log warning and continue without network.
			if h.Logger != nil {
				h.Logger.Warn("firecracker", fmt.Sprintf("slirp4netns setup failed, proceeding without network: %v", slirpErr))
			}
		} else {
			defer slirpCleanup()
			cfg.NetworkInterfaces = append(cfg.NetworkInterfaces, fcNetworkInterface{
				IfaceID:     "eth0",
				GuestMAC:    "AA:FC:00:00:00:01",
				HostDevName: tapName,
			})
		}
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

// setupSlirpNetwork starts slirp4netns in the background to provide rootless
// TAP-based networking for Firecracker. It returns the TAP device name and a
// cleanup function that stops slirp4netns when the VM exits.
//
// slirp4netns creates a TAP device inside the current network namespace and
// routes packets through the host stack without requiring root privileges.
//
// Returns an error if slirp4netns is not on PATH or fails to start. Callers
// should treat errors as non-fatal and proceed without network.
func (h *FirecrackerHypervisor) setupSlirpNetwork(ctx context.Context, tmpDir string) (tapName string, cleanup func(), err error) {
	if _, lookErr := exec.LookPath("slirp4netns"); lookErr != nil {
		return "", nil, fmt.Errorf("slirp4netns not found on PATH: %w", lookErr)
	}

	tapName = "vmtap0"
	pidFile := filepath.Join(tmpDir, "slirp4netns.pid")

	// slirp4netns <pid> <tap-device>
	// Using "--configure" sets up the TAP device address automatically.
	// "--disable-host-loopback" restricts access to 127.0.0.1 on the host.
	// We pass the current process PID so slirp4netns attaches to our netns.
	cmd := exec.CommandContext(ctx, "slirp4netns",
		"--configure",
		"--mtu=1500",
		"--disable-host-loopback",
		fmt.Sprintf("--pid-file=%s", pidFile),
		fmt.Sprintf("%d", os.Getpid()),
		tapName,
	)
	cmd.Stdout = io.Discard
	cmd.Stderr = io.Discard

	if startErr := cmd.Start(); startErr != nil {
		return "", nil, fmt.Errorf("start slirp4netns: %w", startErr)
	}

	if h.Logger != nil {
		h.Logger.Info("firecracker", fmt.Sprintf("slirp4netns started (TAP: %s, pid: %d)", tapName, cmd.Process.Pid))
	}

	// Give slirp4netns a moment to configure the TAP device before Firecracker
	// reads the config.
	time.Sleep(200 * time.Millisecond)

	cleanup = func() {
		if cmd.Process != nil {
			_ = cmd.Process.Kill()
			_ = cmd.Wait()
		}
		if h.Logger != nil {
			h.Logger.Info("firecracker", "slirp4netns stopped")
		}
	}

	return tapName, cleanup, nil
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
	// Ensure writable — zstd inherits nix store read-only permissions
	os.Chmod(dst, 0o644)

	return dst, func() { os.Remove(dst) }, nil
}

// --- Firecracker JSON config types (local to avoid import cycle with
// the firecracker package which imports vmctl). ---

type fcConfig struct {
	BootSource        fcBootSource       `json:"boot-source"`
	Drives            []fcDrive          `json:"drives"`
	MachineConfig     fcMachineConfig    `json:"machine-config"`
	NetworkInterfaces []fcNetworkInterface `json:"network-interfaces,omitempty"`
}

type fcNetworkInterface struct {
	IfaceID     string `json:"iface_id"`
	GuestMAC    string `json:"guest_mac,omitempty"`
	HostDevName string `json:"host_dev_name"`
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
