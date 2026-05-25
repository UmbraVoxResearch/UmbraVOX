// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
)

// Overlay represents a COW qcow2 overlay backed by a base image.
type Overlay struct {
	Path     string // path to the overlay file
	BackFile string // path to the backing image
}

// Remove deletes the overlay file.
func (o *Overlay) Remove() error {
	return os.Remove(o.Path)
}

// DiskManager handles disk image creation and lifecycle.
// Implementation wraps qemu-img and genext2fs calls.
type DiskManager struct {
	// Logger for disk operation messages. May be nil.
	Log Logger
}

// CreateOverlay creates a qcow2 COW overlay on top of baseImage in dir.
// The overlay is created as a temporary file in dir, which must be non-empty
// to avoid writing temp files to the host OS temp directory.
func (dm *DiskManager) CreateOverlay(baseImage, dir string) (*Overlay, error) {
	if dir == "" {
		return nil, fmt.Errorf("CreateOverlay: dir must be specified (refusing to use host OS temp dir)")
	}

	f, err := os.CreateTemp(dir, "vm-overlay.*.qcow2")
	if err != nil {
		return nil, fmt.Errorf("create overlay temp file: %w", err)
	}
	overlayPath := f.Name()
	f.Close()

	cmd := exec.Command("qemu-img", "create",
		"-f", "qcow2",
		"-b", baseImage,
		"-F", "raw",
		overlayPath,
	)
	if out, err := cmd.CombinedOutput(); err != nil {
		os.Remove(overlayPath)
		return nil, fmt.Errorf("qemu-img create overlay: %w\n%s", err, out)
	}

	return &Overlay{
		Path:     overlayPath,
		BackFile: baseImage,
	}, nil
}

// EnsureCacheDisk creates a persistent qcow2 cache/scratch disk at path
// if it does not already exist. Size is a qemu-img size string (e.g. "4G").
func (dm *DiskManager) EnsureCacheDisk(path, size string) error {
	if _, err := os.Stat(path); err == nil {
		return nil // already exists
	}

	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil { // #nosec G301 -- cache directory
		return fmt.Errorf("create cache disk directory: %w", err)
	}

	cmd := exec.Command("qemu-img", "create", "-f", "qcow2", path, size)
	if out, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("qemu-img create cache disk: %w\n%s", err, out)
	}
	return nil
}

// CreateAppDisk creates a compact ext2 image at outputPath containing
// the contents of bundleDir. The image is sized to the directory contents
// plus 10 MB of headroom.
func (dm *DiskManager) CreateAppDisk(bundleDir, outputPath string) error {
	sizeKB, err := dirSizeKB(bundleDir)
	if err != nil {
		return fmt.Errorf("measure bundle dir: %w", err)
	}

	// Add 10 MB headroom and convert to 1K-block count for genext2fs -b.
	blocks := sizeKB + 10*1024

	cmd := exec.Command(
		"genext2fs",
		"-b", strconv.Itoa(blocks),
		"-d", bundleDir,
		outputPath,
	)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("genext2fs: %w\n%s", err, out)
	}
	return nil
}

// CreateSourceDisk creates an ext2 disk image containing the contents of
// srcDir. sizeBlocks is the ext2 block count passed to genext2fs -b.
// The image is written to outputPath; genext2fs creates the file.
func (dm *DiskManager) CreateSourceDisk(srcDir, outputPath string, sizeBlocks int) error {
	cmd := exec.Command("genext2fs", "-b", strconv.Itoa(sizeBlocks), "-d", srcDir, outputPath)
	if out, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("genext2fs: %w\n%s", err, out)
	}
	return nil
}

// dirSizeKB returns the size of dir in kilobytes using du(1).
func dirSizeKB(dir string) (int, error) {
	out, err := exec.Command("du", "-sk", dir).Output()
	if err != nil {
		return 0, fmt.Errorf("du: %w", err)
	}
	fields := strings.Fields(string(out))
	if len(fields) < 1 {
		return 0, fmt.Errorf("du: unexpected output %q", string(out))
	}
	kb, err := strconv.Atoi(fields[0])
	if err != nil {
		return 0, fmt.Errorf("du: parse size: %w", err)
	}
	return kb, nil
}
