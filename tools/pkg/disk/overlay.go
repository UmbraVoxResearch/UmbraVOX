// Package disk handles COW overlay creation and cleanup for QEMU VM images.
//
// This is the Go equivalent of the overlay logic in scripts/vm-dev-run.sh
// (lines ~273-283): creating a qcow2 overlay backed by the base NixOS image,
// and managing the persistent build-cache disk.
package disk

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// Overlay represents a COW qcow2 overlay backed by a base image.
type Overlay struct {
	Path     string // path to the overlay file
	BackFile string // path to the backing image
}

// CreateOverlay creates a new qcow2 COW overlay on top of baseImage.
// The overlay is created as a temporary file in dir, which must be non-empty
// to avoid writing temp files to the host OS temp directory.
func CreateOverlay(baseImage string, dir string) (*Overlay, error) {
	if dir == "" {
		return nil, fmt.Errorf("CreateOverlay: dir must be specified (refusing to use host OS temp dir)")
	}

	f, err := os.CreateTemp(dir, "umbravox-vm-dev-overlay.*.qcow2")
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

// Remove deletes the overlay file.
func (o *Overlay) Remove() error {
	return os.Remove(o.Path)
}

// EnsureCacheDisk creates the persistent build-cache qcow2 disk at path
// if it does not already exist. Size is specified as a qemu-img size string
// (e.g. "4G").
func EnsureCacheDisk(path string, size string) error {
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
