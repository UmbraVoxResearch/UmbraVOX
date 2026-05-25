// Package disk handles COW overlay creation and cleanup for QEMU VM images.
//
// This is the Go equivalent of the overlay logic in scripts/vm-dev-run.sh
// (lines ~273-283): creating a qcow2 overlay backed by the base NixOS image,
// and managing the persistent build-cache disk.
package disk

import (
	"github.com/UmbraVoxResearch/vmctl"
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
	dm := &vmctl.DiskManager{}
	o, err := dm.CreateOverlay(baseImage, dir)
	if err != nil {
		return nil, err
	}
	return &Overlay{
		Path:     o.Path,
		BackFile: o.BackFile,
	}, nil
}

// Remove deletes the overlay file.
func (o *Overlay) Remove() error {
	vo := &vmctl.Overlay{Path: o.Path, BackFile: o.BackFile}
	return vo.Remove()
}

// EnsureCacheDisk creates the persistent build-cache qcow2 disk at path
// if it does not already exist. Size is specified as a qemu-img size string
// (e.g. "4G").
func EnsureCacheDisk(path string, size string) error {
	dm := &vmctl.DiskManager{}
	return dm.EnsureCacheDisk(path, size)
}
