// SPDX-License-Identifier: Apache-2.0

package vmctl

// Overlay represents a COW qcow2 overlay backed by a base image.
type Overlay struct {
	Path     string // path to the overlay file
	BackFile string // path to the backing image
}

// DiskManager handles disk image creation and lifecycle.
// Implementation will wrap qemu-img and genext2fs calls.
type DiskManager struct {
	// Logger for disk operation messages. May be nil.
	Log Logger
}

// CreateOverlay creates a qcow2 COW overlay on top of baseImage in dir.
// Stub -- implementation in Phase 1 continued.
func (dm *DiskManager) CreateOverlay(baseImage, dir string) (*Overlay, error) {
	panic("vmctl: DiskManager.CreateOverlay not yet implemented")
}

// CreateAppDisk creates a compact ext4 image at outputPath containing
// the contents of bundleDir.
// Stub -- implementation in Phase 1 continued.
func (dm *DiskManager) CreateAppDisk(bundleDir, outputPath string) error {
	panic("vmctl: DiskManager.CreateAppDisk not yet implemented")
}

// EnsureCacheDisk creates a persistent qcow2 cache/scratch disk at path
// if it does not already exist. Size is a qemu-img size string (e.g. "4G").
// Stub -- implementation in Phase 1 continued.
func (dm *DiskManager) EnsureCacheDisk(path, size string) error {
	panic("vmctl: DiskManager.EnsureCacheDisk not yet implemented")
}

// CreateSourceDisk exports sourceDir into an ext2 disk image in tmpDir.
// Returns the path to the created image; the caller must remove it.
// Stub -- implementation in Phase 1 continued.
func (dm *DiskManager) CreateSourceDisk(sourceDir, tmpDir string) (string, error) {
	panic("vmctl: DiskManager.CreateSourceDisk not yet implemented")
}
