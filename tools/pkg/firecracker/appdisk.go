// SPDX-License-Identifier: Apache-2.0

package firecracker

import (
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/vmctl"
)

// CreateAppDisk creates a compact ext4 image at outputPath containing the
// contents of bundleDir.  It uses genext2fs (expected in the nix shell) and
// sizes the image to the directory contents plus 10 MB of headroom.
func CreateAppDisk(bundleDir, outputPath string) error {
	dm := &vmctl.DiskManager{}
	return dm.CreateAppDisk(bundleDir, outputPath)
}
