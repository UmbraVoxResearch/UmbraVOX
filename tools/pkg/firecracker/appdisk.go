// SPDX-License-Identifier: Apache-2.0

package firecracker

import (
	"fmt"
	"os/exec"
	"strconv"
	"strings"
)

// CreateAppDisk creates a compact ext4 image at outputPath containing the
// contents of bundleDir.  It uses genext2fs (expected in the nix shell) and
// sizes the image to the directory contents plus 10 MB of headroom.
func CreateAppDisk(bundleDir, outputPath string) error {
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
