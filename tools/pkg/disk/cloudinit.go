// SPDX-License-Identifier: Apache-2.0
package disk

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// CreateCloudInitSeedDisk builds a FAT-formatted NoCloud seed disk containing
// the provided user-data and meta-data files. It mirrors the shell function
// vm_build_seed_disk in scripts/lib-vm.sh. The caller must remove outputPath
// when done.
func CreateCloudInitSeedDisk(userData, metaData, outputPath string) error {
	// Create a 1 MiB zero-filled image.
	f, err := os.Create(outputPath)
	if err != nil {
		return fmt.Errorf("create seed disk: %w", err)
	}
	if err := f.Truncate(1 << 20); err != nil {
		f.Close()
		os.Remove(outputPath)
		return fmt.Errorf("truncate seed disk: %w", err)
	}
	f.Close()

	// Format as FAT with the cidata label (try mkdosfs, then mkfs.vfat).
	formatted := false
	for _, tool := range []string{"mkdosfs", "mkfs.vfat"} {
		if _, err := exec.LookPath(tool); err == nil {
			label := "cidata"
			if tool == "mkfs.vfat" {
				label = "CIDATA"
			}
			cmd := exec.Command(tool, "-n", label, outputPath)
			if out, err := cmd.CombinedOutput(); err != nil {
				os.Remove(outputPath)
				return fmt.Errorf("%s: %w\n%s", tool, err, out)
			}
			formatted = true
			break
		}
	}
	if !formatted {
		os.Remove(outputPath)
		return fmt.Errorf("no FAT formatter found (need mkdosfs or mkfs.vfat)")
	}

	// Write user-data and meta-data via mcopy (mtools).
	if _, err := exec.LookPath("mcopy"); err != nil {
		os.Remove(outputPath)
		return fmt.Errorf("mcopy not found (install mtools)")
	}

	tmpDir, err := os.MkdirTemp("", "uv-seed.*")
	if err != nil {
		os.Remove(outputPath)
		return fmt.Errorf("create temp dir: %w", err)
	}
	defer os.RemoveAll(tmpDir)

	for name, content := range map[string]string{
		"user-data": userData,
		"meta-data": metaData,
	} {
		p := filepath.Join(tmpDir, name)
		if err := os.WriteFile(p, []byte(content), 0o644); err != nil {
			os.Remove(outputPath)
			return fmt.Errorf("write %s: %w", name, err)
		}
		cmd := exec.Command("mcopy", "-i", outputPath, p, "::"+name)
		if out, err := cmd.CombinedOutput(); err != nil {
			os.Remove(outputPath)
			return fmt.Errorf("mcopy %s: %w\n%s", name, err, out)
		}
	}

	return nil
}

// CreateInitISO builds an ISO 9660 image from a single init script. It tries
// genisoimage, mkisofs, and xorriso in order, mirroring vm_build_init_iso in
// scripts/lib-vm.sh. The caller must remove outputPath when done.
func CreateInitISO(scriptContent, outputPath string) error {
	tmpDir, err := os.MkdirTemp("", "uv-initiso.*")
	if err != nil {
		return fmt.Errorf("create temp dir: %w", err)
	}
	defer os.RemoveAll(tmpDir)

	scriptPath := filepath.Join(tmpDir, "init.sh")
	if err := os.WriteFile(scriptPath, []byte(scriptContent), 0o755); err != nil {
		return fmt.Errorf("write init script: %w", err)
	}

	type isoCfg struct {
		bin  string
		args []string
	}
	candidates := []isoCfg{
		{"genisoimage", []string{"-o", outputPath, "-R", "-J", tmpDir}},
		{"mkisofs", []string{"-o", outputPath, "-R", "-J", tmpDir}},
		{"xorriso", []string{"-as", "mkisofs", "-o", outputPath, "-R", "-J", tmpDir}},
	}

	for _, c := range candidates {
		if _, err := exec.LookPath(c.bin); err != nil {
			continue
		}
		cmd := exec.Command(c.bin, c.args...)
		if out, err := cmd.CombinedOutput(); err != nil {
			return fmt.Errorf("%s: %w\n%s", c.bin, err, out)
		}
		return nil
	}

	return fmt.Errorf("no ISO builder found (need genisoimage, mkisofs, or xorriso)")
}
