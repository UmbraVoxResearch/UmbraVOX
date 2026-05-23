// SPDX-License-Identifier: Apache-2.0
package disk

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// CreateSourceDisk exports the worktree at repoRoot into an ext2 disk image.
// initScript is written as .vm-init.sh. execCmd, if non-empty, is written as
// .vm-exec-cmd (read by the init script at boot). Returns the path to the
// disk image file; the caller must remove it when done.
func CreateSourceDisk(repoRoot, tmpDir, initScript, execCmd string) (string, error) {
	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		return "", fmt.Errorf("create temp dir: %w", err)
	}

	diskFile, err := os.CreateTemp(tmpDir, fmt.Sprintf("uv-source.%d.*.ext2", os.Getpid()))
	if err != nil {
		return "", fmt.Errorf("create source disk temp file: %w", err)
	}
	diskPath := diskFile.Name()
	diskFile.Close()
	os.Remove(diskPath) // genext2fs creates it

	srcDir, err := os.MkdirTemp(tmpDir, "uv-src.")
	if err != nil {
		return "", fmt.Errorf("create source temp dir: %w", err)
	}
	defer os.RemoveAll(srcDir)

	// Export worktree via tar, excluding large/generated dirs
	tarCreate := exec.Command("tar",
		"-C", repoRoot,
		"--exclude=.git",
		"--exclude=dist-newstyle",
		"--exclude=build",
		"--exclude=result",
		"-cf", "-", ".")
	tarExtract := exec.Command("tar", "-xf", "-", "-C", srcDir)

	pipe, err := tarCreate.StdoutPipe()
	if err != nil {
		return "", fmt.Errorf("tar pipe: %w", err)
	}
	tarExtract.Stdin = pipe

	if err := tarCreate.Start(); err != nil {
		return "", fmt.Errorf("tar create start: %w", err)
	}
	if err := tarExtract.Start(); err != nil {
		return "", fmt.Errorf("tar extract start: %w", err)
	}
	if err := tarCreate.Wait(); err != nil {
		return "", fmt.Errorf("tar create: %w", err)
	}
	if err := tarExtract.Wait(); err != nil {
		return "", fmt.Errorf("tar extract: %w", err)
	}

	// Write init script
	if err := os.WriteFile(filepath.Join(srcDir, ".vm-init.sh"), []byte(initScript), 0o755); err != nil {
		return "", fmt.Errorf("write init script: %w", err)
	}

	// Write exec command file
	if execCmd != "" {
		if err := os.WriteFile(filepath.Join(srcDir, ".vm-exec-cmd"), []byte(execCmd+"\n"), 0o644); err != nil {
			return "", fmt.Errorf("write exec command file: %w", err)
		}
	}

	// Build ext2 image
	genCmd := exec.Command("genext2fs", "-b", "1048576", "-d", srcDir, diskPath)
	if out, err := genCmd.CombinedOutput(); err != nil {
		return "", fmt.Errorf("genext2fs: %w\n%s", err, out)
	}

	return diskPath, nil
}
