// SPDX-License-Identifier: Apache-2.0
package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/disk"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/netpol"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ninep"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
)

// runDev handles: uv dev [--gui]
func runDev(args []string) int {
	gui := false
	for _, a := range args {
		if a == "--gui" || a == "gui" {
			gui = true
		}
	}

	mode := "interactive"
	if gui {
		mode = "gui"
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	vmCacheDir := filepath.Join(repoRoot, "build", "vm")
	vmImagePath := filepath.Join(vmCacheDir, "image")

	if err := repo.Preflight(vmImagePath, true); err != nil {
		return 1
	}

	diskImg, err := filepath.EvalSymlinks(filepath.Join(vmImagePath, "nixos.img"))
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Cannot resolve nixos.img: %v", err))
		return 1
	}

	// Source disk with interactive init script
	tmpDir := filepath.Join(vmCacheDir, "tmp")
	initScript := generateInitScript(mode, "")
	srcDisk, err := disk.CreateSourceDisk(repoRoot, tmpDir, initScript, "")
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create source disk: %v", err))
		return 1
	}
	defer os.Remove(srcDisk)

	// COW overlay
	overlayDir := filepath.Join(vmCacheDir, "tmp")
	if err := os.MkdirAll(overlayDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create overlay dir: %v", err))
		return 1
	}
	overlay, err := disk.CreateOverlay(diskImg, overlayDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create overlay: %v", err))
		return 1
	}
	defer overlay.Remove()

	// Build cache
	cacheDisk := filepath.Join(vmCacheDir, "build-cache.qcow2")
	if err := disk.EnsureCacheDisk(cacheDisk, "4G"); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create cache disk: %v", err))
		return 1
	}

	// Output dir
	outputDir := filepath.Join(repoRoot, "build", "vm-output")
	if err := os.MkdirAll(outputDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to create output dir: %v", err))
		return 1
	}

	// Network
	policyFile := filepath.Join(repoRoot, "conf/vm-network-policy.conf")
	policy, err := netpol.ParseFile(policyFile)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to parse network policy: %v", err))
		return 1
	}
	netArgs, err := policy.QEMUNetArgs()
	if err != nil {
		log.Fail(tag, fmt.Sprintf("Network policy error: %v", err))
		return 1
	}

	// QEMU config
	outputShare := ninep.DefaultOutputShare(outputDir)
	cfg := qemu.ProfileConfig(qemu.ProfileDev)
	cfg.Drives = []qemu.Drive{
		{Interface: "virtio", Format: qemu.FormatQCOW2, File: overlay.Path},
		{Interface: "virtio", Format: qemu.FormatRaw, File: srcDisk, ReadOnly: true},
		{Interface: "virtio", Format: qemu.FormatQCOW2, File: cacheDisk},
	}
	cfg.VirtFS = []qemu.VirtFS{{
		LocalPath:     outputShare.LocalPath,
		MountTag:      outputShare.MountTag,
		SecurityModel: string(outputShare.SecurityModel),
		ID:            outputShare.ID,
	}}
	cfg.NetArgs = netArgs

	if gui {
		cfg.Display = qemu.DisplayGTK
		log.Info(tag, "Display: GUI (QEMU VGA window)")
	}

	log.Info(tag, fmt.Sprintf("VM: %d cores, %dMB RAM | mode: %s", cfg.SMP, cfg.MemoryMB, mode))
	log.Info(tag, "Booting NixOS development VM...")
	fmt.Fprintln(os.Stderr)

	// Boot
	qemuCmd := exec.Command("qemu-system-x86_64", cfg.Args()...)
	qemuCmd.Stdin = os.Stdin
	qemuCmd.Stdout = os.Stdout
	qemuCmd.Stderr = os.Stderr

	if err := qemuCmd.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ExitCode()
		}
		log.Fail(tag, fmt.Sprintf("QEMU error: %v", err))
		return 1
	}
	return 0
}
