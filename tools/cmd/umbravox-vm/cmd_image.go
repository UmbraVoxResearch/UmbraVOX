// SPDX-License-Identifier: Apache-2.0
package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
)

// runVM handles: uv vm <action>
func runVM(args []string) int {
	if len(args) == 0 {
		printVMHelp()
		return 0
	}

	action := args[0]
	rest := args[1:]

	switch action {
	case "build-image":
		return vmBuildImage(rest)
	case "clean-image":
		return vmCleanImage()
	case "smoke":
		return vmSmoke(rest)
	case "seed":
		return vmSeed(rest)
	case "signal":
		return vmSignal(rest)
	case "integration":
		return vmIntegration(rest)
	case "info":
		return vmInfo()
	default:
		fmt.Fprintf(os.Stderr, "Unknown vm action: %s\n", action)
		printVMHelp()
		return 2
	}
}

func printVMHelp() {
	fmt.Print(`Usage: ./uv vm <action>

Actions:
  build-image [--on-host]    Build NixOS VM image (default: in builder VM)
  clean-image                Remove cached VM image
  smoke [TARGET]             Platform smoke (freebsd, openbsd, netbsd, illumos, dragonfly, arm64)
  seed build|clean           Seed image management
  signal build-jar|run|health Signal Server VM
  integration [--dual-lan]   Multi-VM integration test
  info                       VM config diagnostics
`)
}

func vmBuildImage(args []string) int {
	onHost := false
	for _, a := range args {
		if a == "--on-host" {
			onHost = true
		}
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	if onHost {
		log.Info(tag, "Building VM image on host (writes to /nix/store)...")
		cmd := exec.Command("nix-build", filepath.Join(repoRoot, "nix", "vm-image.nix"),
			"-o", filepath.Join(repoRoot, "build", "vm", "image"))
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("nix-build failed: %v", err))
			return 1
		}
		log.OK(tag, "VM image built successfully.")
		return 0
	}

	// Two-stage build via vm-image-builder.sh
	log.Info(tag, "Building VM image (two-stage pipeline)...")
	script := filepath.Join(repoRoot, "scripts", "vm-image-builder.sh")
	cmd := exec.Command("bash", script)
	cmd.Dir = repoRoot
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	if err := cmd.Run(); err != nil {
		log.Fail(tag, fmt.Sprintf("vm-image-builder.sh failed: %v", err))
		return 1
	}
	log.OK(tag, "VM image built successfully.")
	return 0
}

func vmCleanImage() int {
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}
	imgPath := filepath.Join(repoRoot, "build", "vm", "image")
	if err := os.RemoveAll(imgPath); err != nil {
		log.Fail(tag, fmt.Sprintf("Failed to remove %s: %v", imgPath, err))
		return 1
	}
	log.OK(tag, "VM image removed.")
	return 0
}

func vmSmoke(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ./uv vm smoke <target>")
		fmt.Fprintln(os.Stderr, "Targets: freebsd, openbsd, netbsd, illumos, dragonfly, arm64")
		return 2
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	target := args[0]
	scriptMap := map[string]string{
		"freebsd":   "scripts/vm-freebsd-setup.sh",
		"openbsd":   "scripts/vm-openbsd-setup.sh",
		"netbsd":    "scripts/vm-netbsd-setup.sh",
		"illumos":   "scripts/vm-illumos-setup.sh",
		"dragonfly": "scripts/vm-dragonfly-setup.sh",
	}

	if script, ok := scriptMap[target]; ok {
		log.Info(tag, fmt.Sprintf("Running %s platform smoke test...", target))
		cmd := exec.Command("bash", filepath.Join(repoRoot, script))
		cmd.Dir = repoRoot
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		cmd.Stdin = os.Stdin
		if err := cmd.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("%s smoke test failed: %v", target, err))
			return 1
		}
		log.OK(tag, fmt.Sprintf("%s smoke test passed.", target))
		return 0
	}

	if target == "arm64" {
		log.Info(tag, "Running arm64 platform smoke test...")
		log.Warn(tag, "arm64 smoke requires qemu-system-aarch64")
		// Delegate to the existing arm64 script logic
		cmd := exec.Command("bash", "-c",
			fmt.Sprintf("cd %s && bash scripts/vm-arm64-setup.sh", repoRoot))
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("arm64 smoke test failed: %v", err))
			return 1
		}
		return 0
	}

	fmt.Fprintf(os.Stderr, "Unknown smoke target: %s\n", target)
	return 2
}

func vmSeed(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ./uv vm seed build|clean")
		return 2
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	switch args[0] {
	case "build":
		log.Info(tag, "Building seed VM image...")
		outPath := filepath.Join(repoRoot, "build", "vm", "seed-image")
		os.RemoveAll(outPath)
		cmd := exec.Command("nix-build",
			filepath.Join(repoRoot, "nix", "vm-seed.nix"),
			"-o", outPath)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			log.Fail(tag, fmt.Sprintf("Seed build failed: %v", err))
			return 1
		}
		log.OK(tag, "Seed image built.")
		return 0
	case "clean":
		seedPath := filepath.Join(repoRoot, "build", "vm", "seed-image")
		os.RemoveAll(seedPath)
		log.OK(tag, "Seed image removed.")
		return 0
	default:
		fmt.Fprintf(os.Stderr, "Unknown seed action: %s\n", args[0])
		return 2
	}
}

func vmSignal(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ./uv vm signal build-jar|run|health")
		return 2
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	// Ensure vm-signal binary exists
	signalBin := filepath.Join(repoRoot, "build", "tools", "vm-signal")
	if _, err := os.Stat(signalBin); os.IsNotExist(err) {
		log.Info(tag, "Building vm-signal tool...")
		buildCmd := exec.Command("go", "build", "-o", signalBin, "./cmd/vm-signal/")
		buildCmd.Dir = filepath.Join(repoRoot, "tools")
		buildCmd.Env = append(os.Environ(),
			"GOMODCACHE="+filepath.Join(repoRoot, "build", "go", "mod"),
			"GOCACHE="+filepath.Join(repoRoot, "build", "go", "cache"))
		if out, err := buildCmd.CombinedOutput(); err != nil {
			log.Fail(tag, fmt.Sprintf("Failed to build vm-signal: %v\n%s", err, out))
			return 1
		}
	}

	// Map subcommands
	signalCmd := ""
	switch args[0] {
	case "build-jar":
		signalCmd = "build-jar"
	case "run":
		signalCmd = "interactive"
	case "health":
		signalCmd = "check-health"
	default:
		fmt.Fprintf(os.Stderr, "Unknown signal action: %s\n", args[0])
		return 2
	}

	cmd := exec.Command(signalBin, signalCmd)
	cmd.Dir = repoRoot
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	if err := cmd.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ExitCode()
		}
		log.Fail(tag, fmt.Sprintf("vm-signal %s failed: %v", signalCmd, err))
		return 1
	}
	return 0
}

func vmIntegration(args []string) int {
	agents := "3"
	dualLan := false
	for _, a := range args {
		switch a {
		case "--dual-lan":
			dualLan = true
			agents = "6"
		default:
			if len(a) > 0 && a[0] != '-' {
				agents = a
			}
		}
	}

	for _, a := range args {
		if a == "--agents" {
			// Next arg is the count — handled by positional above
		}
	}

	cmd := fmt.Sprintf("cabal run umbravox -- vm-integration-test --agents=%s", agents)
	if dualLan {
		cmd += " --dual-lan"
	}
	return execInVM(cmd, qemu.ProfileBuild, 60*time.Minute)
}

func vmInfo() int {
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	vmImagePath := filepath.Join(repoRoot, "build", "vm", "image")
	hasImage := false
	if fi, err := os.Stat(vmImagePath); err == nil && fi.IsDir() {
		hasImage = true
	}

	cores, mem := qemu.ScaleToHost(qemu.ProfileDev)
	fmt.Printf("Repository:  %s\n", repoRoot)
	fmt.Printf("VM image:    %v\n", hasImage)
	fmt.Printf("VM profile:  dev (%d cores, %dMB RAM)\n", cores, mem)
	fmt.Printf("KVM:         ")
	if _, err := os.Stat("/dev/kvm"); err == nil {
		fmt.Println("available")
	} else {
		fmt.Println("NOT available")
	}
	return 0
}
