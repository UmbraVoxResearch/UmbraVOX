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

// runRelease handles: uv release [PLATFORM...] [--smoke TARGET] [--compliance]
func runRelease(args []string) int {
	if len(args) == 0 {
		fmt.Println("Usage: ./uv release <platform> [platform...]")
		fmt.Println("Platforms: linux, windows, macos, bsd, freedos, source,")
		fmt.Println("           freebsd, openbsd, netbsd, illumos, arm64, appimage, all")
		fmt.Println("Flags:     --smoke <platform>  Run release smoke test")
		fmt.Println("           --compliance         SBOM/license/checksums")
		return 0
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	// Handle flags
	for i := 0; i < len(args); i++ {
		if args[i] == "--smoke" && i+1 < len(args) {
			return releaseSmoke(repoRoot, args[i+1])
		}
		if args[i] == "--compliance" {
			return releaseCompliance(repoRoot)
		}
	}

	// Map platforms to release-package.sh arguments
	platformMap := map[string]string{
		"linux":   "linux",
		"windows": "windows-cli",
		"macos":   "macos-terminal",
		"bsd":     "bsd-terminal",
		"freedos": "freedos",
		"source":  "source",
		"appimage": "appimage",
	}

	// Platform release packages (via release-package-platform.sh)
	platformPkgMap := map[string]string{
		"freebsd": "freebsd",
		"openbsd": "openbsd",
		"netbsd":  "netbsd",
		"illumos": "illumos",
		"arm64":   "linux-arm64",
	}

	if args[0] == "all" {
		for _, p := range []string{"linux", "windows", "macos", "bsd", "freedos"} {
			if code := buildRelease(repoRoot, platformMap[p]); code != 0 {
				return code
			}
		}
		return 0
	}

	for _, arg := range args {
		if arg[0] == '-' {
			continue
		}
		if pkgArg, ok := platformMap[arg]; ok {
			if code := buildRelease(repoRoot, pkgArg); code != 0 {
				return code
			}
		} else if pkgArg, ok := platformPkgMap[arg]; ok {
			if code := buildPlatformRelease(repoRoot, pkgArg); code != 0 {
				return code
			}
		} else {
			fmt.Fprintf(os.Stderr, "Unknown release platform: %s\n", arg)
			return 2
		}
	}
	return 0
}

func buildRelease(repoRoot, pkgArg string) int {
	log.Info(tag, fmt.Sprintf("Building release: %s", pkgArg))
	script := filepath.Join(repoRoot, "scripts", "release-package.sh")
	cmd := exec.Command("bash", script, pkgArg)
	cmd.Dir = repoRoot
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		log.Fail(tag, fmt.Sprintf("Release %s failed: %v", pkgArg, err))
		return 1
	}
	log.OK(tag, fmt.Sprintf("Release %s complete.", pkgArg))
	return 0
}

func buildPlatformRelease(repoRoot, platform string) int {
	log.Info(tag, fmt.Sprintf("Building platform release: %s", platform))
	cmd := exec.Command("nix-shell",
		filepath.Join(repoRoot, "shell-minimal.nix"),
		"--run", "./scripts/release-package-platform.sh \"$UMBRAVOX_RELEASE_PLATFORM\"")
	cmd.Dir = repoRoot
	cmd.Env = append(os.Environ(), "UMBRAVOX_RELEASE_PLATFORM="+platform)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		log.Fail(tag, fmt.Sprintf("Platform release %s failed: %v", platform, err))
		return 1
	}
	log.OK(tag, fmt.Sprintf("Platform release %s complete.", platform))
	return 0
}

func releaseSmoke(repoRoot, platform string) int {
	log.Info(tag, fmt.Sprintf("Running release smoke test: %s", platform))
	cmd := exec.Command("bash", filepath.Join(repoRoot, "scripts", "release-smoke-microvm.sh"), "qemu")
	cmd.Dir = repoRoot
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Env = append(os.Environ(), fmt.Sprintf("UMBRAVOX_QEMU_PROFILE=%s", platform))
	if err := cmd.Run(); err != nil {
		log.Fail(tag, fmt.Sprintf("Release smoke %s failed: %v", platform, err))
		return 1
	}
	return 0
}

func releaseCompliance(repoRoot string) int {
	log.Info(tag, "Running compliance checks (SBOM, license, checksums)...")
	cmd := `cabal run umbravox -- release-sbom-generate && \
cabal run umbravox -- release-license-check && \
cabal run umbravox -- release-checksums 2>&1`
	return execInVM(cmd, qemu.ProfileDev, 15*time.Minute)
}
