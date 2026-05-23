// SPDX-License-Identifier: Apache-2.0
package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
)

// runClean handles: uv clean [--all]
func runClean(args []string) int {
	all := false
	for _, a := range args {
		if a == "--all" {
			all = true
		}
	}

	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	// Clean build artifacts
	for _, dir := range []string{"build", "dist-newstyle"} {
		p := filepath.Join(repoRoot, dir)
		if err := os.RemoveAll(p); err != nil {
			log.Warn(tag, fmt.Sprintf("Failed to remove %s: %v", dir, err))
		}
	}

	// Clean .tix files
	findCmd := exec.Command("find", repoRoot, "-name", "*.tix", "-delete")
	findCmd.Run()

	// Run cabal clean
	cabalCmd := exec.Command("cabal", "clean")
	cabalCmd.Dir = repoRoot
	cabalCmd.Run()

	log.OK(tag, "Build artifacts cleaned.")

	if all {
		dbDir := filepath.Join(repoRoot, ".umbravox-data")
		if err := os.RemoveAll(dbDir); err != nil {
			log.Warn(tag, fmt.Sprintf("Failed to remove .umbravox-data: %v", err))
		}
		log.OK(tag, "Database and tools cleaned.")
	} else {
		fmt.Println("VM images kept. Use './uv vm clean-image' to remove them.")
	}

	return 0
}
