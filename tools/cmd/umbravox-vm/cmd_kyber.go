// SPDX-License-Identifier: Apache-2.0
package main

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ninep"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
	"github.com/UmbraVoxResearch/vmctl"
)

// vmKyber handles: ./uv vm kyber <subcommand>
func vmKyber(args []string) int {
	if len(args) == 0 || args[0] == "help" {
		fmt.Print(`Usage: ./uv vm kyber <subcommand>

Subcommands:
  generate-kat    Boot the Kyber768 oracle VM and write KAT vectors to
                  test/vectors/nist/mlkem768-kat.json and
                  build/differential/traces/kyber-kat.json.

The VM is built from contrib/oracles/vm-kyber-oracle.nix (placeholder
hashes -- fill in real hashes before running).  The oracle generates 5
KAT vectors using the pq-crystals reference implementation.
`)
		return 0
	}

	switch args[0] {
	case "generate-kat":
		return vmKyberGenerateKAT()
	default:
		fmt.Fprintf(os.Stderr, "Unknown kyber subcommand: %s\n", args[0])
		fmt.Fprintln(os.Stderr, "Run './uv vm kyber help' for usage.")
		return 2
	}
}

// vmKyberGenerateKAT boots the Kyber768 oracle VM, extracts the KAT JSON,
// copies it to the canonical locations, and updates SHA256SUMS.
func vmKyberGenerateKAT() int {
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, fmt.Sprintf("cannot find repo root: %v", err))
		return 1
	}

	// ── Build oracle image ────────────────────────────────────────────
	vmCacheDir := filepath.Join(repoRoot, "build", "vm-images")
	if err := os.MkdirAll(vmCacheDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("cannot create vm-images dir: %v", err))
		return 1
	}
	oracleImg := filepath.Join(vmCacheDir, "kyber-oracle.raw")

	if _, err := os.Stat(oracleImg); os.IsNotExist(err) {
		log.Info(tag, "Building Kyber768 oracle VM image (nix-build)...")
		outLink := filepath.Join(vmCacheDir, "kyber-oracle-result")
		b := &vmctl.NixBuild{
			File:    filepath.Join(repoRoot, "contrib", "oracles", "vm-kyber-oracle.nix"),
			OutLink: outLink,
			Stdout:  os.Stdout,
			Stderr:  os.Stderr,
		}
		if buildErr := b.Build(); buildErr != nil {
			log.Fail(tag, fmt.Sprintf("nix-build failed: %v", buildErr))
			log.Info(tag, "Note: contrib/oracles/vm-kyber-oracle.nix uses placeholder hashes.")
			log.Info(tag, "Fill in real hashes with nix-prefetch-git before running.")
			return 1
		}
		// Copy the raw image out of the Nix store result symlink.
		storePath := filepath.Join(outLink, "nixos.raw")
		if err := copyFile(storePath, oracleImg); err != nil {
			log.Fail(tag, fmt.Sprintf("cannot copy oracle image: %v", err))
			return 1
		}
		log.OK(tag, fmt.Sprintf("Oracle image ready at %s", oracleImg))
	} else {
		log.Info(tag, fmt.Sprintf("Using cached oracle image: %s", oracleImg))
	}

	// ── Prepare tmp + output directories ─────────────────────────────
	tmpDir := filepath.Join(repoRoot, "build", "vm-tmp", "kyber-oracle")
	if err := os.MkdirAll(tmpDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("cannot create tmp dir: %v", err))
		return 1
	}
	defer os.RemoveAll(tmpDir)

	outputDir := filepath.Join(tmpDir, "output")
	if err := os.MkdirAll(outputDir, 0o700); err != nil {
		log.Fail(tag, fmt.Sprintf("cannot create output dir: %v", err))
		return 1
	}

	// ── Boot oracle VM ────────────────────────────────────────────────
	outputShare := ninep.DefaultOutputShare(outputDir)
	spec := &vmctl.VMSpec{
		Hypervisor: vmctl.HypervisorQEMU,
		BaseImage: vmctl.ImageRef{
			Path:   oracleImg,
			Format: vmctl.DiskFormatRaw,
		},
		Shares: []vmctl.ShareSpec{{
			HostPath:      outputShare.LocalPath,
			MountTag:      outputShare.MountTag,
			SecurityModel: string(outputShare.SecurityModel),
			ID:            outputShare.ID,
		}},
		Network:  vmctl.NetworkSpec{RawArgs: "-nic none"},
		Resources: vmctl.Resources{Fraction: 50, MinCores: 1, MinMemMB: 512},
		Timeout:  10 * time.Minute,
		NoReboot: true,
	}

	log.Info(tag, "Booting Kyber768 oracle VM...")
	ctx, cancel := context.WithTimeout(context.Background(), spec.Timeout)
	defer cancel()

	result, bootErr := (&vmctl.QEMUHypervisor{}).Boot(ctx, spec, tmpDir)
	if bootErr != nil {
		log.Fail(tag, fmt.Sprintf("QEMU boot error: %v", bootErr))
		return 1
	}
	if result.ExitCode != 0 {
		log.Fail(tag, fmt.Sprintf("Oracle VM exited with code %d", result.ExitCode))
		return 1
	}

	// ── Extract KAT JSON ──────────────────────────────────────────────
	katSrc := filepath.Join(outputDir, "kyber-kat.json")
	if _, err := os.Stat(katSrc); os.IsNotExist(err) {
		log.Fail(tag, "Oracle VM did not produce kyber-kat.json in /output")
		return 1
	}

	// ── Copy to canonical locations ───────────────────────────────────
	//   1. test/vectors/nist/mlkem768-kat.json
	//   2. build/differential/traces/kyber-kat.json

	vectorsDir := filepath.Join(repoRoot, "test", "vectors", "nist")
	if err := os.MkdirAll(vectorsDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("cannot create vectors dir: %v", err))
		return 1
	}
	vectorsDst := filepath.Join(vectorsDir, "mlkem768-kat.json")
	if err := copyFile(katSrc, vectorsDst); err != nil {
		log.Fail(tag, fmt.Sprintf("cannot copy to vectors: %v", err))
		return 1
	}

	tracesDir := filepath.Join(repoRoot, "build", "differential", "traces")
	if err := os.MkdirAll(tracesDir, 0o755); err != nil {
		log.Fail(tag, fmt.Sprintf("cannot create traces dir: %v", err))
		return 1
	}
	tracesDst := filepath.Join(tracesDir, "kyber-kat.json")
	if err := copyFile(katSrc, tracesDst); err != nil {
		log.Fail(tag, fmt.Sprintf("cannot copy to traces: %v", err))
		return 1
	}

	// ── Update SHA256SUMS ─────────────────────────────────────────────
	if err := updateSHA256Sums(repoRoot, vectorsDst, "nist/mlkem768-kat.json"); err != nil {
		log.Fail(tag, fmt.Sprintf("cannot update SHA256SUMS: %v", err))
		return 1
	}

	log.OK(tag, "Kyber768 KAT vectors generated successfully.")
	log.Info(tag, fmt.Sprintf("  Vectors: %s", vectorsDst))
	log.Info(tag, fmt.Sprintf("  Traces:  %s", tracesDst))
	log.Info(tag, "Run './uv test differential-kyber' to replay the traces.")
	return 0
}

// copyFile copies src to dst, creating or truncating dst.
func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return fmt.Errorf("open src %s: %w", src, err)
	}
	defer in.Close()

	out, err := os.Create(dst)
	if err != nil {
		return fmt.Errorf("create dst %s: %w", dst, err)
	}
	defer out.Close()

	if _, err := io.Copy(out, in); err != nil {
		os.Remove(dst)
		return fmt.Errorf("copy: %w", err)
	}
	return nil
}

// updateSHA256Sums computes the SHA256 of filePath and upserts the entry for
// relName in test/vectors/SHA256SUMS.
func updateSHA256Sums(repoRoot, filePath, relName string) error {
	// Compute hash
	f, err := os.Open(filePath)
	if err != nil {
		return fmt.Errorf("open for hash: %w", err)
	}
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		f.Close()
		return fmt.Errorf("hash: %w", err)
	}
	f.Close()
	digest := hex.EncodeToString(h.Sum(nil))

	sumsPath := filepath.Join(repoRoot, "test", "vectors", "SHA256SUMS")

	// Read existing content (tolerate missing file)
	var existing []byte
	if data, err := os.ReadFile(sumsPath); err == nil {
		existing = data
	}

	// Build updated lines: drop any existing entry for relName, append new one.
	lines := splitLines(string(existing))
	newEntry := digest + "  " + relName
	updated := make([]string, 0, len(lines)+1)
	replaced := false
	for _, line := range lines {
		if len(line) > len(relName) && line[len(line)-len(relName):] == relName {
			updated = append(updated, newEntry)
			replaced = true
		} else if line != "" {
			updated = append(updated, line)
		}
	}
	if !replaced {
		updated = append(updated, newEntry)
	}

	content := ""
	for _, l := range updated {
		content += l + "\n"
	}
	return os.WriteFile(sumsPath, []byte(content), 0o644)
}
