// SPDX-License-Identifier: Apache-2.0
// Package vmctl provides shared VM control helpers used by multiple commands.
package vmctl

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
)

// NixBuild describes a nix-build invocation.
//
// Typical usage:
//
//	b := &vmctl.NixBuild{
//	    File:    filepath.Join(repoRoot, "nix", "vm-image.nix"),
//	    Attr:    "qemu",
//	    OutLink: filepath.Join(repoRoot, "build", "vm", "image"),
//	    Stdout:  os.Stdout,
//	    Stderr:  os.Stderr,
//	}
//	if err := b.Build(); err != nil { ... }
type NixBuild struct {
	File      string    // path to .nix file
	Attr      string    // -A attribute selector
	OutLink   string    // -o output path (result symlink)
	ExtraArgs []string  // additional flags passed verbatim after the standard args
	Stdout    io.Writer // nil → os.Stdout
	Stderr    io.Writer // nil → os.Stderr
}

// FindNixBuild locates the nix-build binary. It first searches PATH; if not
// found there it falls back to the default Nix profile location
// /nix/var/nix/profiles/default/bin/nix-build. Returns an error if neither
// location yields an executable.
func FindNixBuild() (string, error) {
	if p, err := exec.LookPath("nix-build"); err == nil {
		return p, nil
	}
	const defaultPath = "/nix/var/nix/profiles/default/bin/nix-build"
	if _, err := os.Stat(defaultPath); err == nil {
		return defaultPath, nil
	}
	return "", fmt.Errorf("nix-build not found on PATH or at %s", defaultPath)
}

// IsCached reports whether OutLink already exists and resolves to a path
// inside the Nix store (/nix/store/…). A stale or broken symlink returns
// false.
func (b *NixBuild) IsCached() bool {
	target, err := os.Readlink(b.OutLink)
	if err != nil {
		// OutLink is not a symlink (or does not exist).
		fi, statErr := os.Stat(b.OutLink)
		if statErr != nil {
			return false
		}
		// A plain directory/file can exist here too — treat as cached.
		return fi != nil
	}
	// Resolve relative symlinks relative to the symlink's directory.
	if !strings.HasPrefix(target, "/") {
		return false
	}
	return strings.HasPrefix(target, "/nix/store/")
}

// Build runs nix-build with the configured parameters.
//
// Before invoking nix-build it removes any existing OutLink so that a stale
// symlink from a previous interrupted build does not confuse nix-build.
//
// The constructed command is:
//
//	nix-build <File> -A <Attr> -o <OutLink> [ExtraArgs...]
func (b *NixBuild) Build() error {
	nixBuild, err := FindNixBuild()
	if err != nil {
		return err
	}

	// Remove stale output before invoking nix-build; nix-build refuses to
	// overwrite an existing path that it did not create.
	if b.OutLink != "" {
		if err := os.RemoveAll(b.OutLink); err != nil && !os.IsNotExist(err) {
			return fmt.Errorf("nix-build: failed to remove stale output %s: %w", b.OutLink, err)
		}
	}

	args := make([]string, 0, 6+len(b.ExtraArgs))
	args = append(args, b.File)
	if b.Attr != "" {
		args = append(args, "-A", b.Attr)
	}
	if b.OutLink != "" {
		args = append(args, "-o", b.OutLink)
	}
	args = append(args, b.ExtraArgs...)

	cmd := exec.Command(nixBuild, args...)

	stdout := b.Stdout
	if stdout == nil {
		stdout = os.Stdout
	}
	stderr := b.Stderr
	if stderr == nil {
		stderr = os.Stderr
	}
	cmd.Stdout = stdout
	cmd.Stderr = stderr

	if err := cmd.Run(); err != nil {
		attr := b.Attr
		if attr == "" {
			attr = "(default)"
		}
		return fmt.Errorf("nix-build %s -A %s failed: %w", b.File, attr, err)
	}
	return nil
}
