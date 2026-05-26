// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

func TestIsCached_SymlinkToNixStore(t *testing.T) {
	dir := t.TempDir()
	target := filepath.Join(dir, "nix-store-placeholder")
	if err := os.MkdirAll(target, 0o755); err != nil {
		t.Fatalf("create placeholder dir: %v", err)
	}

	link := filepath.Join(dir, "result")
	// IsCached checks the raw readlink target string, not whether the
	// destination exists, so we can point at a fabricated /nix/store/ path.
	os.Remove(link) // ensure clean slate
	if err := os.Symlink("/nix/store/abc123-vm-image", link); err != nil {
		t.Fatalf("create symlink: %v", err)
	}

	b := &NixBuild{OutLink: link}
	if !b.IsCached() {
		t.Error("IsCached: got false for symlink pointing to /nix/store/..., want true")
	}
}

func TestIsCached_SymlinkOutsideNixStore(t *testing.T) {
	dir := t.TempDir()
	target := filepath.Join(dir, "some-other-dir")
	if err := os.MkdirAll(target, 0o755); err != nil {
		t.Fatalf("create target dir: %v", err)
	}

	link := filepath.Join(dir, "result")
	if err := os.Symlink(target, link); err != nil {
		t.Fatalf("create symlink: %v", err)
	}

	b := &NixBuild{OutLink: link}
	if b.IsCached() {
		t.Error("IsCached: got true for symlink pointing outside /nix/store/, want false")
	}
}

func TestIsCached_RelativeSymlink(t *testing.T) {
	dir := t.TempDir()
	link := filepath.Join(dir, "result")
	// A relative symlink target does not start with "/", so IsCached
	// returns false regardless of what it points to.
	if err := os.Symlink("../nix/store/relative", link); err != nil {
		t.Fatalf("create relative symlink: %v", err)
	}

	b := &NixBuild{OutLink: link}
	if b.IsCached() {
		t.Error("IsCached: got true for relative symlink, want false")
	}
}

func TestIsCached_RegularFile(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "result")
	if err := os.WriteFile(file, []byte("data"), 0o644); err != nil {
		t.Fatalf("write file: %v", err)
	}

	b := &NixBuild{OutLink: file}
	// A plain file is not a symlink; Readlink fails, but Stat succeeds,
	// so IsCached returns true (the code treats an existing plain
	// file/dir as cached).
	if !b.IsCached() {
		t.Error("IsCached: got false for regular file, want true")
	}
}

func TestIsCached_RegularDirectory(t *testing.T) {
	dir := t.TempDir()
	sub := filepath.Join(dir, "result")
	if err := os.Mkdir(sub, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	b := &NixBuild{OutLink: sub}
	if !b.IsCached() {
		t.Error("IsCached: got false for existing directory, want true")
	}
}

func TestIsCached_NonExistentPath(t *testing.T) {
	b := &NixBuild{OutLink: filepath.Join(t.TempDir(), "does-not-exist")}
	if b.IsCached() {
		t.Error("IsCached: got true for non-existent path, want false")
	}
}

func TestIsCached_BrokenSymlink(t *testing.T) {
	dir := t.TempDir()
	link := filepath.Join(dir, "result")
	// Points to /nix/store/... but target does not actually exist on disk.
	// IsCached only checks the readlink value, not whether the target exists.
	if err := os.Symlink("/nix/store/broken-does-not-exist", link); err != nil {
		t.Fatalf("create symlink: %v", err)
	}

	b := &NixBuild{OutLink: link}
	if !b.IsCached() {
		t.Error("IsCached: got false for broken symlink into /nix/store/, want true")
	}
}

func TestFindNixBuild_InPATH(t *testing.T) {
	// If nix-build is genuinely on PATH, FindNixBuild should return it.
	if _, err := exec.LookPath("nix-build"); err != nil {
		t.Skip("nix-build not on PATH; skipping")
	}

	path, err := FindNixBuild()
	if err != nil {
		t.Fatalf("FindNixBuild returned unexpected error: %v", err)
	}
	if path == "" {
		t.Error("FindNixBuild returned empty path")
	}
}

func TestFindNixBuild_NotFound(t *testing.T) {
	// Temporarily clear PATH so LookPath cannot find nix-build, and ensure
	// the fallback path also does not exist. We only run this when the
	// default fallback /nix/var/nix/profiles/default/bin/nix-build is
	// absent, to avoid a false pass on systems where it exists.
	const fallback = "/nix/var/nix/profiles/default/bin/nix-build"
	if _, err := os.Stat(fallback); err == nil {
		t.Skip("default nix-build fallback exists; cannot test not-found path")
	}

	orig := os.Getenv("PATH")
	t.Setenv("PATH", t.TempDir()) // empty dir, no executables

	_, err := FindNixBuild()
	if err == nil {
		t.Error("FindNixBuild: expected error when nix-build is not available, got nil")
	}

	// Restore PATH explicitly so deferred cleanup in t.TempDir works.
	os.Setenv("PATH", orig)
}
