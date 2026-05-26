// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"os"
	"path/filepath"
	"testing"
)

func TestDiskManager_ZeroValue(t *testing.T) {
	// A zero-value DiskManager (nil Log) must be constructable without panic.
	dm := DiskManager{}
	if dm.Log != nil {
		t.Error("zero-value DiskManager should have nil Log")
	}
}

func TestDirSizeKB_EmptyDir(t *testing.T) {
	dir := t.TempDir()
	kb, err := dirSizeKB(dir)
	if err != nil {
		t.Fatalf("dirSizeKB on empty dir: %v", err)
	}
	// An empty directory occupies at most a few KB of filesystem metadata.
	if kb < 0 || kb > 16 {
		t.Errorf("empty dir size: got %d KB, want 0..16", kb)
	}
}

func TestDirSizeKB_KnownContent(t *testing.T) {
	dir := t.TempDir()

	// Write 64 KB of data spread across two files.
	for _, name := range []string{"a.bin", "b.bin"} {
		path := filepath.Join(dir, name)
		data := make([]byte, 32*1024) // 32 KB each
		if err := os.WriteFile(path, data, 0o644); err != nil {
			t.Fatalf("write %s: %v", name, err)
		}
	}

	kb, err := dirSizeKB(dir)
	if err != nil {
		t.Fatalf("dirSizeKB: %v", err)
	}

	// Expect at least 64 KB of content; filesystem overhead may add a few KB.
	if kb < 64 {
		t.Errorf("size too small: got %d KB, want >= 64", kb)
	}
	if kb > 128 {
		t.Errorf("size unexpectedly large: got %d KB, want <= 128", kb)
	}
}

func TestDirSizeKB_Subdirectory(t *testing.T) {
	dir := t.TempDir()
	sub := filepath.Join(dir, "nested")
	if err := os.Mkdir(sub, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	data := make([]byte, 16*1024)
	if err := os.WriteFile(filepath.Join(sub, "c.bin"), data, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	kb, err := dirSizeKB(dir)
	if err != nil {
		t.Fatalf("dirSizeKB: %v", err)
	}

	// du -sk recurses, so the nested file must be counted.
	if kb < 16 {
		t.Errorf("size too small: got %d KB, want >= 16", kb)
	}
}

func TestDirSizeKB_NonExistentDir(t *testing.T) {
	_, err := dirSizeKB("/tmp/vmctl-does-not-exist-" + t.Name())
	if err == nil {
		t.Fatal("expected error for non-existent directory, got nil")
	}
}

func TestCreateOverlay_EmptyDirRejected(t *testing.T) {
	dm := DiskManager{}
	_, err := dm.CreateOverlay("/some/base.img", "")
	if err == nil {
		t.Fatal("expected error when dir is empty, got nil")
	}
}

func TestOverlay_Remove(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "test-overlay.qcow2")
	if err := os.WriteFile(path, []byte("fake"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	ov := &Overlay{Path: path, BackFile: "/dev/null"}
	if err := ov.Remove(); err != nil {
		t.Fatalf("Remove: %v", err)
	}

	if _, err := os.Stat(path); !os.IsNotExist(err) {
		t.Error("overlay file still exists after Remove")
	}
}

func TestOverlay_RemoveNonExistent(t *testing.T) {
	ov := &Overlay{Path: "/tmp/vmctl-no-such-overlay-" + t.Name()}
	err := ov.Remove()
	if err == nil {
		t.Fatal("expected error removing non-existent overlay, got nil")
	}
}
