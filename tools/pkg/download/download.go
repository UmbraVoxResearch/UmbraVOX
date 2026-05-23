// SPDX-License-Identifier: Apache-2.0
// Package download fetches files with SHA-256 verification.
package download

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
)

// FetchFile downloads url to dest, verifying the SHA-256 hash matches expected.
// If expected is empty, the hash check is skipped.
func FetchFile(url, dest, expected string) error {
	if err := os.MkdirAll(filepath.Dir(dest), 0o755); err != nil {
		return fmt.Errorf("create destination directory: %w", err)
	}

	resp, err := http.Get(url) //nolint:gosec // URL comes from pinned config
	if err != nil {
		return fmt.Errorf("download %s: %w", url, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("download %s: HTTP %d", url, resp.StatusCode)
	}

	tmp := dest + ".tmp"
	f, err := os.Create(tmp)
	if err != nil {
		return fmt.Errorf("create temp file: %w", err)
	}
	defer func() { os.Remove(tmp) }()

	h := sha256.New()
	w := io.MultiWriter(f, h)

	if _, err := io.Copy(w, resp.Body); err != nil {
		f.Close()
		return fmt.Errorf("write %s: %w", tmp, err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("close %s: %w", tmp, err)
	}

	if expected != "" {
		got := hex.EncodeToString(h.Sum(nil))
		if got != expected {
			return fmt.Errorf("SHA-256 mismatch: got %s, expected %s", got, expected)
		}
	}

	if err := os.Rename(tmp, dest); err != nil {
		return fmt.Errorf("rename %s -> %s: %w", tmp, dest, err)
	}
	return nil
}
