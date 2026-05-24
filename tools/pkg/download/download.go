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
	"strings"
	"time"
)

// FetchFile downloads url to dest, verifying the SHA-256 hash matches expected.
// If expected is empty, the hash check is skipped.
// Returns an error on HTTP errors, timeouts, or hash mismatches.
func FetchFile(url, dest, expected string) error {
	if err := os.MkdirAll(filepath.Dir(dest), 0o755); err != nil {
		return fmt.Errorf("create destination directory: %w", err)
	}

	client := &http.Client{Timeout: 5 * time.Minute}
	resp, err := client.Get(url) //nolint:gosec // URL comes from pinned config
	if err != nil {
		return fmt.Errorf("download %s: %w", url, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("download %s: HTTP %d", url, resp.StatusCode)
	}

	// Reject HTML error pages (GitHub returns 200 with HTML for missing assets)
	ct := resp.Header.Get("Content-Type")
	if strings.Contains(ct, "text/html") {
		return fmt.Errorf("download %s: got HTML response (asset likely missing)", url)
	}

	tmp := dest + ".tmp"
	f, err := os.Create(tmp)
	if err != nil {
		return fmt.Errorf("create temp file: %w", err)
	}
	defer func() { os.Remove(tmp) }()

	h := sha256.New()
	w := io.MultiWriter(f, h)

	if _, err := io.Copy(w, io.LimitReader(resp.Body, 10*1024*1024*1024)); err != nil {
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
