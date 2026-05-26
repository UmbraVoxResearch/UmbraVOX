// SPDX-License-Identifier: Apache-2.0
package download

import (
	"crypto/sha256"
	"encoding/hex"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// sha256hex returns the hex-encoded SHA-256 of data.
func sha256hex(data []byte) string {
	h := sha256.Sum256(data)
	return hex.EncodeToString(h[:])
}

func TestFetchFile_Success(t *testing.T) {
	body := []byte("hello world\n")
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		w.Write(body)
	}))
	defer srv.Close()

	dir := t.TempDir()
	dest := filepath.Join(dir, "out.bin")
	expected := sha256hex(body)

	if err := FetchFile(srv.URL+"/file", dest, expected); err != nil {
		t.Fatalf("FetchFile: %v", err)
	}

	got, err := os.ReadFile(dest)
	if err != nil {
		t.Fatalf("read dest: %v", err)
	}
	if string(got) != string(body) {
		t.Errorf("content mismatch: got %q, want %q", got, body)
	}
}

func TestFetchFile_SuccessNoHash(t *testing.T) {
	body := []byte("no hash check")
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		w.Write(body)
	}))
	defer srv.Close()

	dir := t.TempDir()
	dest := filepath.Join(dir, "out.bin")

	if err := FetchFile(srv.URL+"/file", dest, ""); err != nil {
		t.Fatalf("FetchFile with empty hash: %v", err)
	}

	got, err := os.ReadFile(dest)
	if err != nil {
		t.Fatalf("read dest: %v", err)
	}
	if string(got) != string(body) {
		t.Errorf("content mismatch: got %q, want %q", got, body)
	}
}

func TestFetchFile_HashMismatch(t *testing.T) {
	body := []byte("real content")
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		w.Write(body)
	}))
	defer srv.Close()

	dir := t.TempDir()
	dest := filepath.Join(dir, "out.bin")
	wrongHash := strings.Repeat("ab", 32) // 64 hex chars, wrong

	err := FetchFile(srv.URL+"/file", dest, wrongHash)
	if err == nil {
		t.Fatal("expected SHA-256 mismatch error, got nil")
	}
	if !strings.Contains(err.Error(), "SHA-256 mismatch") {
		t.Errorf("expected SHA-256 mismatch message, got: %v", err)
	}

	// Destination file should not exist after hash failure (tmp was cleaned up).
	if _, statErr := os.Stat(dest); statErr == nil {
		t.Error("dest file should not exist after hash mismatch")
	}
}

func TestFetchFile_HTTP404(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "not found", http.StatusNotFound)
	}))
	defer srv.Close()

	dir := t.TempDir()
	dest := filepath.Join(dir, "out.bin")

	err := FetchFile(srv.URL+"/missing", dest, "")
	if err == nil {
		t.Fatal("expected HTTP 404 error, got nil")
	}
	if !strings.Contains(err.Error(), "HTTP 404") {
		t.Errorf("expected HTTP 404 in error, got: %v", err)
	}
}

func TestFetchFile_HTTP500(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "internal error", http.StatusInternalServerError)
	}))
	defer srv.Close()

	dir := t.TempDir()
	dest := filepath.Join(dir, "out.bin")

	err := FetchFile(srv.URL+"/fail", dest, "")
	if err == nil {
		t.Fatal("expected HTTP 500 error, got nil")
	}
	if !strings.Contains(err.Error(), "HTTP 500") {
		t.Errorf("expected HTTP 500 in error, got: %v", err)
	}
}

func TestFetchFile_HTMLResponse(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/html; charset=utf-8")
		w.Write([]byte("<html><body>Not Found</body></html>"))
	}))
	defer srv.Close()

	dir := t.TempDir()
	dest := filepath.Join(dir, "out.bin")

	err := FetchFile(srv.URL+"/fake", dest, "")
	if err == nil {
		t.Fatal("expected HTML rejection error, got nil")
	}
	if !strings.Contains(err.Error(), "HTML response") {
		t.Errorf("expected HTML response in error, got: %v", err)
	}
}

func TestFetchFile_CreatesParentDirs(t *testing.T) {
	body := []byte("nested")
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		w.Write(body)
	}))
	defer srv.Close()

	dir := t.TempDir()
	dest := filepath.Join(dir, "a", "b", "c", "out.bin")

	if err := FetchFile(srv.URL+"/file", dest, ""); err != nil {
		t.Fatalf("FetchFile: %v", err)
	}

	got, err := os.ReadFile(dest)
	if err != nil {
		t.Fatalf("read dest: %v", err)
	}
	if string(got) != string(body) {
		t.Errorf("content mismatch: got %q, want %q", got, body)
	}
}

func TestFetchFile_InvalidURL(t *testing.T) {
	dir := t.TempDir()
	dest := filepath.Join(dir, "out.bin")

	err := FetchFile("://bad-url", dest, "")
	if err == nil {
		t.Fatal("expected error for invalid URL, got nil")
	}
}

func TestFetchFile_UnreachableHost(t *testing.T) {
	dir := t.TempDir()
	dest := filepath.Join(dir, "out.bin")

	// 127.0.0.1:1 is almost certainly not listening.
	err := FetchFile("http://127.0.0.1:1/file", dest, "")
	if err == nil {
		t.Fatal("expected connection refused error, got nil")
	}
}

func TestFetchFile_EmptyBody(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		// Write nothing.
	}))
	defer srv.Close()

	dir := t.TempDir()
	dest := filepath.Join(dir, "out.bin")
	expected := sha256hex([]byte{})

	if err := FetchFile(srv.URL+"/empty", dest, expected); err != nil {
		t.Fatalf("FetchFile empty body: %v", err)
	}

	info, err := os.Stat(dest)
	if err != nil {
		t.Fatalf("stat dest: %v", err)
	}
	if info.Size() != 0 {
		t.Errorf("expected 0-byte file, got %d bytes", info.Size())
	}
}

func TestFetchFile_LargeBody(t *testing.T) {
	// Verify a non-trivial payload (1 MiB) is downloaded and hashed correctly.
	body := make([]byte, 1024*1024)
	for i := range body {
		body[i] = byte(i % 251) // deterministic fill
	}
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		w.Write(body)
	}))
	defer srv.Close()

	dir := t.TempDir()
	dest := filepath.Join(dir, "large.bin")
	expected := sha256hex(body)

	if err := FetchFile(srv.URL+"/large", dest, expected); err != nil {
		t.Fatalf("FetchFile large body: %v", err)
	}

	got, err := os.ReadFile(dest)
	if err != nil {
		t.Fatalf("read dest: %v", err)
	}
	if len(got) != len(body) {
		t.Fatalf("size mismatch: got %d, want %d", len(got), len(body))
	}
}

func TestFetchFile_TmpCleanedOnHashFailure(t *testing.T) {
	body := []byte("content")
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		w.Write(body)
	}))
	defer srv.Close()

	dir := t.TempDir()
	dest := filepath.Join(dir, "out.bin")
	wrongHash := strings.Repeat("00", 32)

	_ = FetchFile(srv.URL+"/file", dest, wrongHash)

	// Neither dest nor the .tmp file should remain.
	if _, err := os.Stat(dest); err == nil {
		t.Error("dest should not exist after hash mismatch")
	}
	if _, err := os.Stat(dest + ".tmp"); err == nil {
		t.Error(".tmp file should be cleaned up after hash mismatch")
	}
}

func TestFetchFile_OverwritesExisting(t *testing.T) {
	body := []byte("new content")
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/octet-stream")
		w.Write(body)
	}))
	defer srv.Close()

	dir := t.TempDir()
	dest := filepath.Join(dir, "out.bin")

	// Write an existing file at dest.
	if err := os.WriteFile(dest, []byte("old content"), 0o644); err != nil {
		t.Fatalf("write existing: %v", err)
	}

	if err := FetchFile(srv.URL+"/file", dest, ""); err != nil {
		t.Fatalf("FetchFile: %v", err)
	}

	got, err := os.ReadFile(dest)
	if err != nil {
		t.Fatalf("read dest: %v", err)
	}
	if string(got) != string(body) {
		t.Errorf("expected overwrite: got %q, want %q", got, body)
	}
}
