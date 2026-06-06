// SPDX-License-Identifier: Apache-2.0
package netproxy

import (
	"os"
	"path/filepath"
	"testing"
)

func writePolicy(t *testing.T, content string) string {
	t.Helper()
	dir := t.TempDir()
	path := filepath.Join(dir, "allowlist.policy")
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	return path
}

func TestParseAllowlist_ValidEntries(t *testing.T) {
	path := writePolicy(t, `# NixOS binary cache
ALLOW cache.nixos.org 443

ALLOW releases.nixos.org 443
`)
	entries, err := ParseAllowlist(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(entries) != 2 {
		t.Fatalf("expected 2 entries, got %d", len(entries))
	}
	if entries[0].Host != "cache.nixos.org" || entries[0].Port != "443" {
		t.Errorf("entry[0]: got {%q, %q}", entries[0].Host, entries[0].Port)
	}
	if entries[1].Host != "releases.nixos.org" || entries[1].Port != "443" {
		t.Errorf("entry[1]: got {%q, %q}", entries[1].Host, entries[1].Port)
	}
}

func TestParseAllowlist_EmptyFile(t *testing.T) {
	path := writePolicy(t, "")
	entries, err := ParseAllowlist(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(entries) != 0 {
		t.Fatalf("expected empty slice, got %d entries", len(entries))
	}
}

func TestParseAllowlist_NonExistentFile(t *testing.T) {
	_, err := ParseAllowlist("/nonexistent/path/allowlist.policy")
	if err == nil {
		t.Fatal("expected error for non-existent file, got nil")
	}
}

func TestParseAllowlist_SkipsNonAllowLines(t *testing.T) {
	path := writePolicy(t, `DENY evil.example.com 443
BLOCK bad.example.com 80
ALLOW good.example.com 443
`)
	entries, err := ParseAllowlist(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(entries))
	}
	if entries[0].Host != "good.example.com" || entries[0].Port != "443" {
		t.Errorf("entry[0]: got {%q, %q}", entries[0].Host, entries[0].Port)
	}
}

func TestParseAllowlist_CaseInsensitiveKeyword(t *testing.T) {
	path := writePolicy(t, `allow lowercase.example.com 80
Allow Mixed.example.com 8080
ALLOW UPPER.EXAMPLE.COM 443
`)
	entries, err := ParseAllowlist(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(entries) != 3 {
		t.Fatalf("expected 3 entries, got %d", len(entries))
	}
	if entries[0].Host != "lowercase.example.com" || entries[0].Port != "80" {
		t.Errorf("entry[0]: got {%q, %q}", entries[0].Host, entries[0].Port)
	}
	if entries[1].Host != "Mixed.example.com" || entries[1].Port != "8080" {
		t.Errorf("entry[1]: got {%q, %q}", entries[1].Host, entries[1].Port)
	}
	if entries[2].Host != "UPPER.EXAMPLE.COM" || entries[2].Port != "443" {
		t.Errorf("entry[2]: got {%q, %q}", entries[2].Host, entries[2].Port)
	}
}

func TestParseAllowlist_ExtraFieldsIgnored(t *testing.T) {
	path := writePolicy(t, `ALLOW extra.example.com 443 extra1 extra2 extra3
`)
	entries, err := ParseAllowlist(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(entries))
	}
	if entries[0].Host != "extra.example.com" || entries[0].Port != "443" {
		t.Errorf("entry[0]: got {%q, %q}", entries[0].Host, entries[0].Port)
	}
}
