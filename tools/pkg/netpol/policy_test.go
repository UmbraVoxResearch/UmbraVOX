package netpol

import (
	"os"
	"path/filepath"
	"testing"
)

func TestParseFile_Valid(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "policy.conf")
	content := `# Network policy
# Comment line

ALLOW tcp example.com 443
ALLOW udp 10.0.0.0/8 53
ALLOW icmp * *
`
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}

	p, err := parseAndCheck(t, path, 3)
	if err != nil {
		return
	}

	cases := []Rule{
		{Protocol: "tcp", Destination: "example.com", Port: "443"},
		{Protocol: "udp", Destination: "10.0.0.0/8", Port: "53"},
		{Protocol: "icmp", Destination: "*", Port: "*"},
	}
	for i, want := range cases {
		got := p.Rules[i]
		if got.Protocol != want.Protocol || got.Destination != want.Destination || got.Port != want.Port {
			t.Errorf("rule %d: got %+v, want %+v", i, got, want)
		}
	}
}

func TestParseFile_MissingFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "nonexistent.conf")

	p, err := ParseFile(path)
	if err != nil {
		t.Fatalf("unexpected error for missing file: %v", err)
	}
	if len(p.Rules) != 0 {
		t.Errorf("expected zero rules for missing file, got %d", len(p.Rules))
	}
}

func TestParseFile_EmptyFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "empty.conf")
	if err := os.WriteFile(path, []byte(""), 0644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}

	p, err := ParseFile(path)
	if err != nil {
		t.Fatalf("unexpected error for empty file: %v", err)
	}
	if len(p.Rules) != 0 {
		t.Errorf("expected zero rules for empty file, got %d", len(p.Rules))
	}
}

func TestParseFile_CommentsOnly(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "comments.conf")
	content := `# only comments
# another comment
`
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}

	p, err := ParseFile(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(p.Rules) != 0 {
		t.Errorf("expected zero rules for comments-only file, got %d", len(p.Rules))
	}
}

func TestParseFile_MalformedLines(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "malformed.conf")
	// Malformed ALLOW lines (fewer than 4 fields) should be silently skipped.
	content := `ALLOW tcp
ALLOW udp 10.0.0.1
ALLOW tcp example.com 443
ALLOW
`
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}

	parseAndCheck(t, path, 1)
}

func TestParseFile_ExtraFields(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "extra.conf")
	// Extra fields beyond the required four should not break parsing.
	content := `ALLOW tcp example.com 443 extra-field
`
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}

	p, err := parseAndCheck(t, path, 1)
	if err != nil {
		return
	}
	r := p.Rules[0]
	if r.Protocol != "tcp" || r.Destination != "example.com" || r.Port != "443" {
		t.Errorf("unexpected rule: %+v", r)
	}
}

func TestParseFile_UnreadableFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "unreadable.conf")
	if err := os.WriteFile(path, []byte("ALLOW tcp x 80"), 0644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}
	if err := os.Chmod(path, 0000); err != nil {
		t.Fatalf("chmod: %v", err)
	}

	_, err := ParseFile(path)
	if err == nil {
		t.Fatal("expected error for unreadable file, got nil")
	}
}

func TestParseFile_NonAllowLines(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "nonallow.conf")
	content := `DENY tcp example.com 443
BLOCK udp 10.0.0.0/8 53
ALLOW tcp example.com 80
some random text
`
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}

	parseAndCheck(t, path, 1)
}

func TestQEMUNetArgs_DenyAll(t *testing.T) {
	p := &Policy{}
	got, err := p.QEMUNetArgs()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if got != "-nic none" {
		t.Errorf("expected %q, got %q", "-nic none", got)
	}
}

func TestQEMUNetArgs_WithRules(t *testing.T) {
	p := &Policy{
		Rules: []Rule{
			{Protocol: "tcp", Destination: "example.com", Port: "443"},
		},
	}
	_, err := p.QEMUNetArgs()
	if err == nil {
		t.Fatal("expected error when ALLOW rules are present, got nil")
	}
}

func TestQEMUNetArgs_MultipleRules(t *testing.T) {
	p := &Policy{
		Rules: []Rule{
			{Protocol: "tcp", Destination: "example.com", Port: "443"},
			{Protocol: "udp", Destination: "10.0.0.0/8", Port: "53"},
		},
	}
	_, err := p.QEMUNetArgs()
	if err == nil {
		t.Fatal("expected error when ALLOW rules are present, got nil")
	}
}

func TestParseFile_WhitespaceLines(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "whitespace.conf")
	content := "   \n\t\n  ALLOW tcp example.com 443  \n\n"
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}

	parseAndCheck(t, path, 1)
}

func TestRoundTrip_ParseThenQEMU_NoRules(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "empty.conf")
	if err := os.WriteFile(path, []byte("# deny all\n"), 0644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}

	p, err := ParseFile(path)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	got, err := p.QEMUNetArgs()
	if err != nil {
		t.Fatalf("qemu args: %v", err)
	}
	if got != "-nic none" {
		t.Errorf("expected %q, got %q", "-nic none", got)
	}
}

func TestRoundTrip_ParseThenQEMU_WithRules(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "allow.conf")
	if err := os.WriteFile(path, []byte("ALLOW tcp example.com 443\n"), 0644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}

	p, err := ParseFile(path)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	_, err = p.QEMUNetArgs()
	if err == nil {
		t.Fatal("expected error for policy with ALLOW rules, got nil")
	}
}

// parseAndCheck is a helper that parses and asserts the expected rule count.
func parseAndCheck(t *testing.T, path string, wantCount int) (*Policy, error) {
	t.Helper()
	p, err := ParseFile(path)
	if err != nil {
		t.Fatalf("unexpected parse error: %v", err)
		return nil, err
	}
	if len(p.Rules) != wantCount {
		t.Errorf("expected %d rules, got %d", wantCount, len(p.Rules))
	}
	return p, nil
}
