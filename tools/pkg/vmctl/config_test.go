// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"os"
	"testing"
	"time"
)

// writeTemp writes content to a temporary YAML file and returns its path.
// The caller is responsible for removing it via t.Cleanup.
func writeTemp(t *testing.T, content string) string {
	t.Helper()
	f, err := os.CreateTemp(t.TempDir(), "vmspec-*.yaml")
	if err != nil {
		t.Fatalf("create temp file: %v", err)
	}
	if _, err := f.WriteString(content); err != nil {
		t.Fatalf("write temp file: %v", err)
	}
	if err := f.Close(); err != nil {
		t.Fatalf("close temp file: %v", err)
	}
	return f.Name()
}

func TestLoadSpec_Basic(t *testing.T) {
	yaml := `name: test-vm
tier: ci
hypervisor: qemu
display: none

resources:
  fraction: 25
  min_cores: 2
  min_memory: 512

network:
  mode: user

boot:
  timeout: 5m
  no_reboot: true
`
	path := writeTemp(t, yaml)
	name, spec, err := LoadSpec(path)
	if err != nil {
		t.Fatalf("LoadSpec returned unexpected error: %v", err)
	}
	if name != "test-vm" {
		t.Errorf("name: got %q, want %q", name, "test-vm")
	}
	if spec.Hypervisor != HypervisorQEMU {
		t.Errorf("hypervisor: got %v, want HypervisorQEMU", spec.Hypervisor)
	}
	if spec.Display != DisplayNone {
		t.Errorf("display: got %v, want DisplayNone", spec.Display)
	}
	if spec.Network.Mode != NetworkUserMode {
		t.Errorf("network.mode: got %v, want NetworkUserMode", spec.Network.Mode)
	}
	if spec.Resources.Fraction != 25 {
		t.Errorf("resources.fraction: got %d, want 25", spec.Resources.Fraction)
	}
	if spec.Resources.MinCores != 2 {
		t.Errorf("resources.min_cores: got %d, want 2", spec.Resources.MinCores)
	}
	if spec.Resources.MinMemMB != 512 {
		t.Errorf("resources.min_memory: got %d, want 512", spec.Resources.MinMemMB)
	}
	if spec.Timeout != 5*time.Minute {
		t.Errorf("boot.timeout: got %v, want 5m", spec.Timeout)
	}
	if !spec.NoReboot {
		t.Error("boot.no_reboot: got false, want true")
	}
}

func TestLoadSpec_HypervisorStrings(t *testing.T) {
	cases := []struct {
		raw  string
		want HypervisorType
	}{
		{"qemu", HypervisorQEMU},
		{"firecracker", HypervisorFirecracker},
		{"direct", HypervisorDirect},
		{"", HypervisorDirect}, // empty defaults to direct
	}

	for _, tc := range cases {
		yaml := "name: hv-test\nhypervisor: " + tc.raw + "\n"
		if tc.raw == "" {
			yaml = "name: hv-test\n"
		}
		path := writeTemp(t, yaml)
		_, spec, err := LoadSpec(path)
		if err != nil {
			t.Errorf("hypervisor=%q: unexpected error: %v", tc.raw, err)
			continue
		}
		if spec.Hypervisor != tc.want {
			t.Errorf("hypervisor=%q: got %v, want %v", tc.raw, spec.Hypervisor, tc.want)
		}
	}
}

func TestLoadSpec_DisplayStrings(t *testing.T) {
	cases := []struct {
		raw  string
		want DisplayMode
	}{
		{"gtk", DisplayGTK},
		{"none", DisplayNone},
		{"", DisplayNone}, // absent field defaults to none
	}

	for _, tc := range cases {
		yaml := "name: disp-test\n"
		if tc.raw != "" {
			yaml += "display: " + tc.raw + "\n"
		}
		path := writeTemp(t, yaml)
		_, spec, err := LoadSpec(path)
		if err != nil {
			t.Errorf("display=%q: unexpected error: %v", tc.raw, err)
			continue
		}
		if spec.Display != tc.want {
			t.Errorf("display=%q: got %v, want %v", tc.raw, spec.Display, tc.want)
		}
	}
}

func TestLoadSpec_UnknownHypervisorReturnsError(t *testing.T) {
	yaml := "name: bad-hv\nhypervisor: xen\n"
	path := writeTemp(t, yaml)
	_, _, err := LoadSpec(path)
	if err == nil {
		t.Fatal("expected error for unknown hypervisor, got nil")
	}
}

func TestLoadSpec_MissingNameReturnsError(t *testing.T) {
	yaml := "hypervisor: qemu\n"
	path := writeTemp(t, yaml)
	_, _, err := LoadSpec(path)
	if err == nil {
		t.Fatal("expected error for missing name field, got nil")
	}
}

func TestLoadSpec_CommentsAndBlankLinesIgnored(t *testing.T) {
	yaml := `# This is a comment
name: comment-vm

# another comment
hypervisor: qemu # inline comment
`
	path := writeTemp(t, yaml)
	name, spec, err := LoadSpec(path)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if name != "comment-vm" {
		t.Errorf("name: got %q, want %q", name, "comment-vm")
	}
	if spec.Hypervisor != HypervisorQEMU {
		t.Errorf("hypervisor: got %v, want HypervisorQEMU", spec.Hypervisor)
	}
}

func TestLoadSpec_NonExistentFileReturnsError(t *testing.T) {
	_, _, err := LoadSpec("/tmp/does-not-exist-vmspec.yaml")
	if err == nil {
		t.Fatal("expected error for non-existent file, got nil")
	}
}
