// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// YAMLSpec is the on-disk YAML representation of a VM definition.
type YAMLSpec struct {
	Name       string
	Tier       string
	Hypervisor string
	Resources  YAMLResources
	Network    YAMLNetwork
	Boot       YAMLBoot
	Display    string
}

// YAMLResources holds the resource allocation fields from a vm-def.
type YAMLResources struct {
	Fraction  int
	MinCores  int
	MinMemory int
}

// YAMLNetwork holds the network configuration from a vm-def.
type YAMLNetwork struct {
	Mode string
}

// YAMLBoot holds the boot configuration from a vm-def.
type YAMLBoot struct {
	Timeout   string
	NoReboot  bool
}

// LoadSpec reads a YAML vm-def file and converts it to a VMSpec.
// It returns the VM name (from the "name:" field) alongside the spec.
func LoadSpec(path string) (string, *VMSpec, error) {
	ys, err := parseYAMLSpec(path)
	if err != nil {
		return "", nil, fmt.Errorf("load spec %s: %w", path, err)
	}
	spec, err := convertSpec(ys)
	if err != nil {
		return "", nil, err
	}
	return ys.Name, spec, nil
}

// LoadSpecDir reads all .yaml files in dir and returns a map keyed by VM name.
func LoadSpecDir(dir string) (map[string]*VMSpec, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil, fmt.Errorf("read spec dir %s: %w", dir, err)
	}
	specs := make(map[string]*VMSpec)
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		name := e.Name()
		if !strings.HasSuffix(name, ".yaml") && !strings.HasSuffix(name, ".yml") {
			continue
		}
		vmName, spec, err := LoadSpec(filepath.Join(dir, name))
		if err != nil {
			return nil, err
		}
		specs[vmName] = spec
	}
	return specs, nil
}

// parseYAMLSpec reads a simple YAML file into a YAMLSpec.
// It handles the flat key-value format used by vm-defs/ files:
// top-level scalars and one level of nesting (indented keys under
// a section header like "resources:").
func parseYAMLSpec(path string) (*YAMLSpec, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	var ys YAMLSpec
	section := ""
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()

		// Strip comments and trailing whitespace.
		if idx := strings.Index(line, " #"); idx >= 0 {
			line = line[:idx]
		}
		trimmed := strings.TrimSpace(line)
		if trimmed == "" || trimmed[0] == '#' {
			continue
		}

		// Detect indentation: indented lines belong to the current section.
		indented := line != strings.TrimLeft(line, " \t")

		key, value, ok := splitKV(trimmed)
		if !ok {
			continue
		}

		if !indented {
			// Top-level key. If value is empty, it is a section header.
			if value == "" {
				section = key
				continue
			}
			section = ""
			switch key {
			case "name":
				ys.Name = value
			case "tier":
				ys.Tier = value
			case "hypervisor":
				ys.Hypervisor = value
			case "display":
				ys.Display = value
			}
			continue
		}

		// Indented key inside a section.
		switch section {
		case "resources":
			switch key {
			case "fraction":
				ys.Resources.Fraction, err = strconv.Atoi(value)
				if err != nil {
					return nil, fmt.Errorf("%s: resources.fraction: %w", path, err)
				}
			case "min_cores":
				ys.Resources.MinCores, err = strconv.Atoi(value)
				if err != nil {
					return nil, fmt.Errorf("%s: resources.min_cores: %w", path, err)
				}
			case "min_memory":
				ys.Resources.MinMemory, err = strconv.Atoi(value)
				if err != nil {
					return nil, fmt.Errorf("%s: resources.min_memory: %w", path, err)
				}
			}
		case "network":
			if key == "mode" {
				ys.Network.Mode = value
			}
		case "boot":
			switch key {
			case "timeout":
				ys.Boot.Timeout = value
			case "no_reboot":
				ys.Boot.NoReboot = value == "true"
			}
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("%s: scan: %w", path, err)
	}
	if ys.Name == "" {
		return nil, fmt.Errorf("%s: missing required field: name", path)
	}
	return &ys, nil
}

// splitKV splits "key: value" and returns (key, value, true).
// A section header "key:" returns (key, "", true).
// Lines without a colon return ("", "", false).
func splitKV(s string) (string, string, bool) {
	idx := strings.IndexByte(s, ':')
	if idx < 0 {
		return "", "", false
	}
	key := strings.TrimSpace(s[:idx])
	value := strings.TrimSpace(s[idx+1:])
	return key, value, true
}

// convertSpec transforms a parsed YAMLSpec into the runtime VMSpec.
func convertSpec(ys *YAMLSpec) (*VMSpec, error) {
	spec := &VMSpec{
		Resources: Resources{
			Fraction: ys.Resources.Fraction,
			MinCores: ys.Resources.MinCores,
			MinMemMB: ys.Resources.MinMemory,
		},
	}

	// Hypervisor.
	switch ys.Hypervisor {
	case "qemu":
		spec.Hypervisor = HypervisorQEMU
	case "firecracker":
		spec.Hypervisor = HypervisorFirecracker
	case "direct", "":
		spec.Hypervisor = HypervisorDirect
	default:
		return nil, fmt.Errorf("unknown hypervisor %q", ys.Hypervisor)
	}

	// Display.
	switch ys.Display {
	case "gtk":
		spec.Display = DisplayGTK
	case "none", "":
		spec.Display = DisplayNone
	default:
		return nil, fmt.Errorf("unknown display mode %q", ys.Display)
	}

	// Network.
	switch ys.Network.Mode {
	case "user":
		spec.Network.Mode = NetworkUserMode
	case "tap":
		spec.Network.Mode = NetworkTAP
	case "none", "":
		spec.Network.Mode = NetworkNone
	default:
		return nil, fmt.Errorf("unknown network mode %q", ys.Network.Mode)
	}

	// Boot.
	if ys.Boot.Timeout != "" {
		d, err := time.ParseDuration(ys.Boot.Timeout)
		if err != nil {
			return nil, fmt.Errorf("boot.timeout: %w", err)
		}
		spec.Timeout = d
	}
	spec.NoReboot = ys.Boot.NoReboot

	return spec, nil
}
