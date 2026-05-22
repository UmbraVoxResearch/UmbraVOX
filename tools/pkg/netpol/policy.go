// Package netpol parses vm-network-policy.conf and generates QEMU network arguments.
//
// This is the Go equivalent of scripts/vm-network-policy.sh.
// Default policy: deny all network access (-nic none).
// ALLOW rules are currently declarative-only; the parser returns an error
// if enforceable ALLOW rules are present (fail-closed, matching shell behavior).
package netpol

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// Rule represents a single ALLOW rule from the policy file.
type Rule struct {
	Protocol    string // tcp, udp, icmp, any
	Destination string // domain, IP, or CIDR
	Port        string // number, range, or *
}

// Policy represents a parsed vm-network-policy.conf.
type Policy struct {
	Rules []Rule
}

// ParseFile reads and parses a vm-network-policy.conf file.
// Returns a Policy with any ALLOW rules found.
// If the file does not exist, returns an empty policy (deny all).
func ParseFile(path string) (*Policy, error) {
	f, err := os.Open(path)
	if os.IsNotExist(err) {
		return &Policy{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("open policy file: %w", err)
	}
	defer f.Close()

	p := &Policy{}
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		if strings.HasPrefix(line, "ALLOW ") {
			fields := strings.Fields(line)
			if len(fields) < 4 {
				continue // malformed, skip
			}
			p.Rules = append(p.Rules, Rule{
				Protocol:    fields[1],
				Destination: fields[2],
				Port:        fields[3],
			})
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("read policy file: %w", err)
	}
	return p, nil
}

// QEMUNetArgs returns the QEMU network arguments for this policy.
// If no ALLOW rules are present, returns "-nic none" (deny all).
// If ALLOW rules are present, returns an error because enforcement
// translation is not yet implemented (fail-closed).
func (p *Policy) QEMUNetArgs() (string, error) {
	if len(p.Rules) == 0 {
		return "-nic none", nil
	}

	// ALLOW rules are present but enforcement is not implemented.
	// Fail closed, matching the shell script behavior.
	return "", fmt.Errorf(
		"ALLOW rules are present but enforcement is not implemented; " +
			"remove ALLOW rules or implement translation before booting the VM")
}
