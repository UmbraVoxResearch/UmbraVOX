// SPDX-License-Identifier: Apache-2.0
// Package netproxy provides a UNIX socket TCP proxy with domain allowlisting.
//
// Used by the VM builder to restrict network access: QEMU's guestfwd routes
// guest connections through this filter, which only allows connections to
// domains listed in the allowlist file.
package netproxy

import (
	"bufio"
	"fmt"
	"io"
	"net"
	"os"
	"strings"
	"sync"
)

// AllowEntry represents one allowed destination.
type AllowEntry struct {
	Host string
	Port string
}

// Filter listens on a UNIX socket and proxies TCP connections to allowed destinations.
type Filter struct {
	SocketPath string
	Allowed    []AllowEntry
	listener   net.Listener
	wg         sync.WaitGroup
}

// ParseAllowlist reads a policy file with lines like "ALLOW cache.nixos.org 443".
// Blank lines and lines starting with # are ignored.
func ParseAllowlist(path string) ([]AllowEntry, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("open allowlist: %w", err)
	}
	defer f.Close()

	var entries []AllowEntry
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		fields := strings.Fields(line)
		if len(fields) >= 3 && strings.ToUpper(fields[0]) == "ALLOW" {
			entries = append(entries, AllowEntry{Host: fields[1], Port: fields[2]})
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("read allowlist: %w", err)
	}
	return entries, nil
}

// Start begins listening on the UNIX socket. Connections are proxied to
// the first allowed entry (for single-destination use like cache.nixos.org).
// Call Stop to shut down.
func (f *Filter) Start() error {
	os.Remove(f.SocketPath)

	ln, err := net.Listen("unix", f.SocketPath)
	if err != nil {
		return fmt.Errorf("listen on %s: %w", f.SocketPath, err)
	}
	if err := os.Chmod(f.SocketPath, 0o700); err != nil {
		ln.Close()
		return fmt.Errorf("chmod socket: %w", err)
	}
	f.listener = ln

	f.wg.Add(1)
	go func() {
		defer f.wg.Done()
		for {
			conn, err := ln.Accept()
			if err != nil {
				return // listener closed
			}
			f.wg.Add(1)
			go func() {
				defer f.wg.Done()
				f.handleConn(conn)
			}()
		}
	}()
	return nil
}

// Stop shuts down the filter and waits for connections to drain.
func (f *Filter) Stop() {
	if f.listener != nil {
		f.listener.Close()
	}
	os.Remove(f.SocketPath)
	f.wg.Wait()
}

func (f *Filter) handleConn(guest net.Conn) {
	defer guest.Close()

	if len(f.Allowed) == 0 {
		return // deny all
	}

	// Route to the first allowed destination.  Currently each guestfwd
	// UNIX socket corresponds to exactly one allowed host:port entry (e.g.
	// port 443 → cache.nixos.org).  If multiple allowed destinations are
	// needed on the same socket, TLS SNI inspection would be required to
	// disambiguate — see TODO below.
	//
	// TODO(multi-dest): implement SNI-based routing when multiple allowed
	// entries share a single guestfwd socket.
	dest := f.Allowed[0]
	addr := net.JoinHostPort(dest.Host, dest.Port)
	fmt.Fprintf(os.Stderr, "netproxy: routing connection to %s\n", addr)
	upstream, err := net.Dial("tcp", addr)
	if err != nil {
		return
	}
	defer upstream.Close()

	// Bidirectional proxy
	done := make(chan struct{}, 2)
	go func() { io.Copy(upstream, guest); done <- struct{}{} }()
	go func() { io.Copy(guest, upstream); done <- struct{}{} }()
	<-done
}
