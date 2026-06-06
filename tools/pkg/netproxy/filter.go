// SPDX-License-Identifier: Apache-2.0
// Package netproxy provides a UNIX socket TCP proxy with domain allowlisting.
//
// Used by the VM builder to restrict network access: QEMU's guestfwd routes
// guest connections through this filter, which only allows connections to
// domains listed in the allowlist file.
package netproxy

import (
	"bufio"
	"bytes"
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
	// DialFn overrides net.Dial for testing. If nil, net.Dial is used.
	DialFn   func(network, addr string) (net.Conn, error)
	listener net.Listener
	wg       sync.WaitGroup
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

// replayConn wraps a net.Conn and prepends already-read bytes back into the
// read stream so that the TLS ClientHello bytes consumed by peekSNI are not
// lost when the connection is handed off to the bidirectional proxy.
type replayConn struct {
	net.Conn
	r io.Reader // io.MultiReader(bytes.NewReader(peeked), conn)
}

func (c *replayConn) Read(b []byte) (int, error) {
	return c.r.Read(b)
}

// peekSNI reads up to 4096 bytes from conn, attempts to parse a TLS
// ClientHello, and returns the SNI hostname (empty string if not TLS or not
// found).  It always returns a replacement conn that replays the peeked bytes
// before the remainder of the stream, so callers must use the returned conn
// instead of the original.
func peekSNI(conn net.Conn) (string, net.Conn, error) {
	buf := make([]byte, 4096)
	n, err := conn.Read(buf)
	if err != nil && n == 0 {
		return "", conn, err
	}

	peeked := &replayConn{
		Conn: conn,
		r:    io.MultiReader(bytes.NewReader(buf[:n]), conn),
	}

	sni := extractSNI(buf[:n])
	return sni, peeked, nil
}

// extractSNI parses a TLS ClientHello record and returns the SNI hostname.
// Returns empty string if buf is not a TLS ClientHello or SNI is not present.
// Offset-out-of-bounds conditions are handled gracefully (return "").
func extractSNI(buf []byte) string {
	// Minimum viable TLS record header: 5 bytes
	if len(buf) < 5 {
		return ""
	}
	// Byte 0: ContentType must be 0x16 (handshake)
	if buf[0] != 0x16 {
		return ""
	}
	// Bytes 3–4: record length
	recordLen := int(buf[3])<<8 | int(buf[4])
	if len(buf) < 5+recordLen {
		return ""
	}

	// Handshake header starts at offset 5
	// Byte 5: HandshakeType must be 0x01 (ClientHello)
	if len(buf) < 6 || buf[5] != 0x01 {
		return ""
	}
	// Bytes 6–8: handshake body length (3-byte big-endian)
	if len(buf) < 9 {
		return ""
	}
	hsBodyLen := int(buf[6])<<16 | int(buf[7])<<8 | int(buf[8])
	if len(buf) < 9+hsBodyLen {
		return ""
	}

	// ClientHello body starts at offset 9
	// Bytes 9–10: client_version (skip)
	// Bytes 11–42: random (32 bytes, skip)
	// Byte 43: session_id length
	off := 9
	off += 2  // client_version
	off += 32 // random
	if off >= len(buf) {
		return ""
	}
	sidLen := int(buf[off])
	off++
	off += sidLen // session_id

	// cipher_suites length (2 bytes)
	if off+2 > len(buf) {
		return ""
	}
	csLen := int(buf[off])<<8 | int(buf[off+1])
	off += 2
	off += csLen // cipher_suites

	// compression_methods length (1 byte)
	if off+1 > len(buf) {
		return ""
	}
	compLen := int(buf[off])
	off++
	off += compLen // compression_methods

	// extensions length (2 bytes)
	if off+2 > len(buf) {
		return ""
	}
	extsLen := int(buf[off])<<8 | int(buf[off+1])
	off += 2

	extsEnd := off + extsLen
	if extsEnd > len(buf) {
		return ""
	}

	// Walk extensions
	for off+4 <= extsEnd {
		extType := uint16(buf[off])<<8 | uint16(buf[off+1])
		extLen := int(buf[off+2])<<8 | int(buf[off+3])
		off += 4
		if off+extLen > extsEnd {
			return ""
		}
		if extType == 0x0000 { // SNI extension
			// data[0–1]: ServerNameList length
			// data[2]: name_type (0x00 = host_name)
			// data[3–4]: name length
			// data[5...]: hostname bytes
			if extLen < 5 {
				return ""
			}
			d := buf[off : off+extLen]
			if d[2] != 0x00 { // not host_name type
				return ""
			}
			nameLen := int(d[3])<<8 | int(d[4])
			if 5+nameLen > len(d) {
				return ""
			}
			return string(d[5 : 5+nameLen])
		}
		off += extLen
	}

	return ""
}

func (f *Filter) handleConn(guest net.Conn) {
	defer guest.Close()

	if len(f.Allowed) == 0 {
		return // deny all
	}

	sni, peeked, err := peekSNI(guest)
	if err != nil {
		return
	}
	guest = peeked

	// Pick destination: SNI match first, fall back to first entry.
	dest := f.Allowed[0]
	if sni != "" {
		for _, e := range f.Allowed {
			if e.Host == sni {
				dest = e
				break
			}
		}
	}

	addr := net.JoinHostPort(dest.Host, dest.Port)
	fmt.Fprintf(os.Stderr, "netproxy: routing connection to %s (SNI=%q)\n", addr, sni)
	dial := net.Dial
	if f.DialFn != nil {
		dial = f.DialFn
	}
	upstream, err := dial("tcp", addr)
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
