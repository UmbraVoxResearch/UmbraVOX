// SPDX-License-Identifier: Apache-2.0
package netproxy

import (
	"io"
	"net"
	"os"
	"path/filepath"
	"testing"
	"time"
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

// buildClientHello constructs a minimal valid TLS ClientHello record with the
// given SNI hostname.
func buildClientHello(sni string) []byte {
	sniBytes := []byte(sni)
	sniLen := len(sniBytes)

	// SNI extension data: list_len(2) + name_type(1) + name_len(2) + name
	extData := make([]byte, 2+1+2+sniLen)
	extData[0] = byte((sniLen + 3) >> 8)
	extData[1] = byte(sniLen + 3) // list length
	extData[2] = 0x00             // host_name
	extData[3] = byte(sniLen >> 8)
	extData[4] = byte(sniLen)
	copy(extData[5:], sniBytes)

	// Extensions block: type(2) + ext_len(2) + ext_data
	extsLen := 4 + len(extData)
	exts := make([]byte, extsLen)
	exts[0] = 0x00
	exts[1] = 0x00 // SNI type
	exts[2] = byte(len(extData) >> 8)
	exts[3] = byte(len(extData))
	copy(exts[4:], extData)

	// ClientHello body
	body := []byte{0x03, 0x03}          // TLS 1.2 version
	body = append(body, make([]byte, 32)...) // random
	body = append(body, 0x00)                // session_id length
	body = append(body, 0x00, 0x02)         // cipher_suites length
	body = append(body, 0xC0, 0x2B)         // TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256
	body = append(body, 0x01, 0x00)         // compression: length 1, no compression

	// extensions total length + extensions
	extsTotal := len(exts)
	body = append(body, byte(extsTotal>>8), byte(extsTotal))
	body = append(body, exts...)

	// Handshake header: HandshakeType(1) + length(3)
	handshakeLen := len(body)
	handshake := []byte{
		0x01,
		byte(handshakeLen >> 16), byte(handshakeLen >> 8), byte(handshakeLen),
	}
	handshake = append(handshake, body...)

	// TLS record: ContentType(1) + version(2) + length(2) + handshake
	record := []byte{0x16, 0x03, 0x01,
		byte(len(handshake) >> 8), byte(len(handshake)),
	}
	return append(record, handshake...)
}

func TestPeekSNI_ExtractsSNI(t *testing.T) {
	hello := buildClientHello("cache.nixos.org")

	// Use net.Pipe to create a synchronous in-memory connection pair.
	client, server := net.Pipe()
	defer client.Close()
	defer server.Close()

	// Write the ClientHello from the client side, then close writing.
	go func() {
		client.Write(hello)
		client.Close()
	}()

	sni, peeked, err := peekSNI(server)
	if err != nil {
		t.Fatalf("peekSNI returned error: %v", err)
	}
	if sni != "cache.nixos.org" {
		t.Errorf("expected SNI %q, got %q", "cache.nixos.org", sni)
	}

	// The replay conn must return all original bytes.
	got, err := io.ReadAll(peeked)
	if err != nil {
		t.Fatalf("ReadAll from peeked conn: %v", err)
	}
	if string(got) != string(hello) {
		t.Errorf("replay bytes differ: got %d bytes, want %d bytes", len(got), len(hello))
	}
}

func TestPeekSNI_NonTLS(t *testing.T) {
	plaintext := []byte("GET / HTTP/1.1\r\n")

	client, server := net.Pipe()
	defer client.Close()
	defer server.Close()

	go func() {
		client.Write(plaintext)
		client.Close()
	}()

	sni, peeked, err := peekSNI(server)
	if err != nil {
		t.Fatalf("peekSNI returned error: %v", err)
	}
	if sni != "" {
		t.Errorf("expected empty SNI for non-TLS, got %q", sni)
	}

	got, err := io.ReadAll(peeked)
	if err != nil {
		t.Fatalf("ReadAll from peeked conn: %v", err)
	}
	if string(got) != string(plaintext) {
		t.Errorf("replay bytes differ: got %q, want %q", got, plaintext)
	}
}

func TestHandleConn_SNIRouting(t *testing.T) {
	// Start two loopback TCP servers.
	ln1, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen server1: %v", err)
	}
	defer ln1.Close()

	ln2, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen server2: %v", err)
	}
	defer ln2.Close()

	addr1 := ln1.Addr().String()
	addr2 := ln2.Addr().String()

	// Filter: first entry is the fallback, second matches SNI "second.example".
	// DialFn redirects logical host:port to the real loopback listeners so we
	// don't need DNS resolution of the test hostnames.
	filter := &Filter{
		Allowed: []AllowEntry{
			{Host: "first.example", Port: "4001"},
			{Host: "second.example", Port: "4002"},
		},
		DialFn: func(network, addr string) (net.Conn, error) {
			switch addr {
			case "first.example:4001":
				return net.Dial(network, addr1)
			case "second.example:4002":
				return net.Dial(network, addr2)
			default:
				return net.Dial(network, addr)
			}
		},
	}

	// Channel to record which server got a connection.
	got2 := make(chan struct{}, 1)
	go func() {
		conn, err := ln2.Accept()
		if err != nil {
			return
		}
		conn.Close()
		got2 <- struct{}{}
	}()
	// Drain ln1 in case routing incorrectly falls through there.
	go func() {
		conn, _ := ln1.Accept()
		if conn != nil {
			conn.Close()
		}
	}()

	// Build a ClientHello for "second.example".
	hello := buildClientHello("second.example")

	// Create an in-memory connection pair simulating guest→filter.
	guestSide, filterSide := net.Pipe()

	// Run handleConn in background.
	go filter.handleConn(filterSide)

	// Send the ClientHello then close the guest side.
	if _, err := guestSide.Write(hello); err != nil {
		t.Fatalf("write ClientHello: %v", err)
	}
	guestSide.Close()

	// Wait for the second server to accept, with timeout.
	select {
	case <-got2:
		// SNI routing worked — connection arrived at server 2.
	case <-time.After(3 * time.Second):
		t.Fatal("timeout: SNI routing did not reach second server")
	}
}

// itoa converts an int to a decimal string (avoids importing strconv).
func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	neg := false
	if n < 0 {
		neg = true
		n = -n
	}
	var buf [20]byte
	pos := len(buf)
	for n > 0 {
		pos--
		buf[pos] = byte('0' + n%10)
		n /= 10
	}
	if neg {
		pos--
		buf[pos] = '-'
	}
	return string(buf[pos:])
}
