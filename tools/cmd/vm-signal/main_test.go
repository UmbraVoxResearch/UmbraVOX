// SPDX-License-Identifier: Apache-2.0
//
// Finding:      M26.3 — Signal-Server admin health endpoint had no automated
//               test coverage; verification was blocked waiting for a live VM.
// Vulnerability: Without a test, regressions in the HTTP health check logic
//               (wrong path, wrong status interpretation, missing retry handling)
//               would only surface during a full VM run.
// Fix:          Mock HTTP server tests verify checkHealthURL behavior for 200 OK,
//               non-200 failure, and connection-refused cases — no VM required.
// Verified:     All three cases pass with the mock server (net/http/httptest).
package main

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"
)

// TestCheckHealthURL_200OK verifies that checkHealthURL returns nil when the
// server responds with HTTP 200 OK (health check passes).
func TestCheckHealthURL_200OK(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/healthcheck" {
			t.Errorf("unexpected path: %s", r.URL.Path)
			http.NotFound(w, r)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		fmt.Fprint(w, `{"status":"ok","deadlocks":0}`)
	}))
	defer srv.Close()

	client := srv.Client()
	if err := checkHealthURL(client, srv.URL+"/healthcheck"); err != nil {
		t.Fatalf("expected nil error for 200 OK, got: %v", err)
	}
}

// TestCheckHealthURL_Non200 verifies that checkHealthURL returns a non-nil
// error when the server responds with a non-200 status (e.g., 503 starting up).
func TestCheckHealthURL_Non200(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusServiceUnavailable)
		fmt.Fprint(w, `{"status":"starting"}`)
	}))
	defer srv.Close()

	client := srv.Client()
	if err := checkHealthURL(client, srv.URL+"/healthcheck"); err == nil {
		t.Fatal("expected non-nil error for 503, got nil")
	}
}

// TestCheckHealthURL_ConnectionRefused verifies that checkHealthURL returns a
// non-nil error when the server is unreachable (connection refused).
func TestCheckHealthURL_ConnectionRefused(t *testing.T) {
	// Use a server that we immediately close to get a connection-refused port.
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
	}))
	addr := srv.URL
	srv.Close() // now the port is closed

	client := &http.Client{}
	if err := checkHealthURL(client, addr+"/healthcheck"); err == nil {
		t.Fatal("expected non-nil error for connection refused, got nil")
	}
}

// TestCheckHealthURL_Path verifies that the correct /healthcheck path is requested.
func TestCheckHealthURL_Path(t *testing.T) {
	var gotPath string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotPath = r.URL.Path
		w.WriteHeader(http.StatusOK)
	}))
	defer srv.Close()

	client := srv.Client()
	_ = checkHealthURL(client, srv.URL+"/healthcheck")

	if gotPath != "/healthcheck" {
		t.Errorf("expected path /healthcheck, got %q", gotPath)
	}
}
