// SPDX-License-Identifier: Apache-2.0

package vmctl

import "testing"

func TestNetworkMode_String(t *testing.T) {
	cases := []struct {
		mode NetworkMode
		want string
	}{
		{NetworkNone, "none"},
		{NetworkUserMode, "user"},
		{NetworkTAP, "tap"},
		{NetworkSlirp, "slirp"},
	}

	for _, tc := range cases {
		got := tc.mode.String()
		if got != tc.want {
			t.Errorf("NetworkMode(%d).String(): got %q, want %q", tc.mode, got, tc.want)
		}
	}
}

func TestNetworkMode_String_Unknown(t *testing.T) {
	// A value outside the defined iota range must return "unknown".
	unknown := NetworkMode(999)
	got := unknown.String()
	if got != "unknown" {
		t.Errorf("NetworkMode(999).String(): got %q, want %q", got, "unknown")
	}
}

func TestNetworkMode_QEMUNetArgs(t *testing.T) {
	cases := []struct {
		mode NetworkMode
		want string
	}{
		{NetworkUserMode, "-nic user,model=virtio"},
		{NetworkTAP, ""},           // caller supplies raw args
		{NetworkNone, "-nic none"}, // explicit none
		{NetworkSlirp, "-nic none"},
	}

	for _, tc := range cases {
		got := tc.mode.QEMUNetArgs()
		if got != tc.want {
			t.Errorf("NetworkMode(%d).QEMUNetArgs(): got %q, want %q", tc.mode, got, tc.want)
		}
	}
}

func TestNetworkMode_QEMUNetArgs_Unknown(t *testing.T) {
	// An undefined mode must fall through to the default "-nic none".
	unknown := NetworkMode(999)
	got := unknown.QEMUNetArgs()
	if got != "-nic none" {
		t.Errorf("NetworkMode(999).QEMUNetArgs(): got %q, want %q", got, "-nic none")
	}
}

func TestNetworkMode_QEMUNetArgs_ZeroValue(t *testing.T) {
	// The zero value of NetworkMode is NetworkNone (iota starts at 0).
	var m NetworkMode
	got := m.QEMUNetArgs()
	if got != "-nic none" {
		t.Errorf("zero-value NetworkMode.QEMUNetArgs(): got %q, want %q", got, "-nic none")
	}
	if s := m.String(); s != "none" {
		t.Errorf("zero-value NetworkMode.String(): got %q, want %q", s, "none")
	}
}

func TestLoadSpec_NetworkModes(t *testing.T) {
	cases := []struct {
		raw  string
		want NetworkMode
	}{
		{"user", NetworkUserMode},
		{"tap", NetworkTAP},
		{"none", NetworkNone},
		{"", NetworkNone}, // absent defaults to none
	}

	for _, tc := range cases {
		yaml := "name: net-test\n"
		if tc.raw != "" {
			yaml += "network:\n  mode: " + tc.raw + "\n"
		}
		path := writeTemp(t, yaml)
		_, spec, err := LoadSpec(path)
		if err != nil {
			t.Errorf("network.mode=%q: unexpected error: %v", tc.raw, err)
			continue
		}
		if spec.Network.Mode != tc.want {
			t.Errorf("network.mode=%q: got %v, want %v", tc.raw, spec.Network.Mode, tc.want)
		}
	}
}

func TestLoadSpec_UnknownNetworkModeReturnsError(t *testing.T) {
	yaml := "name: bad-net\nnetwork:\n  mode: bridge\n"
	path := writeTemp(t, yaml)
	_, _, err := LoadSpec(path)
	if err == nil {
		t.Fatal("expected error for unknown network mode, got nil")
	}
}
