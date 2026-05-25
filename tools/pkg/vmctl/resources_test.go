// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"runtime"
	"testing"
)

// hostCores and hostMem capture the actual host values so tests can derive
// expected outputs without hard-coding machine-specific numbers.
var (
	hostCores = runtime.NumCPU()
	hostMem   = ReadHostMemoryMB()
)

func TestResolveResources_FractionScaling(t *testing.T) {
	r := Resources{Fraction: 25}
	got := ResolveResources(r)

	wantCores := hostCores * 25 / 100
	wantMem := hostMem * 25 / 100

	// Apply the absolute minimums that ResolveResources itself enforces.
	if wantCores < 1 {
		wantCores = 1
	}
	if wantMem < 256 {
		wantMem = 256
	}

	if got.Cores != wantCores {
		t.Errorf("Cores: got %d, want %d (hostCores=%d, fraction=25)", got.Cores, wantCores, hostCores)
	}
	if got.MemoryMB != wantMem {
		t.Errorf("MemoryMB: got %d, want %d (hostMem=%d, fraction=25)", got.MemoryMB, wantMem, hostMem)
	}
	if got.Fraction != 25 {
		t.Errorf("Fraction: got %d, want 25", got.Fraction)
	}
}

func TestResolveResources_DefaultFraction(t *testing.T) {
	// Zero fraction should default to 50.
	r := Resources{}
	got := ResolveResources(r)

	if got.Fraction != 50 {
		t.Errorf("Fraction: got %d, want 50 (default)", got.Fraction)
	}

	wantCores := hostCores * 50 / 100
	if wantCores < 1 {
		wantCores = 1
	}
	if got.Cores != wantCores {
		t.Errorf("Cores: got %d, want %d with default fraction", got.Cores, wantCores)
	}
}

func TestResolveResources_FractionClamped(t *testing.T) {
	// Fraction > 100 should be clamped to 100.
	r := Resources{Fraction: 200}
	got := ResolveResources(r)
	if got.Fraction != 100 {
		t.Errorf("Fraction: got %d, want 100 (clamped from 200)", got.Fraction)
	}

	// Cores should equal full hostCores (fraction=100).
	wantCores := hostCores * 100 / 100
	if wantCores < 1 {
		wantCores = 1
	}
	if got.Cores != wantCores {
		t.Errorf("Cores: got %d, want %d (fraction clamped to 100)", got.Cores, wantCores)
	}
}

func TestResolveResources_MinCoresFloor(t *testing.T) {
	// Force a very small fraction so scaled cores would normally be zero;
	// MinCores must win.
	r := Resources{
		Fraction: 1,
		MinCores: 4,
	}
	got := ResolveResources(r)

	if got.Cores < 4 {
		t.Errorf("Cores: got %d, want >= 4 (MinCores floor)", got.Cores)
	}
}

func TestResolveResources_MinMemFloor(t *testing.T) {
	// Request an absurdly small fraction; MinMemMB must override.
	r := Resources{
		Fraction: 1,
		MinMemMB: 2048,
	}
	got := ResolveResources(r)

	if got.MemoryMB < 2048 {
		t.Errorf("MemoryMB: got %d, want >= 2048 (MinMemMB floor)", got.MemoryMB)
	}
}

func TestResolveResources_AbsoluteMinimumFloors(t *testing.T) {
	// Even with no minimums specified, the absolute floors (1 core, 256 MB)
	// must apply when scaling would produce zero.
	r := Resources{Fraction: 1}
	got := ResolveResources(r)

	if got.Cores < 1 {
		t.Errorf("Cores: got %d, must be >= 1 (absolute floor)", got.Cores)
	}
	if got.MemoryMB < 256 {
		t.Errorf("MemoryMB: got %d, must be >= 256 (absolute floor)", got.MemoryMB)
	}
}

func TestResolveResources_ExplicitCoresOverrideFraction(t *testing.T) {
	// When Cores is set explicitly it must not be overwritten by fraction scaling.
	r := Resources{
		Cores:    3,
		Fraction: 10,
	}
	got := ResolveResources(r)

	if got.Cores != 3 {
		t.Errorf("Cores: got %d, want 3 (explicit override)", got.Cores)
	}
}

func TestResolveResources_ExplicitMemoryOverrideFraction(t *testing.T) {
	// When MemoryMB is set explicitly it must not be overwritten by fraction scaling.
	r := Resources{
		MemoryMB: 1024,
		Fraction: 10,
	}
	got := ResolveResources(r)

	if got.MemoryMB != 1024 {
		t.Errorf("MemoryMB: got %d, want 1024 (explicit override)", got.MemoryMB)
	}
}

func TestResolveResources_ExplicitCoresBelowMinEnforcesFloor(t *testing.T) {
	// An explicit Cores value below MinCores must still be raised to the floor.
	r := Resources{
		Cores:    1,
		MinCores: 4,
	}
	got := ResolveResources(r)

	if got.Cores < 4 {
		t.Errorf("Cores: got %d, want >= 4 (explicit below MinCores)", got.Cores)
	}
}

func TestResolveResources_ExplicitMemoryBelowMinEnforcesFloor(t *testing.T) {
	// An explicit MemoryMB value below MinMemMB must still be raised to the floor.
	r := Resources{
		MemoryMB: 128,
		MinMemMB: 512,
	}
	got := ResolveResources(r)

	if got.MemoryMB < 512 {
		t.Errorf("MemoryMB: got %d, want >= 512 (explicit below MinMemMB)", got.MemoryMB)
	}
}
