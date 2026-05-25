// SPDX-License-Identifier: Apache-2.0

package vmctl

import (
	"bufio"
	"os"
	"runtime"
	"strconv"
	"strings"
)

// ReadHostMemoryMB reads total host memory from /proc/meminfo.
// Returns 8192 as a fallback on non-Linux or read failure.
func ReadHostMemoryMB() int {
	f, err := os.Open("/proc/meminfo")
	if err != nil {
		return 8192
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "MemTotal:") {
			fields := strings.Fields(line)
			if len(fields) >= 2 {
				kb, err := strconv.Atoi(fields[1])
				if err == nil {
					return kb / 1024
				}
			}
		}
	}
	return 8192
}

// ResolveResources fills in zero-valued fields in r based on host resources.
// If Fraction is zero it defaults to 50 (percent). Cores and MemoryMB are
// clamped to MinCores/MinMemMB floors (which themselves default to 1/256).
func ResolveResources(r Resources) Resources {
	frac := r.Fraction
	if frac <= 0 {
		frac = 50
	}
	if frac > 100 {
		frac = 100
	}

	hostCores := runtime.NumCPU()
	hostMem := ReadHostMemoryMB()

	out := r

	if out.Cores <= 0 {
		out.Cores = hostCores * frac / 100
	}
	if out.MemoryMB <= 0 {
		out.MemoryMB = hostMem * frac / 100
	}

	// Apply caller-specified floors (with sane absolute minimums).
	minC := out.MinCores
	if minC <= 0 {
		minC = 1
	}
	minM := out.MinMemMB
	if minM <= 0 {
		minM = 256
	}
	if out.Cores < minC {
		out.Cores = minC
	}
	if out.MemoryMB < minM {
		out.MemoryMB = minM
	}

	out.Fraction = frac
	return out
}
