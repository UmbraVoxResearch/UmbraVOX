// SPDX-License-Identifier: Apache-2.0
package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/disk"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/log"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/netpol"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/ninep"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/qemu"
	"github.com/UmbraVoxResearch/UmbraVOX/tools/pkg/repo"
	"github.com/UmbraVoxResearch/vmctl"
)

const tag = "UV"

// runExec handles: uv exec -- <command>
func runExec(args []string) int {
	// Skip optional "--" separator
	if len(args) > 0 && args[0] == "--" {
		args = args[1:]
	}
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: uv exec -- <command> [args...]")
		return 2
	}
	cmd := strings.Join(args, " ")
	return execInVM(cmd, qemu.ProfileDev, 30*time.Minute)
}

// prepareVMDisks creates the source disk, COW overlay, and cache disk needed
// to boot the dev VM. The caller must invoke cleanup() when done.
func prepareVMDisks(repoRoot, cmd string) (srcDisk, overlayPath, cacheDisk, outputDir string, cleanup func(), err error) {
	vmCacheDir := filepath.Join(repoRoot, "build", "vm")
	vmImagePath := filepath.Join(vmCacheDir, "image")

	if pfErr := repo.Preflight(vmImagePath, true); pfErr != nil {
		err = pfErr
		return
	}

	diskImg, evalErr := filepath.EvalSymlinks(filepath.Join(vmImagePath, "nixos.img"))
	if evalErr != nil {
		err = fmt.Errorf("cannot resolve nixos.img: %w", evalErr)
		return
	}

	// Create source disk with init script + exec command
	tmpDir := filepath.Join(vmCacheDir, "tmp")
	initScript := generateInitScript("exec", cmd)
	srcDisk, err = disk.CreateSourceDisk(repoRoot, tmpDir, initScript, cmd)
	if err != nil {
		err = fmt.Errorf("failed to create source disk: %w", err)
		return
	}

	// COW overlay
	log.Info(tag, "Creating COW overlay...")
	overlayDir := filepath.Join(vmCacheDir, "tmp")
	if mkErr := os.MkdirAll(overlayDir, 0o755); mkErr != nil {
		os.Remove(srcDisk)
		err = fmt.Errorf("failed to create overlay dir: %w", mkErr)
		return
	}
	overlay, overlayErr := disk.CreateOverlay(diskImg, overlayDir)
	if overlayErr != nil {
		os.Remove(srcDisk)
		err = fmt.Errorf("failed to create overlay: %w", overlayErr)
		return
	}
	overlayPath = overlay.Path

	// Persistent build cache
	cacheDisk = filepath.Join(vmCacheDir, "build-cache.qcow2")
	if cacheErr := disk.EnsureCacheDisk(cacheDisk, "4G"); cacheErr != nil {
		os.Remove(srcDisk)
		overlay.Remove()
		err = fmt.Errorf("failed to create cache disk: %w", cacheErr)
		return
	}

	// Output directory (9p share)
	outputDir = filepath.Join(repoRoot, "build", "vm-output")
	if mkErr := os.MkdirAll(outputDir, 0o755); mkErr != nil {
		os.Remove(srcDisk)
		overlay.Remove()
		err = fmt.Errorf("failed to create output dir: %w", mkErr)
		return
	}
	vmStatusFile := filepath.Join(outputDir, "vm-exec-status")
	os.Remove(vmStatusFile)

	cleanup = func() {
		os.Remove(srcDisk)
		overlay.Remove()
	}
	return
}

// profileToResources converts a qemu.VMProfile to vmctl.Resources,
// preserving the same fraction/floor semantics used by qemu.ScaleToHost.
func profileToResources(p qemu.VMProfile) vmctl.Resources {
	var frac int
	switch p {
	case qemu.ProfileBuild:
		frac = 75
	case qemu.ProfileRuntime:
		frac = 25
	default: // ProfileDev
		frac = 50
	}
	return vmctl.Resources{
		Fraction: frac,
		MinCores: 2,
		MinMemMB: 2048,
	}
}

// execInVM boots the dev VM via vmctl, runs cmd inside it, and returns the
// guest exit code. This is the primary boot path used by all callers.
//
// Resource fractions, network mode, hypervisor selection, and boot timeout
// defaults are loaded from vm-defs/dev.yaml when present.  If the file does
// not exist the function falls back to the hardcoded profile values so that
// repos without a vm-defs/ directory still work.  The caller-supplied timeout
// always overrides the YAML value (0 means "use YAML default").
func execInVM(cmd string, profile qemu.VMProfile, timeout time.Duration) int {
	repoRoot, err := repo.Root()
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}

	srcDisk, overlayPath, cacheDisk, outputDir, cleanup, err := prepareVMDisks(repoRoot, cmd)
	if err != nil {
		log.Fail(tag, err.Error())
		return 1
	}
	defer cleanup()

	// Parse network policy (same as buildExecQEMUConfig).
	policyFile := filepath.Join(repoRoot, "conf/vm-network-policy.conf")
	policy, err := netpol.ParseFile(policyFile)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("failed to parse network policy: %v", err))
		return 1
	}
	netArgs, err := policy.QEMUNetArgs()
	if err != nil {
		log.Fail(tag, fmt.Sprintf("network policy error: %v", err))
		return 1
	}

	outputShare := ninep.DefaultOutputShare(outputDir)
	vmStatusFile := filepath.Join(outputDir, "vm-exec-status")

	// Start with defaults derived from the profile (backward-compat fallback).
	hypervisor := vmctl.HypervisorQEMU
	resources := profileToResources(profile)
	noReboot := true
	effectiveTimeout := timeout

	// Attempt to load vm-defs/dev.yaml and overlay its values.
	if def, defErr := loadVMDef(repoRoot, "dev"); defErr != nil {
		log.Fail(tag, fmt.Sprintf("failed to load vm-def: %v", defErr))
		return 1
	} else if def != nil {
		hypervisor = def.Hypervisor
		resources = def.Resources
		noReboot = def.NoReboot
		// Use the YAML timeout only when the caller did not specify one.
		if timeout == 0 && def.Timeout > 0 {
			effectiveTimeout = def.Timeout
		}
	}

	spec := &vmctl.VMSpec{
		Hypervisor: hypervisor,
		Resources:  resources,
		BaseImage: vmctl.ImageRef{
			Path:   overlayPath,
			Format: vmctl.DiskFormatQCOW2,
		},
		Disks: []vmctl.DiskSpec{
			{Path: srcDisk, Format: vmctl.DiskFormatRaw, ReadOnly: true, Interface: "virtio"},
			{Path: cacheDisk, Format: vmctl.DiskFormatQCOW2, Interface: "virtio"},
		},
		Shares: []vmctl.ShareSpec{{
			HostPath:      outputShare.LocalPath,
			MountTag:      outputShare.MountTag,
			SecurityModel: string(outputShare.SecurityModel),
			ID:            outputShare.ID,
		}},
		Network: vmctl.NetworkSpec{
			RawArgs: netArgs,
		},
		Display:    vmctl.DisplayNone,
		NoReboot:   noReboot,
		Timeout:    effectiveTimeout,
		StatusFile: vmStatusFile,
	}

	log.Info(tag, fmt.Sprintf("exec: %s", cmd))
	fmt.Fprintln(os.Stderr)

	ctx := context.Background()
	if effectiveTimeout > 0 {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, effectiveTimeout)
		defer cancel()
	}

	hyp := &vmctl.QEMUHypervisor{Logger: nil}
	tmpDir := filepath.Join(repoRoot, "build", "vm", "tmp")
	result, err := hyp.Boot(ctx, spec, tmpDir)
	if err != nil {
		log.Fail(tag, fmt.Sprintf("vmctl boot failed: %v", err))
		return 1
	}

	if result.ExitCode == 0 {
		log.OK(tag, "Guest command completed successfully.")
	} else {
		log.Fail(tag, fmt.Sprintf("Guest command failed with exit %d.", result.ExitCode))
	}
	return result.ExitCode
}

// generateInitScript produces the in-guest init script content.
// If the cross-compiled vm-init binary is present on the source disk,
// the script delegates to it; otherwise it falls back to the full
// shell-based implementation.
func generateInitScript(mode, cmd string) string {
	var b strings.Builder

	b.WriteString(`#!/usr/bin/env bash
set -euo pipefail

export HOME=/root
export PATH="/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH"

# Mount source disk early so we can check for the Go binary.
mkdir -p /mnt/src
if ! mountpoint -q /mnt/src 2>/dev/null; then
    mount -o ro /dev/vdb /mnt/src 2>/dev/null || true
fi

# Prefer the cross-compiled Go binary when available.
if [ -x /mnt/src/tools/bin/vm-init ]; then
    exec /mnt/src/tools/bin/vm-init "` + mode + `"
fi

# --- fallback: inline shell implementation ---

# Offline cabal config
mkdir -p /root/.cabal
cat > /root/.cabal/config << 'CABALEOF'
offline: True
nix: False
CABALEOF

# Mount shared output directory (host <-> guest via 9p)
mkdir -p /output
mount -t 9p -o trans=virtio,version=9p2000.L output /output 2>/dev/null || true

# Mount persistent build cache (vdc)
mkdir -p /cache
if [ -b /dev/vdc ]; then
    if ! blkid /dev/vdc >/dev/null 2>&1; then
        mkfs.ext4 -q /dev/vdc
    fi
    mount /dev/vdc /cache 2>/dev/null || true
    mkdir -p /cache/dist-newstyle /cache/.cabal-store
fi

# Copy to writable workspace
if [ ! -f /work/umbravox/.vm-source-ready ]; then
    mkdir -p /work/umbravox
    cp -a /mnt/src/. /work/umbravox/
    touch /work/umbravox/.vm-source-ready
fi

# Link build cache
if [ -d /cache/dist-newstyle ]; then
    rm -rf /work/umbravox/dist-newstyle
    ln -s /cache/dist-newstyle /work/umbravox/dist-newstyle
fi
if [ -d /cache/.cabal-store ]; then
    mkdir -p /root/.cabal
    ln -sf /cache/.cabal-store /root/.cabal/store
fi

cd /work/umbravox
export UMBRAVOX_ROOT=/work/umbravox
export UMBRAVOX_VM=1
export PATH="/work/umbravox/scripts:$PATH"
unset LD_LIBRARY_PATH 2>/dev/null || true

# Set FSTAR_HOME
FSTAR_EXE_PATH=$(command -v fstar.exe 2>/dev/null || true)
if [ -n "$FSTAR_EXE_PATH" ]; then
    FSTAR_REAL=$(readlink -f "$FSTAR_EXE_PATH")
    export FSTAR_HOME=$(dirname "$(dirname "$FSTAR_REAL")")
fi

# Set COQPATH
if [ -d /run/current-system/sw/lib/coq ]; then
    COQPATH_DIRS=""
    for d in /run/current-system/sw/lib/coq/*/user-contrib; do
        [ -d "$d" ] && COQPATH_DIRS="$COQPATH_DIRS:$d"
    done
    [ -n "$COQPATH_DIRS" ] && export COQPATH="${COQPATH_DIRS#:}"
fi
`)

	switch {
	case mode == "interactive" || mode == "gui":
		b.WriteString(`
echo ""
echo -e "\033[35m  ╦ ╦╔╦╗╔╗ ╦═╗╔═╗╦  ╦╔═╗═╗ ╦\033[0m"
echo -e "\033[35m  ║ ║║║║╠╩╗╠╦╝╠═╣╚╗╔╝║ ║╔╩╦╝\033[0m"
echo -e "\033[35m  ╚═╝╩ ╩╚═╝╩╚═╩ ╩ ╚╝ ╚═╝╩ ╚═\033[0m"
echo -e "\033[33m  Post-Quantum Encrypted Messaging\033[0m"
echo ""
echo -e "  \033[1;32m[ VM DEVELOPMENT SHELL ]\033[0m  \033[90m(NixOS QEMU Guest)\033[0m"
echo ""
echo "  Source: /work/umbravox"
echo "  Kernel: $(uname -r)"
echo "  GHC:    $(ghc --numeric-version 2>/dev/null || echo N/A)"
echo "  Cabal:  $(cabal --numeric-version 2>/dev/null || echo N/A)"
echo "  F*:     $(fstar.exe --version 2>/dev/null | head -1 | sed 's/F\* //' || echo N/A)"
echo "  Z3:     $(z3 --version 2>/dev/null | sed 's/Z3 version //' || echo N/A)"
echo ""
echo "  Commands:"
echo "    cabal build all                Build everything"
echo "    cabal test umbravox-test       Run tests"
echo "    exit / Ctrl-D                  Shut down VM"
echo ""
`)
		if mode == "gui" {
			b.WriteString(`cat > /etc/profile.d/umbravox-dev.sh << 'PROFILEEOF'
export HOME=/root
export UMBRAVOX_ROOT=/work/umbravox
export PATH="/work/umbravox/scripts:/run/current-system/sw/bin:/run/current-system/sw/sbin:$PATH"
unset LD_LIBRARY_PATH 2>/dev/null || true
cd /work/umbravox 2>/dev/null || true
PROFILEEOF
echo "  GUI mode: log in on the VGA console (root, no password)."
echo ""
`)
		} else {
			b.WriteString("exec /bin/bash --login\n")
		}

	default: // exec mode
		b.WriteString(`
VM_EXEC_CMD="$(cat /mnt/src/.vm-exec-cmd 2>/dev/null || true)"
echo ""
echo "========================================"
echo "  UmbraVOX VM: $VM_EXEC_CMD"
echo "========================================"
echo ""

set +e
# eval is intentional: the command originates from the host CLI (not guest
# input) and may contain shell features like pipes and redirects.  The VM
# is sandboxed by QEMU with no network access, so this does not expand
# the trust boundary.
#
# Tee the full output to the output share.  The serial console is fed through
# systemd's journal, which rate-limits under burst and silently DROPS lines
# (e.g. a wall of GHC diagnostics), so a failure's real error can vanish from
# the console log.  The /output/vm-exec.log copy is written straight to the 9p
# share, so it is complete and rate-limit-immune (host: build/vm-output/vm-exec.log).
# pipefail + PIPESTATUS preserves the command's exit status across the pipe.
set -o pipefail
if [ -d /output ]; then
    eval "$VM_EXEC_CMD" 2>&1 | tee /output/vm-exec.log
else
    eval "$VM_EXEC_CMD"
fi
STATUS=${PIPESTATUS[0]}
set +o pipefail
set -e

if [ -d /output ]; then
    printf "%s\n" "$STATUS" > /output/vm-exec-status 2>/dev/null || true
    sync 2>/dev/null || true
fi

echo ""
if [ $STATUS -eq 0 ]; then
    echo "VM_RESULT=PASS"
else
    echo "VM_RESULT=FAIL (exit $STATUS)"
fi

systemctl poweroff || true
exit $STATUS
`)
	}

	return b.String()
}
