# vmctl

`vmctl` is a reusable Go package for unified VM management. It abstracts QEMU/KVM,
Firecracker microVMs, and direct `nix-shell` execution behind a single interface,
handles disk lifecycle, scales resources from host capacity, and wraps `nix-build`
for NixOS image management.

Module path: `github.com/UmbraVoxResearch/vmctl`

---

## Overview

vmctl separates the concerns of *what to run* (a `VMSpec`) from *how to run it*
(a `Hypervisor`). Callers build a spec, hand it to `Boot`, and get back a `Result`
with the guest exit code. The package has no knowledge of any specific project.

Key capabilities:

- Single `Boot()` entry point; hypervisor is a field on the spec
- Fraction-based resource scaling against live host CPU and memory
- Disk management: COW qcow2 overlays, ext2 app disks, persistent cache disks
- Host-guest filesystem sharing via 9p/virtfs (QEMU only)
- NixOS image building and cache detection via `NixBuild`
- Preflight checks before any VM is launched

---

## Quick Start

```go
package main

import (
    "context"
    "log"

    "github.com/UmbraVoxResearch/vmctl"
)

func main() {
    spec := &vmctl.VMSpec{
        Hypervisor: vmctl.HypervisorQEMU,
        Resources: vmctl.Resources{
            Fraction: 50, // use 50% of host CPU and RAM
        },
        BaseImage: vmctl.ImageRef{
            Path:   "/path/to/nixos.img",
            Format: vmctl.DiskFormatRaw,
        },
        Network: vmctl.NetworkSpec{Mode: vmctl.NetworkUserMode},
        NoReboot: true,
    }

    result, err := vmctl.Boot(context.Background(), spec, "/tmp/vmwork")
    if err != nil {
        log.Fatal(err)
    }
    log.Printf("guest exited with code %d", result.ExitCode)
}
```

`Boot` runs preflight checks, launches the hypervisor, blocks until the VM exits
or the context is cancelled, and returns a `Result`. The caller is responsible for
creating the `tmpDir` argument; vmctl writes ephemeral files (overlays, Firecracker
JSON config) there.

---

## Hypervisors

### QEMU (`HypervisorQEMU`)

Full-system emulation via `qemu-system-x86_64` with KVM acceleration.

- Machine type: `q35,accel=kvm`; CPU model: `max`
- Supports all features: 9p shares, GTK display, qcow2 overlays
- Disk-boot (default) or direct-kernel boot
- Network: user-mode SLIRP, TAP bridging, or none
- Headless by default (`-nographic -nodefaults -serial stdio`)

**Requirements:** `qemu-system-x86_64` on PATH, `/dev/kvm` present.

```go
spec := &vmctl.VMSpec{
    Hypervisor: vmctl.HypervisorQEMU,
    Resources:  vmctl.Resources{Fraction: 75},
    BaseImage:  vmctl.ImageRef{Path: "nixos.img", Format: vmctl.DiskFormatRaw},
    Display:    vmctl.DisplayGTK, // open a GTK window
    Network:    vmctl.NetworkSpec{Mode: vmctl.NetworkUserMode},
}
```

### Firecracker (`HypervisorFirecracker`)

Lightweight microVM via the `firecracker` binary. Always uses direct-kernel boot;
`spec.Boot` is required.

- Does not support 9p shares, GUI display, or qcow2 overlays
- Rootfs must be a raw image; `.zst`-compressed images are decompressed automatically
- A writable copy of the rootfs is created in `tmpDir` (Nix store paths are read-only)
- Up to one additional drive is attached as `/dev/vdb` (first entry in `spec.Disks`)
- `console=ttyS0` is injected into kernel args if not already present

**Requirements:** `firecracker` on PATH, `/dev/kvm` present.

```go
spec := &vmctl.VMSpec{
    Hypervisor: vmctl.HypervisorFirecracker,
    Resources:  vmctl.Resources{Fraction: 50},
    BaseImage:  vmctl.ImageRef{Path: "rootfs.img", Format: vmctl.DiskFormatRaw},
    Boot: &vmctl.BootSpec{
        KernelPath: "/path/to/vmlinux",
        KernelArgs: "ro quiet",
    },
    Network: vmctl.NetworkSpec{Mode: vmctl.NetworkSlirp},
    NoReboot: true,
}
```

### Direct (`HypervisorDirect`)

No VM is booted. Runs a command on the host inside `nix-shell --pure`. Intended for
CI environments that lack KVM, or for testing without a full VM.

- `spec.Boot.Command` is passed to `nix-shell --run`
- `spec.Boot.ShellNix` must point to a `shell.nix` file
- Kernel fields in `BootSpec` are ignored
- No disk, share, display, or network configuration is used

**Requirements:** `nix-shell` on PATH or at `/nix/var/nix/profiles/default/bin/nix-shell`.

```go
spec := &vmctl.VMSpec{
    Hypervisor: vmctl.HypervisorDirect,
    Boot: &vmctl.BootSpec{
        ShellNix: "/path/to/repo/shell.nix",
        Command:  "make test",
    },
}
```

---

## VM Definition YAML

Instead of building a `VMSpec` in code, you can load one from a YAML file with
`LoadSpec` or a directory of files with `LoadSpecDir`.

```yaml
name: integration
tier: ci

hypervisor: qemu   # qemu | firecracker | direct

resources:
  fraction:   50   # percent of host resources (default 50)
  min_cores:  2    # floor applied after fraction scaling
  min_memory: 512  # floor in MB applied after fraction scaling

network:
  mode: user       # user | tap | none

boot:
  timeout:  10m    # Go duration string; 0 means unlimited
  no_reboot: true

display: none      # none | gtk
```

**Field reference:**

| Field | Type | Values | Notes |
|---|---|---|---|
| `name` | string | any | Required. Returned by `LoadSpec`. |
| `tier` | string | any | Informational label; not used by vmctl. |
| `hypervisor` | string | `qemu`, `firecracker`, `direct` | Default: `direct`. |
| `resources.fraction` | int | 1–100 | Default: 50. |
| `resources.min_cores` | int | >= 0 | Absolute floor after scaling. |
| `resources.min_memory` | int | MB >= 0 | Absolute floor after scaling. |
| `network.mode` | string | `user`, `tap`, `none` | Default: `none`. |
| `boot.timeout` | string | Go duration | e.g. `5m`, `1h30m`. |
| `boot.no_reboot` | bool | `true`/`false` | Halt instead of reboot. |
| `display` | string | `none`, `gtk` | Default: `none`. |

```go
vmName, spec, err := vmctl.LoadSpec("vms/integration.yaml")
if err != nil {
    log.Fatal(err)
}
// Populate fields the YAML format cannot express:
spec.BaseImage = vmctl.ImageRef{Path: imagePath, Format: vmctl.DiskFormatRaw}
spec.StatusFile = filepath.Join(outputDir, "exit-code")

result, err := vmctl.Boot(ctx, spec, tmpDir)
```

Loading a directory returns `map[string]*VMSpec` keyed by the `name` field:

```go
specs, err := vmctl.LoadSpecDir("vms/")
spec := specs["integration"]
```

---

## Resource Scaling

`ResolveResources` computes final CPU and memory values from a `Resources` struct.
The calculation is:

```
resolved_cores  = host_cores  * fraction / 100
resolved_mem_mb = host_mem_mb * fraction / 100
```

Rules:

- `Fraction` defaults to 50 when zero; values above 100 are clamped to 100.
- If `Cores` or `MemoryMB` are set explicitly (non-zero), fraction scaling is skipped
  for that field.
- `MinCores` and `MinMemMB` are applied as floors after scaling. The absolute minimums
  are 1 core and 256 MB regardless of what the caller specifies.
- Host memory is read from `/proc/meminfo`; falls back to 8192 MB on non-Linux.

```go
// Use 25% of host resources, but never fewer than 2 cores or 1 GB:
r := vmctl.Resources{
    Fraction: 25,
    MinCores: 2,
    MinMemMB: 1024,
}
resolved := vmctl.ResolveResources(r)
fmt.Printf("%d cores, %d MB\n", resolved.Cores, resolved.MemoryMB)
```

`Boot` calls `ResolveResources` internally, so callers do not need to call it
directly unless they want to inspect or log the resolved values before booting.

---

## Disk Management

`DiskManager` wraps `qemu-img` and `genext2fs` for disk lifecycle operations.
The `Log` field is optional; set it to a `Logger` implementation to get progress
messages.

```go
dm := &vmctl.DiskManager{Log: myLogger}
```

### COW Overlay (`CreateOverlay`)

Creates a temporary qcow2 copy-on-write overlay on top of a read-only base image.
Mutations go to the overlay; the base image is never modified.

```go
overlay, err := dm.CreateOverlay("/nix/store/.../nixos.img", tmpDir)
if err != nil {
    log.Fatal(err)
}
defer overlay.Remove()

spec.BaseImage = vmctl.ImageRef{
    Path:   overlay.Path,
    Format: vmctl.DiskFormatQCOW2,
}
```

`dir` must be non-empty; passing an empty string is rejected to prevent accidentally
writing to the host OS temp directory.

### Persistent Cache Disk (`EnsureCacheDisk`)

Creates a qcow2 scratch disk at a fixed path if it does not already exist. Safe to
call on every startup — it is a no-op when the file is present.

```go
err := dm.EnsureCacheDisk("/var/cache/myapp/scratch.qcow2", "8G")
```

The size string uses `qemu-img` notation: `4G`, `512M`, etc.

### Application Bundle Disk (`CreateAppDisk`)

Packs a directory into a compact ext2 image sized to its contents plus 10 MB of
headroom. Useful for shipping a read-only application bundle into a VM.

```go
err := dm.CreateAppDisk("/path/to/bundle", "/tmp/app.img")
```

Requires `genext2fs` on PATH.

### Source Disk (`CreateSourceDisk`)

Creates an ext2 image from a directory with an explicit block count. Use this when
you need precise control over the image size.

```go
err := dm.CreateSourceDisk("/path/to/src", "/tmp/src.img", 204800) // 200 MB
```

---

## NixOS Integration

`NixBuild` wraps `nix-build` to build VM images from a `.nix` expression. It
handles stale output removal, stdout/stderr forwarding, and cache detection.

```go
b := &vmctl.NixBuild{
    File:    filepath.Join(repoRoot, "nix", "vm-image.nix"),
    Attr:    "qemu",                                // -A qemu
    OutLink: filepath.Join(repoRoot, "build", "vm", "image"),
    Stdout:  os.Stdout,
    Stderr:  os.Stderr,
}

// Skip the build if the output symlink already points into /nix/store/:
if !b.IsCached() {
    if err := b.Build(); err != nil {
        log.Fatal(err)
    }
}

imagePath := filepath.Join(b.OutLink, "nixos.img")
```

`FindNixBuild` locates `nix-build` on PATH or at the default profile location
`/nix/var/nix/profiles/default/bin/nix-build`. `Build` calls it automatically;
call `FindNixBuild` directly only if you need the path for other purposes.

`ExtraArgs` passes additional flags verbatim after the standard arguments:

```go
b.ExtraArgs = []string{"--arg", "debug", "true"}
```

---

## Preflight Checks

`Boot` automatically calls `Preflight()` on the selected hypervisor before
launching. The standalone functions are also exported for callers that want to
check availability at startup, before constructing a spec:

| Function | Checks |
|---|---|
| `PreflightQEMU()` | `/dev/kvm` exists, `qemu-system-x86_64` on PATH |
| `PreflightFirecracker()` | `/dev/kvm` exists, `firecracker` on PATH |
| `PreflightDirect()` | `nix-shell` on PATH or at default Nix profile location |
| `PreflightKVM()` | `/dev/kvm` exists (used by both VM hypervisors) |

```go
if err := vmctl.PreflightQEMU(); err != nil {
    log.Printf("QEMU unavailable: %v — falling back to direct", err)
    spec.Hypervisor = vmctl.HypervisorDirect
}
```

---

## API Reference

### Core types

```go
// VMSpec is the complete specification for launching a VM.
type VMSpec struct {
    Hypervisor HypervisorType
    Resources  Resources
    BaseImage  ImageRef
    Boot       *BootSpec    // nil for disk-boot (QEMU default)
    Disks      []DiskSpec   // additional drives beyond the base image
    Shares     []ShareSpec  // host-guest filesystem shares (QEMU only)
    Network    NetworkSpec
    Display    DisplayMode
    NoReboot   bool
    Timeout    time.Duration // kill VM after this duration (0 = unlimited)
    StatusFile string        // path written by guest containing its exit code
}

// Resources describes CPU and memory allocation.
type Resources struct {
    Cores    int // explicit vCPU count; 0 = derive from Fraction
    MemoryMB int // explicit RAM in MB; 0 = derive from Fraction
    Fraction int // percent of host resources (default 50, range 1-100)
    MinCores int // minimum vCPU floor (default 1)
    MinMemMB int // minimum RAM floor in MB (default 256)
}

// BootSpec describes kernel/initrd boot parameters.
// For HypervisorDirect only Command and ShellNix are used.
type BootSpec struct {
    KernelPath string // path to kernel image
    InitrdPath string // path to initrd (optional)
    KernelArgs string // kernel command-line arguments
    Command    string // shell command for HypervisorDirect
    ShellNix   string // path to shell.nix for HypervisorDirect
}

// Result holds the outcome of a VM invocation.
type Result struct {
    ExitCode   int    // guest or hypervisor exit code
    StatusFile string // path of the status file that was read, if any
}

// ImageRef identifies a VM base image.
type ImageRef struct {
    Path   string
    Format DiskFormat // DiskFormatRaw, DiskFormatQCOW2, DiskFormatExt2, DiskFormatExt4
}

// DiskSpec describes an additional drive to attach.
type DiskSpec struct {
    Path      string
    Format    DiskFormat
    ReadOnly  bool
    Interface string // e.g. "virtio"
}

// ShareSpec describes a host-guest 9p/virtfs share (QEMU only).
type ShareSpec struct {
    HostPath      string
    MountTag      string
    SecurityModel string // default "mapped-xattr"
    ID            string
}

// NetworkSpec describes VM network configuration.
type NetworkSpec struct {
    Mode    NetworkMode
    RawArgs string // optional raw hypervisor-specific network arguments
}
```

### Enumerations

```go
// Hypervisor backends
HypervisorQEMU        // QEMU/KVM full-system emulation
HypervisorFirecracker // Firecracker microVM
HypervisorDirect      // host nix-shell, no VM booted

// Display modes
DisplayNone  // headless, serial on stdio
DisplayGTK   // GTK window with VGA

// Network modes
NetworkNone      // no network
NetworkUserMode  // QEMU user-mode (SLIRP)
NetworkTAP       // TAP-based bridged networking
NetworkSlirp     // slirp4netns rootless TAP (Firecracker)

// Disk formats
DiskFormatQCOW2  DiskFormat = "qcow2"
DiskFormatRaw    DiskFormat = "raw"
DiskFormatExt2   DiskFormat = "ext2"
DiskFormatExt4   DiskFormat = "ext4"

// Feature flags (queried via Hypervisor.Supports)
Feature9P      // 9p/virtfs host-guest sharing
FeatureGUI     // graphical display output
FeatureResize  // qcow2 overlay / COW disks
```

### Logger interface

```go
type Logger interface {
    Info(tag, msg string)
    Warn(tag, msg string)
    Fail(tag, msg string)
    OK(tag, msg string)
}
```

Assign a `Logger` to `QEMUHypervisor.Logger`, `FirecrackerHypervisor.Logger`,
`DirectHypervisor.Logger`, or `DiskManager.Log` for structured lifecycle output.
All fields are optional; nil loggers silently skip output.

### Hypervisor interface

```go
type Hypervisor interface {
    Preflight() error
    Boot(ctx context.Context, spec *VMSpec, tmpDir string) (*Result, error)
    Supports(feature Feature) bool
}
```

Implement this interface to add a new backend. `Boot` is called by `vmctl.Boot`
after a successful `Preflight`.

---

## Feature matrix

| Feature | QEMU | Firecracker | Direct |
|---|:---:|:---:|:---:|
| Disk boot | yes | no | no |
| Direct-kernel boot | yes | yes (required) | no |
| 9p/virtfs shares | yes | no | no |
| GUI display | yes | no | no |
| qcow2 overlays | yes | no | no |
| Multiple disks | yes | one extra | no |
| KVM required | yes | yes | no |
