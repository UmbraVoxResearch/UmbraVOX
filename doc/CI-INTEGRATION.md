# CI Integration Guide

How to set up a self-hosted runner for the UmbraVOX CI pipeline.

## Runner Requirements

All three CI tiers (PR/Push, Nightly, Pre-release) run tests inside KVM-accelerated
VMs on self-hosted runners. Hosted runners (e.g., GitHub Actions `ubuntu-latest`) do
not have `/dev/kvm` and cannot run the pipeline in the default VM path. See
[--direct Override](#--direct-override) for the fallback when KVM is unavailable.

**Minimum spec:**

| Resource | Minimum | Notes |
|----------|---------|-------|
| Architecture | x86_64 | ARM64 tests are emulated inside x86_64 VMs |
| KVM | `/dev/kvm` must exist | `kvm-ok` or `grep -c vmx /proc/cpuinfo` to verify |
| RAM | 16 GB | Multi-VM integration tests (`./uv vm integration`) spawn 3 VMs concurrently |
| Disk | 100 GB | Builder image ~3 GB, dev VM image ~26 GB, nix-cache.qcow2 up to 60 GB |
| OS | Linux (any modern distro) | NixOS preferred; Ubuntu 22.04+ works |

**Required tools on the host:**

- `qemu-system-x86_64` with KVM support (`qemu-kvm` package on Debian/Ubuntu)
- `nix` package manager (for `nix-shell` and `nix build`)
- `curl` (used during first-run bootstrap to download the builder image)
- `git` (for checkout)

No GHC, cabal, or other Haskell toolchain is needed on the host. Everything
compiles inside NixOS VMs.

Install on Ubuntu/Debian:

```bash
# KVM + QEMU
apt-get install -y qemu-kvm cpu-checker
kvm-ok  # verify KVM works

# Nix (single-user install, or multi-user for shared runners)
curl -L https://nixos.org/nix/install | sh -s -- --no-daemon
# or multi-user:
curl -L https://nixos.org/nix/install | sh -s -- --daemon
```

Add the runner user to the `kvm` group so QEMU can open `/dev/kvm` without sudo:

```bash
usermod -aG kvm <runner-user>
# Re-login or: newgrp kvm
```

---

## GitHub Actions Setup

The workflow files at `.github/workflows/ci-linux.yml` (PR/Push) and
`.github/workflows/ci-nightly.yml` (Nightly) both use `runs-on: self-hosted`.

### Register a self-hosted runner

1. Go to **Settings → Actions → Runners → New self-hosted runner** in the repository.
2. Follow the GitHub-generated instructions to download and configure the runner agent.
3. Start the runner as a service so it survives reboots:

```bash
# As the runner user, from the actions-runner directory:
sudo ./svc.sh install
sudo ./svc.sh start
```

### Required labels

The workflows use `runs-on: self-hosted` with no additional labels. If you register
multiple runners with different capabilities, add a label so only KVM-capable machines
pick up these jobs:

```bash
# During runner config:
./config.sh --url https://github.com/ORG/REPO --token TOKEN --labels kvm,x86_64
```

Then update the workflow `runs-on` to match:

```yaml
runs-on: [self-hosted, kvm, x86_64]
```

### Environment

The runner needs no special environment variables. The `./uv` wrapper manages all
paths internally and writes only inside the project directory (see
[Host Filesystem Isolation](#host-filesystem-isolation)).

---

## GitLab CI Setup

There is no `.gitlab-ci.yml` in this repository, but the pipeline can be adapted.
A minimal config that mirrors the PR/Push tier:

```yaml
# .gitlab-ci.yml
default:
  tags: [kvm, x86_64]

pr-gate:
  script:
    - ./uv check
    - ./uv test
    - ./uv test integrity
    - ./uv test differential
    - ./uv check pre-release
  only:
    - merge_requests
    - main

nightly:
  script:
    - ./uv check
    - ./uv test
    - ./uv test integrity
    - ./uv test differential
    - ./uv check pre-release
    - ./uv test all
    - ./uv test e2e
    - ./uv test tcp
    - ./uv test fault
    - ./uv test recovery
    - ./uv test soak
    - ./uv fuzz differential
    - ./uv verify
    - ./uv coverage --check
    - ./uv vm smoke release
    - ./uv vm smoke freebsd
    - ./uv vm smoke openbsd
    - ./uv vm integration
    - ./uv vm signal test
  only:
    - schedules
```

### Configure a GitLab runner for KVM access

Install and register the GitLab runner:

```bash
# Install runner
curl -L https://packages.gitlab.com/install/repositories/runner/gitlab-runner/script.deb.sh | bash
apt-get install gitlab-runner

# Register (shell executor required — not docker, not kubernetes)
gitlab-runner register \
  --url https://gitlab.example.com \
  --token REGISTRATION_TOKEN \
  --executor shell \
  --tag-list kvm,x86_64 \
  --description "umbravox-kvm-runner"
```

The runner must use the **shell executor**, not Docker or Kubernetes, because the
pipeline needs direct access to `/dev/kvm` and the ability to boot QEMU VMs.

Ensure the `gitlab-runner` user is in the `kvm` group:

```bash
usermod -aG kvm gitlab-runner
```

---

## First Run

On a fresh runner, the first `./uv test` (or any `./uv` command that requires a VM)
will trigger the bootstrap sequence automatically. Here is what happens:

1. **`./uv` compiles itself**: The bootstrap wrapper shell script compiles the Go
   orchestration binary (`umbravox-vm`) from `tools/`. This is the only host
   compilation that ever happens. Output: `build/tools/umbravox-vm`.

2. **Builder image download**: `umbravox-vm` downloads the pre-built builder VM image
   (~300 MB compressed) from the GitHub release assets with SHA-256 verification.
   Cached at `build/vm/builder-image/nixos.img`.

3. **Dev VM image build**: The builder VM boots (QEMU + KVM), mounts a persistent
   nix scratch disk (`build/vm/nix-cache.qcow2`, thin-provisioned up to 100 GB), and
   builds the full dev VM image using `nix-build` inside the guest. This downloads
   packages from `cache.nixos.org` and builds the 26 GB image natively. Output:
   `build/vm/image/nixos.img`.

4. **Test run**: The dev VM boots, mounts the source tree read-only, copies it to a
   tmpfs, compiles, and runs tests. Exit code is propagated back to the host.

**Expected times on first run:**

| Step | Time |
|------|------|
| `umbravox-vm` compile | ~30 s |
| Builder image download | ~2 min (network-dependent) |
| Dev VM image build | ~20–40 min (depends on nix cache warmth) |
| `./uv test` (subsequent runs) | ~10 min (VM boot + compile + test) |

Subsequent runs skip steps 1–3 because `build/vm/image/nixos.img` already exists
and the nix scratch disk (`nix-cache.qcow2`) retains downloaded packages.

---

## Host Filesystem Isolation

Nothing writes outside the project directory. All paths are project-local:

| Data | Location |
|------|----------|
| Build artifacts | `dist-newstyle/`, `build/` |
| Cabal store | `dist-newstyle/cabal-store/` |
| VM images | `build/vm/` |
| Nix scratch disk | `build/vm/nix-cache.qcow2` |
| Test temp files | `build/test-tmp/` |
| VM output share | `build/vm-output/` |

There is no `~/.cabal`, no `~/go/`, and no writes to `/nix/store` on the host
from compilation. The host nix store may receive packages from `nix-shell` and
`nix build` operations (VM image derivations), but no Haskell toolchain is pulled
onto the host.

---

## Disk Management

### nix store growth

The nix scratch disk (`build/vm/nix-cache.qcow2`) is a thin-provisioned QCOW2 image
with a 100 GB ceiling. It starts small and grows as the builder VM downloads packages.
On a runner that only serves this repository, a fully warm cache typically uses
30–60 GB of actual disk space.

Monitor actual disk usage (not the thin-provisioned ceiling):

```bash
du -sh build/vm/nix-cache.qcow2
```

### Reclaiming space

To run nix garbage collection inside the builder VM (removes unreferenced store paths
while keeping current image derivations):

```bash
./uv clean --nix-gc
```

To remove all cached VM images and scratch disk entirely (next build will re-download
and rebuild from scratch):

```bash
./uv vm clean-image
```

Use `./uv vm clean-image` when:
- The scratch disk is corrupted
- You want a completely clean build for pre-release verification
- Disk is critically full and `--nix-gc` did not free enough space

### Scratch disk sizing

The default thin-provisioned ceiling is 100 GB. On a runner with limited disk, you
can reduce this by setting `UMBRAVOX_NIX_BUILD_DIR` before running:

```bash
export UMBRAVOX_NIX_BUILD_DIR=build/vm/tmp
```

For Nightly runners that run the soak test and multi-VM integration, plan for at
least 150 GB total free disk (100 GB nix cache + 26 GB dev image + headroom for
build artifacts).

---

## Security

### Runner isolation

Each job gets a fresh VM. The dev VM mounts the source tree **read-only** and copies
it to a tmpfs before building. The host filesystem (outside the project directory)
is not accessible from inside the VM.

The runner process itself should run as a dedicated non-root user (e.g.,
`github-actions` or `gitlab-runner`). It needs only:

- Write access to the project directory
- Membership in the `kvm` group
- No sudo, no capabilities beyond `kvm` group access

### No secrets in VMs

The pipeline does not inject secrets, tokens, or credentials into VMs. Tests that
require a Signal server use a local instance started inside the VM (`./uv vm signal
run`). No external network calls are made from inside test VMs.

The nix build config (`nix/vm-build.env`) enforces local-only builds:

```
UMBRAVOX_NIX_LOCAL_ONLY=1
UMBRAVOX_NIX_REQUIRE_CONFIG=1
```

Remote builder variables (`NIX_REMOTE`, `builders`) are rejected when these are set.

### Network policy

- The **builder VM** has outbound network access (required for `cache.nixos.org`)
- The **dev VM** (used for build/test/verify) has no outbound network access by default
- Test VMs for platform smoke (`./uv vm smoke freebsd` etc.) are isolated with no
  outbound routes

Do not attach runner machines to networks that have access to production systems.
The VMs themselves cannot reach outside the host, but the runner process runs on
the host and has whatever access the host network allows.

---

## Troubleshooting

### KVM permission denied

```
/dev/kvm: Permission denied
```

The runner user is not in the `kvm` group, or the group membership has not taken
effect in the current session.

```bash
# Verify group membership
groups <runner-user>

# Add to group (requires re-login to take effect)
usermod -aG kvm <runner-user>

# Verify KVM is accessible without sudo
ls -la /dev/kvm
# should show: crw-rw---- 1 root kvm ...
```

### Disk full during VM image build

If `nix-cache.qcow2` has grown to consume available disk:

```bash
# Check actual usage
df -h .
du -sh build/vm/

# Free nix store garbage
./uv clean --nix-gc

# Nuclear option: remove everything and start over
./uv vm clean-image
```

If the partition itself is too small, move the project directory to a larger
partition or mount point before running.

### Nix cache miss (slow builds)

If the builder VM downloads packages slowly or packages are not found:

- Verify outbound HTTPS access to `cache.nixos.org` from the runner host
- Check that `build/vm/nix-cache.qcow2` exists and is intact (not truncated)
- If the scratch disk is corrupted, remove it and let the builder VM recreate it:

```bash
rm build/vm/nix-cache.qcow2
./uv vm build-image
```

### Builder image download fails

The bootstrap falls back to building the builder VM locally via `nix-build` on the
host if the GitHub release download fails. This writes ~3 GB to the host nix store
and is slower. If it happens consistently, check network connectivity to GitHub
release assets.

### VM fails to boot (serial console debugging)

For interactive debugging of VM boot failures:

```bash
# Open an interactive serial console inside the dev VM
./uv dev

# GUI window instead of serial
./uv dev --gui
```

Inside the VM, check:

```bash
journalctl -u umbravox-smoke   # boot service logs
cat /output/vm-exec-status     # exit code from last exec command
cat /mnt/src/.vm-exec-cmd      # command that was sent to the VM
```

### Multi-VM integration test requires CAP_NET_ADMIN

`./uv vm integration` with `--dual-lan` requires bridge and TAP device setup:

```bash
sudo ./uv vm netsetup setup 6
./uv vm integration --dual-lan
sudo ./uv vm netsetup teardown
```

Standard `./uv vm integration` (without `--dual-lan`) does not require elevated
privileges and works in a standard runner environment.

---

## --direct Override

`--direct` bypasses the VM and runs build/test commands on the host via `nix-shell`.
It is the fallback for CI environments without KVM.

### When to use it

- GitHub-hosted runners (no `/dev/kvm`)
- Quick local iteration where VM boot time is a bottleneck
- Pre-release regression: `./uv test --direct` verifies the host build path works
  (listed as a Pre-release tier check in `doc/CI-TESTING.md`)

### When not to use it

Do not use `--direct` as a workaround for a misconfigured KVM runner. The VM path
is the authoritative test environment. `--direct` results on the host are
non-authoritative and depend on host state.

### How to use it in CI scripts

`--direct` shows a confirmation prompt by default to prevent accidental host builds.
Use `--direct-bypass-interactive` to suppress the prompt in non-interactive CI:

```bash
./uv test --direct --direct-bypass-interactive
./uv build --direct --direct-bypass-interactive
```

The `--direct-bypass-interactive` flag still shows a warning in the log but skips
the `[y/N]` prompt. This is the correct form for any CI script that intentionally
runs on the host path.

`./uv check` does not need `--direct` — it already runs on the host (lint, format,
license, complexity, generated-header checks do not require compilation).

### --direct prerequisites

`--direct` requires `nix-shell` and `shell.nix` on the host. This pulls the full
Haskell toolchain (GHC, cabal, F*, Z3) into the host nix store. On a self-hosted
runner with KVM, prefer the standard VM path; reserve `--direct` for runners that
genuinely cannot use KVM.
