# Nix Cache Policy

UmbraVOX uses large VM builds, so Nix needs a soft disk-pressure policy
instead of relying on manual cleanup alone.

## Recommended Host Policy

Apply this on the host machine that runs `./uv`:

```nix
{
  nix.gc.automatic = true;
  nix.gc.dates = "03:15";

  nix.settings = {
    min-free = 40 * 1024 * 1024 * 1024;
    max-free = 60 * 1024 * 1024 * 1024;
    auto-optimise-store = true;
  };
}
```

Behavior:

- `min-free` triggers garbage collection once free space in `/nix/store`
  drops below the threshold during a build.
- `max-free` stops the cleanup once enough free space has been recovered.
- `auto-optimise-store` deduplicates identical files to reduce store growth.
- `nix.gc.automatic` and `nix.gc.dates` keep old, unreachable store paths
  from accumulating forever.

## Builder Image Policy

The builder VM that produces cached UmbraVOX images should use the same
soft-cap values. In this repository that policy lives in:

- [nix/tiers/builder.nix](../nix/tiers/builder.nix)

That tier is inherited by the builder VM image in:

- [nix/vm-builder.nix](../nix/vm-builder.nix)

This keeps the cache-image policy aligned with the host policy and gives
the cold-bootstrap path headroom before it hits the repo-local disk guard.
