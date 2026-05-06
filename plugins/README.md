UmbraVOX Feature Plugin Registry
================================

This directory holds manifest-only descriptors for packaged feature extension
slots.

Current scope:
- no dynamic loading
- no external code execution
- no runtime enablement

These manifests exist so the feature-plugin registry can point at concrete
on-disk descriptors for future packaged modules without turning them on in the
MVP. Transport/provider descriptors live in `providers/` and use a separate
schema so connectivity substrates do not get conflated with UX/data-plane
feature modules.

Manifest contract:
- `api=uvx-plugin-v1` declares the expected manifest/runtime contract version
- `host=ipc-stdio` declares the intended activation model for generated packaged
  plugins: a separate executable plugin process with an IPC protocol over
  standard input/output
- `entrypoint=exec:generated/<plugin>-plugin` reserves the future generated
  executable target for the packaged plugin
- until that generated artifact exists, the plugin remains discoverable but not
  load-ready

This means future code generation can target `plugins/<id>/generated/...`
without changing the core registry shape again. The runtime still does not load
or execute these modules yet; this only defines the artifact contract. The core
runtime now derives a typed launch spec from `host + entrypoint`, so future
loaders can consume cached execution metadata instead of reparsing manifest
strings at activation time. Manifests using an unknown `api=` tag are now
treated as unsupported runtime contracts instead of being silently accepted.

Planned packaged modules:
- `group-chat`
- `location-sharing`
- `image-sharing`
- `file-sharing`

Each module directory contains a `manifest.uvx` file with stable metadata.
