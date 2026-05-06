UmbraVOX Transport Provider Registry
====================================

This directory holds manifest descriptors for transport/provider stubs.

Current scope:
- no dynamic loading
- no provider execution/runtime host
- no movement of secure session semantics out of core

The provider registry exists so connectivity substrates and bridge adapters can
be modeled separately from packaged feature plugins.

Current policy:
- the secure session core remains built in
- providers sit below that core as carriers or bridges
- manifests here are metadata and launch-contract stubs only
- closed-service bridges are non-assurance placeholders until explicitly
  implemented, reviewed, and documented

Manifest contract:
- `api=uvx-provider-v1` declares the provider manifest/runtime contract version
- `class=` identifies direct carriers, overlay carriers, open bridges, or
  closed bridges
- `inherits=` records provider trait inheritance or upstream dependency
- `capabilities=` records coarse provider abilities for future registry-based
  selection and policy
- `host + entrypoint` reserve either a built-in module target or a future IPC
  executable target

These manifests are intentionally separate from `plugins/` because provider
adapters are connectivity substrates, not feature modules.
