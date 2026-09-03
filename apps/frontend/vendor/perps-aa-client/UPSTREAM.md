# Vendored `@plether/perps-aa-client`

This directory is a checked-in runtime package consumed through the frontend's
`file:vendor/perps-aa-client` dependency.

## Provenance

- Repository: `plether-core`
- Source path: `packages/perps-aa-client`
- Protection snapshot commit: `3472427ed15b0a478248af7d025535da349a8592`
- Base commit: `bc8f6290c540665e4ff61328ea83a4c3d421a8d4`
- Reviewed patch: `.codex-artifacts/plether-core-self-hosted-aa.patch`
- Patch SHA-256: `d1c6941c03f37cc9a93b35b95dc73a876ee87dab624524ed9c4d6336022f2955`
- Upstream package version: `0.1.0`

The vendored package combines the protection modules from the reviewed snapshot
with the native sponsorship, paymaster, and orchestration modules produced by
applying the reviewed patch to its exact base. The shared entry point and types
were rebuilt from that union so both API sets are exported together. The patch
manifest records the native client test, typecheck, and build results.

The local `package.json` retains the upstream runtime entry points and
dependency range. It intentionally omits upstream build/test scripts and
development dependencies so npm treats this directory as a runtime artifact
instead of installing a second package toolchain. The exact upstream manifest
is retained as `upstream-package.json`.

## Updating

1. Prefer a new immutable `plether-core` commit that contains both API sets. If
   that is not available, review each content-addressed source independently.
2. Export `packages/perps-aa-client` from that source into a clean temporary
   directory.
3. Run `npm ci`, `npm test`, and `npm run build` in the exported package.
4. Replace `README.md`, `upstream-package.json`, and the complete `dist/`
   directory with the clean build output.
5. Reconcile only the runtime fields in the local `package.json`, update the
   immutable provenance above and in `pletherVendoredFrom`, and regenerate
   `SHA256SUMS`.
6. Run `npm install --package-lock-only --ignore-scripts` and the frontend build.

Do not patch ABI encoding or orchestration logic in this directory. Make code
changes upstream, review a new commit, and revendor it.
