# Vendored `@plether/perps-aa-client`

This directory is a checked-in runtime package consumed through the frontend's
`file:vendor/perps-aa-client` dependency.

## Provenance

- Repository: `plether-core`
- Source path: `packages/perps-aa-client`
- Base commit: `bc8f6290c540665e4ff61328ea83a4c3d421a8d4`
- Reviewed patch: `.codex-artifacts/plether-core-self-hosted-aa.patch`
- Patch SHA-256: `44410efdc9eccc81d3f558782b39514c8323abda70dbd6a3c89eef17ea994a82`
- Upstream package version: `0.1.0`

`README.md`, `upstream-package.json`, and every file under `dist/` are
byte-for-byte copies of the package produced by applying the reviewed patch to
the base commit. Because upstream does not track `dist/`, the artifact was
rebuilt from the patched source before vendoring. The patch manifest records
the upstream client test, typecheck, and build results.

The local `package.json` retains the upstream runtime entry points and
dependency range. It intentionally omits upstream build/test scripts and
development dependencies so npm treats this directory as a runtime artifact
instead of installing a second package toolchain. The exact upstream manifest
is retained as `upstream-package.json`.

## Updating

1. Select and review a new immutable `plether-core` commit or content-addressed
   patch plus its exact base commit.
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
