# Vendored `@plether/perps-aa-client`

This directory is a checked-in runtime package consumed through the frontend's
`file:vendor/perps-aa-client` dependency.

## Provenance

- Repository: `plether-core`
- Source path: `packages/perps-aa-client`
- Reviewed commit: `c205cb450e98a3d40ba22c627ebce90f7ec1bf74`
- Upstream package version: `0.1.0`

`README.md`, `upstream-package.json`, and every file under `dist/` are
byte-for-byte copies of the upstream package at that commit. Because upstream
does not track `dist/`, the artifact was also rebuilt from the committed source
and compared file-for-file before vendoring.

The local `package.json` retains the upstream runtime entry points and
dependency range. It intentionally omits upstream build/test scripts and
development dependencies so npm treats this directory as a runtime artifact
instead of installing a second package toolchain. The exact upstream manifest
is retained as `upstream-package.json`.

## Updating

1. Select and review a new immutable `plether-core` commit.
2. Export `packages/perps-aa-client` from that commit into a clean temporary
   directory.
3. Run `npm ci`, `npm test`, and `npm run build` in the exported package.
4. Replace `README.md`, `upstream-package.json`, and the complete `dist/`
   directory with the clean build output.
5. Reconcile only the runtime fields in the local `package.json`, update the
   commit above and in `pletherVendoredFrom`, and regenerate `SHA256SUMS`.
6. Run `npm install --package-lock-only --ignore-scripts` and the frontend build.

Do not patch ABI encoding or orchestration logic in this directory. Make code
changes upstream, review a new commit, and revendor it.
