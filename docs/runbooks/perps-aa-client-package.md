# Adopt the Core AA client package

The replacement is `@plether-fi/perps-aa-client@0.1.0`, published by
`Plether-Fi/plether-core` to GitHub Packages with tag
`perps-aa-client-v0.1.0`. The app now consumes this exact registry version;
the legacy vendor directory and Core patch have been removed.

[Core PR #94](https://github.com/Plether-Fi/plether-core/pull/94) was merged at
`f9e29c1b3ac5937e0519108cebffb4d09048de36`. The
[approved release run](https://github.com/Plether-Fi/plether-core/actions/runs/34480888502)
published the tested tarball. Its immutable source, tag and SHA-512 integrity
are recorded in `config/perps-aa-client-release.json` and the frontend lockfile.
The package is public and `plether-app` has Actions Read access. Organization-wide
public package creation remains disabled after the one-package visibility change.

## Release prerequisites

Merge the reviewed Core PR containing both native sponsorship and position
protection. Configure required reviewers on Core's `github-packages` environment,
then run its manual `publish-perps-aa-client.yml` workflow from `master` with
`version=0.1.0`. That workflow tests a tarball in an isolated consumer and publishes
those exact bytes after approval. Verify its registry integrity and source SHA,
tag, and GitHub Release. A package release does not deploy the paymaster.

Verify public visibility, the package's Core repository link, and Actions read
access for `Plether-Fi/plether-app`. All four frontend-install workflows now use
`packages: read`, the scoped registry, and `GITHUB_TOKEN` only at `npm ci`.
No package token is available to the frontend build or bundled application.

Local developers need a classic PAT with `read:packages` in their user-level npm
configuration. The migration also uses read-only GitHub CLI package/PR/tag APIs,
so its CLI identity needs package-read access. Never commit authentication data.

```bash
npm login --scope=@plether-fi --auth-type=legacy --registry=https://npm.pkg.github.com
```

GitHub requires authentication for public npm packages too; see
[its registry documentation](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-npm-registry).

## Historical one-time migration

The following migration has been applied. Do not rerun it on the adopted branch:

```bash
node scripts/migrate-perps-aa-client.mjs 0.1.0 f9e29c1b3ac5937e0519108cebffb4d09048de36 94
```

The command first verifies public visibility, repository ownership, merged PR
ancestry, the immutable tag/release, registry `gitHead` and SHA-512 integrity, and
an isolated registry install followed by `npm ci`. It makes no tracked changes
when those prerequisites fail. Once they pass, it updates the exact dependency,
imports, and lockfile; rewrites runbook and GitBook provenance; records
`config/perps-aa-client-release.json`; and removes the old patch, manifest,
vendor directory, and vendor refresh script. These deletions remain recoverable
from git. Installation failures after the dependency edit leave a reviewable
working diff and retain the vendor artifacts until the install succeeds.

Only AA source references change in the diagram catalog and manifest. The
independent contract-release review pin and SVGs are preserved; migrating the
client is not evidence that every protocol diagram was re-reviewed.

Review the resulting diff and run:

```bash
npm ci --prefix apps/frontend
node --test scripts/migrate-perps-aa-client.test.mjs scripts/perps-aa-rollout.test.mjs
npm run lint --prefix apps/frontend
npm test --prefix apps/frontend
npm run build --prefix apps/frontend
npm run build-storybook --prefix apps/frontend
node --test apps/gitbook/scripts/diagrams.test.mjs
```

Run the existing frontend perps integration tests with their local fixtures and
the backend native-AA PostgreSQL integration CI. Keep the existing feature and
deployment settings. Update PR #204's description with the Core PR and package
release. Its frontend CI must confirm a clean registry install using the app's
`GITHUB_TOKEN` before merging; a local PAT install does not prove Actions access.

The migration helper deliberately retains the old paths as cleanup targets.
Runtime imports, the dependency lockfile, and active provenance must contain no
remaining vendor/patch dependency after adoption.
