# MockUSDG perps integration

The current application targets a **new perps deployment** using six-decimal
MockUSDG. Its canonical configuration is `config/perps/mock-usdg-prepared.json`:
schema 4, order interface 3, phase `prepared`, and null deployment addresses,
runtime hashes and deployment block. Perps trading, funding, vault interactions
and deployment guards remain unavailable until reviewed deployment evidence is
installed. Spot assets keep their existing token configuration. No deployment,
Terraform apply or database migration is performed by this source change.

Historical manifests and signed-operation evidence retain their original bytes
and token names. Current consumers do not parse them or fall back to them.
This is a breaking new-deployment interface: `settlementToken`, `*Assets` and
`*AssetAtoms` replace token-specific perps identifiers throughout the contract,
SDK, app, API, worker and database surfaces. Environment/Terraform bindings are
`PERPS_SETTLEMENT_TOKEN` and `perps_settlement_token`. There are no compatibility
aliases. Existing deployments require their matching historical consumers.

## Prepare a candidate after deployment evidence exists

The following command only writes a new local candidate directory:

```bash
node scripts/prepare-perps-release.mjs \
  /path/to/verified-deployed-core-manifest.json \
  /path/to/reviewed-integration-profile.json \
  /tmp/new-perps-app-candidate
```

Start the profile from `config/perps/mock-usdg-integration-profile.example.json`.
Fill its null fields with the reviewed bundle SHA-256, a fresh AA manifest
version, the first whole minute after the deployment timestamp, and reviewed AA
infrastructure addresses. Keep `orderInterfaceVersion: 3`. The tool rejects
historical schemas, prepared releases, missing addresses or hashes, unsuccessful
deployed-phase verification, incompatible MockUSDG metadata and existing output
directories. It emits:

- `mock-usdg-prepared.json`: the verified Core release plus app integration and
  manifest/bundle checksums; the filename remains the current consumer path.
- `perps-aa-manifest.json`: matching contracts, signed token deposits unavailable,
  null EIP-712 settings and sponsorship disabled.
- `perps-core.tfvars.json` and `perps-backend.env.json`: matching perps/vault
  bindings and fresh indexing boundaries.
- `activation-review.json`: provenance and deployment-specific follow-up work.

Candidate generation checks local consistency. Later activation requires the
verified chain deployment, matching ABI artifacts, database schema transition,
reviewed AA sponsor/worker configuration and release-isolated history. Review
those together. Current frontend/backend deployment workflows reject prepared
configuration and compare candidate bindings with the canonical release.
Terraform defaults contain no previous perps addresses. Historical overlays
must not be used to activate the new interface.

The authored SQL migration is a separate operator step and has not been run.
Current backend startup rejects old perps column names before additive schema
setup; deploying new consumers against an unchanged database is unsupported.

## Testnet funding

After activation, the funding button requests unrestricted `mint(address,uint256)`
for 100,000 MockUSDG to the confirmed Plether Trading Account. The connected owner
wallet pays Arbitrum Sepolia ETH and confirms the mint. The token runtime hash is
checked first. An exact approval plus `depositMargin` then deposits the balance
from the Trading Account. Funding can be repeated; a failed deposit reuses the
minted balance when retried. MockUSDG has no EIP-3009, permit or bridge funding.
Unrestricted mock minting does not establish competition faucet provenance.

## SDK and ABI provenance

The app consumes the breaking `@plether-fi/perps-aa-client@0.4.0` artifact from
Core commit `e35cf0e0e9b880e07812e36321dee98bd299dcbd`. The package record,
lockfile and vendored checksum identify the same immutable build. Order interface
3, configuration V5, digest V6 and lifecycle receipt V5 retain their existing
version labels; their canonical field names and Solidity selectors come from
the new source. Old aliases are not exported.

Regenerate current frontend and worker ABI consumers from compiled Core sources:

```bash
node scripts/generate-perps-consumer-abis.mjs \
  --artifacts-dir /path/to/plether-core/packages/perps/out \
  --source-root /path/to/plether-core/packages/perps
```

Use `--check` to verify reproducibility. Source-derived ABI provenance does not
claim a deployed address or runtime hash. Ordinary/protected closes require no
execution bounty; protection trigger bounty remains separate. Protection intents
include explicit close financial limits. Full terminal evidence is verified
against the canonical V5 event and receipt hash, including its terminal block.

## Future rewards

MockUSDG earns no GDN rewards. The manifest records `not_applicable`, identifies
`marginClearinghouse` and `housePool` as prospective holding contracts, and leaves
payout address/controller null. No reward income enters margin, solvency or LP
accounting. For an official USDG deployment, eligibility and payout control need
provider confirmation. A separately controlled reward recipient and explicit
allocation/accounting are future work; sending tokens to the clearinghouse does
not credit an account automatically.

## Local checks

```bash
node --test scripts/prepare-perps-release.test.mjs scripts/perps-aa-rollout.test.mjs scripts/aa-sepolia.test.mjs
```

These checks generate synthetic candidates and validate activation guards without
RPC or deployment. Historical replay tests remain scoped to their archived bytecode.

Core companion: [Plether-Fi/plether-core#131](https://github.com/Plether-Fi/plether-core/pull/131).
