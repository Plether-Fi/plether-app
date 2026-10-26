# MockUSDG perps integration

This prepares a **new perps deployment** using six-decimal MockUSDG. It does not
switch the running USDC release. The active deployment remains
`config/perps/arbitrum-sepolia-v3.json`; no new deployed addresses or runtime
hashes are available until a separately authorized deployment exists.

The frontend reads settlement metadata and all perps bindings from that active
manifest. The backend embeds the same manifest at build time, with the compiler
tracking it as a dependency. Schema 4 uses `contracts.settlementToken` and the
separate `settlementToken` metadata object. Historical manifests still read
`contracts.mockUsdc`. A malformed canonical entry never falls back to USDC.

## Prepare a candidate after deployment evidence exists

The following command performs only local file generation. It cannot deploy,
change active configuration, enable sponsorship, or send a transaction:

```bash
node scripts/prepare-perps-release.mjs \
  /path/to/verified-deployed-core-manifest.json \
  /path/to/reviewed-integration-profile.json \
  /tmp/new-perps-app-candidate
```

Start the integration profile from
`config/perps/mock-usdg-integration-profile.example.json`. Fill its null fields
with the reviewed release bundle SHA-256, a fresh AA manifest version, the first
whole minute after the deployment block's timestamp, and the AA infrastructure
addresses for the new release. The example deliberately has no inherited
paymaster or factory address. Keep `orderInterfaceVersion: 3` to match the
current app and SDK. An incomplete profile fails validation.

The command rejects a prepared manifest, missing addresses/hashes, an absent
successful deployed-phase verification, and incompatible MockUSDG metadata. It
also rejects an output directory that already exists. The output contains:

- `arbitrum-sepolia-v3.json`: Core manifest plus app integration fields and
  source-manifest/bundle checksums, including the new token runtime hash.
- `perps-aa-manifest.json`: new contract bindings with EIP-3009 unavailable,
  null EIP-712 settings, and sponsorship disabled pending separate activation.
- `perps-core.tfvars.json`: perps and vault-history bindings and deployment
  blocks generated from the same Core manifest.
- `perps-backend.env.json`: equivalent address and indexing environment values.
- `activation-review.json`: provenance and remaining deployment-specific work.

`PERPS_USDC`, `perps_usdc`, AA `usdc`, existing public ABI names, and historical
SQL column names remain compatibility keys. They identify the release's
settlement asset and do not imply its display symbol. The backend release check
verifies the token runtime hash and the clearinghouse, engine and HousePool
asset bindings before serving the current order-interface V3 deployment.

Candidate generation checks manifest consistency, not live chain state or the
external bundle's contents. Keep the source manifest's deployed verification
evidence and use the existing checksum-verifying worker ABI generator against
the reviewed release bundle when preparing activation. Do not reuse historical
AA infrastructure, integrated close-preview/close-assistance bindings, or
release-specific workflow pins without review. Backend/frontend workflow
checks and `infra/terraform/deployment_target.tf` intentionally retain the
running release's pins at this stage; update those with the reviewed candidate
only when activation is requested.

## Testnet funding

Mint MockUSDG to the Trading Account address, then submit an exact approval and
`depositMargin` call from that account. MockUSDG has unrestricted minting, six
decimals, and ordinary ERC-20 transfer behavior. It has no
`receiveWithAuthorization` or EIP-3009 domain. A USDC bridge route cannot fund
this mock; use the testnet mint path.

## SDK compatibility

This integration remains compatible with the pinned
`@plether-fi/perps-aa-client@0.2.0` vendored artifact from the current master
release. That artifact already builds the
ordinary Trading Account `approve` + `depositMargin` batch used by MockUSDG;
no unpublished SDK functionality is required. Focused integration tests and TypeScript checks use that pinned artifact. The app does not call or display
`derivePerpsCapabilities().firstDeposit.reason` or `mapPerpsExecutionError`;
its deposit gating and token labels come from the reviewed release metadata.

Core's SDK wording/comment changes have not been published. To consume those
updated SDK diagnostics later, authorize a normal SDK versioned release and
update the app dependency, lockfile and `config/perps-aa-client-release.json`
provenance together. Do not republish or relabel an existing version. This publication
is not a prerequisite for the MockUSDG funding/trading behavior implemented here.

## Future rewards

MockUSDG does not earn GDN rewards. Schema 4 records `rewards.status` as
`not_applicable`, names `marginClearinghouse` and `housePool` as prospective
holding contracts, and leaves the payout address/controller null. There is no
reward distributor or expected yield in margin, solvency or LP accounting.

For a future official-USDG release, confirm eligibility and payout controls
with the provider and choose a separately controlled reward recipient. Do not
send payouts directly into the clearinghouse: an unsolicited token transfer
does not credit an account. Keep reward allocation a separate feature and
recognize received funds only through deliberate accounting operations.

## Local checks

```bash
node --test scripts/prepare-perps-release.test.mjs
cd apps/backend
cabal test plether-api-test --offline --test-options='--match=Manifest'
```

No chain deployment or transaction is required for these checks. Frontend
release parsing/funding tests cover capability gating; backend tests cover
legacy/canonical token lookup, including rejection of incomplete canonical
entries.

Core companion implementation: [Plether-Fi/plether-core#131](https://github.com/Plether-Fi/plether-core/pull/131).
