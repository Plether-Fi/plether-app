# MockUSDG perps integration — Core v2.1.0

The application selects `config/perps/arbitrum-sepolia-v2.1.0.json`, the November
27, 2026 deployment on Arbitrum Sepolia (421614). The canonical schema is 4,
order interface is 3, settlement is six-decimal MockUSDG, and Core's phase is
`active`. All 31 contract addresses and runtime hashes come from the published
[release](https://github.com/Plether-Fi/plether-core/releases/tag/v2.1.0).
The graph includes `cfdEnginePlanningReader` and `cfdOrderAdmissionValidator`;
junior maintenance APR at deployment is zero. Pool, Junior fee, engine and router
settings have 72-hour timelocks; the vault UI reflects that notice period. Core
v2.1.0 also introduces flat global carry (`carryIndex`/`currentCarryIndex`), a
30-second max order execution window, a 2-second minimum oracle settlement delay,
a 0.25% minimum open slippage tolerance, 0.25 USDG max open/protection bounties,
and 10m/5m FAD shoulders at 2.00% margin. This source change performs no
deployment, Terraform apply or database migration.

## Evidence and reproducibility

`config/perps/evidence/v2.1.0/manifest.json` preserves the release asset bytes.
Its SHA-256 is `c3bbe008ddf765525863abc52ccac3edd26e3f1c255b773692a47ae30d1820e3`.
The deployed source commit and release tag are
`d5ef9725f72f5e131272da27004266d8a3d51e50`.

`deployment-block.json` records block 333192048, timestamp 1795796729
(2026-11-27 16:25:29 UTC). Both indexers begin at that block; certified volume
begins at 1795796760, the first whole minute after deployment (16:26 UTC).
`runtime-verification.json` records matching runtime hashes for all 31 contracts
at one safe block, plus the engine planning-reader, order admission validator,
and token-decimal checks. These are historical observations, not ongoing
liquidity or oracle readiness checks.

`consumer-bundle.json` inventories SHA-256 hashes of the deployment evidence,
compiled consumer ABIs and vendored SDK. Its checksum is the app's `bundleSha256`.
It is an app evidence inventory; the release does not publish a compiler archive.
The ABI record separately identifies source hashes/compiler version, without
claiming reproducible deployed bytecode.

Regenerate the app configurations into a new local candidate directory:

```bash
node scripts/prepare-perps-release.mjs \
  config/perps/evidence/v2.1.0/manifest.json \
  config/perps/evidence/v2.1.0/integration-profile.json \
  /tmp/perps-v210-app-candidate
```

Candidate generation keeps sponsorship disabled even after the reviewed public
manifest activation. The candidate's `arbitrum-sepolia-v2.1.0.json`, `perps-aa-manifest.json`,
`perps-core.tfvars.json` and `perps-backend.env.json` match the selected release,
frontend public AA manifest (apart from its reviewed sponsorship activation),
`infra/terraform/sepolia-core-v2.1.0.tfvars.json`
and evidence `backend.env.json`, respectively. The tool rejects existing output
directories and does not activate infrastructure. The profile's fresh AA identity
is `perps-aa-arbitrum-sepolia-usdg-210-v2`.

## Application rollout

The frontend registry, backend embedded manifest, workers, Docker, deployment
workflows and Terraform guards all select the same release. Backend startup
checks contract relationships and runtime hashes, including the planning reader
and order admission validator. Environment overrides cannot select another graph.
Terraform defaults remain empty; use the v2.1.0 overlay with the normal
environment configuration when planning rollout. Do not apply historical perps
overlays.

Before deploying the backend:

1. Review and apply `apps/backend/config/migrations/perps-settlement-assets-v1.sql`
   against a backed-up database, or provision a fresh database using the current
   schema. Backend startup rejects legacy perps column names. The migration has
   not been run by this source change.
2. Use the new router and deployment block for history and vault indexing. Existing
   cursors are router-scoped. Do not bind a previous competition to this market;
   a future competition requires its own release identity and baseline.
3. Review RPC, signer funding and oracle readiness for the new deployment. Core's
   published release recorded `authenticatedPythDryRun.passed = true` at
   `2026-11-27T16:15:49Z` and included initial liquidity transactions; confirm
   current settled liquidity and live oracle health before testing orders.
4. Verify AA infrastructure and matching backend policy before enabling
   preparation, submission, sponsorship or readiness enforcement. The public AA
   manifest selects the existing reviewed paymaster with sponsorship enabled.
   `aa-activation.json` records its verified runtime, EntryPoint/account policy,
   active signer, unpaused state, deposit and stake. Its implementation is
   unchanged across the protocol upgrade (pragma bump only) and it binds signed
   calldata rather than a specific market. Confirm the backend KMS signer and
   runtime configuration match that evidence. Core's release itself does not
   supply AA infrastructure.
5. Keep close-assistance issuance disabled until its separate policy rollout.
   Protection workers default to observe-only. Verify worker policies and signers
   before enabling execution. The integrated v2.1.0 close-preview pin is selected.

Deploy the backend first, then frontend, following `AGENTS.md`. Complete public
endpoint and trading/funding/vault smoke checks after the workflows succeed.
No service or AA flag is enabled by generating these files.

## Funding and interfaces

Current consumers use `settlementToken`, `*Assets` and `*AssetAtoms`; environment
and Terraform use `PERPS_SETTLEMENT_TOKEN` and `perps_settlement_token`. Historical
USDC evidence retains its original bytes and is not a compatibility fallback.

After AA rollout, the welcome flow calls the protected backend faucet to mint
100,000 MockUSDG to the confirmed Plether Trading Account. The dedicated faucet
signer pays mint gas; the owner wallet does not need testnet ETH for this flow.
The backend verifies the configured release graph and runtime hashes at startup.
The faucet persists its signed transaction before broadcasting and returns a
durable submitted state. Retries recover the same allocation per account/token.
After confirmation, exact approval plus sponsored `depositMargin` deposits from
the Trading Account. A failed deposit reuses the minted balance; completed
allocations cannot be minted again through this endpoint. The existing faucet
signer, proxy credential, database schema and rate limits are reused.
There is no EIP-3009, permit, bridge funding, or GDN reward income. A new
competition still requires its own reviewed release identity and provenance rules.

## SDK and ABI provenance

The vendored `@plether-fi/perps-aa-client@0.4.0` artifact retains its build commit
`e35cf0e0e9b880e07812e36321dee98bd299dcbd` and original integrity. Its runtime
sources are identical to v2.1.0. Local order and lifecycle CI use deployed Core
source `d5ef9725f72f5e131272da27004266d8a3d51e50` and check that equivalence.

Consumer ABIs were regenerated from compiled v2.1.0 sources using Solidity 0.8.39.
The engine switches from per-side utilization carry to flat global carry
(`carryIndex`, `carryTimestamp`, `currentCarryIndex`) and exposes
`minOpenSlippageToleranceBps`; `PletherOracle` exposes `minOrderSettlementDelay`;
`CfdEngineLens` exposes `orderAdmissionValidator` and adds
`postEligibleEquityAssets` to `quoteMaxOpen`. The order, public-lens, vault and
protection wire formats remain compatible with SDK 0.4.0. Interface 3,
configuration V5, digest V6 and receipt V5 retain their labels. Full terminal
evidence is joined to the canonical event and receipt hash, including terminal block.

```bash
node scripts/generate-perps-consumer-abis.mjs \
  --artifacts-dir /path/to/pinned-core/packages/perps/out \
  --source-root /path/to/pinned-core/packages/perps --check
node --test scripts/prepare-perps-release.test.mjs scripts/perps-v2-release.test.mjs scripts/perps-aa-rollout.test.mjs scripts/aa-sepolia.test.mjs
```

To update ABIs, omit `--check`, review generated changes and refresh the evidence
inventory/profile checksum together. The original prepared manifest remains an
inactive safety fixture. Historical replay tests keep their archived bytecode.
