# MockUSDG perps integration — Core v2.0.0

The application selects `config/perps/arbitrum-sepolia-v2.0.0.json`, the October
28, 2026 deployment on Arbitrum Sepolia (421614). The canonical schema is 4,
order interface is 3, settlement is six-decimal MockUSDG, and Core's phase is
`active`. All 30 contract addresses and runtime hashes come from the published
[release](https://github.com/Plether-Fi/plether-core/releases/tag/v2.0.0).
The graph includes `cfdEnginePlanningReader`; junior maintenance APR at deployment
is zero. Pool, Junior fee, engine and router settings now have 72-hour timelocks;
the vault UI reflects that notice period. This source change performs no deployment, Terraform apply or database
migration.

## Evidence and reproducibility

`config/perps/evidence/v2.0.0/manifest.json` preserves the release asset bytes.
Its SHA-256 is `584cdacada35f8225d2a88811edfbd9622764fe0e6da4b275ba8f33d6f8c0c74`.
The release record was published at Core commit
`f3acb044da3a72200d07acccfa48548f0fbbb6b4`; the deployed source/tag is
`1209ca8a5b94a4fb6323c7c4585846ed93db3932`.

`deployment-block.json` records block 323658447, timestamp 1793185148
(2026-10-28 10:59:08 UTC). Both indexers begin at that block; certified volume
begins at 1793185200, the first whole minute after deployment (11:00 UTC).
`runtime-verification.json` records matching runtime hashes for all 30 contracts
at one safe block, plus the engine planning-reader binding and token decimals.
These are historical observations, not ongoing liquidity or oracle readiness checks.

`consumer-bundle.json` inventories SHA-256 hashes of the deployment evidence,
compiled consumer ABIs and vendored SDK. Its checksum is the app's `bundleSha256`.
It is an app evidence inventory; the release does not publish a compiler archive.
The ABI record separately identifies source hashes/compiler version, without
claiming reproducible deployed bytecode.

Regenerate the app configurations into a new local candidate directory:

```bash
node scripts/prepare-perps-release.mjs \
  config/perps/evidence/v2.0.0/manifest.json \
  config/perps/evidence/v2.0.0/integration-profile.json \
  /tmp/perps-v2-app-candidate
```

The candidate's `arbitrum-sepolia-v2.0.0.json`, `perps-aa-manifest.json`,
`perps-core.tfvars.json` and `perps-backend.env.json` match the selected release,
frontend public AA manifest, `infra/terraform/sepolia-core-v2.0.0.tfvars.json`
and evidence `backend.env.json`, respectively. The tool rejects existing output
directories and does not activate infrastructure. The profile's fresh AA identity
is `perps-aa-arbitrum-sepolia-usdg-200-v2`.

## Application rollout

The frontend registry, backend embedded manifest, workers, Docker, deployment
workflows and Terraform guards all select the same release. Backend startup
checks contract relationships and runtime hashes, including the new planning reader.
Environment overrides cannot select another graph. Terraform defaults remain empty;
use the v2 overlay with the normal environment configuration when planning rollout.
Do not apply historical perps overlays.

Before deploying the backend:

1. Review and apply `apps/backend/config/migrations/perps-settlement-assets-v1.sql`
   against a backed-up database, or provision a fresh database using the current
   schema. Backend startup rejects legacy perps column names. The migration has
   not been run by this source change.
2. Use the new router and deployment block for history and vault indexing. Existing
   cursors are router-scoped. Do not bind a previous competition to this market;
   a future competition requires its own release identity and baseline.
3. Review RPC, signer funding and oracle readiness for the new deployment. Core's
   published snapshot had only permanent seeds and pending liquidity; confirm
   current settled liquidity before testing orders. Its authenticated Pyth dry-run
   record was false at publication.
4. Configure and verify AA infrastructure separately. The public AA manifest has
   `paymasterAddress: null` and `sponsorshipEnabled: false`; it intentionally cannot
   authorize sponsored trading. Core's release does not supply a paymaster.
   Install a reviewed paymaster address/profile and matching backend policy before
   enabling preparation, submission, sponsorship or readiness enforcement. Do not
   reuse a historical paymaster solely because it is on the same chain.
5. Keep close-assistance issuance disabled until its separate policy rollout.
   Protection workers default to observe-only. Verify worker policies and signers
   before enabling execution. The integrated v2 close-preview pin is selected.

Deploy the backend first, then frontend, following `AGENTS.md`. Complete public
endpoint and trading/funding/vault smoke checks after the workflows succeed.
No service or AA flag is enabled by generating these files.

## Funding and interfaces

Current consumers use `settlementToken`, `*Assets` and `*AssetAtoms`; environment
and Terraform use `PERPS_SETTLEMENT_TOKEN` and `perps_settlement_token`. Historical
USDC evidence retains its original bytes and is not a compatibility fallback.

After AA rollout, funding mints 100,000 MockUSDG to the confirmed Plether Trading
Account. The connected wallet pays testnet ETH and confirms the mint. The token
runtime hash is checked before minting; exact approval plus `depositMargin`
deposits from the Trading Account. A failed deposit reuses the minted balance.
There is no EIP-3009, permit, bridge funding, or GDN reward income. Unrestricted
mock minting does not establish competition faucet provenance.

## SDK and ABI provenance

The vendored `@plether-fi/perps-aa-client@0.4.0` artifact retains its build commit
`e35cf0e0e9b880e07812e36321dee98bd299dcbd` and original integrity. Its runtime
sources are identical to v2.0.0. Local order and lifecycle CI use deployed Core
source `1209ca8a5b94a4fb6323c7c4585846ed93db3932` and check that equivalence.

Consumer ABIs were regenerated from compiled v2 sources using Solidity 0.8.35.
The engine exposes additional treasury errors; the order, public-lens, vault and
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
