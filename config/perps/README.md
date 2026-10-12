# Arbitrum Sepolia perps release

## Pending v1.3.0 address configuration

`arbitrum-sepolia-v3.json` contains the exact deployment manifest from
[Core PR #111](https://github.com/Plether-Fi/plether-core/pull/111), pinned to
record commit `c9b121702e7228259706e1229a0300605efa8e89`. Its 29 contract
addresses and runtime hashes target source commit
`cf86189bcca4813f39d6405a338b77176783eb42`, at L2 deployment blocks
318367917–318367988. `arbitrum-sepolia-v3.provenance.json` records the manifest
and build archive checksums separately, preserving the upstream manifest bytes.
The frontend exposes these bindings through `PERPS_ARBITRUM_SEPOLIA_V3` in
`perpsV3Addresses.ts`, including the new close preview and recovery sidecar.

This stack is deployed/inactive: trading is disabled, both tranches are unseeded,
and the guardian is unset. Consumer cutover is pending. The active V2 registry,
AA manifest, backend embedded manifest, and Terraform bindings still target
v1.2.3. Do not substitute V3 addresses into those V2 consumers: first migrate
frontend ABIs and order preparation, keeper/indexer formats, and AA calldata
policy together. Historical execution requires `getOrderExecutionFee(bytes[])`;
closes and `depositFor` have changed semantics. The old supplemental close
preview and sponsored-close lens are specific to v1.2.3 and must not be reused.

Bootstrap and trading activation require their separate release gates. Keep
sponsorship and bridge funding disabled for the pending stack. Prior positions,
orders, balances, claims, and vault obligations have no automatic migration.
No deployment or onchain transaction is performed by this address preparation.

## Current v1.2.3 consumer configuration

`arbitrum-sepolia-v2.json` pins plether-core v1.2.3 (the filename describes the
bounded V2 order intent protocol). It includes all 27 contract addresses and
runtime hashes, immutable source provenance, and the original deployment snapshot.
The frontend and backend target this stack; indexers start at block 307397196.
The first full volume-history minute is 1789038420, derived from that block's
onchain timestamp (1789038416, block hash
`0x602f96ab814dab7cef750a28f929dd5dd2feb177ac81fa09206ddecc57b87dd5`).

Source: https://github.com/Plether-Fi/plether-core/releases/tag/v1.2.3

The original release manifest predates seeding and guardian configuration. Its
`release`, `economics`, and `verification` fields remain the published snapshot,
including the standard 1-USDC seed defaults. The checksum-pinned evidence files
under `evidence/v1.2.3/` record subsequent operations: both tranches received
**0.01 mock USDC** (10000 raw units) each, and the guardian was set to
`0x6b72fe6cc52201a1eb7892a813c6c10cce62745c`. Trading was still inactive at the
last recorded verification, block 307407614. Do not seed initialized tranches again.
Before consumer cutover, verify live state against these actual seed amounts,
verify activation and oracle/pause state, and arrange servicing of old-stack
positions, orders, protections, balances, claims, and LP obligations. This is a
fresh stack with no live-state migration. Never combine multiple deployments
within one competition.

Order intents remain `PletherOrderIntentV2`; receipts and execution configuration
use `PletherOrderReceiptV3` and `PletherExecutionConfigV3`. The AA manifest keeps
its required `-v2` suffix and changes its deployment date to invalidate prior
identity bindings. Position protection has no separate activation switch;
normal pause, position, margin, and oracle checks still apply. Triggering a
protection queues a close and does not guarantee execution.
This repository update does not deploy services or activate contracts.

The frontend ABI subset is checked against the checksum-verified release bundle,
including the new clearinghouse `getOrderReservation` FIFO tuple and lens
`quoteMaxOpen` view. The latter is a planner-valid bound; Router and terminal-book
gates still apply. Existing AA client actions remain wire-compatible. Carry now
consumes active position margin first, then free settlement; price-risk estimates
use the remaining margin and claims cannot pay residual carry.

Regenerate the protection worker ABI from the pinned release archive:

```sh
node scripts/generate-protection-worker-abi.mjs /path/to/perps-v1.2.3-arbitrum-sepolia.tar.gz
```

The backend embeds the manifest at compile time through `Plether.Perps.Manifest`.
Missing fields fail the build, and Cabal tracks the JSON as a build dependency.
Environment variables still configure runtime addresses; release validation and
keeper code-hash checks compare them with the embedded manifest. Docker copies
the same JSON into its build context. Edit the manifest when changing releases,
then rebuild; do not add deployed addresses or hashes to Haskell modules.

Indexer format names are defined in `Plether.Perps.IndexerFormat`, independently
of addresses. The configured lifecycle-book protocol selects the worker format;
current competition queries use bounded V2. The archived July competition retains
its original V1 format. Every cursor and lock remains scoped to one competition's
immutable chain and router.
