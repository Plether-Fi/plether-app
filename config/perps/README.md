# Arbitrum Sepolia perps release

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
