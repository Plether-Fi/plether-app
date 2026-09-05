# Arbitrum Sepolia perps release

`arbitrum-sepolia-v2.json` pins plether-core v1.2.1 (the filename describes the
bounded V2 order intent protocol). It includes all 26 contract addresses and
runtime hashes, immutable source provenance, and the release's bootstrap state.
The frontend and backend target this stack; indexers start at block 305627052.
The first full volume-history minute is 1788596760, derived from that block's
onchain timestamp (1788596732).

Source: https://github.com/Plether-Fi/plether-core/releases/tag/v1.2.1

The published release is deployed but unseeded and trading inactive. Before
shipping this configuration to live consumers, complete and verify bootstrap
and activation using the tagged core deployment runbook. The competition has
not started and is pinned exclusively to this deployment. Never combine multiple
deployments within one competition; this deployment has no live-state migration.
Protection creation remains disabled pending its separate readiness process.
This repository update does not deploy services or activate contracts.

Order intents remain `PletherOrderIntentV2`; receipts and execution configuration
use `PletherOrderReceiptV3` and `PletherExecutionConfigV3`. The AA manifest keeps
its required `-v2` suffix and changes its deployment date to invalidate prior
identity bindings.

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
