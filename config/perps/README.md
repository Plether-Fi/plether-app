# Perps release configuration

## Frontend active deployment

The frontend ships with the Arbitrum Sepolia release described below. A different
release requires a complete, reviewed JSON object in the **build-time**
`VITE_PERPS_DEPLOYMENT_JSON` variable. Leaving the variable unset retains the
shipped Sepolia deployment. An empty, malformed, partial, or unsupported value
fails validation; individual contracts never fall back to Sepolia addresses.
No mainnet deployment is supplied by this repository change.

The schema is defined and validated by
`apps/frontend/src/contracts/perpsDeployment.ts`:

- `schemaVersion`: `1`.
- `releaseId`: a nonempty identifier using letters, numbers, periods, hyphens,
  or underscores, up to 128 characters.
- `chainId`: `42161` (Arbitrum One) or `421614` (Arbitrum Sepolia).
- `deploymentBlock`: the positive safe-integer first deployment block for
  scanning this release's history.
- `contracts`: exactly `pyth`, `usdc`, `perpsPublicLens`, `marginClearinghouse`,
  `orderRouter`, `orderRouterAdmin`, `cfdEngine`, `cfdEnginePlanner`,
  `cfdEngineSettlementSidecar`, `cfdEngineAdmin`, `housePool`, `seniorVault`,
  `juniorVault`, `pletherOracle`, `cfdEngineLens`, `cfdEngineAccountLens`,
  `orderLifecycleBook`, `policyEvaluator`, and `positionProtectionBook`, each
  with its nonzero deployed address.
- `runtimeCodeHashes`: exactly `orderRouter`, `orderLifecycleBook`, and
  `positionProtectionBook`, each with its verified 32-byte runtime code hash.
- `closePreview`: exactly `address`, `runtimeCodeHash`, `chainId`, `cfdEngine`,
  `orderRouter`, and `policyEvaluator`. The chain and three core addresses must
  match the selected deployment. The preview address must differ from the
  execution evaluator. Mainnet cannot reuse the shipped Sepolia preview as an
  implicit fallback.

Populate those fields from the verified release packet, then provide its JSON
as one environment variable to the frontend build. The parser checks structure
and internal consistency; it does not establish that supplied addresses are
deployed. Before releasing a build, verify deployment provenance and live code.
Order preparation checks the active AA manifest's core addresses and RPC chain,
then checks the immutable graph at one block. Close review and position
protection additionally verify their configured runtime hashes.

`PERPS_ACTIVE_DEPLOYMENT`, `PERPS_CONTRACTS`, `PERPS_CHAIN_ID`, `PERPS_CHAIN`, and
`PERPS_DEPLOYMENT_BLOCK` are the current frontend registry exports. Existing
`PERPS_ARBITRUM_SEPOLIA*` names remain compatibility aliases **to the active
deployment**, including when its chain is Arbitrum One. New code should use the
network-neutral names. Arbitrum One browser RPC uses `VITE_ARBITRUM_RPC_URL` or
the chain's public default; `VITE_MAINNET_RPC_URL` still means Ethereum mainnet.

This variable selects the frontend read and trading graph. A release also needs
matching backend, AA manifest, sponsorship service, indexer, and optional funding
manifest configuration. The funding manifest alone does not switch perps reads
or trading. Changing a frontend build does not deploy contracts, migrate balances,
or enable a bridge provider. Native sponsorship and close-assistance capabilities
remain subject to their separately reviewed service configuration.

## Shipped Arbitrum Sepolia release

`arbitrum-sepolia-v2.json` pins plether-core v1.2.3 (the filename describes the
bounded V2 order intent protocol). It includes all 27 contract addresses and
runtime hashes, immutable source provenance, and the original deployment snapshot.
The default frontend and the pinned backend target this stack; indexers start at block 307397196.
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

## Backend release selection

The backend embeds one complete release artifact at compile time through
`Plether.Perps.Manifest`. With `PLETHER_PERPS_RELEASE_MANIFEST` unset, it uses
`config/perps/arbitrum-sepolia-v2.json` through the existing repository/Docker
path resolution. To build for another reviewed release, set that **build-time**
variable to the artifact's absolute path or a path relative to `apps/backend`
(the compiler's working directory). An explicitly empty path, unreadable file,
missing consumed field, malformed contract address/hash, or unsupported chain
fails compilation. Supported release chains are Arbitrum One (`42161`) and
Arbitrum Sepolia (`421614`). The artifact must retain the complete existing
schema, including `contracts.mockUsdc` as the historical settlement-token field
name; this field name does not make mainnet USDC a mintable mock.

Changing the selected path or its build environment requires a **clean rebuild**;
environment changes alone do not invalidate Cabal's compilation cache. Cabal
tracks the selected JSON file as a dependency after compilation. Docker builds
must copy the selected artifact into the build context and provide the variable
during compilation. The existing Dockerfile continues to ship the default
artifact. This variable has no runtime effect.

Runtime contract addresses remain separately configured and must match the
compiled release. Managed and native AA reject a different `PERPS_CHAIN_ID`;
RPC attestations, Pimlico routing, preparation requests, paymaster/EntryPoint
hashes, recovery credentials, reconciler cursors, and sponsorship health queries
use the compiled chain. Sepolia signature domains and existing preparation
fingerprints are preserved. Mainnet fingerprints and cryptographic domains are
separate even when addresses or operation fields are identical. Use a separate
backend/database deployment for a different release: AA authorization and budget
tables are not a shared multi-chain ledger.

This selection does not enable every operational feature on mainnet. Close
assistance, `single-provider-sepolia`, the mock-token faucet, active LP settlement,
the vault activity indexer, and the September 2026 competition retain explicit
Sepolia restrictions. Native sponsorship remains disabled by default and needs
its complete reviewed service, contract, signer, and independent-RPC setup.
No mainnet deployment or provider configuration is included here.

Indexer format names are defined in `Plether.Perps.IndexerFormat`, independently
of addresses. The configured lifecycle-book protocol selects the worker format;
current competition queries use bounded V2. The archived July competition retains
its original V1 format. Every cursor and lock remains scoped to one competition's
immutable chain and router.
