# Keepers and permissionless execution

Keepers are independent operators that submit permissionless transactions advancing Plether from one valid onchain state to the next. There is no allowlist, registration process or privileged onchain keeper role — any funded address submitting a valid call acts as keeper for that transaction.

Keepers never custody trader or LP[^lp] funds and cannot choose arbitrary fills. Smart contracts independently verify every call against queue order, Pyth[^pyth] proofs, price limits and solvency checks.

## What keepers do across Plether

Plether separates liveness responsibilities into dedicated, unprivileged services (all reference operators in `Plether-Fi/plether-app`):

| Worker | Implementation | Responsibility |
| --- | --- | --- |
| **Order keeper** (`plether-keeper`) | Haskell (`apps/backend/app/Keeper.hs`) | Processes the global FIFO[^fifo] order queue (`executeOrder` / `executeOrderBatch`) with historical Pyth proofs and cleans up expired queue heads (`expireOrder`) |
| **LP settlement keeper** *(embedded in `plether-keeper`)* | Haskell (`LP_SETTLEMENT_MODE`) | Settles matured hourly LP vault epochs in strict 4-phase order: **1. Senior withdraw → 2. Junior withdraw → 3. Junior deposit → 4. Senior deposit** |
| **Liquidation worker** (`plether-liquidation-worker`) | Haskell (`apps/backend/app/LiquidationWorker.hs`) | Indexes `PositionOpened`, verifies account health onchain, simulates liquidations with fresh Pyth payloads and submits valid full liquidations |
| **TP/SL protection worker** (`plether-position-protection-worker`) | Node.js (`apps/backend/protection-worker/main.mjs`) | Indexes `PositionProtectionBook`, triggers armed TP/SL exits (queuing a full close into FIFO) and retries latched `Expired` close attempts |
| **Basket cache worker** (`plether-basket-worker`) | Haskell (`apps/backend`) | Polls Hermes every `5s` for batched 6-feed Pyth updates and caches signed payloads in PostgreSQL |
| **Oracle mark updater** (`perps-oracle-worker.mjs`) | Node.js ([`apps/frontend/scripts/perps-oracle-worker.mjs`](https://github.com/Plether-Fi/plether-app/blob/master/apps/frontend/scripts/perps-oracle-worker.mjs)) | Optional updater that polls every `5s` and refreshes `PletherOracle`'s accepted live basket onchain every `30s` (`50s` max payload age) |

## What a keeper can and cannot control

Keepers provide **liveness**, not pricing discretion. Contracts enforce:

* **Strict FIFO:** Keepers must start at `nextExecuteId` and cannot skip an eligible head.
* **Post-commit historical pricing:** Live/FAD execution requires the first eligible Pyth observation strictly after commitment inside the settlement window.
* **Market-state rules, acceptable prices & risk checks:** Keepers cannot override `oracleFrozen`, trader execution limits, margin rules or pool solvency.

The web app routes execution and expiry cleanup to Plether's keepers so traders never pay ETH; direct permissionless callers can always invoke the same contracts with their own tooling.

## Keeper incentives and operating costs (Core v2.1.0)

| Action | Keeper reward | Operator costs |
| --- | --- | --- |
| **Execute or clean up an open / increase** | `1 bp` of commit notional, clamped **`0.01–0.25 USDG`**[^usdg] (credited to caller's Margin Account) | Network gas + Pyth update fee (for priced execution) |
| **Execute or clean up a reduction / close** | **`0 USDG`** (including TP/SL closes) | Network gas + Pyth update fee |
| **Trigger an armed TP/SL protection** | **`0.25 USDG`** | Network gas + Pyth update fee |
| **Liquidate an unhealthy account** | **`50%`** of collected `0.10%` liquidation charge (min **`0.50 USDG`**; remaining `50%` goes to LPs, `0%` to protocol) | Network gas + Pyth update fee |
| **Settle hourly LP epoch** | No direct USDG bounty (protocol liveness maintenance) | Network gas + Pyth update fee (when fresh mark needed) |

Rewards are never guaranteed profit — gas spikes, reverts or keeper races can exceed rewards.

### Hermes data access vs onchain Pyth update fees

* **Hermes API access** is an offchain dependency (`PYTH_HERMES_URL`, optionally `PYTH_API_KEY`) that fetches signed 6-feed Pyth payloads. Contracts verify Pyth's cryptographic signatures regardless of which Hermes-compatible server supplied the bytes (you may point `PYTH_HERMES_URL` at Pyth's hosted endpoint, a third-party provider or a self-hosted Hermes instance).
* **Onchain Pyth update fees** are paid in native ETH inside the transaction alongside network gas. Review Pyth's [Hermes guidance](https://docs.pyth.network/price-feeds/core/how-pyth-works/hermes) and [current onchain fees](https://docs.pyth.network/price-feeds/core/current-fees).

## Running the Core v2.1.0 reference workers

### Single-source manifest verification (31 contracts)

In Core v2.1.0, `plether-keeper`, `plether-liquidation-worker` and `protection-worker` no longer rely on ad-hoc per-contract address environment variables. Instead, the checked-in **schema-4 release manifest** (`config/perps/arbitrum-sepolia-v2.1.0.json`, pinning all **31 contract addresses and runtime bytecode hashes** plus `deploymentBlock: 333192048`) is embedded at compile time (Haskell `Plether.Perps.Manifest`) or loaded directly (Node.js workers), and startup audits RPC chain ID (`421614`), bytecode hashes and contract bindings fail-closed. See [Deployments & live parameters](../reference/deployments-and-parameters.md).

### 1. Shared configuration (Haskell workers)

Requires GHC 9.4+, Cabal 3.0+, PostgreSQL and an Arbitrum Sepolia RPC endpoint. Use **separate dedicated funded EOAs** for every worker (never reuse keys across order, liquidation, LP settlement, protection or oracle workers):

```bash
export DATABASE_URL=postgresql://USER:PASSWORD@HOST:PORT/DATABASE
export PERPS_RPC_URL=https://YOUR_ARBITRUM_SEPOLIA_RPC
export CHAIN_ID=421614
export PERPS_CHAIN_ID=421614
export PYTH_HERMES_URL=https://hermes.pyth.network
# export PYTH_API_KEY=YOUR_SERVER_SIDE_API_KEY   # if required by provider
export KEEPER_PRIVATE_KEY=0xYOUR_ORDER_KEEPER_KEY
```

### 2. Run the basket cache & order keeper

Keep the Pyth payload cache running in its own process:

```bash
RPC_URL=$PERPS_RPC_URL \
cabal run plether-basket-worker -- --latest-loop
```

Dry-run, then start the continuous order keeper:

```bash
cabal run plether-keeper -- --once --dry-run
cabal run plether-keeper
```

### 3. Enable the embedded LP settlement keeper (Arbitrum Sepolia)

The LP epoch settlement worker runs on its own thread, database connection, advisory lock and signer inside `plether-keeper`. It supports three modes via `LP_SETTLEMENT_MODE`:

* **`off`** — disabled (default; required on non-Sepolia networks)
* **`observe`** — audits invariants, builds and simulates exact settlement transactions without signing or broadcasting
* **`execute`** — persists signed intents in PostgreSQL before broadcast and recovers/replaces at the same nonce across restarts

Active modes (`observe` / `execute`) require `LP_SETTLEMENT_POLL_SECONDS=15` and a dedicated funded `LP_SETTLEMENT_PRIVATE_KEY`. Always pass the read-only preflight gate first:

```bash
export LP_SETTLEMENT_MODE=observe
export LP_SETTLEMENT_POLL_SECONDS=15
export LP_SETTLEMENT_PRIVATE_KEY=0xYOUR_DEDICATED_LP_SETTLEMENT_KEY

cabal run plether-keeper -- --lp-settlement-preflight
```

Require exit code `0` (`lp_settlement_preflight_succeeded`) before switching to `LP_SETTLEMENT_MODE=execute`.

### 4. Run the liquidation worker

```bash
LIQUIDATION_KEEPER_PRIVATE_KEY=0xYOUR_DEDICATED_LIQUIDATION_KEY \
cabal run plether-liquidation-worker -- --once --dry-run

LIQUIDATION_KEEPER_PRIVATE_KEY=0xYOUR_DEDICATED_LIQUIDATION_KEY \
cabal run plether-liquidation-worker
```

### 5. Run the TP/SL protection worker (Node.js)

[`apps/backend/protection-worker/main.mjs`](https://github.com/Plether-Fi/plether-app/blob/master/apps/backend/protection-worker/main.mjs) runs independently of the Haskell workers using the same `DATABASE_URL`, `PERPS_RPC_URL` and Core v2.1.0 manifest:

* **Observe-only by default:** `PROTECTION_WORKER_EXECUTION_ENABLED=false` (never signs transactions unless explicitly enabled with a dedicated funded `PROTECTION_WORKER_PRIVATE_KEY`).
* **Operational behavior:** Indexes `PositionProtectionBook` events **12 confirmations behind head** (with reorg rollback/replay), evaluates up to 50 candidates every **`5 seconds`**, requires fresh cached Pyth payloads (max `15s` old), submits one simulated transaction per action (no fee-bumping), and automatically retries **only** latched `Expired` close attempts (other failure reasons require operator review).

## Keepers are executors, not administrators

A keeper's authority begins and ends with submitting permissionless calls anyone can submit. Keepers cannot alter parameters, bypass FIFO or oracle validation, or move user funds — **open execution, enforced entirely onchain.**

[^lp]: Liquidity provider, a participant that supplies USDG capital to the liquidity pool.
[^fifo]: First in, first out; orders at the front of the queue are processed before later orders.
[^pyth]: Plether's external price-data provider for the six currency feeds used to calculate its index.
[^usdg]: USDG is used for margin and settlement. The testnet uses MockUSDG, a test token with no real-world value.
