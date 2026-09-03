# Plether API Backend

Haskell/Scotty backend API for the Plether DeFi protocol. Aggregates on-chain data from Ethereum to reduce frontend RPC calls.

## Prerequisites

- GHC 9.4+
- Cabal 3.0+
- Ethereum RPC endpoint (Alchemy, Infura, etc.)
- PostgreSQL 14+ (optional, for transaction history)

## Quick Start

```bash
# Copy environment file
cp .env.example .env

# Edit .env with your RPC URL
vim .env

# Build
cabal build

# Export the file, then run (the backend does not load dotenv files itself)
set -a
source .env
set +a
cabal run plether-api
```

Server starts at `http://localhost:3001`.

## ECS OpenTelemetry Logs

The ECS task definitions route application `stdout` and `stderr` through an
AWS for Fluent Bit FireLens container. The router enriches every record with
OpenTelemetry resource attributes, sends OTLP/HTTP logs to PostHog, and keeps a
second copy in the existing CloudWatch log group.

| ECS container | OpenTelemetry `service.name` |
|---------------|------------------------------|
| `plether-api` | `plether-api` |
| `plether-keeper` | `plether-keeper` |
| `plether-liquidation-worker` | `plether-liquidation-worker` |
| `plether-perps-indexer` | `plether-indexer` |
| `plether-basket-worker` | `plether-basket-worker` |
| `plether-oracle-worker` | `plether-oracle-worker` |

The router also sets `service.version` to the deployed Git commit and
`deployment.environment.name` to the Terraform environment. Consolidated
workers remain in one Fargate task, but run as separate containers so their
service identities do not get mixed together.

Application records use a shared JSON-line schema with `event`, `message`,
`level`, and typed context fields such as block ranges, order IDs, HTTP status,
durations, and transaction hashes. Messages are capped at 4 KiB, string
attributes at 2 KiB, arrays at 20 items, and URL paths are redacted so RPC API
keys cannot leak through exception text. Reserved envelope fields cannot be
overridden by call-site attributes.

The steady-state volume controls are:

- API success traffic is aggregated into at most one request summary per
  minute. Individual 5xx responses are limited to one every 10 seconds, and
  slow-request warnings to one per minute.
- Indexer and basket-cache success progress emits at most once every five
  minutes per event type. In candle dual-write mode, successful price and
  volume source polls also provide five-minute writer heartbeats even when a
  market is closed, a block range has no trades, or the indexer is caught up.
  Terraform alarms only after three consecutive five-minute windows contain no
  heartbeat. Each heartbeat also reports base-minute coverage state and excess
  finalization lag after subtracting normal bucket alignment and configured
  source lateness; this detects a live loop whose durable coverage has frozen
  without making hourly or daily candle resolution part of the liveness
  calculation.
- Recurring worker warnings and errors emit at most once per minute per event
  type. The next emitted record includes `suppressed_count` so repeated failures
  remain visible without producing one log per poll.
- Important state changes such as startup, reorg detection, keeper order
  failures, and mined keeper transactions emit immediately. Repetitive oracle
  success/no-op states emit at most once every five minutes.
- The liquidation worker emits structured discovery, opportunity, submission,
  replacement, confirmation, and circuit-breaker events. A successful-iteration
  heartbeat is emitted at most once every five minutes so missing-log alerts can
  detect a stalled or crash-looping worker; recurring RPC and candidate errors
  are limited to one per minute with a `suppressed_count` on recovery.
- FireLens suppresses repeated delivery diagnostics from each output for one
  minute, while unlimited OTLP retries avoid discarding a batch solely because
  a temporary PostHog outage exhausted a retry count.

The FireLens handoff explicitly maps structured severity fields, caps each
container's Docker-side queue at 4,096 records, reserves 128 MiB for the router,
and allows up to 120 seconds for router shutdown. PostHog delivery retries
without a fixed attempt limit, while the independent CloudWatch copy retries 15
times and provides a second place to recover logs during a PostHog outage.

The FireLens parser keeps plaintext output from third-party libraries as a
fallback, but first-party services should use the structured logger instead of
writing directly to `stdout` or `stderr`.

Set these Terraform variables before applying the ECS changes:

```hcl
posthog_project_token = "phc_YOUR_POSTHOG_PROJECT_TOKEN"
posthog_otlp_host     = "eu.i.posthog.com"
```

Use `us.i.posthog.com` for a US-hosted PostHog project. Terraform stores the
complete authorization header in SSM Parameter Store as a `SecureString`; ECS
injects it into the FireLens output through `secretOptions`, so it is not part
of the image or task definition JSON. Keep Terraform state encrypted and never
commit a populated tfvars file.

The backend deployment workflow builds and pushes both the application image
and `otel-log-router` image. Apply Terraform first so the ECR repository, SSM
parameter, IAM permissions, and FireLens-enabled task definition revisions
exist before running the workflow. For a brand-new environment, bootstrap the
ECR repositories and images before creating the ECS services, as both service
images must exist for the first task to start.

## Database Setup (Optional)

PostgreSQL is required for transaction history. Without it, history endpoints return 503.

```bash
# Create database
createdb plether

# Initialize schema
psql --set=ON_ERROR_STOP=1 plether < schema.sql

# Add DATABASE_URL to .env
echo 'DATABASE_URL=postgresql://localhost/plether' >> .env
```

The indexer runs automatically on startup and polls for new blocks every 12 seconds.

The static bootstrap creates the additive candle read-model and history-target
tables, but
intentionally does not build the Perps event/activity history indexes because a
fresh database does not have their source tables yet. After the API or Perps
indexer has initialized its history schema, run `plether-candle-admin migrate`;
the command validates both prerequisite tables and builds and verifies four
indexes concurrently: bounded-time backfill access plus block-number access for
reorg discovery and deletion on both history tables.
The protected workflow defaults `migrate` to a 60-second lock timeout; this
schema/index operation is distinct from candle data backfill.

Production backfill, repair, closed-price-gap recovery, and controlled indexer
replay run only through the protected `candle-admin.yml` workflow. They require
`PERPS_CANDLE_WRITE_MODE=dual` and enforce lock, statement, and absolute runtime
limits; backfill and repair also refuse an empty canonical source domain. The
admin and backend deployment workflows share an environment-specific
concurrency group so a deployment cannot change write mode during a mutation.
Replay is Sepolia-only, accepts an inclusive range of at most 5,000 blocks, and
runs from a stable deployed indexer digest without moving its canonical cursor
or coverage certification.

`recover-closed-price-gap` is also Sepolia-only. It is not a candle backfill:
it never inserts or rewrites a price. The one-shot basket worker requires an
empty authenticated Pyth minute-history range for all six feeds, validates the
latest payload through the deployed Pyth contract, matches that signed state to
the last stored signed observation, and permits the coverage-only publication
only inside the conservative Friday 22:00–Sunday 21:00 UTC frozen window. The
protected `from_timestamp` is the exact stored minute coverage terminal;
`to_timestamp` is an exclusive operator-approved deadline so a delayed approval
cannot cross into the live FX session.

An arbitrary price-history start is selected with the protected
`set-history-target` action. Selection is desired state only: the basket worker
bulk-fetches and proves the exact frozen range while the previous published
target remains live. After `status` reports `publication_ready=true`, run a
price-only backfill with no narrowed bounds; CandleAdmin builds any missing
rollups and atomically publishes coverage, generation, and the new active
target. Selecting an earlier start adds a history prefix, while selecting a
later start moves the public lower bound without deleting physical history.
This does not ingest old contract releases. Candles before the current router's
proven volume coverage expose unknown volume rather than zero.

## Local Perps Stack

For local perps work, run the API, basket cache, and relevant keeper workers as separate foreground processes in separate terminals. This keeps logs visible and makes it obvious which service failed.

### 1. Start PostgreSQL

The local Docker database used by this workspace is exposed on host port `55432` and uses the `postgres` role:

```bash
docker ps --format '{{.Names}} {{.Ports}} {{.Status}}'
```

Expected container:

```text
plether-postgres 0.0.0.0:55432->5432/tcp ... Up
```

Use this database URL with the backend services:

```bash
export DATABASE_URL=postgresql://postgres@localhost:55432/plether
```

If you run PostgreSQL directly on `5432` instead, adjust `DATABASE_URL` accordingly, for example `postgresql://localhost/plether`.

### 2. Start The Backend API

The perps frontend talks to the backend API for cached basket prices, historical chart data, reveal payloads, and older protocol endpoints.

```bash
cd apps/backend

RPC_URL="$ARB_SEPOLIA_RPC_URL" \
CHAIN_ID=421614 \
PORT=3001 \
CORS_ORIGINS="http://localhost:5173 http://127.0.0.1:5173" \
DATABASE_URL=postgresql://postgres@localhost:55432/plether \
cabal run plether-api
```

The API emits an `api_started` record and listens on:

```text
http://localhost:3001
```

Useful checks:

```bash
curl http://127.0.0.1:3001/api/perps/basket/latest
curl http://127.0.0.1:3001/api/perps/pyth/cached-latest
```

### 3. Start The Basket Worker

`plether-basket-worker` is separate from the API server. It keeps the six-feed Pyth basket cache fresh and stores the update payloads that self-execution can later use.

Latest loop:

```bash
cd apps/backend

RPC_URL="$ARB_SEPOLIA_RPC_URL" \
CHAIN_ID=421614 \
DATABASE_URL=postgresql://postgres@localhost:55432/plether \
cabal run plether-basket-worker -- --latest-loop
```

Useful one-off modes:

```bash
# Fetch one latest six-feed Hermes batch and exit.
RPC_URL="$ARB_SEPOLIA_RPC_URL" \
CHAIN_ID=421614 \
DATABASE_URL=postgresql://postgres@localhost:55432/plether \
cabal run plether-basket-worker -- --once

# Backfill historical chart data from Pyth Benchmarks.
RPC_URL="$ARB_SEPOLIA_RPC_URL" \
CHAIN_ID=421614 \
DATABASE_URL=postgresql://postgres@localhost:55432/plether \
cabal run plether-basket-worker -- --backfill-once --backfill-days 7
```

Notes:

- `--latest-loop` defaults to one batched six-feed Hermes request every `5s`.
- Hermes and Pyth Benchmarks requests use `PYTH_API_KEY`; the key must be entitled to all six basket feeds, including FX feeds.
- The known legacy `https://hermes.pyth.network` endpoint is rejected because its payloads cannot be verified by the deployed upgraded Pyth contract.
- On Hermes `429`, the worker backs off before polling again.
- The worker writes to `perps_basket_snapshots` and `perps_pyth_update_payloads`.
- The worker does not update the on-chain oracle by itself.

### 4. Start The Perps History Indexer

`plether-perps-indexer` owns Perps order and activity history. The frontend reads this indexed database history instead of scanning browser RPC logs.

```bash
cd apps/backend

RPC_URL="$ARB_SEPOLIA_RPC_URL" \
CHAIN_ID=421614 \
DATABASE_URL=postgresql://postgres@localhost:55432/plether \
PERPS_INDEXER_START_BLOCK=302257125 \
cabal run plether-perps-indexer -- --loop
```

Useful one-off modes:

```bash
# Index one safe block range and exit.
RPC_URL="$ARB_SEPOLIA_RPC_URL" \
CHAIN_ID=421614 \
DATABASE_URL=postgresql://postgres@localhost:55432/plether \
cabal run plether-perps-indexer -- --once

# Production replay is dispatched only through the protected workflow from
# master, with exact inclusive block bounds and scope=none.
gh workflow run candle-admin.yml \
  --repo Plether-Fi/plether-app \
  --ref master \
  -f environment=sepolia \
  -f action=replay \
  -f scope=none \
  -f from_block=123 \
  -f to_block=456 \
  -f confirmation='RUN REPLAY ON SEPOLIA'
```

Notes:

- The indexer only writes finalized/safe history. Default finality delay is `1` block.
- Never use the legacy `--backfill --from ... --to ...` invocation for an
  operational replay. It lacks the protected workflow's range, topology,
  digest, deadline, cancellation, and cleanup guardrails.
- Use `PERPS_INDEXER_RPC_URLS` with comma, space, or newline separated RPC URLs for fallback providers.
- Exact execution economics use Alchemy `debug_traceTransaction` through
  `PERPS_INDEXER_RPC_URLS`. Trace failures remain non-authoritative pending
  evidence and are surfaced to monitoring.
- It writes `perps_events`, `perps_orders`, `perps_account_activity`, and `perps_indexer_state`.
- Every activity row retains the normalized emitting contract address. Re-indexing
  safely fills this provenance for matching legacy rows; rows whose emitter cannot
  be proven remain untrusted for competition cash-flow scoring.
- Expired-order cleanup appears in Order History as `Expired / Cleaned up` and in Transaction History as `Cleaned up expired order`.

Useful checks:

```bash
curl http://127.0.0.1:3001/api/perps/indexer/status
curl "http://127.0.0.1:3001/api/perps/accounts/0xYOUR_ADDRESS/orders?limit=10"
curl "http://127.0.0.1:3001/api/perps/accounts/0xYOUR_ADDRESS/activity?limit=10"
```

### 5. Start The Insights Snapshot Worker

`plether-insights-worker` reads every registered trading account at the same
confirmation-delayed block. It persists the baseline, live, and final account
ledger snapshots used by the public leaderboard.

```bash
cd apps/backend

RPC_URL="$ARB_SEPOLIA_RPC_URL" \
PERPS_RPC_URL="$ARB_SEPOLIA_RPC_URL" \
CHAIN_ID=421614 \
PERPS_CHAIN_ID=421614 \
PERPS_USDC=0x1647e41f49ED6D688936092B5a291c4B28106343 \
PERPS_ORDER_ROUTER=0x97A901dE2B267c307E264FD5F71403F8072F73e7 \
PERPS_ORDER_LIFECYCLE_BOOK=0xa210928a7E0AE27626B8d0E67Bbd82305438aB9E \
PERPS_MARGIN_CLEARINGHOUSE=0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211 \
INSIGHTS_SNAPSHOT_MULTICALL_SIZE=10 \
DATABASE_URL=postgresql://postgres@localhost:55432/plether \
cabal run plether-insights-worker
```

By default the worker groups ten account-lens reads into each exact-block
Multicall3 request and processes those chunks sequentially. A failed subcall or
malformed response discards the whole snapshot batch. Set
`INSIGHTS_SNAPSHOT_MULTICALL_SIZE=0` to restore one direct `eth_call` per
participant; values above `100` are rejected at startup.

Insights persists these configured contract addresses with the competition.
Deposit and withdrawal adjustments count only events emitted by that exact
MarginClearinghouse for that exact mock-USDC asset.

Register the scored Plether Trading Account (which may differ from the
controlling wallet) and manage the post-competition review with the audited
admin CLI. `TRADER_REFERENCE` must be a stable, opaque identifier from the
private registration system; it enforces one entry per beneficial trader and is
never returned by the public API or the `list` command.

```bash
# In a second terminal with the same RPC, chain, and database variables set:
cabal run plether-insights-admin -- register testnet-trading-2026-09 TRADER_REFERENCE 0xTRADING_ACCOUNT "Public alias"
cabal run plether-insights-admin -- list testnet-trading-2026-09
cabal run plether-insights-admin -- review testnet-trading-2026-09 0xTRADING_ACCOUNT eligible reviewer-name
cabal run plether-insights-admin -- finalize testnet-trading-2026-09 reviewer-name
```

The optional review reason is public leaderboard copy, so keep it generic (for
example, `competition rules violation`). Store private investigation evidence
in the restricted review system, not in this CLI field. Legacy development rows
without a trader reference must be re-registered before finalization.

`finalize` is a one-way, audited transition. It fails closed until the scoring
cutoff has passed, the canonical boundary blocks and complete baseline/final
snapshot batches exist (with one common final hash), every participant has a
private trader reference, and every review is resolved to `eligible` or
`ineligible`. Only reviewed, mechanically qualified
participants receive prize places. Exact final-P&L ties share the combined paid
places equally.

Keep the Perps history indexer running first. The Insights worker deliberately
uses its finalized cursor so account snapshots and event-derived statistics
share one canonical upper bound.

Production registration, review, finalization, deployment order, and payout
checks are documented in `../../specs/insights-operations-runbook.md`.

#### Competition metadata safety

The first Insights process to use a database inserts the competition's network,
contract addresses, UTC schedule, scoring versions, eligibility thresholds, and
prizes. Later API, worker, and admin starts validate that immutable seed instead
of rewriting it. Startup fails with the exact mismatched fields if deployed
configuration or code would reinterpret an existing leaderboard.

Treat that failure as a release/configuration error: restore the configuration
and code that originally seeded the competition, or introduce a deliberately
versioned competition under a new slug. Never edit or delete a live competition
row to bypass the check. A disposable pre-launch database can be reset
explicitly only when its competition data is no longer needed.

The one known development seed correction—from the old
`2026-08-09T23:59:59Z` payout timestamp to the configured published deadline—is
migrated automatically only while it is the sole mismatch and the row is
unfinalized with neither resolved boundary blocks nor account snapshots. If any
of those conditions is not met, startup fails for manual review rather than
changing historical meaning.

### 6. Start The Liquidation Worker

`plether-liquidation-worker` independently discovers accounts from CFD engine `PositionOpened` events, verifies current position state onchain, and simulates the canonical liquidation call with the latest cached Pyth payload. It submits only when that simulation succeeds.

Keep the basket worker running so a current six-feed payload is available. Use a separately funded signer to avoid nonce contention with the order keeper:

```bash
cd apps/backend

PERPS_RPC_URL="$ARB_SEPOLIA_RPC_URL" \
DATABASE_URL=postgresql://postgres@localhost:55432/plether \
LIQUIDATION_KEEPER_PRIVATE_KEY=0xYOUR_LIQUIDATION_KEEPER_PRIVATE_KEY \
cabal run plether-liquidation-worker
```

For one discovery and scan iteration without submitting a transaction:

```bash
PERPS_RPC_URL="$ARB_SEPOLIA_RPC_URL" \
DATABASE_URL=postgresql://postgres@localhost:55432/plether \
LIQUIDATION_KEEPER_PRIVATE_KEY=0xYOUR_LIQUIDATION_KEEPER_PRIVATE_KEY \
cabal run plether-liquidation-worker -- --once --dry-run
```

The worker keeps its own low-confirmation discovery cursor and monotonic candidate registry. It verifies zero-size positions at a confirmed block, persists each signed transaction and signer before broadcast, and uses same-nonce fee bumps for stale transactions. Deterministically rejected Pyth payloads and unaffordable signer transactions open persistent retry circuits; raw pending broadcasts and fresh signer repricing are bounded to one attempt per minute. Closed or already-liquidated candidates are removed only after confirmed CFD-engine state reports that no position remains. Do not rotate `LIQUIDATION_KEEPER_PRIVATE_KEY` while a transaction is pending; if that happens, the worker keeps the pending transaction as a circuit breaker and requires manual reconciliation instead of crossing signer nonce lanes.

### 7. Optional: Start The On-Chain Oracle Updater

The frontend repo contains a small Node worker that reads cached Pyth payloads from the backend and submits `updateMarkPrice` transactions independently of the keeper workers.

```bash
cd apps/frontend

ARBITRUM_SEPOLIA_RPC_URL="$ARB_SEPOLIA_RPC_URL" \
PERPS_ORACLE_UPDATER_BACKEND_URL=http://127.0.0.1:3001 \
PERPS_ORACLE_UPDATER_PRIVATE_KEY=0xYOUR_UPDATER_PRIVATE_KEY \
npm run perps:oracle-worker -- --loop
```

Use a dedicated funded updater key. Sharing this key with the order keeper or
liquidation worker creates a cross-process nonce race.

For a no-transaction check:

```bash
cd apps/frontend

DRY_RUN=true \
ARBITRUM_SEPOLIA_RPC_URL="$ARB_SEPOLIA_RPC_URL" \
PERPS_ORACLE_UPDATER_BACKEND_URL=http://127.0.0.1:3001 \
npm run perps:oracle-worker -- --once
```

Keep the basket worker running before starting the oracle updater. If the cached payload is older than the updater's freshness window, the updater will skip the transaction instead of pushing stale data onchain.

### 8. Companion Frontend Services

The API and workers can run without UI servers, but the usual local perps development stack is:

```bash
# Trading UI
cd apps/frontend
npm run dev -- --host 127.0.0.1

# Storybook
cd apps/frontend
npm run storybook

# Landing page
cd apps/landing
npm run dev -- --host 127.0.0.1 --port 5174
```

Local URLs:

| Service | URL |
|---------|-----|
| Trading UI | `http://127.0.0.1:5173/` |
| Landing page | `http://127.0.0.1:5174/` |
| Storybook | `http://127.0.0.1:6006/` |
| Backend API | `http://127.0.0.1:3001/` |

### Troubleshooting

| Symptom | Fix |
|---------|-----|
| `role "stan" does not exist` | Use `postgresql://postgres@localhost:55432/plether`; libpq otherwise defaults to your macOS username. |
| Basket/history endpoints return `DATABASE_URL is not configured` | Start the API with `DATABASE_URL` set. |
| Currency cards are stale | Keep `plether-basket-worker -- --latest-loop` running. |
| On-chain DXY price is stale | Run the optional oracle updater with `PERPS_ORACLE_UPDATER_PRIVATE_KEY`; the basket worker only updates the database cache. |
| Order or transaction history is stale | Keep `plether-perps-indexer -- --loop` running and check `/api/perps/indexer/status`. |
| Liquidatable positions are not being processed | Keep both `plether-basket-worker -- --latest-loop` and `plether-liquidation-worker` running; verify that the liquidation signer has native ETH. |
| Browser CORS error from `127.0.0.1:5173` | Include `http://127.0.0.1:5173` in `CORS_ORIGINS`. |

## Configuration

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `RPC_URL` | Yes | - | Ethereum RPC endpoint |
| `CHAIN_ID` | No | `11155111` | Chain ID (1=mainnet, 11155111=sepolia, 421614=Arbitrum Sepolia, 31337=local) |
| `PORT` | No | `3001` | Server port |
| `CORS_ORIGINS` | No | `http://localhost:5173` | Space-separated allowed origins |
| `DATABASE_URL` | No | - | PostgreSQL connection string (enables history) |
| `INDEXER_START_BLOCK` | No | `0` | Block to start indexing from (Sepolia: 10188700) |
| `PERPS_RPC_URL` | Keeper/faucet | - | Arbitrum Sepolia RPC endpoint for perps services and testnet faucet |
| `RPC_AUTH_TOKEN` | No | - | Optional bearer token for `RPC_URL`; keeps provider credentials out of endpoint URLs |
| `PERPS_RPC_AUTH_TOKEN` | No | - | Optional bearer token for `PERPS_RPC_URL`; intentionally separate from `RPC_AUTH_TOKEN` |
| `KEEPER_PRIVATE_KEY` | Keeper | - | Private key used by `plether-keeper` to submit executions |
| `LIQUIDATION_KEEPER_PRIVATE_KEY` | Liquidation worker | - | Separately funded private key used to submit liquidations and Pyth fees |
| `PERPS_CHAIN_ID` | No | `421614` | Chain ID used for keeper transaction signing |
| `VAULT_HISTORY_HOUSE_POOL_ADDRESS` | No | Arbitrum Sepolia HousePool deployment | HousePool identity used to isolate vault-performance snapshots across deployments |
| `VAULT_HISTORY_SENIOR_VAULT_ADDRESS` | No | Arbitrum Sepolia Senior Vault deployment | Senior TrancheVault read at each hourly performance checkpoint |
| `VAULT_HISTORY_JUNIOR_VAULT_ADDRESS` | No | Arbitrum Sepolia Junior Vault deployment | Junior TrancheVault read at each hourly performance checkpoint |
| `VAULT_HISTORY_DEPLOYMENT_BLOCK` | No | `302257125` | Earliest block eligible for the configured vault deployment's history |
| `VAULT_HISTORY_CONFIRMATIONS` | No | `12` | Blocks subtracted from the live head before sampling; avoids unsupported `safe`/`finalized` tags and short reorgs |
| `PERPS_USDC` | No | Arbitrum Sepolia deployment | Perps mock USDC minted by the testnet faucet |
| `PERPS_ORDER_ROUTER` | No | Arbitrum Sepolia deployment | Perps order router address |
| `PERPS_ORDER_LIFECYCLE_BOOK` | With managed sponsorship | `0xa210928a7E0AE27626B8d0E67Bbd82305438aB9E` | Pinned V2 lifecycle-book address used for canonical intent and finalization receipts |
| `PERPS_HOUSE_POOL` | No | v1.2.0 Arbitrum Sepolia HousePool | HousePool identity verified against the Settlement Monitor facade at keeper startup |
| `PERPS_SENIOR_VAULT` | With active LP settlement | v1.2.0 Arbitrum Sepolia Senior Vault | Must match both the Settlement Monitor and HousePool binding |
| `PERPS_JUNIOR_VAULT` | With active LP settlement | v1.2.0 Arbitrum Sepolia Junior Vault | Must match both the Settlement Monitor and HousePool binding |
| `PERPS_SETTLEMENT_MONITOR_LENS` | No | v1.2.0 Arbitrum Sepolia facade | Operational LP settlement facade; never configure the monitor sidecar |
| `PERPS_CFD_ENGINE` | No | Arbitrum Sepolia deployment | CFD engine allowed by the managed sponsorship policy and used for liquidation discovery |
| `PERPS_CFD_ENGINE_SETTLEMENT_SIDECAR` | No | Arbitrum Sepolia deployment | Settlement sidecar authenticated when decoding exact execution economics from call traces |
| `PERPS_MARGIN_CLEARINGHOUSE` | No | Arbitrum Sepolia deployment | Margin clearinghouse allowed by managed sponsorship and authoritative for scored mock-USDC transfers |
| `PERPS_PLETHER_ORACLE` | No | Arbitrum Sepolia deployment | Plether oracle address for update fees and reveal window |
| `PERPS_ACCOUNT_LENS` | No | Arbitrum Sepolia deployment | Account lens used for exact-block Insights snapshots and liquidation candidate prefiltering |
| `PERPS_INDEXER_START_BLOCK` | No | `302257125` | Arbitrum Sepolia perps release first block to start keeper/history indexing from |
| `FAUCET_PRIVATE_KEY` | Faucet | - | Arbitrum Sepolia mock-USDC signer; configuring it also requires the dedicated proxy token |
| `FAUCET_PROXY_ORIGIN_TOKEN` | With faucet signer | - | Dedicated secret required from the exact trusted Pages/Vite faucet proxy path |
| `FAUCET_CLIENT_REQUESTS_PER_HOUR` | No | `20` | Rolling-hour accepted request limit per pseudonymous trusted client IP |
| `FAUCET_GLOBAL_REQUESTS_PER_HOUR` | No | `200` | Rolling-hour accepted request limit across the single API process |
| `AA_PROXY_ORIGIN_TOKEN` | With managed sponsorship | - | Shared secret required from the trusted Pages/Vite proxy |
| `PIMLICO_API_KEY` | With managed sponsorship | - | Server-only Pimlico API key |
| `PIMLICO_SPONSORSHIP_POLICY_ID` | With managed sponsorship | - | Server-injected Pimlico policy ID; browser context is replaced |
| `AA_SPONSORSHIP_ENABLED` | No | `false` | Authoritative issuance/submission kill switch; enable only after `/api/aa/status` verifies the bounded-V2 release |
| `AA_IP_RATE_LIMIT_PER_MINUTE` | No | `120` | Per-IP issuance limit; recovery reads receive four times this budget |
| `AA_ACCOUNT_RATE_LIMIT_PER_MINUTE` | No | `30` | Per-Trading-Account-and-IP issuance limit; Pimlico policy budgets remain the global account control |
| `AA_MAX_REQUEST_BYTES` | No | `262144` | Maximum JSON-RPC request body size |
| `AA_SPONSORED_GAS_ALERT_WEI_PER_HOUR` | No | `0` | Actual sponsored gas-cost threshold logged once per hour; `0` disables it |
| `KEEPER_POLL_SECONDS` | No | `1` | Keeper polling interval while pending orders exist |
| `KEEPER_IDLE_POLL_SECONDS` | No | `5` | Keeper polling interval while its durable pending queue is empty; must be at least `KEEPER_POLL_SECONDS` |
| `KEEPER_MAX_BATCH_SIZE` | No | `20` | Maximum queued orders evaluated per iteration |
| `KEEPER_CONFIRMATIONS` | No | `1` | L2 confirmations before indexing order-router logs |
| `KEEPER_GAS_BUFFER_BPS` | No | `2000` | Gas-limit buffer for keeper submissions |
| `KEEPER_FEE_BUFFER_BPS` | No | `2500` | Fee buffer for keeper EIP-1559 fields |
| `LP_SETTLEMENT_MODE` | No | `off` | `off`, read/simulate-only `observe`, or durable `execute`; legacy `LP_SETTLEMENT_ENABLED=true` is rejected |
| `LP_SETTLEMENT_PRIVATE_KEY` | With active LP settlement or preflight | - | Separately funded signer; must differ from order, liquidation, and oracle-updater keys |
| `LP_SETTLEMENT_POLL_SECONDS` | No | `15` | Exact interval between active LP settlement cycles; `observe` and `execute` reject any value other than `15` |
| `LP_SETTLEMENT_MAX_DRAIN_TRANSACTIONS` | No | `4` | Maximum confirmed settlement transactions drained from a fresh observation cycle |
| `LP_SETTLEMENT_PENDING_REPLACEMENT_SECONDS` | No | `60` | Age at which an unconfirmed durable transaction is replaced at the same nonce |
| `LP_SETTLEMENT_MAX_REPLACEMENTS` | No | `3` | Maximum same-nonce fee replacements before the nonce lane requires manual review |
| `LP_SETTLEMENT_MAX_TX_COST_WEI` | Execute mode | `0` | Hard maximum of transaction value plus gas-limit times max-fee; execute requires a positive observed-derived cap |
| `LIQUIDATION_WORKER_POLL_SECONDS` | No | `600` | Delay between full liquidation discovery/health scans; submitted transactions are still reconciled every 60 seconds |
| `LIQUIDATION_WORKER_SCAN_BATCH_SIZE` | No | `1000` | Maximum candidate accounts checked per iteration |
| `LIQUIDATION_WORKER_MULTICALL_SIZE` | No | `10` | Account-lens reads per Multicall3 request (`1`–`100`) |
| `LIQUIDATION_WORKER_EXECUTION_BATCH_SIZE` | No | `20` | Candidate accounts per `executeLiquidationBatch` transaction (`1`–`256`); one Pyth update is shared by the batch |
| `LIQUIDATION_WORKER_START_BLOCK` | No | `PERPS_INDEXER_START_BLOCK` | CFD engine block where independent candidate discovery starts |
| `LIQUIDATION_WORKER_CONFIRMATIONS` | No | `1` | L2 confirmations before indexing position openings |
| `LIQUIDATION_WORKER_INDEX_BATCH_SIZE` | No | `5000` | Maximum discovery block span per iteration |
| `LIQUIDATION_WORKER_REORG_OVERLAP_BLOCKS` | No | `12` | Recently indexed blocks rescanned to heal short L2 reorgs |
| `LIQUIDATION_WORKER_PENDING_REPLACEMENT_SECONDS` | No | `120` | Age at which an unconfirmed transaction is fee-bumped at the same nonce |
| `LIQUIDATION_WORKER_GAS_BUFFER_BPS` | No | `KEEPER_GAS_BUFFER_BPS` | Gas-limit buffer for liquidation submissions |
| `LIQUIDATION_WORKER_FEE_BUFFER_BPS` | No | `KEEPER_FEE_BUFFER_BPS` | EIP-1559 fee buffer for liquidation submissions |
| `PERPS_INDEXER_RPC_URLS` | No | `RPC_URL` | Fallback RPC URL list for Perps history indexing |
| `PERPS_INDEXER_CONFIRMATIONS` | No | `1` | Blocks to wait before indexing Perps history |
| `PERPS_INDEXER_BATCH_SIZE` | No | `5000` | Maximum block span per Perps history indexing pass |
| `PERPS_INDEXER_POLL_SECONDS` | No | `12` | Perps history indexer delay after every successful iteration |
| `INSIGHTS_SNAPSHOT_POLL_SECONDS` | No | `60` | Insights finalized account snapshot interval (minimum `10`) |
| `INSIGHTS_SNAPSHOT_MULTICALL_SIZE` | No | `10` | Exact-block account-lens reads per Multicall3 request (`1`–`100`); set to `0` to use direct calls |
| `INSIGHTS_ACTIVE_COMPETITION_SLUG` | No | `testnet-trading-2026` | Exact versioned competition selected for seeding, current APIs, and snapshots |
| `INSIGHTS_COMPETITION_RELEASE_ID` | September release binding | - | Omit during registration-only activation. After contract deployment, set it to `testnet-trading-2026-09` together with explicit nonzero, pairwise-distinct addresses absent from the July manifest and a positive new indexer start; the release then binds once before the baseline and becomes immutable. |
| `PYTH_HERMES_URL` | No | `https://pyth.dourolabs.app/hermes` | Upgraded Hermes endpoint used by the API and basket worker |
| `PYTH_API_KEY` | With hosted Pyth endpoints | - | Server-only bearer token sent to Hermes, Benchmarks, and Pyth Pro History, entitled to all six basket feeds including FX; blank values fail before a hosted Hermes request |
| `PYTH_BENCHMARKS_URL` | No | `https://benchmarks.pyth.network` | Benchmarks endpoint used for signed historical update payloads |
| `PYTH_HISTORY_URL` | No | `https://pyth.dourolabs.app/v1` | Pyth Pro History API used for OHLC backfills and closed-market recovery evidence; the retired Benchmarks TradingView shim is not supported |
| `PYTH_BACKFILL_DAYS` | No | `7` | Default historical backfill window |
| `PYTH_SAMPLE_INTERVAL_SECONDS` | No | `60` | Historical backfill sample interval |
| `PYTH_LATEST_MAX_AGE_SECONDS` | No | `10` | Maximum age accepted when promoting a latest Hermes payload to the cache; values above `10` are rejected to preserve headroom below the oracle's 15-second staleness limit |
| `PYTH_INGESTION_ENABLED` | No | `false` | Legacy API-owned ingestion switch; prefer `plether-basket-worker` for local/prod parity |
| `PERPS_CANDLE_WRITE_MODE` | No | `off` | OHLCV write mode: `off` keeps legacy-only ingestion; `dual` writes legacy data and rollups |
| `PERPS_CANDLE_READ_MODE` | No | `legacy` | Candle API read mode: `legacy` keeps rollup routes closed; `rollup` enables allowlisted intervals only with strict coverage. `shadow` is reserved and currently performs no comparison or traffic switch. |
| `PERPS_CANDLE_READ_INTERVALS` | No | empty | Comma/space-separated canonical intervals eligible for strict rollup reads; empty exposes no rollup interval |
| `PERPS_CANDLE_SHADOW_SAMPLE_BPS` | No | `0` | Reserved for a future bounded shadow comparison; currently has no runtime effect (`0`–`10000`) |
| `PERPS_CANDLE_STRICT_COVERAGE` | No | `true` | Mandatory public rollup validation switch. Rollup routes fail closed unless this is `true`; native history validates price coverage while legacy compatibility remains bounded by combined price/volume coverage. |
| `PERPS_CANDLE_LATENESS_SECONDS` | No | `120` | Source-watermark lateness window before price candles may be finalized (`0`–`86400`) |
| `PERPS_CANDLE_FINALIZATION_GRACE_SECONDS` | No | `15` | Bounded reader grace for the asynchronous writer to publish an eligible finalized watermark (`0`–`60`). This never exposes rows beyond the stored finalized watermark. |

Vault performance history, vault activity, UserOperation receipt recovery, and
transaction tracing all use the server-side Alchemy `PERPS_RPC_URL` (or the
explicit server-only Perps RPC list). Run `plether-provider-preflight` before a
rollout to verify archive calls, vault logs, Bundler receipts, and Debug support.
The command is read-only and does not acquire an indexer lock or mutate the
database:

```bash
cabal run plether-provider-preflight
```

For Terraform deployments, prefer `pyth_api_key_ssm_parameter_name` to reference
an existing SecureString. To let Terraform manage the key instead, set
`enable_pyth_api_key = true` and provide the sensitive `pyth_api_key`. Apply
Terraform before rolling the API and worker services. The image-only backend
deployment workflow reuses existing task-definition environment and secret
settings, so it does not apply this endpoint migration by itself. Its preflight
refuses a normal rollout until the API and a basket worker have the upgraded
RPC/contract wiring and the referenced key successfully fetches the exact six
configured feeds; use the manual bootstrap override only for first-time
task-definition provisioning.

Review the Terraform plan before changing an existing environment from a
Terraform-managed Pyth parameter to `pyth_api_key_ssm_parameter_name`. If the
managed resource already owns the same SSM name, migrate or detach its state
without destroying the SecureString first; `prevent_destroy` intentionally
blocks accidental key deletion.

For the Sepolia managed proxy, keep `provision_aa_proxy = true` even when
`enable_aa_sponsorship = false`; this preserves Pimlico receipt/status access
while the issuance kill switch is active. The public API origin must use the
certificate-backed `https://` hostname configured by `api_hostname` and
`alb_certificate_arn`.

After changing the Terraform AA variables:

1. Point the certificate-backed API hostname at the ALB.
2. Apply Terraform so the SSM parameters and latest task-definition revision
   exist.
3. Run the `Deploy Backend` workflow for `sepolia` so ECS activates that
   revision.
4. Set Pages `SEPOLIA_BACKEND_URL` to the HTTPS API hostname and use the same
   `AA_PROXY_ORIGIN_TOKEN` in Pages and the backend.

Set `operations_alarm_sns_topic_arn` to route all Terraform-managed service,
database, candle, sponsored-gas, and keeper alarms to an operations channel;
it is required for `mainnet`. Keep Pimlico's
policy-level budget alerts enabled as the authoritative view of sponsored gas;
the backend alert is a receipt-based secondary signal.

## API Endpoints

### Managed account abstraction

| Endpoint | Description |
|----------|-------------|
| `GET /api/aa/status` | Public bounded-V2 release fingerprint, startup binding verification state, and sponsorship kill-switch state |
| `POST /api/aa/pimlico` | Authenticated, fail-closed Pimlico JSON-RPC proxy for the approved Arbitrum Sepolia SimpleAccount and Plether action surface |

### Protocol

| Endpoint | Description |
|----------|-------------|
| `GET /api/protocol/status` | Prices, oracle data, staking stats |
| `GET /api/protocol/config` | Contract addresses, decimals, constants |

### User

| Endpoint | Description |
|----------|-------------|
| `GET /api/user/:address/dashboard` | Balances + positions (aggregated) |
| `GET /api/user/:address/balances` | Token balances only |
| `GET /api/user/:address/positions` | Leverage/lending positions |
| `GET /api/user/:address/allowances` | Token approvals |

### Quotes

| Endpoint | Description |
|----------|-------------|
| `GET /api/quotes/mint?amount=` | Mint quote (USDC amount in wei) |
| `GET /api/quotes/burn?amount=` | Burn quote (token amount in wei) |
| `GET /api/quotes/zap?direction=&amount=` | Zap quote (buy/sell) |
| `GET /api/quotes/trade?from=&amount=` | Trade quote (usdc/bear) |
| `GET /api/quotes/leverage?side=&principal=&leverage=` | Leverage quote |

### History (requires PostgreSQL)

| Endpoint | Description |
|----------|-------------|
| `GET /api/user/:address/history` | Transaction history |
| `GET /api/user/:address/history/leverage` | Leverage positions only |
| `GET /api/user/:address/history/lending` | Lending activity only |
| `GET /api/perps/accounts/:address/orders` | Indexed Perps order history |
| `GET /api/perps/accounts/:address/activity` | Indexed Perps transaction history |
| `GET /api/perps/indexer/status` | Perps history indexer cursor/status |
| `GET /api/perps/vaults/history?range=7d&interval=3600` | Alchemy-backed historical vault performance |
| `GET /api/perps/vaults/activity` | Confirmed holder balances and newest deposit/withdraw requests for both tranches; returns `503` until the first full backfill |
| `GET /api/perps/vaults/:tranche/accounts/:address/request-ids?limit=&cursor=` | Confirmed request IDs newest-first for Public Lens hydration; limit defaults to 100 and is capped at 250 |
| `GET /api/perps/basket/history?range=&interval=` | Legacy sampled basket history retained during the rollup migration; both query parameters are required exactly once |
| `GET /api/perps/basket/candles?interval=&cursor=` | Finalized OHLCV rollups in a fixed 500-bucket window ending at the exclusive cursor |
| `GET /api/perps/basket/candles/current?interval=` | Mutable current OHLCV candle and dataset generation |

Query params: `page`, `limit`, `type` (mint/burn/swap/etc.), `side` (bear/bull)

Perps history query params: `limit`, `cursor`. Cursor format is `blockNumber:tieBreaker` and is returned as `nextCursor` when another page may exist.

Candle intervals are restricted to `60`, `180`, `300`, `900`, `1800`, `3600`,
and `86400` seconds. Intervals and historical candle cursors use their unique
positive decimal representation: signs, whitespace, and leading zeroes are
rejected. Historical candle cursors are positive Unix timestamps
aligned to `interval * 500`; responses are ascending and expose
`previousCursor`, coverage/finalization watermarks, and `datasetGeneration`.
Historical pages contain finalized price rows only. `volumeUsdc` and
`tradeCount` are nullable on both native historical and mutable current candles:
before current-router volume coverage, null means unknown; inside complete
coverage, zero means the indexer proved no trades in that bucket. Per-candle
`complete` remains the legacy combined value `priceComplete && volumeComplete`;
therefore a valid pre-router price candle intentionally has `complete: false`.
Native chart consumers use page-level price coverage and `priceComplete`, with
`volumeComplete` interpreted independently. Native candle responses identify
the immutable basket definition with
`seriesId`, `configurationHash`, and the lossless `displayPriceCap`, plus the
current volume scope in `volumeChainId` and normalized `volumeRouter`. The
same response exposes trusted `volumeCoverageStart`, `volumeCoverageEnd`,
`volumeFinalizedThrough`, and `volumeCoverageComplete` for that exact scope;
unusable or absent volume coverage is represented by three null bounds and
`volumeCoverageComplete: false`. OHLC
fields use explicit `raw*Price` names and lossless decimal strings.

When strict rollup reads are enabled for an effective interval, the legacy
`/basket/history` route is served from bounded candle pages and performs no raw
snapshot/volume scan. Oversized requests snap upward to the smallest canonical
resolution that keeps the response bounded (for example, 30-day minute requests
use five-minute rollups and one-year minute requests use hourly rollups).
The route accepts canonical `range` values (`24h`, `7d`, `30d`, or `1y`) and a
canonical positive decimal `interval`, each exactly once. `includeComponents`
may appear at most once and must be exactly `true` or `false`; missing,
duplicate, unknown, or malformed query parameters return `400` before database
access.
Component-bearing history remains on the legacy source because candle rollups
do not store per-component point metadata, and is therefore accepted only for
the UI's bounded `range=24h`, `interval=3600`,
`includeComponents=true` request. Other component shapes return `400` rather
than starting an unbounded raw-source scan. This compatibility response does
not query market activity: its point `volumeUsdc` values are deliberately zero
and non-authoritative, and its volume query timing/row metrics remain zero.
`GET /api/perps/market/stats` is the authoritative source for rolling 24-hour
volume. Browsers refresh the component payload no more often than every five
minutes.

### Insights (requires PostgreSQL)

| Endpoint | Description |
|----------|-------------|
| `GET /api/insights/v1/competitions/current` | Current competition rules and schedule |
| `GET /api/insights/v1/competitions/:slug/leaderboard` | Net account P&L standings and eligibility state |
| `GET /api/insights/v1/competitions/:slug/wallets/:address` | Net score, directional realized P&L, and competition activity |
| `GET /api/insights/v1/status` | Snapshot and Perps indexer coverage |

Leaderboard query params: `limit` and integer offset `cursor`. Wallet detail
accepts `activityLimit`.

`finalPnlUsdc` is the cash-flow-adjusted net competition result. The separate
`realizedPnlUsdc` field sums directional close/liquidation P&L before execution
fees, VPI, carry, rewards, and manual competition adjustments.

## Response Format

All responses follow this structure:

```json
{
  "data": { ... },
  "meta": {
    "blockNumber": 12345678,
    "chainId": 11155111,
    "cached": false,
    "cachedAt": 1234567890,
    "stale": false
  }
}
```

## Caching

Responses are cached in-memory using STM:

- `/protocol/status` - Global cache invalidated when the block advances
- `/user/:address/dashboard` - Per-address cache invalidated when the block advances
- `/user/:address/allowances` - Per-address cache invalidated when the block advances
- `/api/perps/basket/candles` - Successful finalized pages only, for five seconds
  with a 64-page process-local bound; concurrent requests for the same page
  share one database load.
- `/api/perps/basket/candles/current` - Coherent raw snapshots only, for 850ms
  from the start of their load and keyed by chain, router, interval, and mutable
  bucket. Concurrent misses for the same bucket share one database load, while
  every request still composes and strictly validates its response against its
  own sampled clock. Explicit `no-cache`/`no-store` requests force a database
  revalidation. Coalesced request time is exposed separately as
  `plether_singleflight_wait` in `Server-Timing`.

Cached responses include `meta.cached: true` and `meta.cachedAt` timestamp.

## Development

```bash
# Build with warnings
cabal build

# Run tests
cabal test

# Run the deterministic Perps critical-path gate against an isolated PostgreSQL database.
# The database name must contain "critical_path"; the suite refuses any other database.
PERPS_CRITICAL_PATH_REQUIRED=1 \
PERPS_CRITICAL_PATH_DATABASE_URL=postgresql://postgres:postgres@127.0.0.1:5432/plether_critical_path \
  cabal test plether-api-integration-test --test-show-details=direct -j1

# Run the perps keeper once without submitting transactions
cabal run plether-keeper -- --once --dry-run

# Verify the LP settlement deployment, signer balance, monitor schema, and any
# currently eligible settlement simulation without locks or database writes
cabal run plether-keeper -- --lp-settlement-preflight

# Discover and simulate liquidations once without submitting transactions
cabal run plether-liquidation-worker -- --once --dry-run

# Run with live reload (requires ghcid)
ghcid --command="cabal repl plether-api" --test=":main"
```

See [the LP settlement keeper runbook](../../docs/runbooks/lp-settlement-keeper.md)
for the Sepolia activation, cost-cap, alarm, manual-review, and rollback procedure.

The critical-path gate runs the real Perps history indexer and HTTP API against
PostgreSQL and an in-process scripted chain. It covers delayed Alchemy trace
evidence, finalized-value stability, stale evidence guards, stale
keeper suppression, and canonical reorg replacement. In CI, its PostgreSQL
prerequisites are mandatory; missing configuration or an unexpected RPC request
fails the job rather than silently skipping it.

## Project Structure

```
apps/backend/
├── app/
│   ├── Main.hs           # API entry point
│   ├── Keeper.hs         # FIFO order keeper entry point
│   └── LiquidationWorker.hs # Liquidation worker entry point
├── src/Plether/
│   ├── Api.hs            # Scotty routes
│   ├── Cache.hs          # STM caching
│   ├── Config.hs         # Environment config
│   ├── Database.hs       # PostgreSQL connection pool
│   ├── Database/         # Schema & queries
│   ├── Indexer.hs        # Event indexer runner
│   ├── Indexer/          # Event parsing & contracts
│   ├── Types/            # API types
│   ├── Handlers/         # Route handlers
│   ├── Ethereum/         # RPC client & contracts
│   ├── LiquidationWorker.hs # Liquidation discovery and execution
│   └── Utils/            # Helpers
├── config/
│   ├── addresses.arbitrum-sepolia.json
│   ├── addresses.mainnet.json
│   └── addresses.sepolia.json
├── schema.sql            # Database schema
└── test/
    └── Spec.hs
```

## License

AGPL-3.0-or-later
