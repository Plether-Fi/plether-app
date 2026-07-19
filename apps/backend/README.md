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

# Run
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
  minutes per event type.
- Recurring worker warnings and errors emit at most once per minute per event
  type. The next emitted record includes `suppressed_count` so repeated failures
  remain visible without producing one log per poll.
- Important state changes such as startup, reorg detection, keeper order
  failures, and mined keeper transactions emit immediately. Repetitive oracle
  success/no-op states emit at most once every five minutes.
- FireLens suppresses repeated delivery diagnostics from each output for one
  minute, while unlimited OTLP retries avoid discarding a batch solely because
  a temporary PostHog outage exhausted a retry count.

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
psql plether < schema.sql

# Add DATABASE_URL to .env
echo 'DATABASE_URL=postgresql://localhost/plether' >> .env
```

The indexer runs automatically on startup and polls for new blocks every 12 seconds.

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
PERPS_INDEXER_START_BLOCK=273137426 \
cabal run plether-perps-indexer -- --loop
```

Useful one-off modes:

```bash
# Index one safe block range and exit.
RPC_URL="$ARB_SEPOLIA_RPC_URL" \
CHAIN_ID=421614 \
DATABASE_URL=postgresql://postgres@localhost:55432/plether \
cabal run plether-perps-indexer -- --once

# Backfill a known range.
RPC_URL="$ARB_SEPOLIA_RPC_URL" \
CHAIN_ID=421614 \
DATABASE_URL=postgresql://postgres@localhost:55432/plether \
cabal run plether-perps-indexer -- --backfill --from 123 --to 456
```

Notes:

- The indexer only writes finalized/safe history. Default finality delay is `120` blocks.
- Use `PERPS_INDEXER_RPC_URLS` with comma, space, or newline separated RPC URLs for fallback providers.
- It writes `perps_events`, `perps_orders`, `perps_account_activity`, and `perps_indexer_state`.
- Expired-order cleanup appears in Order History as `Expired / Cleaned up` and in Transaction History as `Cleaned up expired order`.

Useful checks:

```bash
curl http://127.0.0.1:3001/api/perps/indexer/status
curl "http://127.0.0.1:3001/api/perps/accounts/0xYOUR_ADDRESS/orders?limit=10"
curl "http://127.0.0.1:3001/api/perps/accounts/0xYOUR_ADDRESS/activity?limit=10"
```

### 5. Start The Liquidation Worker

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

The worker keeps its own low-confirmation discovery cursor and monotonic candidate registry. It verifies zero-size positions at a confirmed block, persists each signed transaction and signer before broadcast, and uses same-nonce fee bumps for stale transactions. Closed or already-liquidated candidates are removed only after confirmed CFD-engine state reports that no position remains. Do not rotate `LIQUIDATION_KEEPER_PRIVATE_KEY` while a transaction is pending; if that happens, the worker keeps the pending transaction as a circuit breaker and requires manual reconciliation instead of crossing signer nonce lanes.

### 6. Optional: Start The On-Chain Oracle Updater

The frontend repo contains a small Node worker that reads cached Pyth payloads from the backend and submits `updateMarkPrice` transactions independently of the keeper workers.

```bash
cd apps/frontend

ARBITRUM_SEPOLIA_RPC_URL="$ARB_SEPOLIA_RPC_URL" \
PERPS_ORACLE_UPDATER_BACKEND_URL=http://127.0.0.1:3001 \
PERPS_ORACLE_UPDATER_PRIVATE_KEY=0xYOUR_UPDATER_PRIVATE_KEY \
npm run perps:oracle-worker -- --loop
```

For a no-transaction check:

```bash
cd apps/frontend

DRY_RUN=true \
ARBITRUM_SEPOLIA_RPC_URL="$ARB_SEPOLIA_RPC_URL" \
PERPS_ORACLE_UPDATER_BACKEND_URL=http://127.0.0.1:3001 \
npm run perps:oracle-worker -- --once
```

Keep the basket worker running before starting the oracle updater. If the cached payload is older than the updater's freshness window, the updater will skip the transaction instead of pushing stale data onchain.

### 7. Companion Frontend Services

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
| `KEEPER_PRIVATE_KEY` | Keeper | - | Private key used by `plether-keeper` to submit executions |
| `LIQUIDATION_KEEPER_PRIVATE_KEY` | Liquidation worker | - | Separately funded private key used to submit liquidations and Pyth fees |
| `PERPS_CHAIN_ID` | No | `421614` | Chain ID used for keeper transaction signing |
| `PERPS_USDC` | No | Arbitrum Sepolia deployment | Perps mock USDC minted by the testnet faucet |
| `PERPS_ORDER_ROUTER` | No | Arbitrum Sepolia deployment | Perps order router address |
| `PERPS_PLETHER_ORACLE` | No | Arbitrum Sepolia deployment | Plether oracle address for update fees and reveal window |
| `PERPS_CFD_ENGINE` | No | Arbitrum Sepolia deployment | CFD engine address used for candidate discovery and position checks |
| `PERPS_INDEXER_START_BLOCK` | No | `273137426` | Arbitrum Sepolia perps release first log block to start keeper/history indexing from |
| `KEEPER_POLL_SECONDS` | No | `1` | Keeper polling interval |
| `KEEPER_MAX_BATCH_SIZE` | No | `20` | Maximum queued orders evaluated per iteration |
| `KEEPER_CONFIRMATIONS` | No | `1` | L2 confirmations before indexing order-router logs |
| `KEEPER_GAS_BUFFER_BPS` | No | `2000` | Gas-limit buffer for keeper submissions |
| `KEEPER_FEE_BUFFER_BPS` | No | `2500` | Fee buffer for keeper EIP-1559 fields |
| `LIQUIDATION_WORKER_POLL_SECONDS` | No | `1` | Delay between liquidation discovery and scan iterations |
| `LIQUIDATION_WORKER_SCAN_BATCH_SIZE` | No | `100` | Maximum candidate accounts checked per iteration |
| `LIQUIDATION_WORKER_START_BLOCK` | No | `PERPS_INDEXER_START_BLOCK` | CFD engine block where independent candidate discovery starts |
| `LIQUIDATION_WORKER_CONFIRMATIONS` | No | `1` | L2 confirmations before indexing position openings |
| `LIQUIDATION_WORKER_INDEX_BATCH_SIZE` | No | `5000` | Maximum discovery block span per iteration |
| `LIQUIDATION_WORKER_REORG_OVERLAP_BLOCKS` | No | `12` | Recently indexed blocks rescanned to heal short L2 reorgs |
| `LIQUIDATION_WORKER_PENDING_REPLACEMENT_SECONDS` | No | `120` | Age at which an unconfirmed transaction is fee-bumped at the same nonce |
| `LIQUIDATION_WORKER_GAS_BUFFER_BPS` | No | `KEEPER_GAS_BUFFER_BPS` | Gas-limit buffer for liquidation submissions |
| `LIQUIDATION_WORKER_FEE_BUFFER_BPS` | No | `KEEPER_FEE_BUFFER_BPS` | EIP-1559 fee buffer for liquidation submissions |
| `PERPS_INDEXER_RPC_URLS` | No | `RPC_URL` | Fallback RPC URL list for Perps history indexing |
| `PERPS_INDEXER_CONFIRMATIONS` | No | `120` | Blocks to wait before indexing Perps history |
| `PERPS_INDEXER_BATCH_SIZE` | No | `5000` | Maximum block span per Perps history indexing pass |
| `PERPS_INDEXER_POLL_SECONDS` | No | `12` | Perps history indexer loop delay when caught up |
| `PYTH_HERMES_URL` | No | `https://hermes.pyth.network` | Hermes endpoint used by the basket worker |
| `PYTH_API_KEY` | No | - | Optional bearer token for API-key backed Hermes providers |
| `PYTH_BENCHMARKS_URL` | No | `https://benchmarks.pyth.network` | Benchmarks endpoint used for historical backfills |
| `PYTH_BACKFILL_DAYS` | No | `7` | Default historical backfill window |
| `PYTH_SAMPLE_INTERVAL_SECONDS` | No | `60` | Historical backfill sample interval |
| `PYTH_INGESTION_ENABLED` | No | `false` | Legacy API-owned ingestion switch; prefer `plether-basket-worker` for local/prod parity |

## API Endpoints

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

Query params: `page`, `limit`, `type` (mint/burn/swap/etc.), `side` (bear/bull)

Perps history query params: `limit`, `cursor`. Cursor format is `blockNumber:tieBreaker` and is returned as `nextCursor` when another page may exist.

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

Responses are cached in-memory using STM. Cache invalidates when block number advances:

- `/protocol/status` - Global cache
- `/user/:address/dashboard` - Per-address cache
- `/user/:address/allowances` - Per-address cache

Cached responses include `meta.cached: true` and `meta.cachedAt` timestamp.

## Development

```bash
# Build with warnings
cabal build

# Run tests
cabal test

# Run the perps keeper once without submitting transactions
cabal run plether-keeper -- --once --dry-run

# Discover and simulate liquidations once without submitting transactions
cabal run plether-liquidation-worker -- --once --dry-run

# Run with live reload (requires ghcid)
ghcid --command="cabal repl plether-api" --test=":main"
```

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
