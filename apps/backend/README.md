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

For local perps work, run the API, the basket cache worker, and any UI servers as separate foreground processes in separate terminals. This keeps logs visible and makes it obvious which service failed.

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

The API should print the route list and start on:

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
PERPS_INDEXER_START_BLOCK=288439939 \
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

### 5. Optional: Start The On-Chain Oracle Updater

The frontend repo contains a small Node worker that reads cached Pyth payloads from the backend and submits `updateMarkPrice` transactions. This is the only service in this local stack that sends transactions.

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

### 6. Companion Frontend Services

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
| `PERPS_CHAIN_ID` | No | `421614` | Chain ID used for keeper transaction signing |
| `PERPS_USDC` | No | Arbitrum Sepolia deployment | Perps mock USDC minted by the testnet faucet |
| `PERPS_ORDER_ROUTER` | No | Arbitrum Sepolia deployment | Perps order router address |
| `PERPS_CFD_ENGINE` | No | Arbitrum Sepolia deployment | CFD engine allowed by the managed sponsorship policy |
| `PERPS_MARGIN_CLEARINGHOUSE` | No | Arbitrum Sepolia deployment | Margin clearinghouse allowed by the managed sponsorship policy |
| `PERPS_PLETHER_ORACLE` | No | Arbitrum Sepolia deployment | Plether oracle address for update fees and reveal window |
| `PERPS_INDEXER_START_BLOCK` | No | `288439939` | Arbitrum Sepolia perps release first block to start keeper/history indexing from |
| `AA_PROXY_ORIGIN_TOKEN` | With managed sponsorship | - | Shared secret required from the trusted Pages/Vite proxy |
| `PIMLICO_API_KEY` | With managed sponsorship | - | Server-only Pimlico API key |
| `PIMLICO_SPONSORSHIP_POLICY_ID` | With managed sponsorship | - | Server-injected Pimlico policy ID; browser context is replaced |
| `AA_SPONSORSHIP_ENABLED` | No | `false` | Authoritative issuance/submission kill switch; recovery reads remain available |
| `AA_IP_RATE_LIMIT_PER_MINUTE` | No | `120` | Per-IP issuance limit; recovery reads receive four times this budget |
| `AA_ACCOUNT_RATE_LIMIT_PER_MINUTE` | No | `30` | Per-Trading-Account-and-IP issuance limit; Pimlico policy budgets remain the global account control |
| `AA_MAX_REQUEST_BYTES` | No | `262144` | Maximum JSON-RPC request body size |
| `AA_SPONSORED_GAS_ALERT_WEI_PER_HOUR` | No | `0` | Actual sponsored gas-cost threshold logged once per hour; `0` disables it |
| `KEEPER_POLL_SECONDS` | No | `1` | Keeper polling interval |
| `KEEPER_MAX_BATCH_SIZE` | No | `20` | Maximum queued orders evaluated per iteration |
| `KEEPER_CONFIRMATIONS` | No | `1` | L2 confirmations before indexing order-router logs |
| `KEEPER_GAS_BUFFER_BPS` | No | `2000` | Gas-limit buffer for keeper submissions |
| `KEEPER_FEE_BUFFER_BPS` | No | `2500` | Fee buffer for keeper EIP-1559 fields |
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

Set `operations_alarm_sns_topic_arn` to route the Terraform-managed sponsored
gas and keeper-task CloudWatch alarms to an operations channel. Keep Pimlico's
policy-level budget alerts enabled as the authoritative view of sponsored gas;
the backend alert is a receipt-based secondary signal.

## API Endpoints

### Managed account abstraction

| Endpoint | Description |
|----------|-------------|
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

# Run with live reload (requires ghcid)
ghcid --command="cabal repl plether-api" --test=":main"
```

## Project Structure

```
apps/backend/
├── app/
│   └── Main.hs           # Entry point
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
