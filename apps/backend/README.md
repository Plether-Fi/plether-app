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

## Configuration

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `RPC_URL` | Yes | - | Ethereum RPC endpoint |
| `CHAIN_ID` | No | `11155111` | Chain ID (1=mainnet, 11155111=sepolia, 31337=local) |
| `PORT` | No | `3001` | Server port |
| `CORS_ORIGINS` | No | `http://localhost:5173` | Space-separated allowed origins |
| `DATABASE_URL` | No | - | PostgreSQL connection string (enables history) |
| `INDEXER_START_BLOCK` | No | `0` | Block to start indexing from (Sepolia: 10188700) |

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

Query params: `page`, `limit`, `type` (mint/burn/swap/etc.), `side` (bear/bull)

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
│   ├── addresses.mainnet.json
│   └── addresses.sepolia.json
├── schema.sql            # Database schema
└── test/
    └── Spec.hs
```

## License

AGPL-3.0-or-later
