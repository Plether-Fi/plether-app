# Plether Backend API Specification

This document specifies the backend API for the Plether frontend. The API serves as a caching and aggregation layer between the frontend and Ethereum blockchain, protecting RPC credentials and improving performance.

## Overview

### Goals

1. **Protect Alchemy API key** - Keep RPC credentials server-side
2. **Speed up data loading** - Reduce 10+ RPC calls to 1-2 API calls per page
3. **Enable caching** - Cache blockchain data with appropriate TTLs
4. **Reduce RPC costs** - Batch calls via Multicall3, cache aggressively
5. **Provide transaction history** - Index events for historical data

### Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     Plether Frontend                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────┐  │
│  │ React Query  │  │   WebSocket  │  │  wagmi (tx signing)  │  │
│  │   Hooks      │  │   Client     │  │                      │  │
│  └──────┬───────┘  └──────┬───────┘  └──────────────────────┘  │
└─────────┼─────────────────┼─────────────────────────────────────┘
          │                 │
          ▼                 ▼
┌─────────────────────────────────────────────────────────────────┐
│                     Backend API Server                           │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────┐  │
│  │  REST API    │  │  WebSocket   │  │   Event Indexer      │  │
│  │  Handlers    │  │  Server      │  │                      │  │
│  └──────┬───────┘  └──────┬───────┘  └──────────┬───────────┘  │
│         │                 │                      │              │
│         ▼                 ▼                      ▼              │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │                    Redis Cache                            │  │
│  └──────────────────────────────────────────────────────────┘  │
│         │                                        │              │
│         ▼                                        ▼              │
│  ┌──────────────────────────┐    ┌──────────────────────────┐  │
│  │  Multicall3 Batcher      │    │      PostgreSQL          │  │
│  │  (RPC Optimization)      │    │   (Transaction History)  │  │
│  └──────────┬───────────────┘    └──────────────────────────┘  │
└─────────────┼───────────────────────────────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────────────────────────────┐
│              Ethereum RPC (Alchemy/Infura)                       │
│              Mainnet / Sepolia                                   │
└─────────────────────────────────────────────────────────────────┘
```

---

## Base URL

| Environment | URL |
|-------------|-----|
| Production | `https://api.plether.io` |
| Staging | `https://api-staging.plether.io` |
| Development | `http://localhost:3001` |

---

## Authentication

All endpoints are public (no authentication required). User-specific endpoints use the Ethereum address as a path parameter.

---

## Common Response Format

All responses follow this structure:

```typescript
interface ApiResponse<T> {
  data: T;
  meta: {
    cached: boolean;
    cachedAt?: number;      // Unix timestamp when data was cached
    blockNumber: number;    // Block number data is valid for
    chainId: number;        // 1 = mainnet, 11155111 = sepolia
  };
}
```

### Error Response

```typescript
interface ApiError {
  error: {
    code: string;           // Machine-readable error code
    message: string;        // Human-readable message
    details?: unknown;      // Additional context
  };
}
```

Error codes:
- `INVALID_ADDRESS` - Malformed Ethereum address
- `INVALID_AMOUNT` - Amount is zero or negative
- `INVALID_SIDE` - Side must be "bear" or "bull"
- `RPC_ERROR` - Upstream RPC failed
- `RATE_LIMITED` - Too many requests
- `FORBIDDEN` - Request did not arrive through the trusted Plether app proxy
- `UPGRADE_REQUIRED` - Faucet client must refresh and send the current request shape
- `INTERNAL_ERROR` - Server error
- `NETWORK_ERROR` - Upstream network failure
- `NOT_FOUND` - Requested resource does not exist

---

## Endpoints

### Testnet Faucet

`POST /api/testnet/faucet` is available to visitors through the official
Sepolia app proxy. Direct origin calls fail with HTTP 403. The current request
shape is:

```json
{
  "address": "0x...",
  "confirmationMode": "async"
}
```

Missing or unsupported confirmation modes return HTTP 426 with
`UPGRADE_REQUIRED`. Accepted requests share rolling-hour limits of 20 per
pseudonymous client IP and 200 globally; quota failures return HTTP 429 with a
`Retry-After` header. Successful `submitted`, `minted`, `already_claimed`, and
`already_funded` responses retain their existing shapes.

---

### Protocol Endpoints

#### `GET /api/protocol/status`

Returns current protocol state including prices, status, and aggregate statistics.

**Cache TTL:** 30 seconds (invalidated on new block)

**Response:**
```typescript
interface ProtocolStatus {
  prices: {
    bear: string;           // plDXY-BEAR price in USDC (8 decimals)
    bull: string;           // plDXY-BULL price in USDC (8 decimals)
    cap: string;            // Total cap price (8 decimals)
  };
  status: "ACTIVE" | "PAUSED" | "LIQUIDATED";
  oracle: {
    price: string;          // DXY oracle price (8 decimals)
    updatedAt: number;      // Unix timestamp of last update
    decimals: number;       // Always 8
  };
  staking: {
    bear: {
      totalAssets: string;  // Total plDXY-BEAR staked (18 decimals)
      totalShares: string;  // Total sDXY-BEAR shares (18 decimals)
      exchangeRate: string; // Assets per share (18 decimals)
    };
    bull: {
      totalAssets: string;
      totalShares: string;
      exchangeRate: string;
    };
  };
  apy: {
    bear: {
      supply: number;       // Lending supply APY (percentage)
      borrow: number;       // Lending borrow APY (percentage)
      utilization: number;  // Market utilization (0-1)
    };
    bull: {
      supply: number;
      borrow: number;
      utilization: number;
    };
  };
  timestamp: number;        // Server response timestamp
}
```

**Example:**
```bash
curl https://api.plether.io/api/protocol/status
```

---

#### `GET /api/protocol/config`

Returns static protocol configuration (addresses, constants).

**Cache TTL:** 1 hour

**Response:**
```typescript
interface ProtocolConfig {
  contracts: {
    usdc: string;
    dxyBear: string;
    dxyBull: string;
    sdxyBear: string;
    sdxyBull: string;
    syntheticSplitter: string;
    curvePool: string;
    zapRouter: string;
    leverageRouter: string;
    bullLeverageRouter: string;
    basketOracle: string;
    morpho: string;
    morphoBearMarket: string;
    morphoBullMarket: string;
  };
  decimals: {
    usdc: 6;
    plDxyBear: 18;
    plDxyBull: 18;
    oraclePrice: 8;
    morphoShares: 18;
  };
  constants: {
    maxSlippage: number;    // 100 = 1%
    minLeverage: number;    // 110 = 1.1x
    maxLeverage: number;    // 1000 = 10x
    liquidationLtv: number; // 860000000000000000 = 86%
  };
  chainId: number;
}
```

---

### User Endpoints

#### `GET /api/user/{address}/dashboard`

Returns all data needed for the Dashboard page in one call.

**Cache TTL:** 12 seconds (1 block)

**Path Parameters:**
- `address` - User's Ethereum address (0x...)

**Response:**
```typescript
interface UserDashboard {
  balances: {
    usdc: string;           // USDC balance (6 decimals)
    bear: string;           // plDXY-BEAR balance (18 decimals)
    bull: string;           // plDXY-BULL balance (18 decimals)
    stakedBear: string;     // sDXY-BEAR shares (18 decimals)
    stakedBull: string;     // sDXY-BULL shares (18 decimals)
    stakedBearAssets: string; // Underlying BEAR in staking (18 decimals)
    stakedBullAssets: string; // Underlying BULL in staking (18 decimals)
  };
  leverage: {
    bear: LeveragePosition | null;
    bull: LeveragePosition | null;
  };
  lending: {
    bear: LendingPosition | null;
    bull: LendingPosition | null;
  };
}

interface LeveragePosition {
  collateral: string;       // Collateral in plDXY tokens (18 decimals)
  collateralUsd: string;    // Collateral value in USDC (6 decimals)
  debt: string;             // USDC debt (6 decimals)
  healthFactor: string;     // Health factor (18 decimals, 1e18 = 1.0)
  liquidationPrice: string; // Price at which position liquidates (8 decimals)
  leverage: string;         // Current leverage multiplier (2 decimals, 200 = 2.0x)
  netValue: string;         // Collateral USD - debt (6 decimals)
}

interface LendingPosition {
  supplied: string;         // USDC supplied (6 decimals)
  suppliedShares: string;   // Morpho supply shares (18 decimals)
  borrowed: string;         // USDC borrowed (6 decimals)
  borrowedShares: string;   // Morpho borrow shares (18 decimals)
  availableToBorrow: string;// Max additional borrowable (6 decimals)
  collateral: string;       // plDXY tokens as collateral (18 decimals)
  healthFactor: string;     // Health factor (18 decimals)
}
```

**Example:**
```bash
curl https://api.plether.io/api/user/0x1234.../dashboard
```

---

#### `GET /api/user/{address}/balances`

Returns only token balances (lighter endpoint for polling).

**Cache TTL:** 12 seconds

**Response:**
```typescript
interface UserBalances {
  usdc: string;
  bear: string;
  bull: string;
  stakedBear: string;
  stakedBull: string;
  stakedBearAssets: string;
  stakedBullAssets: string;
}
```

---

#### `GET /api/user/{address}/positions`

Returns leverage and lending positions only.

**Cache TTL:** 12 seconds

**Response:**
```typescript
interface UserPositions {
  leverage: {
    bear: LeveragePosition | null;
    bull: LeveragePosition | null;
  };
  lending: {
    bear: LendingPosition | null;
    bull: LendingPosition | null;
  };
}
```

---

#### `GET /api/user/{address}/allowances`

Returns token allowances for protocol contracts.

**Cache TTL:** 5 minutes

**Query Parameters:**
- `spenders` - Comma-separated list: `splitter,zap,morpho,leverageBear,leverageBull,curveBear`

**Response:**
```typescript
interface UserAllowances {
  usdc: {
    splitter: string;       // Allowance for SyntheticSplitter
    zap: string;            // Allowance for ZapRouter
    morphoBear: string;     // Allowance for Morpho BEAR market
    morphoBull: string;     // Allowance for Morpho BULL market
  };
  bear: {
    splitter: string;
    staking: string;
    leverageRouter: string;
    curvePool: string;
  };
  bull: {
    splitter: string;
    staking: string;
    leverageRouter: string;
  };
}
```

---

### Quote Endpoints

All quote endpoints have a **5 second cache TTL** and accept amounts as query parameters.

#### `GET /api/quotes/mint`

Preview mint operation (USDC -> BEAR + BULL).

**Query Parameters:**
- `amount` - USDC amount (6 decimals)

**Response:**
```typescript
interface MintQuote {
  usdcIn: string;           // Input USDC (6 decimals)
  bearOut: string;          // Output plDXY-BEAR (18 decimals)
  bullOut: string;          // Output plDXY-BULL (18 decimals)
  pricePerToken: string;    // USDC per token pair (6 decimals)
}
```

---

#### `GET /api/quotes/burn`

Preview burn operation (BEAR + BULL -> USDC).

**Query Parameters:**
- `amount` - Pair amount to burn (18 decimals)

**Response:**
```typescript
interface BurnQuote {
  pairIn: string;           // Input pair amount (18 decimals)
  usdcOut: string;          // Output USDC (6 decimals)
  bearIn: string;           // Required BEAR (18 decimals)
  bullIn: string;           // Required BULL (18 decimals)
}
```

---

#### `GET /api/quotes/zap`

Preview zap buy/sell operation.

**Query Parameters:**
- `direction` - `buy` or `sell`
- `amount` - Input amount (6 decimals for buy, 18 decimals for sell)

**Response:**
```typescript
interface ZapQuote {
  direction: "buy" | "sell";
  input: {
    token: "usdc" | "bull";
    amount: string;
  };
  output: {
    token: "bull" | "usdc";
    amount: string;
    minAmount: string;      // After slippage
  };
  priceImpact: string;      // Price impact percentage (2 decimals, 100 = 1%)
  route: string[];          // Swap route description
}
```

---

#### `GET /api/quotes/trade`

Preview Curve pool swap.

**Query Parameters:**
- `from` - `usdc` or `bear`
- `amount` - Input amount

**Response:**
```typescript
interface TradeQuote {
  from: "usdc" | "bear";
  to: "bear" | "usdc";
  amountIn: string;
  amountOut: string;
  minAmountOut: string;     // After slippage
  spotPrice: string;        // Current pool spot price
  priceImpact: string;      // Percentage (2 decimals)
  fee: string;              // Pool fee amount
}
```

---

#### `GET /api/quotes/leverage`

Preview opening a leverage position.

**Query Parameters:**
- `side` - `bear` or `bull`
- `principal` - USDC principal amount (6 decimals)
- `leverage` - Leverage multiplier (2 decimals, 200 = 2.0x)

**Response:**
```typescript
interface LeverageQuote {
  side: "bear" | "bull";
  principal: string;        // Input USDC (6 decimals)
  leverage: string;         // Requested leverage
  positionSize: string;     // Resulting collateral (18 decimals)
  positionSizeUsd: string;  // Collateral in USDC terms (6 decimals)
  debt: string;             // USDC to borrow (6 decimals)
  healthFactor: string;     // Initial health factor (18 decimals)
  liquidationPrice: string; // Liquidation price (8 decimals)
  priceImpact: string;      // Swap price impact
  borrowRate: string;       // Current borrow APY
}
```

---

### History Endpoints

Transaction history requires the event indexer (Phase 3).

#### `GET /api/user/{address}/history`

Returns paginated transaction history.

**Query Parameters:**
- `page` - Page number (default: 1)
- `limit` - Items per page (default: 20, max: 100)
- `type` - Filter by type: `mint`, `burn`, `zap`, `stake`, `unstake`, `leverage_open`, `leverage_close`, `leverage_adjust`, `supply`, `withdraw`, `borrow`, `repay`
- `side` - Filter by side: `bear`, `bull`

**Response:**
```typescript
interface TransactionHistory {
  transactions: Transaction[];
  pagination: {
    page: number;
    limit: number;
    total: number;
    hasMore: boolean;
  };
}

interface Transaction {
  id: string;               // Transaction hash
  type: TransactionType;
  timestamp: number;        // Unix timestamp
  blockNumber: number;
  side?: "bear" | "bull";
  data: TransactionData;    // Type-specific data
  status: "success" | "failed";
}

type TransactionType =
  | "mint"
  | "burn"
  | "zap_buy"
  | "zap_sell"
  | "swap"
  | "stake"
  | "unstake"
  | "leverage_open"
  | "leverage_close"
  | "collateral_add"
  | "collateral_remove"
  | "supply"
  | "withdraw"
  | "borrow"
  | "repay";

// Type-specific data examples:
interface MintData {
  usdcIn: string;
  bearOut: string;
  bullOut: string;
}

interface LeverageOpenData {
  side: "bear" | "bull";
  principal: string;
  leverage: string;
  positionSize: string;
  debt: string;
}
```

---

#### `GET /api/user/{address}/history/leverage`

Returns leverage transaction history only.

**Query Parameters:**
- `side` - Filter by side: `bear`, `bull`
- `page`, `limit` - Pagination

---

#### `GET /api/user/{address}/history/lending`

Returns lending transaction history only.

**Query Parameters:**
- `side` - Filter by side: `bear`, `bull`
- `page`, `limit` - Pagination

---

### WebSocket Endpoint

#### `WS /api/ws`

Real-time updates for prices and user data.

**Query Parameters:**
- `address` - (Optional) User address for position updates

**Connection:**
```javascript
const ws = new WebSocket('wss://api.plether.io/api/ws?address=0x1234...');
```

**Server -> Client Messages:**

```typescript
// Price update (every block)
{
  type: "prices",
  data: {
    bear: string;
    bull: string;
    oracle: string;
    timestamp: number;
    blockNumber: number;
  }
}

// Protocol status change
{
  type: "status",
  data: {
    status: "ACTIVE" | "PAUSED" | "LIQUIDATED";
  }
}

// User balance update (when address provided)
{
  type: "balance",
  data: {
    token: "usdc" | "bear" | "bull" | "stakedBear" | "stakedBull";
    amount: string;
  }
}

// User position update (when address provided)
{
  type: "position",
  data: {
    type: "leverage" | "lending";
    side: "bear" | "bull";
    position: LeveragePosition | LendingPosition | null;
  }
}

// New block notification
{
  type: "block",
  data: {
    number: number;
    timestamp: number;
  }
}

// Heartbeat (every 30 seconds)
{
  type: "ping"
}
```

**Client -> Server Messages:**

```typescript
// Subscribe to user updates (alternative to query param)
{
  type: "subscribe",
  address: "0x..."
}

// Unsubscribe from user updates
{
  type: "unsubscribe"
}

// Heartbeat response
{
  type: "pong"
}
```

---

## Caching Strategy

### Cache Tiers

| Data Type | TTL | Invalidation Strategy |
|-----------|-----|----------------------|
| Protocol config | 1 hour | Manual deployment |
| Oracle price | 12 seconds | New block |
| Token prices | 12 seconds | New block |
| Protocol stats | 30 seconds | New block |
| APY calculations | 30 seconds | New block |
| User balances | 12 seconds | User tx or new block |
| User positions | 12 seconds | User tx or new block |
| Allowances | 5 minutes | User approval tx |
| Quotes | 5 seconds | Time-based |

### Cache Keys

```
protocol:config:{chainId}
protocol:prices:{chainId}
protocol:status:{chainId}
protocol:apy:{chainId}:{side}
user:{chainId}:{address}:balances
user:{chainId}:{address}:positions
user:{chainId}:{address}:allowances
quote:mint:{chainId}:{amount}
quote:trade:{chainId}:{from}:{amount}
quote:leverage:{chainId}:{side}:{principal}:{leverage}
```

### Block-Based Invalidation

Subscribe to new blocks via WebSocket. On each block:
1. Invalidate price caches
2. Broadcast price update to WebSocket clients
3. Invalidate user caches for users with pending transactions

---

## RPC Optimization

### Multicall3 Batching

Batch multiple `eth_call` operations into single RPC request:

```typescript
// Instead of 10 separate calls:
const [usdcBalance, bearBalance, bullBalance, ...] = await Promise.all([
  usdc.balanceOf(user),
  bear.balanceOf(user),
  bull.balanceOf(user),
  // ...7 more calls
]);

// Single multicall:
const results = await multicall3.aggregate3([
  { target: usdc, callData: balanceOf(user) },
  { target: bear, callData: balanceOf(user) },
  { target: bull, callData: balanceOf(user) },
  // ...all calls batched
]);
```

### Precomputation

Calculate derived values server-side to reduce frontend complexity:

- **Health factor**: `collateralValue / debt / lltv`
- **Liquidation price**: Price at which health factor = 1
- **Leverage ratio**: `positionValue / (positionValue - debt)`
- **USD values**: Token amounts * oracle prices
- **APY**: From Morpho interest rate model
- **Exchange rates**: Staking share to asset conversion

---

## Event Indexer

### Events to Index

| Contract | Event | Purpose |
|----------|-------|---------|
| SyntheticSplitter | `Minted(address indexed user, uint256 amount)` | Mint history |
| SyntheticSplitter | `Burned(address indexed user, uint256 amount)` | Burn history |
| ZapRouter | `ZapMint(address indexed user, uint256 usdcIn, uint256 bullOut)` | Zap buy history |
| ZapRouter | `ZapBurn(address indexed user, uint256 bullIn, uint256 usdcOut)` | Zap sell history |
| CurvePool | `TokenExchange(address indexed buyer, int128 sold_id, uint256 tokens_sold, int128 bought_id, uint256 tokens_bought)` | Swap history |
| LeverageRouter | `PositionOpened(address indexed user, uint256 principal, uint256 leverage, uint256 size, uint256 debt)` | Leverage open |
| LeverageRouter | `PositionClosed(address indexed user, uint256 collateral, uint256 debt)` | Leverage close |
| LeverageRouter | `CollateralAdded(address indexed user, uint256 amount)` | Add collateral |
| LeverageRouter | `CollateralRemoved(address indexed user, uint256 amount)` | Remove collateral |
| Morpho | `Supply(bytes32 indexed id, address indexed caller, address indexed onBehalf, uint256 assets, uint256 shares)` | Lending supply |
| Morpho | `Withdraw(bytes32 indexed id, address indexed caller, address indexed onBehalf, address receiver, uint256 assets, uint256 shares)` | Lending withdraw |
| Morpho | `Borrow(bytes32 indexed id, address indexed caller, address indexed onBehalf, address receiver, uint256 assets, uint256 shares)` | Lending borrow |
| Morpho | `Repay(bytes32 indexed id, address indexed caller, address indexed onBehalf, uint256 assets, uint256 shares)` | Lending repay |
| StakedToken | `Deposit(address indexed caller, address indexed owner, uint256 assets, uint256 shares)` | Stake |
| StakedToken | `Withdraw(address indexed caller, address indexed receiver, address indexed owner, uint256 assets, uint256 shares)` | Unstake |

### Database Schema

```sql
CREATE TABLE transactions (
    id SERIAL PRIMARY KEY,
    tx_hash VARCHAR(66) NOT NULL UNIQUE,
    log_index INTEGER NOT NULL,
    block_number BIGINT NOT NULL,
    block_timestamp BIGINT NOT NULL,
    chain_id INTEGER NOT NULL,
    user_address VARCHAR(42) NOT NULL,
    contract_address VARCHAR(42) NOT NULL,
    event_name VARCHAR(64) NOT NULL,
    tx_type VARCHAR(32) NOT NULL,
    side VARCHAR(4),
    data JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT NOW(),

    UNIQUE(tx_hash, log_index)
);

CREATE INDEX idx_transactions_user ON transactions(user_address, chain_id);
CREATE INDEX idx_transactions_timestamp ON transactions(block_timestamp DESC);
CREATE INDEX idx_transactions_type ON transactions(tx_type);
CREATE INDEX idx_transactions_side ON transactions(side) WHERE side IS NOT NULL;
CREATE INDEX idx_transactions_block ON transactions(block_number);
```

### Indexer Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Event Indexer Service                         │
│                                                                  │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────────┐  │
│  │ Block        │───▶│ Event        │───▶│ Transaction      │  │
│  │ Listener     │    │ Parser       │    │ Writer           │  │
│  └──────────────┘    └──────────────┘    └────────┬─────────┘  │
│         │                                          │            │
│         │           ┌──────────────┐               │            │
│         └──────────▶│ Checkpoint   │◀──────────────┘            │
│                     │ Manager      │                            │
│                     └──────────────┘                            │
└─────────────────────────┼───────────────────────────────────────┘
                          │
                          ▼
                   ┌──────────────┐
                   │  PostgreSQL  │
                   └──────────────┘
```

**Components:**

1. **Block Listener**: Subscribes to new blocks, triggers event fetching
2. **Event Parser**: Decodes events by signature, normalizes data
3. **Transaction Writer**: Batches writes to PostgreSQL
4. **Checkpoint Manager**: Tracks last processed block for restart recovery

---

## Rate Limiting

| Endpoint Pattern | Rate Limit |
|------------------|------------|
| `/api/protocol/*` | 100 req/min |
| `/api/user/*` | 60 req/min per address |
| `/api/quotes/*` | 30 req/min |
| WebSocket | 5 connections per IP |

Rate limit headers:
```
X-RateLimit-Limit: 60
X-RateLimit-Remaining: 45
X-RateLimit-Reset: 1706900060
```

---

## Error Handling

### RPC Errors

1. **Retry**: 3 attempts with exponential backoff (100ms, 500ms, 2s)
2. **Fallback**: Switch to secondary RPC if primary fails
3. **Stale data**: Return cached data with `stale: true` if all RPCs fail

### Response with Stale Data

```typescript
{
  data: { ... },
  meta: {
    cached: true,
    cachedAt: 1706899000,
    stale: true,              // Indicates data may be outdated
    blockNumber: 12345670,    // Last known good block
    chainId: 1
  }
}
```

---

## Migration Path

### Phase 1: Core API + Caching
1. Deploy API server with protocol and user endpoints
2. Implement Redis caching with TTLs
3. Implement Multicall3 batching
4. Frontend creates API client, migrates read operations

### Phase 2: WebSocket + Real-Time
1. Add WebSocket endpoint for price streaming
2. Implement block subscription for cache invalidation
3. Frontend subscribes to price updates

### Phase 3: Event Indexer + History
1. Deploy event listener and PostgreSQL
2. Backfill historical events from block 0
3. Add history endpoints
4. Frontend replaces mock history with real data

---

## Frontend Integration

### API Client Usage

```typescript
import { plethApi } from './api/client';

// Fetch dashboard data
const { data, meta } = await plethApi.getUserDashboard(address);

// Get quote
const quote = await plethApi.getMintQuote(amount);

// WebSocket subscription
plethApi.subscribeToPrices((prices) => {
  console.log('New prices:', prices);
});
```

### React Query Integration

```typescript
import { useProtocolStatus, useUserDashboard } from './api/hooks';

function Dashboard() {
  const { data: protocol } = useProtocolStatus();
  const { data: user } = useUserDashboard(address);

  // Data automatically refreshes based on staleTime
}
```

See `src/api/` for complete implementation.
