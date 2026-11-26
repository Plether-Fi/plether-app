# Plether Insights — Trader & LP Protocol Intelligence Specification

## 1. Scope & Product Decisions

Plether Insights (`apps/insights`) is Plether's **Perps-only Protocol & Infrastructure Intelligence portal** designed for **Traders** and **Liquidity Providers (Senior & Junior LPs)**.

### Locked Decisions
1. **Homepage (`/`):** Protocol Pulse (executive overview across execution quality, LP settlement health, protocol verification, infrastructure readiness, and Divisions activity). Plether Divisions lives at `/divisions` while preserving `/results`, `/methodology`, `/register`, `/divisions/wallets/:address`, and `/archive/september-2026`.
2. **Protocol Scope:** **Perps only** — Arbitrum Sepolia (`chainId: 421614`) today, architected to support **Arbitrum One (`chainId: 42161`)** without frontend forks or hardcoded testnet URLs.
3. **Zero Duplication with Main App (`apps/frontend`):** Do **not** replicate views already present on the main trading app:
   - Main app `/vaults` already owns Senior/Junior TVL, 7d APY, 7d/30d share-price & asset charts, the 8-component Junior Return Breakdown waterfall, current pool exposure/sensitivity, and holder/deposit-withdrawal tables. Insights instead owns **`/liquidity` (LP Ops & Risk Forensics)**: hourly LP epoch settlement execution & decoded blocker/warning/fault masks, attribution & snapshot freshness forensics, and multi-window pool flow/liquidation-split analytics, with outbound links to `app.*.plether.com/vaults`.
   - Main app `/perps` already owns the TradingView price chart, 6 FX Pyth component rail, market phase banner, and connected-wallet trade ticket. Insights instead owns **`/execution` (Execution Quality & Protocol Tape)**: canonical keeper order fill/expiry rates, commit-to-execution latency percentiles, TP/SL protection worker reliability, liquidation engine telemetry, and the protocol-wide receipt-level trade tape.
4. **Public Infrastructure Transparency:** Expose complete user-impacting protocol and off-chain infrastructure status using strictly sanitized, public-safe payloads. Never expose signer addresses/balances, private RPC URLs, client IPs, raw exception strings, calldata, or secrets.
5. **Account Resolution (`/wallets/:address`):** Resolves **Trading Account** addresses directly (no owner-to-account indirection).

---

## 2. Accounting, Freshness & Network Invariants

1. **No Manufactured Zeros ("Fail Closed, Show Unavailable"):** Missing snapshots, lagging indexers, or unavailable attribution traces must render explicit unavailable/stale states—never coerce missing values to `0`.
2. **Release & Asset Isolation:** Never splice metrics across contract upgrades (`v1.3.1` mock USDC vs. `v2.0.0` USDG). Always include `release.version` and `network.settlementAssetSymbol`. Format settlement amounts without `$` (e.g. `1,250.00 USDG`).
3. **Network-Agnostic Presentation:** Chain identity, explorer base URLs (`https://sepolia.arbiscan.io` vs. `https://arbiscan.io`), trading app URLs (`https://app.sepolia.plether.com` vs. `https://app.plether.com`), and testnet badges come from `chainId`.
4. **Anonymous Edge Caching:** All public analytics endpoints live under `/api/insights/v1/*`, are strictly read-only (`GET`/`HEAD`), strip `Cookie` and `Authorization` headers at Cloudflare Pages (`apps/insights/public/_worker.js`), and cache only `200–299` responses with per-route TTLs.

---

## 3. Route Map

| Route | Purpose | Primary Audience | Stage |
| --- | --- | --- | --- |
| `/` | **Protocol Pulse (Overview)** — executive telemetry across execution, LP settlements, protocol safety, infra readiness & Divisions | Traders + LPs | Stage 0 → 5 |
| `/execution` | **Execution Quality & Protocol Tape** — canonical keeper fill/expiry rates, p50/p95 latency, TP/SL protection reliability, liquidation telemetry, receipt-level protocol trade tape | Traders | Stage 2 |
| `/liquidity` | **LP Ops & Risk Forensics** — LP epoch settlement audit & decoded blocker/warning/fault masks, attribution/snapshot freshness audit, trader-vs-pool flows & liquidation splits (links to main app `/vaults` for deposits & share-price UI) | LPs | Stage 1 |
| `/keepers` | **Permissionless Keepers Monitor** — dynamically detects every on-chain keeper (`tx.from`) across Order execution, LP settlement, TP/SL protection, Liquidation, and Oracle update transactions: executions & share %, latency (`p50`/`p95`), live ETH balance, bounties earned, and recent tx feed (no special-casing for Plether-run keepers) | Traders + LPs + Keepers | Stage 2 / 5 |
| `/divisions` | **Plether Divisions** — live weekly rankings, tier ladder, Discord registration count | Traders | Existing (moved from `/`) |
| `/results` | **Weekly Results Archive** — published weekly Divisions results | Traders | Existing |
| `/methodology` | **Divisions Methodology** — scoring & tier progression rules | Traders | Existing |
| `/register` | **Link Discord** — Discord OAuth + owner wallet verification | Traders | Existing |
| `/wallets/:address` | **Public Trading Account Inspector** — Divisions rank/badge, open position & margin health, receipt-level trade history, TP/SL protections, LP request history | Traders + LPs | Stage 0 → 3 |
| `/divisions/wallets/:address` | Backward-compatible Divisions trader view | Traders | Existing |
| `/protocol` | **Protocol Mechanics & Contracts** — verified Core manifest, 30+ contract addresses & runtime code hashes, live vs. manifest economic parameters, pause flags, release timeline | Traders + LPs + Verifiers | Stage 4 |
| `/infra` | **Infrastructure & Reliability** — action readiness (`deposit`/`open`/`close`/`protection`), workers/indexers block-lag matrix, Managed AA (Alto + Paymaster + Reconciler), Pyth oracle freshness/confidence, self-hosted Arbitrum Nitro RPC status | Traders + LPs + Verifiers | Stage 5 |
| `/archive/september-2026` | September 2026 testnet competition archive (+ `/methodology`, `/competitions/:slug/wallets/:address`) | Archive | Existing |

---

## 4. Public API Contract (`/api/insights/v1/*`)

All endpoints return the standard Plether envelope `{ "data": ..., "meta": { "chainId": number, "blockNumber": number } }`.

### Edge Proxy Allowlist & Cache TTLs (`apps/insights/public/_worker.js`)

| Path Pattern | Methods | Edge Cache TTL | Notes |
| --- | --- | --- | --- |
| `/api/insights/v1/status` | GET, HEAD | 30s | Existing |
| `/api/insights/v1/overview` | GET, HEAD | 15s | Executive pulse aggregate |
| `/api/insights/v1/execution/quality` | GET, HEAD | 15s | Order fill/expiry rates, latency p50/p95, protections, liquidations |
| `/api/insights/v1/execution/trades` | GET, HEAD | 15s | Paginated protocol-wide trade & liquidation tape with receipts |
| `/api/insights/v1/liquidity/settlements` | GET, HEAD | 30s | Current & historical LP epoch settlement observations & decoded masks |
| `/api/insights/v1/liquidity/forensics` | GET, HEAD | 30s | Attribution/indexer freshness, stale-mark audit, pool flows & fee splits |
| `/api/insights/v1/keepers/status` | GET, HEAD | 15s | All on-chain detected keepers (`tx.from`): roles, executions, share %, latency, live ETH balance, bounties |
| `/api/insights/v1/keepers/activity` | GET, HEAD | 15s | Paginated recent permissionless keeper transactions (orders, LP settlements, protections, liquidations, oracle) |
| `/api/insights/v1/wallets/0x<40-hex>/summary` | GET, HEAD | 15s | Public Trading Account summary |
| `/api/insights/v1/protocol/manifest` | GET, HEAD | 60s | Verified contracts, runtime hashes, live economic parameters, pause flags |
| `/api/insights/v1/infra/status` | GET, HEAD | 15s | Action readiness, workers, AA health, Pyth freshness, indexer lag, RPC status |
| `/api/insights/v1/divisions/*` | GET, HEAD | 15s | Existing (`stats`, `traders`, `live`, `current`, `history`) |
| `/api/insights/v1/competitions/:slug/leaderboard` | GET, HEAD | 15s | Existing |
| `/api/insights/v1/competitions/:slug/wallets/:address` | GET, HEAD | 15s | Existing |

> Registration routes (`/divisions/registrations/*`, `/competitions/:slug/registrations/*`) and `/competitions/current` remain strictly `no-store`. Unknown routes default to `no-store`.

### Public-Safe Redaction Rules (Mandatory for Backend Handlers)
1. **Never expose:** KMS key ARNs, RPC URLs or auth tokens, client IP addresses, pseudonymous client keys, raw Postgres/RPC exception text, or raw signed transaction calldata.
2. **Permissionless keepers (`/api/insights/v1/keepers/*`):** Discover every keeper strictly from canonical on-chain transactions (`tx.from`) on protocol contracts (`OrderRouter`, `HousePool`, `PositionProtectionBook`, `CfdEngine`, `PletherOracle`). Do not hardcode or privilege Plether-operated signers; display the same on-chain fields (address, detected roles, execution counts & share %, latency percentiles, live `eth_getBalance`, bounties earned, last active block) identically for every detected address. Alto bundler telemetry belongs on `/infra`, not `/keepers`.
3. **LP settlement observations:** expose decoded bitmask labels and epoch numbers; omit `calldata` and raw `rpc_error`.
4. **Order reliability:** follow the canonical counting contract in `docs/operations/order-reliability.md` (1h and 24h cohorts shifted back 120s for settlement grace; snapshots older than 10 minutes marked `stale: true` / unavailable rather than `0`).
