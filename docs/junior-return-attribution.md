# Junior vault return attribution

The Junior Vault's **Return breakdown** sits directly below “Seven-day share
price.” It uses the same deployment-scoped
`GET /api/perps/vaults/history?range=7d&interval=3600` response and existing
performance metric cards. It has no independent period selector or duplicate
return/APY footer. Amounts use six-decimal USDG.

## Backend producer

`apps/backend/return-attribution` reconstructs attribution from the deployed V2
contracts, pinned to core commit `1209ca8a5b94a4fb6323c7c4585846ed93db3932`.
The API starts and supervises this read-only Node worker alongside the hourly
vault-performance sampler. The backend Docker image includes its locked
runtime dependencies, ABI and database migration. No signing key is needed or
passed to the worker.

The worker verifies the RPC chain, contract runtime hashes and asset/share
decimals. It indexes canonical logs from deployment, replays position entries
before the reporting window, and reads successful Engine → SettlementSidecar
call traces. It binds each decoded settlement to its position receipt and the
Router → Oracle quote, including the neutral oracle mark. Negative or missing
historical evidence cannot be replaced with zeros.

Collected VPI less rebates, junior-owned liquidation fees, collected carry,
confidence/frozen-market spread and confidence-neutral settled trader PnL are
separate sources. Carry realized before a settlement is counted once; waived
charges and uncollectible price losses are excluded. Terminal-book valuation
changes are measured independently. Junior-funded senior coupon includes both
checkpointed payment and the change in pending accrual. Maintenance dilution
uses effective supply, including pending fee shares; minting already-accrued
shares does not charge the holder twice.

For each hourly boundary and protocol-event block (including its predecessor),
the worker samples share supply, assets, conversion price, terminal NAV and
coupon state at an exact block. Income is converted to share-price changes
using the effective supply at its event, following capital-flow log order.
ERC-4626 virtual assets/shares are included. Contributions are linked to the
starting diluted share price and accumulated using integer fixed point.
Deposits, funded redemptions and bootstrap assignments are capital flows,
never earnings. There is no average-capital or ending-TVL denominator.

Only a complete 169-point seven-day history can receive a report. Period
boundaries must be fresh; stale intermediate snapshots do not block publication
and are disclosed in the chart's notes. Internal stale-mark pool previews cannot manufacture
income: their changes telescope through the independently observed net asset
change, while open valuation comes from the terminal book. A maintenance-fee
change needing a stale valuation fails closed. Senior impairment, unsupported
releases and incomplete entry/trace evidence also prevent publication.

PostgreSQL stores canonical logs, validated evidence, the indexing checkpoint
and the report. A session advisory lock elects one worker across API replicas.
Reorg detection clears orphaned derived evidence before rebuilding the release
prefix. The API attaches a report only when deployment, boundary block numbers,
hashes, prices, timestamps and report schema version 2 exactly match the returned
history. A report from the previous hour is never attached to a new window.

## Reconciliation limitations

For LP epoch settlements, the worker reads the terminal NAV returned to the
pool from the transaction's successful call trace, before capital is funded or
shares are minted. It binds that valuation to each canonical capital receipt.
Valuation changes are split at these observed boundaries and unitized using the
share supply before/after each flow. Engine mutations within the LP settlement,
missing terminal reads and mismatched receipts are rejected. Without complete
valuation timing evidence, a mixed-block change remains in an explicit residual;
it is never guessed from the final return or quietly absorbed into PnL.

Other unclassified economic changes and integer rounding also remain explicit.
These limitations are visible below the chart, not hidden in a tooltip.

A read-only validation on November 4, 2026 replayed 106 trading settlement
traces, LP settlement traces and 674 valuation boundaries. The sampled seven-day share-price return was
+0.2996892076%. The first producer left +0.0004483698 pp / 451.582565 USDG
unassigned because it skipped every valuation change in a block with an LP flow.
The investigation traced the entire difference to block 325853248 and
[its LP settlement transaction](https://sepolia.arbiscan.io/tx/0xf50e4afe2c3d3d3588124b3ba532ba77b0d57e05aaca966684d45e187c804d92).
That transaction refreshed the oracle mark before minting deposit shares; the
pool's independently observed terminal NAV had already increased by exactly
451.582565 USDG. The corrected producer allocates this to open-position
valuation using the pre-deposit share supply.

The same period now reconciles exactly to 18,395.305533 USDG, with only about
1.4e-16 pp floating-point noise and no residual bar. The observed share-price
return and cash-flow treatment are unchanged. Cache schema version 2 prevents
previously calculated reports from being reused. These are historical validation
results, not production constants or an independent accounting audit.

## API contract

`junior.returnAttribution` is null until a matching report is ready:

```typescript
{
  method: 'diluted-share-price-v1',
  startTimestamp: number, // actual sampled block time, seconds UTC
  endTimestamp: number,
  startDilutedSharePrice: string, // WAD integer matching the first junior point
  endDilutedSharePrice: string,   // WAD integer matching the last junior point
  components: {
    netVpi: { contributionPp: number, amountUsdg: string },
    liquidationFees: { contributionPp: number, amountUsdg: string },
    realizedCarry: { contributionPp: number, amountUsdg: string },
    oracleSpread: { contributionPp: number, amountUsdg: string },
    settledTraderPnl: { contributionPp: number, amountUsdg: string },
    openPositionValuation: { contributionPp: number, amountUsdg: string },
    seniorCoupon: { contributionPp: number, amountUsdg: string },
    maintenanceDilution: { contributionPp: number, amountUsdg: string }
  },
  netReturnUsdg: string,
  notes: string[],
  sourceCommit: string,
  sourceBlockCount: number
}
```

Contributions are additive percentage points; signed amount strings are raw
six-decimal USDG. USDG totals describe pool economics, not a particular holder's
profit. Missing sources are unavailable, never zero. The frontend validates the
entire contract, checks its period against history and independently computes:

```
returnPp = (ending diluted share price / starting diluted share price - 1) * 100
APY = (exp(log(ending price / starting price) * 31536000 / elapsedSeconds) - 1) * 100
```

The total bar starts at zero. Any difference from component contributions or
USDG totals gets an outlined **Unexplained difference** bar and warning. Only
floating-point noise below `1e-10` pp is suppressed; USDG differences are exact.
The existing APY tooltip explains repeated compounding and is not a forecast.

## Operations and validation

Deploy the backend before the frontend. Existing startup migrations create the
report table; the worker creates its evidence/checkpoint tables idempotently.
Initial backfill requires an archive RPC supporting `debug_traceTransaction`
with `callTracer`. It can take several minutes and consumes more RPC quota than
steady-state indexing. Validated evidence is cached across restarts. The default
is `PERPS_RPC_URL` / `PERPS_RPC_AUTH_TOKEN`; optional
`VAULT_RETURN_ATTRIBUTION_RPC_URL` / `VAULT_RETURN_ATTRIBUTION_RPC_AUTH_TOKEN`
allow a separate archive provider. Chain and runtime bindings are still verified.
The database uses the existing `DATABASE_URL` and certificate configuration.
RPC/database errors are logged without credentials or request bodies.

Standalone local validation from `apps/backend/return-attribution`:

```sh
npm ci --ignore-scripts --no-audit --no-fund
npm test
# Use a dedicated test database; integration cases create/drop their own schemas.
ATTRIBUTION_TEST_DATABASE_URL=postgres://localhost/attribution_test npm test
# Requires existing hourly performance snapshots in DATABASE_URL.
node main.mjs --once
```

Tests cover liquidity changes, ordered events, pending/issued dilution, coupon
capitalization, partial closes, capped loss recovery, spread/carry collection,
trace-proven valuation around one or multiple LP flows, explicit residuals,
deployment ABI decoding, restart/reorg behavior and invalid receipts. Compact public-chain fixtures cover actual V2 open, close and
liquidation calldata and the LP transaction that caused the former residual. Haskell database tests reject mismatched cached reports;
frontend tests validate reconciliation, invalid notes and chart explanations.
CI runs the worker suite against PostgreSQL.

Storybook's baseline components remain illustrative. The rounded +0.523% /
14.56% APY example uses full-precision fixture prices: exactly 0.523% over exactly
14 days would round to 14.57% APY. Neither value is hardcoded in production.
