# GitBook diagram review

Reviewed against **Plether Core v2.0.0** (`config/perps/arbitrum-sepolia-v2.0.0.json`, commit `1209ca8a5b94a4fb6323c7c4585846ed93db3932`, active Arbitrum Sepolia MockUSDG deployment). Covers all **33** diagram assets and their synchronized article references.

## Full Core v2.0.0 review & expansion — 2026-11-20

* **Pinned contract release updated:** Migrated `generate-diagrams.mjs` from archived `config/perps/arbitrum-sepolia-v3.json` (v1.2.2/v1.3.1) to active `config/perps/arbitrum-sepolia-v2.0.0.json` (`1209ca8a5b94a4fb6323c7c4585846ed93db3932`, 30 contracts, schema version 4, MockUSDG).
* **Account-abstraction client updated:** Evidence references updated to `@plether-fi/perps-aa-client@0.4.0` (`e35cf0e0e9b880e07812e36321dee98bd299dcbd`) and self-hosted native AA v2 (`PletherVerifyingPaymaster` + private Alto bundler + first-time Trading Account `safe`-block confirmation wait).
* **Added 3 onboarding diagrams (30 → 33 total):**
  * `plether-system-overview.svg` (`README.md`) — 3-layer map of Pyth FX oracle, gas-sponsored traders and Senior/Junior LP vaults.
  * `dollar-index-bounded-payoff.svg` (`welcome/how-plether-works-in-5-minutes.md`) — inverse raw basket to `plDXY = 2.00 − B` display transform and bounded LONG/SHORT max payouts.
  * `lp-hourly-epoch-timeline.svg` (`liquidity-provider-quickstart.md`) — hourly 5-minute cutoff, strict 4-phase settlement order (`Senior withdraw → Junior withdraw → Junior deposit → Senior deposit`), `Shares ready` / `Withdrawal ready` states and 1-hour cooldown rules.
* **Terminology & semantic updates across existing diagrams:**
  * Removed internal `V2` and `HousePool` user-facing labels from diagram categories, notes and descriptions.
  * Updated `usdg-account-flow.svg` for direct-to-Margin-Account testnet faucet funding (`Get 100,000 USDG`) and owner-wallet shortfall `Transfer & Deposit`.
  * Updated `authorization-and-gas-sponsorship.svg` for native paymaster preparation and first-time Trading Account `safe`-block wait.
  * Updated `trader-claim-lifecycle.svg` and `claim-to-owner-wallet.svg` to reflect onchain aggregate claim coverage checks.

## Source of truth

* **Core Perps contracts:** `Plether-Fi/plether-core`, release **v2.0.0**, commit `1209ca8a5b94a4fb6323c7c4585846ed93db3932` (`config/perps/arbitrum-sepolia-v2.0.0.json`, deployment block `323658447`).
* **AA client & web integration:** `@plether-fi/perps-aa-client@0.4.0` (`e35cf0e0e9b880e07812e36321dee98bd299dcbd`), `apps/frontend/src/utils/sponsoredOperation.ts`, `apps/frontend/src/perps-aa/SponsoredOperationRecovery.tsx`, `apps/frontend/src/pages/Vaults.tsx`.

Each of the 33 records in [the manifest](.gitbook/assets/diagrams/diagram-manifest.json) lists its article, SVG, accessible description and reviewed source functions. `diagram-catalog.mjs` is the editable content source. Regeneration fails whenever the pinned contract commit changes so a visual rebuild cannot silently masquerade as a new semantic review.

## Core semantic invariants preserved

| Area | Verified invariant | Primary release source |
| --- | --- | --- |
| Weekly calendar | America/New_York DST-aware schedule: Fri 16:30–17:00 live close-only (30m), Fri 17:00–Sun 17:00 frozen, Sun 17:00–17:15 live close-only (15m). Non-scaled timeline. | `MarketCalendarLib.marketStatus`, `newYorkMarketBoundary` |
| Order lifecycle & failures | Sponsored commit confirmation ≠ execution. Unexpected engine/receipt rollbacks stay pending; only terminal reasons fail an order. Executed and Failed are alternatives. | `OrderV2Types.PendingReason`, `TerminalReason`; `OrderRouterV2ExecutionSidecar` |
| Sponsorship & AA v2 | Native paymaster data is prepared before owner signing; first-time Trading Account deployment waits for Arbitrum Sepolia `safe` confirmation. | `@plether-fi/perps-aa-client@0.4.0`, `SponsoredOperationRecovery.tsx` |
| Close accounting & carry | Carry collects from active position margin first, then eligible free USDG. Price losses net same-account claims before collectible pledge; action charges use separate sources (full close only waives uncollectible charges). | `MarginClearinghouseAccountingLib`, `CfdEnginePlanLib`, `CfdEngineSettlementSidecar` |
| Close execution reward | Reductions and full closes (including TP/SL closes) reserve and pay `0 USDG` execution reward; opens reserve `0.01–0.20 USDG`; TP/SL reserves `0.20 USDG` trigger reward. | `CfdEngine.reserveCloseOrderExecutionBounty`, `PositionProtectionBook` |
| Payouts & claims | Residual price payouts are all-or-nothing after existing claims; funded payouts credit PnL pledge if open or free USDG if flat; unpaid action rebates are waived. | `CfdEngine._payOrRecordTraderClaim`, `_settleTraderClaimBalance` |
| LP waterfall & epochs | Junior absorbs first loss; gains restore Senior high-water mark first; target coupon capped by Junior principal. Hourly settlement runs Senior withdraw → Junior withdraw → Junior deposit → Senior deposit. | `HousePoolWaterfallAccountingLib`, `docs/runbooks/lp-settlement-keeper.md` |

## Visual system

* Self-contained SVGs: no Graphviz runtime, external fonts, raster labels or `foreignObject`.
* **Dark-plum product alignment:** Matches the live Plether frontend and Storybook UI tokens in `apps/frontend/src/index.css` (`--color-app-bg: #250917`, `--color-surface-panel: #3B212D`, `--color-content-primary: #FFF5F9`, `--color-content-secondary: #D8CBD0`, `--color-positive: #00FF99`) combined with [Plether media-kit](https://plether.com/media-kit) swatches (`#FF512F`, `#FFAB96`, `#F7D977`, `#F7E4B2`, `#9A8890`). Low-contrast orange `#FF512F` is reserved strictly for strokes, accent bars and graphics — never small text labels.
* **Bespoke visual primitives for flagship diagrams:** Architecture node-and-arrow flow (`plether-system-overview`), horizontal `0.00–1.00–2.00` inverse payoff axis (`dollar-index-bounded-payoff`), color-coded weekly FX phase strip (`weekly-market-state-schedule`), stacked Senior/Junior loss & HWM recovery bars (`liquidity-pool-tranche-waterfall`, `senior-junior-waterfall-rules`), before/after borrow-base carry bars (`margin-reduces-future-carry`), and 2×2 4-phase LP epoch settlement grid (`lp-hourly-epoch-timeline`).
* Distinct semantic borders and top accent bars (`#00FF99` success, `#F7D977` warning, `#FF512F` danger, `#FFAB96` accent) plus centered lane connectors across all 33 diagrams.
* Consistent 640px artboards (minimum font size 22px; ≥11px at 320px).
* Automated Playwright checks (`verify-diagrams.mjs`) verify real text bounds, pairwise overlaps, ≥4.5:1 contrast and minimum font sizes at 640px, 360px and 320px.

## Rebuild and verify

From the repository root:

```bash
node apps/gitbook/scripts/generate-diagrams.mjs
node apps/gitbook/scripts/generate-diagrams.mjs --check
node --test apps/gitbook/scripts/diagrams.test.mjs
node apps/gitbook/scripts/verify-diagrams.mjs
```

When an article edit shifts screenshot locations, synchronize the separate screenshot inventory without recapturing application images:

```bash
node apps/frontend/scripts/capture-gitbook-screenshots.mjs --sync-manifest-only
```
