# GitBook diagram review

Reviewed on 2026-09-07. Covers all 30 current diagram assets and their article references, including the previously unreferenced LP quickstart waterfall. Historical, unused images outside `assets/diagrams` and application screenshots are not redesigned.

## Source of truth

Contract semantics were checked against **Plether Core v1.2.2**, commit `d704122c779d4d681d0fa2be517707b7f7df3902`, the deployment pin at the time of that review. The source was read at that exact commit, not from a possibly newer working tree. This is a release-source review, not a claim that mutable live parameters were freshly queried.

The app now pins v1.2.3. The 2026-09-10 AA package migration updates only client-source references in the catalog and manifest; it does not certify all 30 diagrams against v1.2.3 or change their SVGs. Full regeneration remains blocked by the existing contract-review guard until a separate semantic review updates this record and the generator's reviewed commit.

Account-abstraction ordering was checked against [@plether-fi/perps-aa-client@0.1.0](https://github.com/Plether-Fi/plether-core/blob/f9e29c1b3ac5937e0519108cebffb4d09048de36/packages/perps-aa-client/src/orchestrator.ts), the frontend operation-status mapping, withdrawal action encoding, and receipt-recovery code. Package provenance is recorded in `config/perps-aa-client-release.json`.

Each of the 30 records in [the manifest](.gitbook/assets/diagrams/diagram-manifest.json) lists its article, SVG, accessible description and reviewed source functions. `diagram-catalog.mjs` is the editable content source. Regeneration fails when the pinned contract commit changes so that a visual rebuild cannot silently masquerade as a new semantic review.

## Correctness changes

| Area | Correction | Primary release source |
| --- | --- | --- |
| Weekly calendar | Replace fixed UTC hours and the three-hour runway with New York time, US DST, a 30-minute Friday lead and 15-minute Sunday lag. Label the timeline as not to scale and retain override/data-availability qualifications. | `MarketCalendarLib.marketStatus`, `newYorkMarketBoundary` |
| Order lifecycle | Separate successful submission from execution. Pending and terminal failure do not imply the requested position update. | `OrderRouterV2ExecutionSidecar.executeOrder`, `OrderV2Types` |
| Order failures | Distinguish V2 pending reasons from terminal reasons; unexpected engine/receipt failures are not automatically terminal. Executed and Failed are alternatives, never consecutive steps. | `OrderV2Types.PendingReason`, `TerminalReason`; sidecar rollback boundary |
| Sponsorship | Obtain final paymaster data before the wallet signs. Do not equate a transaction’s inclusion with successful execution of the inner operation. | Released client `orchestrator.ts`; frontend receipt recovery |
| Price risk | Use PnL pledge + nettable own claim + price PnL for V2 price-risk equity. Keep action charges separate. | `PositionRiskAccountingLib.buildExactPriceRiskState` |
| Close funding | Replace the obsolete shared fee → base obligation → spread queue. Price losses net the same account’s claim before collectible pledge. Charges have separate eligible funds; partial closes cannot waive them. | `CfdEnginePlanLib._planIsolatedCloseSettlement`, `_planCloseActionSettlement` |
| Frozen spread | Show recovery attribution after fee, carry and positive VPI, not an all-purpose collateral queue. Note net negative-VPI offsets and waiver. | `CfdEngineSettlementSidecar._recoveredFrozenSpreadUsdc` |
| Payouts and claims | Residual price payouts are all-or-nothing after existing claims. A live-position credit is PnL pledge; a flat-account credit is free settlement. Unpaid action rebates are waived, not turned into claims. | `CfdEngine._payOrRecordTraderClaim`, `_settleTraderClaimBalance`; planner |
| Margin release | Do not promise release of all pro-rata pledge on a partial loss. Preserve the remaining terminal collectible cap. | `CfdEnginePlanLib._maxPricePledgeUnlockPreservingTerminalCap` |
| Carry | Explain the maximum-profit-minus-pledge borrow base, zero floor and constant-other-conditions assumption. Free USDC alone does not lower the assigned borrow base. | `PositionRiskAccountingLib.computeBorrowBaseUsdc`, `computeIndexedCarryUsdc` |
| LP waterfall | Preserve Junior-first loss absorption and Senior high-water-mark restoration. Add that the separate coupon is capped by Junior principal, not guaranteed yield. | `HousePoolWaterfallAccountingLib.absorbLoss`, `distributeRevenue`, `paySeniorCoupon` |
| Account flow | Distinguish wallet custody, free settlement, pledge and reserves. Remove the false implication that a deposit must pass through trading before withdrawal. | `MarginClearinghouse`; AA action builders |

The directly related schedule, claim, collection and troubleshooting explanations were corrected alongside the graphs. This is not an independent audit of every older worked example, parameter or screenshot elsewhere in GitBook.

## Visual system

- Self-contained SVGs: no Graphviz runtime, external fonts, raster labels or `foreignObject`.
- Exact [Plether media-kit](https://plether.com/media-kit) swatches, verified against the published page: orange `#FF512F`, peach `#FFAB96`, yellow `#F7D977`, pale yellow `#F7E4B2`, dark plum `#250917`, and gray `#9A8890`. The cream surface `#FFF5F9` matches the media-kit page background.
- Positive green `#00FF99` matches the frontend's `--color-positive` token in `apps/frontend/src/index.css`. It marks successful/positive steps and accents cream outcome cards; pending/warning states retain yellow markers and pale-yellow panels.
- Dark-plum type, orange brand accents, peach/yellow comparison panels, and inverse cream-on-plum notes. Bright colors, including green, are not used as low-contrast small text on cream. Clear reading rails and explicit outcome labels preserve meaning without relying on color alone.
- Consistent 640px artboards. Body text is 24px; the smallest category label is 22px. At 320px, these are 12px and 11px respectively.
- Meaning is present in text, not color alone. Each asset has a title, description and matching article alt text.
- Automatic browser checks measure real text bounds, pairwise overlaps, 4.5:1 text contrast, and minimum rendered font size at 640px, 360px and 320px.
- Palette regression tests reject off-palette SVG colors, check the swatches against the landing page and frontend sources, and keep positive green out of text labels.
- All 30 renders were visually inspected in contact sheets. Desktop/mobile PNG proofs and the machine-readable report can be recreated with the command below.

## Rebuild and verify

From the repository root:

```bash
node apps/gitbook/scripts/generate-diagrams.mjs
node apps/gitbook/scripts/generate-diagrams.mjs --check
node --test apps/gitbook/scripts/diagrams.test.mjs
node apps/gitbook/scripts/verify-diagrams.mjs --output=/private/tmp/plether-diagram-review
```

The browser verifier uses Playwright from `apps/frontend`; install that package’s normal dependencies and Chromium first if unavailable. Without `--output`, verification creates no screenshot artifacts. Output directories are not committed or published.

When an article edit shifts screenshot locations, synchronize the separate screenshot inventory without recapturing application images:

```bash
node apps/frontend/scripts/capture-gitbook-screenshots.mjs --sync-manifest-only
```
