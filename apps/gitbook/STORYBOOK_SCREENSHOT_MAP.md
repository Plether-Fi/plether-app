# GitBook screenshot-to-Storybook map

This inventory maps every screenshot currently embedded in a published `apps/gitbook` article to its deterministic Storybook state. Full capture runs remove unused screenshot assets so the GitBook directory contains only mapped, current-interface visuals.

References use the article path and current screenshot line so the capture script can locate and synchronize each embed.

All Storybook values are deterministic examples, not live account or market readings. A screenshot may illustrate layout and field meaning, but protocol constants and available actions must still match the current deployment.

The generated `.gitbook/assets/screenshots/storybook-screenshots.json` contains only the currently mapped captures. Documentation-only prototypes and invented interface compositions must not be mapped or retained as GitBook assets.

Run Storybook from `apps/frontend`:

```bash
npm run storybook
```

The links below assume Storybook is available at `http://localhost:6006`.

## Current-state exclusions

- The current deployment uses a separate SimpleAccount v0.8 Trading Account. Obsolete same-address, EIP-7702 and owner-wallet transfer-authorization captures are not mapped.
- The current sponsored trader interface relies on keepers for order finalization and expired-order cleanup. Manual **Finalize Trade** and **Clean Up** captures are not mapped.

## Story sources

| Surface | Component or story source | Coverage |
| --- | --- | --- |
| Position, order history and position-margin modal | `PerpsAccountPanel` | Current application components with deterministic fixtures |
| Executed close reconciliation | `PerpsTradeTicket` | Current application component with deterministic executed-receipt evidence |
| Market-phase banner | `PerpsMarketStatePanel` | Current application component with deterministic countdown text |
| Vault overview, detail, position, activity and transaction previews | `VaultsDocumentation.stories.tsx` | Current production Vaults components with deterministic pool, history, account and request fixtures |

## How PnL is calculated

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/how-pnl-is-calculated.md:75` | Current Position fields | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Current component, illustrative values |
| `how-plether-works/how-pnl-is-calculated.md:540` | Executed close reconciliation in Final Result | [Executed Close Reconciliation](http://localhost:6006/?path=/story/perps-trade-ticket--executed) | Current component, deterministic executed-receipt evidence |

## Margin, leverage and liquidation

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/margin-leverage-and-liquidation.md:164` | Current Position fields | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Current component, illustrative values |
| `how-plether-works/margin-leverage-and-liquidation.md:201` | Position-margin form | [Edit Position Margin](http://localhost:6006/?path=/story/perps-account-panel--edit-position-margin) | Current component, illustrative values |

## Market states and oracle closures

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/market-states-and-oracle-closures.md:518` | Open followed by the three-hour close-only runway | [Open Then Close Only](http://localhost:6006/?path=/story/perps-market-state-panel--open-then-close-only) | Current component, illustrative countdown |

## Liquidity pool and tranche waterfall

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/the-liquidity-pool-and-tranche-waterfall.md:553` | Current Vaults withdrawal preview | [Withdrawal Preview](http://localhost:6006/?path=/story/documentation-vaults--withdrawal-preview) | Current production component, illustrative values |

## LP quickstart

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `liquidity-provider-quickstart.md:93` | Vaults overview with current pool liquidity and Senior and Junior Vault cards | [Overview](http://localhost:6006/?path=/story/documentation-vaults--overview) | Current production components, illustrative values |

## Providing liquidity

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `providing-liquidity/deposit-liquidity.md:103` | Deposit preview for a queued Senior or Junior Vault deposit | [Deposit Preview](http://localhost:6006/?path=/story/documentation-vaults--deposit-preview) | Current production component, illustrative values |
| `providing-liquidity/lp-risks-and-safeguards.md:13` | Current Junior Vault overview section with pool status and safeguards | [Junior Overview Section](http://localhost:6006/?path=/story/documentation-vaults--junior-overview-section) | Current production Vault overview section, illustrative values |
| `providing-liquidity/lp-troubleshooting.md:144` | Pending deposit and withdrawal records with their current actions | [Pending Activity](http://localhost:6006/?path=/story/documentation-vaults--pending-activity) | Current production Vault activity view, illustrative values |
| `providing-liquidity/manage-a-pending-deposit.md:20` | Pending deposit records with their expected processing time, status and available actions | [Pending Activity](http://localhost:6006/?path=/story/documentation-vaults--pending-activity) | Current production Vault activity view, illustrative values |
| `providing-liquidity/read-your-lp-position-and-pool-health.md:30` | Senior Vault Your position view with active shares and the current empty pending-request state | [Position](http://localhost:6006/?path=/story/documentation-vaults--position) | Current production Vault activity view, illustrative values |
| `providing-liquidity/read-your-lp-position-and-pool-health.md:143` | Senior Vault Overview with current value, share-price context and operating rules | [Senior Overview Section](http://localhost:6006/?path=/story/documentation-vaults--senior-overview-section) | Current production Vault overview section, illustrative values |
| `providing-liquidity/withdraw-liquidity.md:137` | Withdrawal preview | [Withdrawal Preview](http://localhost:6006/?path=/story/documentation-vaults--withdrawal-preview) | Current production component, illustrative values |

## Read your position and account health

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/read-your-position-and-account-health.md:44` | Current Position fields | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Current component, illustrative values |
| `trading-on-plether-perps/read-your-position-and-account-health.md:210` | Position-margin form | [Edit Position Margin](http://localhost:6006/?path=/story/perps-account-panel--edit-position-margin) | Current component, illustrative values |

## Reduce or close a position

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/reduce-or-close-a-position.md:52` | Existing position fields before composing a reduction | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Current component, illustrative values |

## Your Margin Account

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/your-margin-account.md:207` | Position-margin form | [Edit Position Margin](http://localhost:6006/?path=/story/perps-account-panel--edit-position-margin) | Current component, illustrative values |

## Inventory result

- Screenshot references found: **18**
- Unique screenshot assets: **11**
- References mapped to a Storybook target: **18**
- Unmapped references: **0**
