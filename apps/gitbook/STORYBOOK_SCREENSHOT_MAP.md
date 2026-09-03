# GitBook screenshot-to-Storybook map

This inventory maps every screenshot currently embedded in a published `apps/gitbook` article to its deterministic Storybook state. It intentionally excludes unused capture assets and stories that describe flows the current Plether interface does not expose.

References use the article path and current screenshot line so the capture script can locate and synchronize each embed.

All Storybook values are deterministic examples, not live account or market readings. A screenshot may illustrate layout and field meaning, but protocol constants and available actions must still match the current deployment.

The generated `.gitbook/assets/screenshots/storybook-screenshots.json` is a historical capture catalog and can include unused assets from earlier runs. The inventory below—not that catalog’s capture count—is the source of truth for screenshots currently embedded in published pages.

Run Storybook from `apps/frontend`:

```bash
npm run storybook
```

The links below assume Storybook is available at `http://localhost:6006`.

## Current-state exclusions

- The current deployment uses a separate SimpleAccount v0.8 Trading Account. Obsolete same-address, EIP-7702 and owner-wallet transfer-authorization captures are not mapped.
- The current sponsored trader interface relies on keepers for order finalization and expired-order cleanup. Manual **Finalize Trade** and **Clean Up** captures are not mapped.
- Aggregate trader-claim coverage is not preflighted by the live trader card. The retained coverage screenshot is a documentation prototype and its article labels it as illustrative.

## Story sources

| Surface | Component or story source | Coverage |
| --- | --- | --- |
| Position, order history and position-margin modal | `PerpsAccountPanel` | Current application components with deterministic fixtures |
| Market-phase banner | `PerpsMarketStatePanel` | Current application component with deterministic countdown text |
| Unrealized-PnL explanation | `PerpsMetricDetailsDocumentation.stories.tsx` | Documentation composition around the current position component |
| Trader-claim coverage model | `PerpsClaimPanel.stories.tsx` | Documentation prototype; not a live coverage preflight |
| Vault overview, detail, position, activity and transaction previews | `VaultsDocumentation.stories.tsx` | Current production Vaults components with deterministic pool, history, account and request fixtures |
| Executed increase composition | `PerpsOpenIncreaseDocumentation.stories.tsx` | Current position and order-history components in one deterministic state |
| Disabled review messages | `PerpsDocumentationWorkspace.stories.tsx` | Deterministic examples of current validation-message patterns |

## How PnL is calculated

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/how-pnl-is-calculated.md:75` | Current Position fields | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Current component, illustrative values |
| `how-plether-works/how-pnl-is-calculated.md:250` | Position plus metric explanation | [Unrealized PnL](http://localhost:6006/?path=/story/documentation-metric-details--unrealized-pnl) | Documentation composition, illustrative values |
| `how-plether-works/how-pnl-is-calculated.md:544` | Transaction History close row | [Transaction History Close Result](http://localhost:6006/?path=/story/perps-account-panel--transaction-history-close-result) | Current component, illustrative values |

## Margin, leverage and liquidation

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/margin-leverage-and-liquidation.md:164` | Current Position fields | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Current component, illustrative values |
| `how-plether-works/margin-leverage-and-liquidation.md:201` | Position-margin form | [Edit Position Margin](http://localhost:6006/?path=/story/perps-account-panel--edit-position-margin) | Current component, illustrative values |

## Market states and oracle closures

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/market-states-and-oracle-closures.md:518` | Open followed by the three-hour close-only runway | [Open Then Close Only](http://localhost:6006/?path=/story/perps-market-state-panel--open-then-close-only) | Current component, illustrative countdown |

## Settlement liquidity and trader claims

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/settlement-liquidity-and-trader-claims.md:308` | Claim balance, aggregate coverage, settlement availability and flat-account Margin Account destination | [Available to Settle](http://localhost:6006/?path=/story/documentation-trader-claims--available-to-settle) | Documentation prototype of the flat-account branch; the article states that open-position settlement credits PnL pledge and the live card does not preflight aggregate coverage |

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
| `providing-liquidity/lp-risks-and-safeguards.md:13` | Junior Vault risk safeguards and shared pool status | [Risk and Liquidity Pool](http://localhost:6006/?path=/story/documentation-vaults--risk-and-liquidity-pool) | Documentation composition of current production components, illustrative values |
| `providing-liquidity/lp-troubleshooting.md:144` | Pending deposit and withdrawal records with their current actions | [Pending Activity](http://localhost:6006/?path=/story/documentation-vaults--pending-activity) | Documentation composition of the current production component, illustrative values |
| `providing-liquidity/manage-a-pending-deposit.md:20` | Pending deposit records with their expected processing time, status and available actions | [Pending Activity](http://localhost:6006/?path=/story/documentation-vaults--pending-activity) | Documentation composition of the current production component, illustrative values |
| `providing-liquidity/read-your-lp-position-and-pool-health.md:30` | Senior Vault Your position view with active shares and the current empty pending-request state | [Position](http://localhost:6006/?path=/story/documentation-vaults--position) | Documentation composition of the current production component, illustrative values |
| `providing-liquidity/read-your-lp-position-and-pool-health.md:143` | Senior Vault Overview with current value, share-price context and operating rules | [Senior Vault Detail](http://localhost:6006/?path=/story/documentation-vaults--senior-vault-detail) | Current production components, illustrative values |
| `providing-liquidity/withdraw-liquidity.md:137` | Withdrawal preview | [Withdrawal Preview](http://localhost:6006/?path=/story/documentation-vaults--withdrawal-preview) | Current production component, illustrative values |

## Open or increase a position

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/open-or-increase-a-position.md:416` | Updated position beside its terminal order record | [Executed Position and Order History](http://localhost:6006/?path=/story/documentation-open-or-increase-position--executed-position-and-order-history) | Current components, illustrative values |

## Read your position and account health

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/read-your-position-and-account-health.md:44` | Current Position fields | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Current component, illustrative values |
| `trading-on-plether-perps/read-your-position-and-account-health.md:210` | Position-margin form | [Edit Position Margin](http://localhost:6006/?path=/story/perps-account-panel--edit-position-margin) | Current component, illustrative values |

## Reduce or close a position

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/reduce-or-close-a-position.md:52` | Existing position fields before composing a reduction | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Current component, illustrative values |

## Trader troubleshooting

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/trader-troubleshooting.md:110` | Insufficient-margin, minimum-size and skew-cap message patterns | [Disabled Review Messages](http://localhost:6006/?path=/story/documentation-trader-workspace--disabled-review-messages) | Current message patterns, illustrative values |

## Your Margin Account

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/your-margin-account.md:207` | Position-margin form | [Edit Position Margin](http://localhost:6006/?path=/story/perps-account-panel--edit-position-margin) | Current component, illustrative values |

## Inventory result

- Screenshot references found: **22**
- Unique screenshot assets: **15**
- References mapped to a Storybook target: **22**
- Unmapped references: **0**
