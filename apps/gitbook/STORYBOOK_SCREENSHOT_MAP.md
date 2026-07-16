# GitBook screenshot-to-Storybook map

This inventory maps every screenshot, screenshot placeholder and requested support screenshot in `apps/gitbook` to a deterministic Storybook state.

Run Storybook from `apps/frontend`:

```bash
npm run storybook
```

The links below assume Storybook is available at `http://localhost:6006`.

## Story sources

| Surface | Component or story source | Coverage |
| --- | --- | --- |
| Trade compose, previews, margin actions and lifecycle modals | `PerpsTradeTicket` | Existing component, extended with deterministic initial story states |
| Position, open orders, order history and transaction history | `PerpsAccountPanel` | Existing component, extended with initial tab and position-margin modal state |
| Market header and oracle freshness | `PerpsInstrumentPanel` | Existing component, extended with stale and visible-tooltip stories |
| Market phase banners | `PerpsMarketStatePanel` | Existing component, extended with close-only and degraded stories |
| Finalization and final result | `PerpsFinalRevealModal.stories.tsx` | Existing stories |
| Live, FAD-only and oracle-frozen close comparison | `PerpsTradingRegimes.stories.tsx` | Existing stories |
| Trading Account identity and sponsor/bundler states | `PerpsTradingAccountPanel` | New reusable component |
| Trader claim and settlement states | `PerpsClaimPanel` | New reusable component |
| LP screens not yet implemented in the application | `LpPrototypePanel` | New documentation prototype, explicitly outside trader sponsorship |
| Full trader-workspace compositions | `PerpsDocumentationWorkspace.stories.tsx` | New documentation stories composed from real perps components |
| Testnet faucet and Trading Account funding | `TestnetWelcomeModal` and `PerpsTradingAccountPanel` | Existing faucet component plus documentation account-flow story |

## Trader quickstart

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trader-quickstart.md:46` | Testnet welcome window asking for the Trading Account address before minting 100,000 MockUSDC | [Request Funds](http://localhost:6006/?path=/story/testnet-welcome-modal--request-funds) | Existing |
| `trader-quickstart.md:80` | First deposit flow showing a limited owner-wallet USDC authorization and the sponsored Trading Account deposit operation | [First Deposit Authorization](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--first-deposit-authorization) | New |

## How orders execute

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/how-orders-execute.md:71` | Commit Preview costs and limits | [Open Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--open-long-preview) | Existing |
| `how-plether-works/how-orders-execute.md:219` | Finalization countdown and manual action | [Manual Finalization Ready](http://localhost:6006/?path=/story/perps-final-reveal-modal--manual-finalization-ready) | Existing |
| `how-plether-works/how-orders-execute.md:444` | Final Result values | [Automatically Finalized Success](http://localhost:6006/?path=/story/perps-final-reveal-modal--automatically-finalized-success) | Existing |
| `how-plether-works/how-orders-execute.md:446` | Frozen close final result | [Frozen Close Result](http://localhost:6006/?path=/story/documentation-trader-claims--frozen-close-result) | New |
| `how-plether-works/how-orders-execute.md:497` | Pending and expired Open Orders | [Open Orders Pending and Expired](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending-and-expired) | Extended |

## How PnL is calculated

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/how-pnl-is-calculated.md:89` | Complete Current Position fields | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Extended |
| `how-plether-works/how-pnl-is-calculated.md:264` | Unrealized PnL tooltip | [Unrealized PnL Tooltip](http://localhost:6006/?path=/story/perps-account-panel--unrealized-pnl-tooltip) | Extended |
| `how-plether-works/how-pnl-is-calculated.md:511` | Margin Account summary | [Margin Account Summary](http://localhost:6006/?path=/story/perps-trade-ticket--margin-account-summary) | Extended |
| `how-plether-works/how-pnl-is-calculated.md:556` | Close row and gross Result | [Transaction History Close Result](http://localhost:6006/?path=/story/perps-account-panel--transaction-history-close-result) | Extended |
| `how-plether-works/how-pnl-is-calculated.md:558` | Complete close receipt reconciliation | [Completed Full Close](http://localhost:6006/?path=/story/documentation-trader-claims--completed-full-close) | New |
| `how-plether-works/how-pnl-is-calculated.md:560` | Claim balance, status and action | [Available to Settle](http://localhost:6006/?path=/story/documentation-trader-claims--available-to-settle) | New |

## Margin, leverage and liquidation

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/margin-leverage-and-liquidation.md:166` | Current Position annotations | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Extended |
| `how-plether-works/margin-leverage-and-liquidation.md:203` | Position and Edit Position Margin modal | [Edit Position Margin](http://localhost:6006/?path=/story/perps-account-panel--edit-position-margin) | Extended |
| `how-plether-works/margin-leverage-and-liquidation.md:368` | Pending close remains exposed | [Open Orders Pending](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending) | Extended |

## Market states and oracle closures

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/market-states-and-oracle-closures.md:313` | Margin Call Simulator warning | [Margin Call Simulator Confirmation](http://localhost:6006/?path=/story/perps-trade-ticket--margin-call-simulator-confirmation) | Extended |
| `how-plether-works/market-states-and-oracle-closures.md:546` | Open-market countdown | [Open Then Close Only](http://localhost:6006/?path=/story/perps-market-state-panel--open-then-close-only) | Existing |
| `how-plether-works/market-states-and-oracle-closures.md:548` | Close-only and Reduce-only ticket | [Close Only Reduce Only](http://localhost:6006/?path=/story/documentation-trader-workspace--close-only-reduce-only) | New |
| `how-plether-works/market-states-and-oracle-closures.md:550` | Stale oracle freshness | [Stale Oracle](http://localhost:6006/?path=/story/perps-instrument-panel--stale-oracle) | Extended |
| `how-plether-works/market-states-and-oracle-closures.md:552` | Frozen close VPI and fixed spread | [Oracle Frozen Close](http://localhost:6006/?path=/story/perps-trading-regime-comparison--oracle-frozen-close) | Existing |

## Settlement liquidity and trader claims

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/settlement-liquidity-and-trader-claims.md:245` | Close settlement result | [Automatically Finalized Success](http://localhost:6006/?path=/story/perps-final-reveal-modal--automatically-finalized-success) | Existing |
| `how-plether-works/settlement-liquidity-and-trader-claims.md:328` | Trader claim panel | [Available to Settle](http://localhost:6006/?path=/story/documentation-trader-claims--available-to-settle) | New |
| `how-plether-works/settlement-liquidity-and-trader-claims.md:457` | Complete HousePool liquidity breakdown | [LP Overview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--overview) | New prototype |

## HousePool and tranche waterfall

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/the-housepool-and-tranche-waterfall.md:377` | Deposit request lifecycle | [Pending Deposit](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--pending-deposit) | New prototype |
| `how-plether-works/the-housepool-and-tranche-waterfall.md:603` | Senior and Junior overview | [LP Overview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--overview) | New prototype |
| `how-plether-works/the-housepool-and-tranche-waterfall.md:605` | LP withdrawal preview | [LP Withdrawal Preview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--withdrawal-preview) | New prototype |
| `how-plether-works/the-housepool-and-tranche-waterfall.md:620` | Pool liquidity tooltip | [Pool Liquidity Tooltip Visible](http://localhost:6006/?path=/story/perps-instrument-panel--pool-liquidity-tooltip-visible) | Extended |

## Trading costs

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/trading-costs-fees-carry-and-vpi.md:716` | Commit Preview cost lines | [Open Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--open-long-preview) | Existing |
| `how-plether-works/trading-costs-fees-carry-and-vpi.md:718` | Frozen close preview costs | [Oracle Frozen Close](http://localhost:6006/?path=/story/perps-trading-regime-comparison--oracle-frozen-close) | Existing |
| `how-plether-works/trading-costs-fees-carry-and-vpi.md:729` | Market header and accrued position carry | [Market and Account Readiness](http://localhost:6006/?path=/story/documentation-trader-workspace--market-and-account-readiness) | New |
| `how-plether-works/trading-costs-fees-carry-and-vpi.md:744` | Estimated-to-final result | [Automatically Finalized Success](http://localhost:6006/?path=/story/perps-final-reveal-modal--automatically-finalized-success) | Existing |

## LP quickstart

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `liquidity-provider-quickstart.md:143` | LP overview | [LP Overview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--overview) | New prototype |
| `liquidity-provider-quickstart.md:169` | Deposit preview | [LP Deposit Preview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--deposit-preview) | New prototype |
| `liquidity-provider-quickstart.md:258` | Pending deposit | [Pending Deposit](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--pending-deposit) | New prototype |
| `liquidity-provider-quickstart.md:289` | LP position | [LP Position](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--position) | New prototype |
| `liquidity-provider-quickstart.md:349` | Withdrawal preview | [LP Withdrawal Preview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--withdrawal-preview) | New prototype |

## Check and settle a trader claim

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/check-and-settle-a-trader-claim.md:49` | Close created a claim | [Close Created Claim](http://localhost:6006/?path=/story/documentation-trader-claims--close-created-claim) | New |
| `trading-on-plether-perps/check-and-settle-a-trader-claim.md:83` | Claim balance and status | [Waiting for Liquidity](http://localhost:6006/?path=/story/documentation-trader-claims--waiting-for-liquidity) | New |
| `trading-on-plether-perps/check-and-settle-a-trader-claim.md:159` | Settlement confirmation | [Settlement Confirmation](http://localhost:6006/?path=/story/documentation-trader-claims--settlement-confirmation) | New |
| `trading-on-plether-perps/check-and-settle-a-trader-claim.md:193` | Zero claim and account credit | [Successfully Settled](http://localhost:6006/?path=/story/documentation-trader-claims--successfully-settled) | New |

## Gas-sponsored trading and Trading Account

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md:33` | Wallet, Trading Account, model and Margin Account | [Account Identity](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--account-identity) | New |
| `trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md:152` | Trading Account setup | [Account Identity](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--account-identity) | New |
| `trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md:190` | First deposit authorization and batch | [First Deposit Authorization](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--first-deposit-authorization) | New |
| `trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md:221` | Sponsored withdrawal destination | [Withdrawal Confirmation](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--withdrawal-confirmation) | New |
| `trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md:312` | Sponsor unavailable | [Sponsor Unavailable](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--sponsor-unavailable) | New |

## Open or increase a position

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/open-or-increase-a-position.md:65` | Market and account readiness | [Market and Account Readiness](http://localhost:6006/?path=/story/documentation-trader-workspace--market-and-account-readiness) | New |
| `trading-on-plether-perps/open-or-increase-a-position.md:247` | Opening preview | [Open Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--open-long-preview) | Existing |
| `trading-on-plether-perps/open-or-increase-a-position.md:251` | Increase preview | [Increase Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--increase-long-preview) | Existing |
| `trading-on-plether-perps/open-or-increase-a-position.md:386` | Pending order | [Open Orders Pending](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending) | Extended |
| `trading-on-plether-perps/open-or-increase-a-position.md:460` | Executed position | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Extended |

## Read your position and account health

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/read-your-position-and-account-health.md:37` | Market header and protocol state | [Degraded Market and Account](http://localhost:6006/?path=/story/documentation-trader-workspace--degraded-market-and-account) | New |
| `trading-on-plether-perps/read-your-position-and-account-health.md:54` | Complete Current Position | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Extended |
| `trading-on-plether-perps/read-your-position-and-account-health.md:220` | Edit Position Margin | [Edit Position Margin](http://localhost:6006/?path=/story/perps-account-panel--edit-position-margin) | Extended |
| `trading-on-plether-perps/read-your-position-and-account-health.md:237` | Margin Account summary | [Margin Account Summary](http://localhost:6006/?path=/story/perps-trade-ticket--margin-account-summary) | Extended |

## Reduce or close a position

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/reduce-or-close-a-position.md:64` | Open position and close context | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Extended |
| `trading-on-plether-perps/reduce-or-close-a-position.md:127` | Reduce ticket | [Reduce Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--reduce-long-preview) | Existing |
| `trading-on-plether-perps/reduce-or-close-a-position.md:160` | Partial-reduction preview | [Reduce Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--reduce-long-preview) | Existing |
| `trading-on-plether-perps/reduce-or-close-a-position.md:162` | Full-close review | [Close Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--close-long-preview) | Existing |
| `trading-on-plether-perps/reduce-or-close-a-position.md:225` | Pending close | [Open Orders Pending](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending) | Extended |
| `trading-on-plether-perps/reduce-or-close-a-position.md:504` | Partial reduction and remaining position | [Executed Partial Reduction](http://localhost:6006/?path=/story/documentation-trader-workspace--executed-partial-reduction) | New |
| `trading-on-plether-perps/reduce-or-close-a-position.md:506` | Full-close final costs and claim | [Completed Full Close](http://localhost:6006/?path=/story/documentation-trader-claims--completed-full-close) | New |

## Trader troubleshooting

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/trader-troubleshooting.md:19` | Complete market, account and order context | [Market and Account Readiness](http://localhost:6006/?path=/story/documentation-trader-workspace--market-and-account-readiness) | New |
| `trading-on-plether-perps/trader-troubleshooting.md:123` | Disabled Review error states | [Disabled Review Messages](http://localhost:6006/?path=/story/documentation-trader-workspace--disabled-review-messages) | New |
| `trading-on-plether-perps/trader-troubleshooting.md:202` | Finalization and expired cleanup | [Pending Finalization and Cleanup](http://localhost:6006/?path=/story/documentation-trader-workspace--pending-finalization-and-cleanup) | New |
| `trading-on-plether-perps/trader-troubleshooting.md:269` | Failed Order History | [Order History Failures](http://localhost:6006/?path=/story/perps-account-panel--order-history-failures) | Extended |
| `trading-on-plether-perps/trader-troubleshooting.md:368` | Withdraw exceeds limit | [Withdraw Exceeds Available](http://localhost:6006/?path=/story/perps-trade-ticket--withdraw-exceeds-available) | Extended |
| `trading-on-plether-perps/trader-troubleshooting.md:539` | Support attachment: Open Orders | [Open Orders Pending and Expired](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending-and-expired) | Extended |
| `trading-on-plether-perps/trader-troubleshooting.md:540` | Support attachment: Order History | [Order History Failures](http://localhost:6006/?path=/story/perps-account-panel--order-history-failures) | Extended |
| `trading-on-plether-perps/trader-troubleshooting.md:541` | Support attachment: account or position field | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Extended |

## Why is my order pending or failed?

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/why-is-my-order-pending-or-failed.md:52` | Four failure classes side by side | [Failure State Comparison](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--failure-state-comparison) | New |
| `trading-on-plether-perps/why-is-my-order-pending-or-failed.md:239` | Finalization modal details | [Manual Finalization Ready](http://localhost:6006/?path=/story/perps-final-reveal-modal--manual-finalization-ready) | Existing |
| `trading-on-plether-perps/why-is-my-order-pending-or-failed.md:355` | Pending and expired Open Orders | [Open Orders Pending and Expired](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending-and-expired) | Extended |

## Your Margin Account

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/your-margin-account.md:49` | Margin Account overview | [Margin Account Summary](http://localhost:6006/?path=/story/perps-trade-ticket--margin-account-summary) | Extended |
| `trading-on-plether-perps/your-margin-account.md:135` | Deposit Margin | [Deposit Margin](http://localhost:6006/?path=/story/perps-trade-ticket--deposit-margin) | Extended |
| `trading-on-plether-perps/your-margin-account.md:178` | Pending reservations | [Open Orders Pending](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending) | Extended |
| `trading-on-plether-perps/your-margin-account.md:226` | Edit Position Margin | [Edit Position Margin](http://localhost:6006/?path=/story/perps-account-panel--edit-position-margin) | Extended |
| `trading-on-plether-perps/your-margin-account.md:288` | Withdraw Margin | [Withdraw Margin](http://localhost:6006/?path=/story/perps-trade-ticket--withdraw-margin) | Extended |
| `trading-on-plether-perps/your-margin-account.md:346` | Trader claim | [Available to Settle](http://localhost:6006/?path=/story/documentation-trader-claims--available-to-settle) | New |

## Inventory result

- Screenshot references found: **79**
- References mapped to a Storybook target: **79**
- Unmapped references: **0**
- LP references use clearly labelled documentation prototypes because the LP frontend is not implemented.
