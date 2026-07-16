# GitBook screenshot-to-Storybook map

This inventory maps every screenshot, screenshot placeholder and requested support screenshot in `apps/gitbook` to a deterministic Storybook state.

Acceptance criteria are internal capture requirements formerly embedded in published articles. A screenshot is complete only when every item in its acceptance-criteria cell is visible in one deterministic Storybook state. These instructions are not reader-facing documentation.

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
| Market header and oracle freshness | `PerpsInstrumentPanel` | Existing component, extended with deterministic market states |
| Unrealized PnL and pool-liquidity explanations | `PerpsMetricDetailsDocumentation.stories.tsx` | Documentation compositions pairing real interface components with stable metric guides |
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
| `trader-quickstart.md:46` | Testnet welcome window asking for the Trading Account address before minting 100,000 MockUSDC. | [Request Funds](http://localhost:6006/?path=/story/testnet-welcome-modal--request-funds) | Existing |
| `trader-quickstart.md:80` | First deposit flow showing a limited owner-wallet USDC authorization and the sponsored Trading Account deposit operation. | [First Deposit Authorization](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--first-deposit-authorization) | New |

## How orders execute

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/how-orders-execute.md:55` | Commit Preview costs and limits | [Open Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--open-long-preview) | Existing |
| `how-plether-works/how-orders-execute.md:203` | Finalization countdown and manual action | [Manual Finalization Ready](http://localhost:6006/?path=/story/perps-final-reveal-modal--manual-finalization-ready) | Existing |
| `how-plether-works/how-orders-execute.md:424` | Final Result values | [Automatically Finalized Success](http://localhost:6006/?path=/story/perps-final-reveal-modal--automatically-finalized-success) | Existing |
| `how-plether-works/how-orders-execute.md:426` | Frozen close final result | [Frozen Close Result](http://localhost:6006/?path=/story/documentation-trader-claims--frozen-close-result) | New |
| `how-plether-works/how-orders-execute.md:477` | Pending and expired Open Orders | [Open Orders Pending and Expired](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending-and-expired) | Extended |

## How PnL is calculated

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/how-pnl-is-calculated.md:75` | Complete Current Position fields | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Extended |
| `how-plether-works/how-pnl-is-calculated.md:250` | Unrealized PnL metric and settlement context | [Unrealized PnL](http://localhost:6006/?path=/story/documentation-metric-details--unrealized-pnl) | New documentation composition |
| `how-plether-works/how-pnl-is-calculated.md:497` | Margin Account summary | [Margin Account Summary](http://localhost:6006/?path=/story/perps-trade-ticket--margin-account-summary) | Extended |
| `how-plether-works/how-pnl-is-calculated.md:545` | Close row and gross Result | [Transaction History Close Result](http://localhost:6006/?path=/story/perps-account-panel--transaction-history-close-result) | Extended |
| `how-plether-works/how-pnl-is-calculated.md:547` | Complete close receipt reconciliation | [Completed Full Close](http://localhost:6006/?path=/story/documentation-trader-claims--completed-full-close) | New |
| `how-plether-works/how-pnl-is-calculated.md:549` | Claim balance, status and action | [Available to Settle](http://localhost:6006/?path=/story/documentation-trader-claims--available-to-settle) | New |

## Margin, leverage and liquidation

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/margin-leverage-and-liquidation.md:164` | Current Position annotations | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Extended |
| `how-plether-works/margin-leverage-and-liquidation.md:201` | Position and Edit Position Margin modal | [Edit Position Margin](http://localhost:6006/?path=/story/perps-account-panel--edit-position-margin) | Extended |
| `how-plether-works/margin-leverage-and-liquidation.md:366` | Pending close remains exposed | [Open Orders Pending](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending) | Extended |

## Market states and oracle closures

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/market-states-and-oracle-closures.md:294` | Margin Call Simulator warning | [Margin Call Simulator Confirmation](http://localhost:6006/?path=/story/perps-trade-ticket--margin-call-simulator-confirmation) | Extended |
| `how-plether-works/market-states-and-oracle-closures.md:527` | Open-market countdown | [Open Then Close Only](http://localhost:6006/?path=/story/perps-market-state-panel--open-then-close-only) | Existing |
| `how-plether-works/market-states-and-oracle-closures.md:529` | Close-only and Reduce-only ticket | [Close Only Reduce Only](http://localhost:6006/?path=/story/documentation-trader-workspace--close-only-reduce-only) | New |
| `how-plether-works/market-states-and-oracle-closures.md:531` | Stale oracle freshness | [Stale Oracle](http://localhost:6006/?path=/story/perps-instrument-panel--stale-oracle) | Extended |
| `how-plether-works/market-states-and-oracle-closures.md:533` | Frozen close VPI and fixed spread | [Oracle Frozen Close](http://localhost:6006/?path=/story/perps-trading-regime-comparison--oracle-frozen-close) | Existing |

## Settlement liquidity and trader claims

| Documentation reference | Required visual | Storybook target | Coverage | Acceptance criteria |
| --- | --- | --- | --- | --- |
| `how-plether-works/settlement-liquidity-and-trader-claims.md:229` | Full close reconciliation separating released margin, trading economics, immediate credit and the complete trader claim | [Completed Full Close](http://localhost:6006/?path=/story/documentation-trader-claims--completed-full-close) | New documentation component | Show released margin, realized PnL, signed VPI, execution fee, carry, frozen spread assessed, frozen spread paid, frozen spread waived, net settlement, immediate Margin Account credit and trader claim created. |
| `how-plether-works/settlement-liquidity-and-trader-claims.md:308` | Trader claim with aggregate coverage, settlement availability, Margin Account destination and action | [Available to Settle](http://localhost:6006/?path=/story/documentation-trader-claims--available-to-settle) | Extended documentation component | Show the account’s claim balance, aggregate coverage status, `Settlement available` or `Settlement unavailable`, the destination as `Margin Account`, and the `Settle claim` action. |
| `how-plether-works/settlement-liquidity-and-trader-claims.md:431` | HousePool liquidity breakdown showing the obligations protected before LP withdrawals | [LP Overview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--overview) | Extended documentation prototype | Show canonical assets, maximum live trader liability, aggregate trader claims, total withdrawal reserve, free LP liquidity and separate Senior and Junior maximum withdrawals. |

## HousePool and tranche waterfall

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/the-housepool-and-tranche-waterfall.md:368` | Deposit request lifecycle | [Pending Deposit](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--pending-deposit) | New prototype |
| `how-plether-works/the-housepool-and-tranche-waterfall.md:594` | Senior and Junior overview | [LP Overview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--overview) | New prototype |
| `how-plether-works/the-housepool-and-tranche-waterfall.md:596` | LP withdrawal preview | [LP Withdrawal Preview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--withdrawal-preview) | New prototype |
| `how-plether-works/the-housepool-and-tranche-waterfall.md:611` | Pool liquidity definition, capacities and minimums | [Pool Liquidity](http://localhost:6006/?path=/story/documentation-metric-details--pool-liquidity) | New documentation composition |

## Trading costs

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `how-plether-works/trading-costs-fees-carry-and-vpi.md:702` | Commit Preview cost lines | [Open Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--open-long-preview) | Existing |
| `how-plether-works/trading-costs-fees-carry-and-vpi.md:704` | Frozen close preview costs | [Oracle Frozen Close](http://localhost:6006/?path=/story/perps-trading-regime-comparison--oracle-frozen-close) | Existing |
| `how-plether-works/trading-costs-fees-carry-and-vpi.md:715` | Market header and accrued position carry | [Market and Account Readiness](http://localhost:6006/?path=/story/documentation-trader-workspace--market-and-account-readiness) | New |
| `how-plether-works/trading-costs-fees-carry-and-vpi.md:730` | Estimated-to-final result | [Automatically Finalized Success](http://localhost:6006/?path=/story/perps-final-reveal-modal--automatically-finalized-success) | Existing |

## LP quickstart

| Documentation reference | Required visual | Storybook target | Coverage | Acceptance criteria |
| --- | --- | --- | --- | --- |
| `liquidity-provider-quickstart.md:139` | Prototype LP overview comparing Senior and Junior alongside pool-level liquidity | [LP Overview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--overview) | Extended documentation prototype | Show the future `[Liquidity]` page with Senior and Junior cards. Include total assets, share price, target or historical return, relative risk, current deposit mode, active fee and withdrawal availability. |
| `liquidity-provider-quickstart.md:163` | Prototype tranche deposit preview with pricing, routing, fee, approval and verified vault context | [LP Deposit Preview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--deposit-preview) | Extended documentation prototype | Show the selected tranche, deposit amount, estimated shares, share price, deposit mode, active fee and approval status. The vault address must be visible or linked to the verified deployment page. |
| `liquidity-provider-quickstart.md:250` | Prototype pending-deposit request with epoch timing, lifecycle state and available actions | [Pending Deposit](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--pending-deposit) | Extended documentation prototype | Show requested USDC, selected tranche, epoch number, estimated activation time and the lifecycle `Pending → Active → Finalized → Shares claimed`. Include the appropriate `Cancel request`, `Finalize epoch` and `Claim shares` actions. |
| `liquidity-provider-quickstart.md:275` | Prototype LP position with value, share performance, withdrawal capacity and pending epochs | [LP Position](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--position) | Extended documentation prototype | Show tranche shares, current USDC value, share price, change in value, current withdrawable amount, cooldown and any pending epochs. |
| `liquidity-provider-quickstart.md:333` | Prototype LP withdrawal preview with requested amount, burned shares, fee and expected receipt | [LP Withdrawal Preview](http://localhost:6006/?path=/story/documentation-lp-interface-prototype--withdrawal-preview) | Extended documentation prototype | Show total position value, share balance, current maximum withdrawal, requested amount, shares burned, cooldown, active fee and expected wallet receipt. |

## Check and settle a trader claim

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/check-and-settle-a-trader-claim.md:40` | Close created a claim | [Close Created Claim](http://localhost:6006/?path=/story/documentation-trader-claims--close-created-claim) | New |
| `trading-on-plether-perps/check-and-settle-a-trader-claim.md:74` | Claim balance and status | [Waiting for Liquidity](http://localhost:6006/?path=/story/documentation-trader-claims--waiting-for-liquidity) | New |
| `trading-on-plether-perps/check-and-settle-a-trader-claim.md:150` | Settlement confirmation | [Settlement Confirmation](http://localhost:6006/?path=/story/documentation-trader-claims--settlement-confirmation) | New |
| `trading-on-plether-perps/check-and-settle-a-trader-claim.md:180` | Zero claim and account credit | [Successfully Settled](http://localhost:6006/?path=/story/documentation-trader-claims--successfully-settled) | New |

## Gas-sponsored trading and Trading Account

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md:25` | Wallet, Trading Account, model and Margin Account | [Account Identity](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--account-identity) | New |
| `trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md:132` | Trading Account setup | [Account Identity](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--account-identity) | New |
| `trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md:165` | First deposit authorization and batch | [First Deposit Authorization](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--first-deposit-authorization) | New |
| `trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md:190` | Sponsored withdrawal destination | [Withdrawal Confirmation](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--withdrawal-confirmation) | New |
| `trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md:281` | Sponsor unavailable | [Sponsor Unavailable](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--sponsor-unavailable) | New |

## Open or increase a position

| Documentation reference | Required visual | Storybook target | Coverage | Acceptance criteria |
| --- | --- | --- | --- | --- |
| `trading-on-plether-perps/open-or-increase-a-position.md:53` | Open-market readiness with available collateral and existing-position context | [Market and Account Readiness](http://localhost:6006/?path=/story/documentation-open-or-increase-position--market-and-account-readiness) | New documentation composition | Show the `Open` market state, Available to Trade and either an empty Position panel or an existing same-direction position. |
| `trading-on-plether-perps/open-or-increase-a-position.md:233` | Opening preview with complete exposure, risk, execution-limit and cost information | [Opening Preview](http://localhost:6006/?path=/story/documentation-open-or-increase-position--opening-preview) | New documentation composition | Show direction, exposure, leverage, margin, execution limit, liquidation price, execution fee, VPI, confidence adjustment and execution reward. |
| `trading-on-plether-perps/open-or-increase-a-position.md:235` | Current position beside its projected post-increase result | [Increase Projection Comparison](http://localhost:6006/?path=/story/documentation-open-or-increase-position--increase-projection-comparison) | New documentation composition | Place the current position beside the projected result. Include total exposure, average entry price, resulting margin, leverage and liquidation price. |
| `trading-on-plether-perps/open-or-increase-a-position.md:362` | Pending reveal with expiry, cancellation state and manual finalization | [Pending Reveal With Manual Finalization](http://localhost:6006/?path=/story/documentation-open-or-increase-position--pending-reveal-with-manual-finalization) | New documentation composition | Show `Pending reveal`, the expiry countdown, `Cancel unavailable` and the manual-finalization action. |
| `trading-on-plether-perps/open-or-increase-a-position.md:434` | Executed position paired with its matching Order History record | [Executed Position and Order History](http://localhost:6006/?path=/story/documentation-open-or-increase-position--executed-position-and-order-history) | New documentation composition | Show the updated Position panel together with the matching entry in Order History. |

## Read your position and account health

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/read-your-position-and-account-health.md:29` | Market header and protocol state | [Degraded Market and Account](http://localhost:6006/?path=/story/documentation-trader-workspace--degraded-market-and-account) | New |
| `trading-on-plether-perps/read-your-position-and-account-health.md:46` | Complete Current Position | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Extended |
| `trading-on-plether-perps/read-your-position-and-account-health.md:212` | Edit Position Margin | [Edit Position Margin](http://localhost:6006/?path=/story/perps-account-panel--edit-position-margin) | Extended |
| `trading-on-plether-perps/read-your-position-and-account-health.md:229` | Margin Account summary | [Margin Account Summary](http://localhost:6006/?path=/story/perps-trade-ticket--margin-account-summary) | Extended |

## Reduce or close a position

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/reduce-or-close-a-position.md:52` | Open position and close context | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Extended |
| `trading-on-plether-perps/reduce-or-close-a-position.md:117` | Reduce ticket | [Reduce Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--reduce-long-preview) | Existing |
| `trading-on-plether-perps/reduce-or-close-a-position.md:150` | Partial-reduction preview | [Reduce Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--reduce-long-preview) | Existing |
| `trading-on-plether-perps/reduce-or-close-a-position.md:152` | Full-close review | [Close Long Preview](http://localhost:6006/?path=/story/perps-trade-ticket--close-long-preview) | Existing |
| `trading-on-plether-perps/reduce-or-close-a-position.md:209` | Pending close | [Open Orders Pending](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending) | Extended |
| `trading-on-plether-perps/reduce-or-close-a-position.md:488` | Partial reduction and remaining position | [Executed Partial Reduction](http://localhost:6006/?path=/story/documentation-trader-workspace--executed-partial-reduction) | New |
| `trading-on-plether-perps/reduce-or-close-a-position.md:490` | Full-close final costs and claim | [Completed Full Close](http://localhost:6006/?path=/story/documentation-trader-claims--completed-full-close) | New |

## Trader troubleshooting

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/trader-troubleshooting.md:19` | Complete market, account and order context | [Market and Account Readiness](http://localhost:6006/?path=/story/documentation-trader-workspace--market-and-account-readiness) | New |
| `trading-on-plether-perps/trader-troubleshooting.md:112` | Disabled Review error states | [Disabled Review Messages](http://localhost:6006/?path=/story/documentation-trader-workspace--disabled-review-messages) | New |
| `trading-on-plether-perps/trader-troubleshooting.md:191` | Finalization and expired cleanup | [Pending Finalization and Cleanup](http://localhost:6006/?path=/story/documentation-trader-workspace--pending-finalization-and-cleanup) | New |
| `trading-on-plether-perps/trader-troubleshooting.md:258` | Failed Order History | [Order History Failures](http://localhost:6006/?path=/story/perps-account-panel--order-history-failures) | Extended |
| `trading-on-plether-perps/trader-troubleshooting.md:357` | Withdraw exceeds limit | [Withdraw Exceeds Available](http://localhost:6006/?path=/story/perps-trade-ticket--withdraw-exceeds-available) | Extended |
| `trading-on-plether-perps/trader-troubleshooting.md:528` | Support attachment: Open Orders | [Open Orders Pending and Expired](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending-and-expired) | Extended |
| `trading-on-plether-perps/trader-troubleshooting.md:529` | Support attachment: Order History | [Order History Failures](http://localhost:6006/?path=/story/perps-account-panel--order-history-failures) | Extended |
| `trading-on-plether-perps/trader-troubleshooting.md:530` | Support attachment: account or position field | [Connected Position](http://localhost:6006/?path=/story/perps-account-panel--connected-position) | Extended |

## Why is my order pending or failed?

| Documentation reference | Required visual | Storybook target | Coverage |
| --- | --- | --- | --- |
| `trading-on-plether-perps/why-is-my-order-pending-or-failed.md:34` | Four failure classes side by side | [Failure State Comparison](http://localhost:6006/?path=/story/documentation-trading-account-and-sponsorship--failure-state-comparison) | New |
| `trading-on-plether-perps/why-is-my-order-pending-or-failed.md:221` | Finalization modal details | [Manual Finalization Ready](http://localhost:6006/?path=/story/perps-final-reveal-modal--manual-finalization-ready) | Existing |
| `trading-on-plether-perps/why-is-my-order-pending-or-failed.md:330` | Pending and expired Open Orders | [Open Orders Pending and Expired](http://localhost:6006/?path=/story/perps-account-panel--open-orders-pending-and-expired) | Extended |

## Your Margin Account

| Documentation reference | Required visual | Storybook target | Coverage | Acceptance criteria |
| --- | --- | --- | --- | --- |
| `trading-on-plether-perps/your-margin-account.md:41` | Margin Account and Position overview with collateral, health and withdrawal values | [Overview](http://localhost:6006/?path=/story/documentation-margin-account--overview) | New documentation composition | Capture the Margin Account and Position panels with Available to Trade, Position margin, Portfolio value, Maintenance margin and Withdrawable visible. |
| `trading-on-plether-perps/your-margin-account.md:125` | First deposit showing owner-wallet, Trading Account and sponsored operation states | [Deposit](http://localhost:6006/?path=/story/documentation-margin-account--deposit) | New documentation composition | Capture the deposit window with owner-wallet balance, Trading Account balance, deposit amount, `Max`, authorization status and sponsored-operation status. |
| `trading-on-plether-perps/your-margin-account.md:166` | Margin Account and Open Orders paired to explain pending reservations | [Pending Reservations](http://localhost:6006/?path=/story/documentation-margin-account--pending-reservations) | New documentation composition | Pair the Margin Account and Open Orders panels. Show the change in Available to Trade after order margin and the execution reward have been reserved. |
| `trading-on-plether-perps/your-margin-account.md:210` | Add-position-margin form with current and resulting collateral and leverage | [Add Position Margin](http://localhost:6006/?path=/story/documentation-margin-account--add-position-margin) | New documentation composition | Capture available USDC, current position margin, amount being added, resulting position margin and resulting leverage. |
| `trading-on-plether-perps/your-margin-account.md:270` | Withdrawal preview with current limit, requested amount and resulting balance | [Withdrawal](http://localhost:6006/?path=/story/documentation-margin-account--withdrawal) | New documentation composition | Capture the withdrawal window with Withdrawable, requested amount and the resulting Margin Account balance. |
| `trading-on-plether-perps/your-margin-account.md:319` | Trader claim with settlement state, action and Margin Account destination | [Trader Claim](http://localhost:6006/?path=/story/documentation-margin-account--trader-claim) | New documentation composition | Show the claim amount, settlement status, `Settle claim` action and Margin Account destination. |

## Inventory result

- Screenshot references found: **79**
- References mapped to a Storybook target: **79**
- Unmapped references: **0**
- LP references use clearly labelled documentation prototypes because the LP frontend is not implemented.
