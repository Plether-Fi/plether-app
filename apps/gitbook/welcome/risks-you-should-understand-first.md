# Full risk disclosures

> **Plether can bound a modeled obligation — it cannot remove risk.** Traders can lose their collateral, liquidity providers can lose principal, and either may face temporary or extended delays accessing USDG[^usdg].

For a 2-minute summary before your first test trade, read [Key risks at a glance](key-risks-at-a-glance.md). For audit status, contract immutability and timelock controls, see [Security & governance](../reference/security-and-governance.md). This page details the full economic, operational and technical risks you accept when using Plether Perps[^perps]; smart contracts and markets can also fail in undocumented ways.

## Read Plether's safeguards precisely

| Safeguard | What it does | What it does *not* do |
| --- | --- | --- |
| **Fixed `0.00–2.00` range** | Bounds modeled directional payoff | Keep external FX[^fx] inside `[0.00, 2.00]` or prevent basis risk[^basis-risk] |
| **Entry solvency check** | Rejects new opens if effective backing `< maxLiability + 0.25%` buffer | Guarantee zero bad debt, instant cash payouts or LP[^lp] principal safety |
| **No counterparty ADL[^adl]** | Another trader's default never trims your winning trade | Prevent full liquidation of your own under-margined position |
| **Delayed FIFO[^fifo] execution** | Reduces front-running and keeper price-cherry-picking | Guarantee execution, remove slippage or eliminate all MEV[^mev] |
| **Gas-sponsored Trading Account** | Removes ETH gas need for eligible web trading actions | Guarantee sponsor/bundler uptime or allow gasless direct LP vault actions |
| **LP withdrawal firewall** | Keeps trader-encumbered cash inside the pool | Guarantee immediate or fixed-date LP withdrawals |
| **Non-upgradeable perps core** | Prevents silent in-place logic changes on core perps contracts | Prove code is bug-free or make the testnet UUPS `SimpleAccount` immutable |
| **Senior tranche[^tranche] priority** | Places Junior first-loss ahead of Senior | Guarantee Senior principal or Senior APY[^apy] |

## How losses flow through the protocol

| Event | First affected | What follows |
| --- | --- | --- |
| **Trader realizes a price loss** | Same-account trader claim **first**, then position margin | Uncovered excess price loss becomes explicit LP bad debt; uncollectible full-close action charges are waived |
| **Trader realizes a profit** | Free recognized pool cash above existing claims | Funded in full immediately (to position margin if open, free USDG if flat), or recorded in full as a senior trader claim |
| **Pool realizes a loss** | Junior tranche principal | Senior absorbs excess loss only after Junior reaches zero |
| **Senior accrues target coupon** | Available Junior principal | Capped by Junior principal; unpaid coupon never accrues as debt |
| **Terminal shortfall breaches solvency** | Protocol latches **Degraded mode** | Blocks new trader risk, new LP deposits and new LP withdrawal USDG allocation until solvency recovers and the owner clears the latch |

## Trader risks

* **Directional & leverage risk:** Leverage magnifies losses relative to collateral. Even a directionally right trade can lose money after execution fees (`0.04%`), VPI[^vpi], carry[^carry] and the `0.25×` adverse confidence shift.
* **Full-liquidation & FAD jump risk:** Plether never partially liquidates. Crossing maintenance (`0.10%` live; **`2.00%` during FAD close-only**) — or having uncovered carry or an underfunded VPI rebate reserve — puts the entire position at risk of immediate permissionless liquidation (with a `0.10%` charge, min `1.00 USDG`, split `50%` keeper / `50%` LPs).
* **Isolated price collateral vs carry drain:** Free Margin Account USDG and committed opening margin do **not** back price losses or raise price equity. Carry collects from **active position margin first**, then eligible free USDG — meaning carry can lower position equity and trigger liquidation even while free USDG sits in your account.
* **Binding delayed execution:** Committed orders cannot be cancelled or repriced. Pending orders wait in global FIFO for post-commit Pyth observations, can expire (`> 30s`), fail on slippage or risk checks, and (for opens/increases) still pay their `0.01–0.25 USDG` execution reward on failure or expiry (or forfeit it to treasury on liquidation). A pending close provides **zero** liquidation protection.
* **TP/SL automation limits:** Armed TP/SL triggers queue a standard full close — they never guarantee a fill price or prevent liquidation. The current web UI submits TP/SL close intents with **uncapped `uint256.max` close bounds** (offering trigger automation, not finite slippage/cost protection). The protection worker indexes 12 blocks behind chain head, polls every `5s`, submits one transaction without fee-bumping, and automatically retries only latched `Expired` attempts; a latched exit remains binding even if price reverses.
* **Oracle, FX-closure & frozen-spread risk:** Live trading requires all 6 Pyth feeds to be fresh, aligned within `5s` and within `0.10%` confidence. Voluntary closes during `oracleFrozen` incur the `50 bps` frozen-close spread (waived only when uncollectible on a full close). Carry accrues 24/7 through every closure and outage.
* **Delayed profit settlement (trader claims):** An unfunded profit payout becomes a non-interest-bearing, non-transferable trader claim with no guaranteed settlement date. Claims settle all-or-nothing only when `Physical pool assets >= Aggregate trader claims`, credit your Margin Account (position margin if open, free USDG if flat) and can be netted first against later same-account price losses.
* **Index & basis risk:** `plDXY` is an arithmetic, non-rebalancing, DXY-inspired[^dxy] basket bounded to `[0.00, 2.00]`. Effective currency weights drift and returns can diverge from ICE DXY or single-currency expenses, especially if external FX moves outside the `2.00` cap.

## Liquidity provider risks

Before depositing, also review [LP risks and safeguards](../providing-liquidity/lp-risks-and-safeguards.md).

* **Counterparty & directional skew[^skew] risk:** The liquidity pool backs net trader exposure. Profitable traders, VPI rebates and bad debt directly reduce LP capital.
* **Junior vs Senior tranche risk:** Junior takes first loss, funds Senior's target coupon and can be wiped to zero while Senior stays intact. Senior has last-loss priority inside the LP stack, not a principal or yield guarantee — once Junior hits zero, Senior takes losses and can also be wiped out.
* **Withdrawal illiquidity & hourly queue risk:** Trader liabilities, the `0.25%` buffer and trader claims rank ahead of both vaults; Senior withdrawals settle ahead of Junior. Positive Terminal NAV (which includes non-cash collectible marked trader losses) never guarantees immediate cash. All requests execute at future hourly batch prices (with a 5-minute cutoff, narrow pre-epoch cancellation window, 1-hour cooldown that restarts on withdrawal cancellation or share returns, and `0.25%` Senior / `0.75%` Junior frozen-oracle withdrawal fees).
* **Direct wallet gas:** Vault transactions execute from your owner wallet (not your Trading Account) and require Arbitrum Sepolia ETH.

## Shared infrastructure, smart-account and governance risks

* **Pre-audit testnet status:** Plether Perps Core v2.1.0 runs on Arbitrum Sepolia with **MockUSDG** (zero real-world value). It has completed an external pre-audit security consultation on an earlier snapshot, **not** a formal production audit.
* **Core immutability vs testnet smart-account upgradeability:** Core perps contracts are non-upgradeable — defects cannot be patched in place and would require migration. Separately, sponsored trading uses a deterministic ERC-4337 `SimpleAccount` v0.8 (EntryPoint v0.8) controlled by your owner wallet (external wallet or Privy embedded wallet). On testnet, `SimpleAccount` uses a **UUPS upgradeable proxy** and does not meet production immutability requirements; losing control of your owner wallet compromises the Trading Account and all positions, margin and claims it holds.
* **First-time `safe` confirmation wait & sponsorship availability:** Deploying a new Trading Account waits for Arbitrum Sepolia **`safe`** confirmation (up to **30 minutes**) before further actions can proceed. Ongoing web trading depends on Plether's self-hosted verifying paymaster (`PletherVerifyingPaymaster`) and private Alto bundler[^bundler]. Rate limits, budget caps, simulation failures or bundler/RPC[^rpc] outages can delay actions — including urgent closes or margin additions — while positions, carry and liquidation remain live. Because the web app has no owner-wallet fallback, sponsorship outages delay web actions (though the sponsor can never forge signatures or move your funds).
* **Oracle & keeper[^keeper] liveness:** Feed compromise, Pyth outages, Hermes rate limits or offline keepers can stall order execution, liquidations, TP/SL triggers and hourly LP settlement.
* **USDG & chain risk:** Real USDG carries issuer, freeze, depeg and upgrade risks; Arbitrum sequencer outages, congestion or reorgs can disrupt execution. Always treat onchain contract state and transaction receipts as authoritative over UI or indexer views.
* **Governance & `72-hour` timelocks:** While core perps logic and the `2.00` cap are immutable, governance controls margin/carry/fee/calendar/oracle parameters under a **`72-hour` timelock** and can activate emergency pauses immediately. Timelocks provide notice — not protection against bad decisions or key compromise.

## Pre-action checklists

* **Traders:** Confirm you understand the inverse raw-basket convention, isolated position margin vs carry drain, `2.00%` FAD margin jump, binding FIFO execution, `uint256.max` TP/SL close bounds, trader-claim settlement delays, sponsorship/safe-confirmation dependencies and verified Arbitrum Sepolia addresses.
* **LPs:** Confirm you accept principal loss up to full wipeout, Senior vs Junior priority, hourly batch pricing, 1-hour cooldown rules, trader-first withdrawal firewalls, frozen withdrawal fees and direct owner-wallet ETH gas requirements.

[^usdg]: USDG is used for margin and settlement. The testnet uses MockUSDG, a test token with no real-world value.
[^perps]: Perpetual contracts, derivatives with no scheduled expiry.
[^fx]: Foreign exchange, the market for trading one currency against another.
[^basis-risk]: The risk that a hedge and the exposure it is intended to offset do not move together.
[^lp]: Liquidity provider, a participant that supplies USDG capital to the liquidity pool.
[^adl]: Auto-deleveraging, the forced reduction of profitable positions to manage counterparty insolvency.
[^fifo]: First in, first out; orders at the front of the queue are processed before later orders.
[^mev]: Maximal extractable value, value obtained by controlling transaction inclusion or ordering.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^apy]: Annual percentage yield, an annualized return measure that includes compounding.
[^vpi]: Virtual Price Impact, a separate USDG charge or rebate based on how a trade changes pool directional imbalance.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^dxy]: The U.S. Dollar Index; Plether uses its six-currency composition as inspiration but does not track raw DXY.
[^skew]: The imbalance between aggregate LONG USD and SHORT USD exposure.
[^bundler]: A service that packages smart-account operations and submits them for onchain inclusion.
[^rpc]: Remote Procedure Call, an interface used to communicate with a blockchain node.
[^keeper]: A permissionless actor or bot that submits order-finalization or protocol-maintenance transactions.
