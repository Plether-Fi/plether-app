# Understand LP returns and share value

> **LP return appears through the value of your vault shares. It is variable compensation for underwriting trader liabilities — not fixed interest promised on a deposit.**

Active LP[^lp] deposits hold 9-decimal ERC-4626[^erc4626] tranche[^tranche] shares: **`psLP`** (Senior Vault) or **`pjLP`** (Junior Vault). Each share represents a proportional claim on one tranche's accounting principal inside the shared liquidity pool.

All LP metrics, 7-day charts and queued deposit/withdrawal controls live on the **Vaults** page (the Perps page's **Deposit** and **Withdraw** controls operate a trader Margin Account).

## How share value works

```text
Current position value ≈ wallet shares × tranche share price
Tranche share price    = tranche accounting value ÷ effective share supply
```

LP revenue and losses change tranche principal rather than minting periodic reward tokens, so performance shows up directly in USDG[^usdg] value per share. Exact conversions apply ERC-4626 rounding, Plether's `1,000`-share virtual offset (`VIRTUAL_SHARES`), and — when Junior's annual maintenance fee is nonzero — accrued fee shares in Junior's effective supply.

Because Senior and Junior have separate supplies and principals, their share prices are not directly comparable: a lower share price does not mean a cheaper entry.

## What increases LP value

| Source | How it adds value |
| --- | --- |
| **Collectible marked & collected trader losses** | Realized losses add physical USDG to the pool; open marked losses increase Terminal NAV only up to collectible collateral and same-account claims (not physical cash until realized) |
| **Realized cost of carry**[^carry] | Time-based fee paid by traders (up to `5.00%` APR max) on LP-financed payout capacity |
| **Net positive VPI**[^vpi] | Imbalance charges paid by traders when increasing pool directional skew |
| **Paid frozen-close spread** | `50 bps` spread collected from voluntary closes during `oracleFrozen` (waived amounts on full closes are excluded) |
| **LP share of liquidation charges** | `50%` of collected liquidation charges (`0.10%` of notional, min `1.00 USDG`; the other `50%` goes to the keeper, `0%` to protocol) |
| **Temporary frozen withdrawal fee** | Tranche-specific fee (`0.25%` Senior / `0.75%` Junior) retained inside that tranche when a withdrawal settles during `oracleFrozen` |

## What reduces LP value

* Realized and marked trader profits owed by the pool
* Negative VPI rebates paid to traders who reduce directional skew
* Bad debt when a trader's uncovered price loss exceeds their collateral and same-account claim
* For Junior specifically: target coupon transfers to Senior, and share dilution if a nonzero **Annual vault fee** is configured (`0%` on the active v2.1.0 deployment; changes require a `72-hour` timelock)
* Oracle, smart-contract, USDG or operational failures

## What is *not* LP revenue

| Item | Actual destination or treatment |
| --- | --- |
| **Protocol execution fee (`0.04%`)** | Protocol treasury |
| **Opening execution reward (`0.01–0.25 USDG`)** | Executing or cleanup keeper (or treasury if forfeited on liquidation) |
| **Liquidation keeper share (`50%`, min `0.50 USDG`)** | Executing liquidator |
| **Released trader margin** | Returned to the trader |
| **New LP deposits or recapitalization** | New capital exchanged for shares, not yield |
| **Uncollectible marked losses & waived close charges** | Excluded from LP NAV and earnings |
| **Senior target coupon** | Reallocation from Junior to Senior, not new pool income |

## How the waterfall splits returns and losses

1. **Losses:** Junior absorbs reconciled losses first until Junior principal reaches zero; Senior absorbs only excess losses beyond Junior.
2. **Gains and recovery:** If Senior is impaired (`Senior principal < Senior high-water mark`), reconciled LP-owned gains restore Senior first; remaining ordinary LP revenue flows to Junior.
3. **Senior target coupon:** Transfers available Junior principal to Senior up to the configured nominal rate (`8.00%` APR on current test deployments), capped by remaining Junior principal. Unpaid coupon never accrues as debt.

On the **Junior Vault → Performance** tab, the **Return breakdown** panel breaks down Junior's 7-day share-price movement across these exact revenue, loss and Senior-coupon components whenever complete deployment-matched history is available.

## Terminal NAV vs withdrawable cash

Plether prices queued hourly deposits and withdrawals from one exact signed, collateral-capped **Terminal NAV** snapshot:

* Marked trader gains subtract from LP NAV immediately as potential pool liabilities.
* Marked trader losses add to LP NAV only up to collectible collateral — and remain non-cash receivables until collected.

Separately, the **withdrawal firewall** reserves physical pool USDG for maximum bounded trader liabilities, the `0.25%` settlement buffer and all outstanding trader claims before allocating free cash in strict hourly order (**Senior withdraw → Junior withdraw → Junior deposit → Senior deposit**). That is why **Current vault value** and **Share price** can be positive while **Available withdrawal liquidity** is lower or zero — especially for Junior behind Senior priority.

## Reading Performance metrics without traps

* **7d realized APY**[^apy] and **7d return** measure historical hourly checkpoints over the last seven days. They can be negative and never forecast future returns. If complete 7-day history is unavailable, the **Performance** section is omitted (do not read omission as zero).
* Deposit and withdrawal previews never lock a share price or USDG payout: queued funds and shares remain exposed to pool performance until hourly settlement executes (see [Manage a pending deposit](manage-a-pending-deposit.md) and [Withdraw liquidity](withdraw-liquidity.md)).

Continue to [Read your LP position and pool health](read-your-lp-position-and-pool-health.md), [LP risks and safeguards](lp-risks-and-safeguards.md) or the full mechanics in [The liquidity pool and tranche waterfall](../how-plether-works/the-liquidity-pool-and-tranche-waterfall.md).

[^lp]: Liquidity provider, a participant that supplies USDG capital to the liquidity pool.
[^erc4626]: The Ethereum tokenized-vault standard used for Plether tranche shares.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^usdg]: USDG is used for margin and settlement. The testnet uses MockUSDG, a test token with no real-world value.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^vpi]: Virtual Price Impact, a separate USDG charge or rebate based on how a trade changes pool directional imbalance.
[^apy]: Annual percentage yield, an annualized return measure that includes compounding.
