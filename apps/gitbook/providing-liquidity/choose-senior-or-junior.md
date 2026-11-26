# Choose Senior or Junior

> **Both vaults underwrite the same trader liabilities. Senior changes loss and withdrawal priority relative to Junior; it does not eliminate risk.**

Plether LPs[^lp] supply USDG[^usdg] through one of two ERC-4626[^erc4626] tranche[^tranche] vaults (both issuing 9-decimal vault shares):

* **Senior Vault (`psLP`):** Last-loss LP tranche with a target return funded from Junior principal and priority over Junior withdrawals.
* **Junior Vault (`pjLP`):** First-loss LP tranche that funds the Senior target return in exchange for residual trading revenue.

Both vaults are claims on the **same liquidity pool**, not separate strategies. All LP actions happen on the **Vaults** page from your connected owner wallet (not through the Perps **Deposit** button, which funds a trader Margin Account).

## Compare the two vaults

| Dimension | Senior Vault (`psLP`) | Junior Vault (`pjLP`) |
| --- | --- | --- |
| **Vault-card label** | More protected option | Higher-risk option |
| **Return model** | Target nominal APR funded from available Junior principal | Variable residual return from trading activity |
| **Loss order** | Absorbs losses only after Junior reaches zero | Absorbs pool losses first |
| **Recovery order** | Restored to its protected high-water mark first | Receives residual revenue only after Senior is restored |
| **Withdrawal priority** | Matured Senior requests funded first | Funded after matured Senior demand, subject to the `80%` max Senior-share cap |
| **Annual maintenance fee** | None (**Zero fees**) | Live rate shown in UI (`0%` on the active v2.0.0 deployment; configurable with a `72-hour` timelock; paid by share issuance when nonzero) |
| **Can principal fall or be wiped?** | Yes (after Junior is exhausted) | Yes (before Senior is impaired) |
| **Is return guaranteed?** | No | No |

"More protected" compares Senior to Junior inside the LP stack — it does not mean risk-free or principal-protected.

## What choosing Senior means

Senior sits behind Junior for losses and ahead of Junior for withdrawals and recovery:

* Its target coupon transfers available Junior principal into Senior principal. It is **not** external yield, **not** guaranteed, and any unpaid amount does **not** accrue as debt.
* Paid coupon increases Senior's protected high-water mark while unimpaired. If a loss ever impairs Senior, future reconciled LP-owned value restores Senior toward that mark before Junior receives new residual revenue.
* Senior deposits are bounded by two governed caps visible in **Senior deposit limits** (`40,000,000 USDG` absolute limit and `80%` max Senior share of protected tranche capital).

## What choosing Junior means

Junior takes first loss and subordinated withdrawals in exchange for variable residual upside:

* Junior receives residual LP-owned revenue (collectible trader losses, carry, net positive VPI, paid frozen-close spread and the `50%` LP share of liquidation charges) after any Senior impairment is restored and the Senior target coupon is funded.
* Junior can decline — or be wiped to zero — even while Senior remains unimpaired.
* If governance sets a nonzero **Annual vault fee** (after a `72-hour` timelock), accrued fee shares dilute existing `pjLP` holders rather than withdrawing USDG from the pool.

## Decide by the outcome you can accept

| Scenario | Senior outcome | Junior outcome |
| --- | --- | --- |
| **Junior cannot cover full Senior target coupon** | Receives only what Junior can fund; shortfall does not become debt | Can be drawn down to zero by the coupon transfer |
| **Loss smaller than Junior principal** | Protected ahead of Junior | Absorbs the entire loss |
| **Loss exceeds Junior principal** | Absorbs the remainder; becomes impaired | Wiped to zero first |
| **Later pool profit while Senior is impaired** | Restored toward high-water mark first | Waits until Senior is restored |
| **Trader liabilities lock pool cash** | Cannot withdraw reserved trader cash | Cannot withdraw reserved trader cash |

If you require guaranteed principal, a fixed yield or instant on-demand liquidity, neither tranche offers that.

## Trader obligations rank ahead of both vaults

"Senior" means priority over Junior, **not** priority over traders. Before any LP withdrawal is funded, Plether reserves physical pool USDG for maximum bounded trader liabilities, the `0.25%` settlement buffer and all outstanding trader claims. Remaining free liquidity then settles strictly in four phases each hour: **1. Senior withdrawals → 2. Junior withdrawals → 3. Junior deposits → 4. Senior deposits**.

Positive NAV[^nav] is therefore not immediate cash: Junior can hold positive share value while its current withdrawal liquidity is zero, and Senior can also wait when pool cash is reserved for traders.

## Review live state before deciding

On the **Vaults** overview and individual vault page, compare **Vault value**, **Share price**, **7d realized APY** (historical only — omitted if complete 7-day data is unavailable), **Total pool funds**, **Reserved funds**, **Available liquidity**, **Estimated withdrawal liquidity**, **Senior deposit limits** (Senior) and **Market exposure** / **Return breakdown** (Junior).

Avoid common shortcuts:

* **Lower share price ≠ cheaper:** Senior and Junior have independent supplies and principals.
* **Available liquidity ≠ your personal payout:** Trader reserves, Senior priority, cooldowns and hourly batch settlement all apply.
* **Splitting 50/50 does not diversify protocol risk:** Both tranches share the same pool, contracts, oracle and USDG.

Continue to [Understand LP returns and share value](understand-lp-returns-and-share-value.md) and [LP risks and safeguards](lp-risks-and-safeguards.md), or read the canonical accounting rules in [The liquidity pool and tranche waterfall](../how-plether-works/the-liquidity-pool-and-tranche-waterfall.md).

[^lp]: Liquidity provider, a participant that supplies USDG capital to the liquidity pool.
[^usdg]: USDG is used for margin and settlement. The testnet uses MockUSDG, a test token with no real-world value.
[^erc4626]: The Ethereum tokenized-vault standard used for Plether tranche shares.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^nav]: Net asset value, the accounting value of a pool or tranche after assets and liabilities.
