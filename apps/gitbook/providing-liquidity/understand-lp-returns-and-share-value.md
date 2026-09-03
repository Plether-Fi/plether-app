# Understand LP returns and share value

> **LP return appears through the value of your vault shares. It is variable compensation for underwriting trader liabilities—not interest promised on a USDC deposit.**

When an LP[^lp] deposit becomes active, the selected vault issues ERC-4626[^erc4626] shares. Senior shares use the `psLP` symbol; Junior shares use `pjLP`.

Each share represents a proportional claim on one tranche[^tranche] of the liquidity pool. Its USDC[^usdc] value can rise or fall as the pool earns revenue, pays traders and allocates losses through the Senior–Junior waterfall.

> **Use the Vaults page for LP actions and performance**
>
> The current `Vaults` interface displays vault accounting, seven-day share-price history, 7d return and 7d realized APY[^apy] when the complete history for the active deployment is available. Its deposit and withdrawal controls use the hourly vault queue. The Perps page's `Deposit` and `Withdraw` controls instead operate a trader's **Margin Account**, not an LP vault.

### What a vault share represents

Senior and Junior each have their own:

* accounting principal;
* effective share supply;
* share price;
* loss and revenue priority;
* frozen-oracle withdrawal fee;
* withdrawal limit.

Conceptually:

```
Current position value
= vault shares held × current value per share
```

LP-owned revenue and losses normally change tranche principal without changing the number of shares you hold. Your return therefore appears as a change in USDC value per share. There is no separate periodic interest payment to harvest.

The exact conversion follows ERC-4626 rounding, Plether's virtual-share protections and, for Junior, accrued maintenance-fee shares in effective supply. A share is an accounting claim on its tranche—not an unconditional claim on the same fraction of the liquidity pool's raw wallet balance.

All deposits enter the hourly queue. The deposit request escrows USDC first; the final number of shares is fixed when eligible hourly settlement processes the batch. Processed shares already participate in vault performance while they are held as a claim in vault escrow. The holder then uses **Move shares to wallet** as a separate transaction.

### What can increase LP value

Potential sources of LP-owned value include:

| Source | How it affects LP economics |
| --- | --- |
| **Collectible marked trader losses** | Can increase accounting NAV up to the collateral- and claim-capped amount represented by Terminal NAV; they do not add physical withdrawal cash before collection |
| **Collected trader losses** | Add physical USDC to the liquidity pool when realized and collected |
| **Realized carry**[^carry] | Compensates LPs for bounded payout capacity committed through time |
| **Positive VPI**[^vpi] | A charge paid by a trader to the liquidity pool for increasing directional imbalance |
| **Paid frozen-close spread** | Compensates LPs for eligible voluntary closes executed during `oracleFrozen` |
| **Temporary withdrawal pricing fee** | Tranche value retained when an eligible withdrawal is funded while live market pricing is unavailable |
| **Other authorized trading revenue** | Enters pool accounting before the tranche waterfall allocates it |

These sources are variable. They can be smaller than pool losses during the same period.

A frozen-close spread counts only when it is retained or collected. Any portion waived on an eligible terminal full close is uncollected revenue. It is not an LP receivable, protocol revenue or bad debt.

### What can reduce LP value

LP value can fall through:

* profits paid or owed to traders;
* negative VPI rebates funded by the liquidity pool;
* liquidation shortfalls and bad debt;
* losses caused by oracle, smart-contract, stablecoin, governance or operational failures;
* the Senior target coupon, when viewed from Junior;
* the Junior annual vault fee, paid by issuing fee shares and diluting existing Junior holders;
* other losses applied through pool reconciliation.

Trader claims are liabilities of the liquidity pool and rank ahead of LP withdrawals. They are not LP revenue and cannot be reused as backing for another position.

High carry, VPI or spread revenue should never be read without the liability side of the pool. LPs earn because their capital stands behind trader payouts.

### What is not LP revenue

| Item | Economic destination or treatment |
| --- | --- |
| **Protocol execution fee** | Protocol treasury when physically cash-credited |
| **Order execution reward** | Order executor or clearer; certain liquidation cleanup can forfeit it to the treasury |
| **Liquidation bounty** | Successful liquidator |
| **Released trader margin** | Return of the trader's own collateral |
| **Recapitalization** | New capital introduced to repair backing; not trading return |
| **Uncollectible portion of a marked trader loss** | Excluded from LP NAV; only the collateral- and claim-capped collectible portion enters the signed Terminal NAV delta |
| **Waived frozen-close spread** | Uncollected amount; not an LP receivable |
| **A new LP deposit** | New principal exchanged for shares; not yield earned by existing LPs |

The Senior target coupon also does not create new pool revenue. It reallocates existing value from Junior principal to Senior principal.

See [**Trading costs: fees, carry and VPI**](../how-plether-works/trading-costs-fees-carry-and-vpi.md) for the complete destination of each trader-side charge.

### How the waterfall allocates the result

Pool reconciliation first determines the value economically distributable to LPs after protected obligations are accounted for. That result then moves through the tranche waterfall.

When reconciliation applies a loss:

1. Junior principal absorbs the loss.
2. Senior absorbs only the remainder after Junior reaches zero.

When reconciliation applies LP-owned value:

1. Any Senior impairment is restored toward the Senior high-water mark.
2. Remaining ordinary LP-owned value becomes Junior principal.

Separately, the configured Senior target coupon transfers available Junior principal to Senior. The transfer is capped by what Junior can fund. An unpaid amount does not accumulate as debt.

This is why the two share prices can move differently even though both vaults underwrite the same liquidity pool.

For the high-water-mark rules, coupon checkpointing and full allocation mechanics, see [**The liquidity pool and tranche waterfall**](../how-plether-works/the-liquidity-pool-and-tranche-waterfall.md).

### Terminal NAV and physical liquidity are different

Plether uses the same exact signed, collateral-capped Terminal NAV snapshot for deposit and withdrawal accounting:

* Marked trader gains reduce distributable LP value because they are potential pool liabilities.
* Marked trader losses can increase distributable LP value only up to the collectible amount capped by pledged collateral and eligible same-account claims.
* That positive marked receivable is accounting value, not physical USDC held by the liquidity pool, so it does not increase free withdrawal liquidity until collected.

Deposit and withdrawal quotes can still differ because ERC-4626 uses different rounding directions and the frozen-oracle fee applies only to withdrawal funding. Every ordinary entry uses the hourly deposit queue so an eligible batch is priced from one reconciled snapshot.

### Read each `Vaults` metric as a different question

| Metric | What it tells you | What it does not tell you |
| --- | --- | --- |
| **Current vault value** | Current accounting value assigned to the selected vault | How much every holder can withdraw now |
| **Share price** | Current accounting value per `psLP` or `pjLP` share | A guaranteed future redemption price |
| **7d realized APY** | The last seven days' share-price return annualized for comparison | A promised rate or forecast |
| **7d return** | The actual share-price change over the displayed seven days | The next seven days' result |
| **Seven-day share price** | Hourly historical checkpoints for the selected vault | Intrahour prices or a guaranteed continuous series |
| **How returns work** | Senior target return or Junior variable return model | A guaranteed return amount |
| **Estimated withdrawal liquidity** | The tranche's estimated USDC funding capacity at hourly processing | Your personal maximum after holder checks |
| **Current value** | Current accounting value of your wallet-held share balance under **Your position** | Wallet USDC or an unconditional cash claim |
| **Shares available to withdraw** | Shares currently eligible for a new withdrawal request after the holder cooldown and vault limit | USDC already allocated to a withdrawal |
| **USDC ready for wallet** | USDC already allocated to processed withdrawals and available to claim | Funds that move automatically without a wallet transaction |

The interface exposes **Performance** only when it has a complete, deployment-matched seven-day series. If that section or its APY is absent, treat the history as unavailable—not as zero return and not as evidence of a specific APY.

Senior and Junior share prices are not directly comparable. A lower numerical price does not make one tranche “cheaper”; each vault has a separate supply, principal and risk position.

### Share value is not withdrawable USDC

Plether can remain solvent while having less free cash than LPs collectively want to withdraw.

Before funding an LP withdrawal, the withdrawal firewall reserves physical USDC held by the liquidity pool for:

* maximum bounded liability on remaining trader positions;
* the configured liability-scaled settlement buffer;
* outstanding trader claims;
* other protected claimant buckets and explicit reserves.

Only the remaining free LP liquidity can leave. Matured Senior requests are funded first. Once that queue is clear, Junior is capped by remaining free cash, Junior principal and the governed Senior-share ratio; dormant Senior principal is not fully reserved.

The amount you can queue is then limited by:

* your share balance;
* the holder cooldown;
* a valid current share quote; and
* complete wallet and vault data.

Those checks determine the fixed share amount that enters the queue. The later USDC allocation also depends on oracle and protocol state, the tranche's temporary pricing fee, hourly-settlement status and available withdrawal liquidity.

Consequently:

* Senior accounting value can exceed Senior's current withdrawal capacity.
* Junior can have positive share value and zero current withdrawal capacity.
* Positive **Estimated withdrawal liquidity** does not guarantee that your requested amount will pass every check or be funded at the first eligible hour.
* Withdrawal capacity can improve or deteriorate as positions, claims and physical cash change.

Read [**Settlement liquidity and trader claims**](../how-plether-works/settlement-liquidity-and-trader-claims.md) for why traders rank first, then [**Withdraw liquidity**](withdraw-liquidity.md) for the holder-level exit flow.

### A deposit does not lock a rate or redemption value

The deposit preview estimates how many shares an amount of USDC may buy. It does not lock:

* a future APY;
* a future share price;
* an amount of USDC available on demand;
* the final shares for the queued deposit.

The contract assigns requests by their transaction's block-inclusion timestamp. Inclusion strictly before the five-minute cutoff targets the next hourly processing time; inclusion at or after it targets the following hour. Signing or sending earlier is not enough if confirmation lands after the cutoff, so the confirmed request record is authoritative. Until eligible settlement processes the request, the depositor has funded the queue but does not hold active tranche shares or earn the Senior targeted return or Junior residual return. The settlement path is permissionless, but the current interface exposes no user finalization action; when the automated LP worker is enabled, a healthy keeper submits the normal processing transaction.

After processing, the shares are active and **Shares ready** appears under **Your position**. The depositor must separately select **Move shares to wallet**. Receiving those shares starts or restarts the one-hour withdrawal cooldown for every share in that wallet's position in the selected vault. Learn the complete lifecycle in [**Manage a pending deposit**](manage-a-pending-deposit.md).

Withdrawals use the same hourly cadence. A request escrows the selected shares, and those shares continue to gain or lose value until the withdrawal is funded. Matured Senior withdrawals receive funding priority. After eligible processing allocates USDC, **USDC ready** appears and the holder separately selects **Move USDC to wallet**. Only when the full remaining-share amount quotes to zero assets and enters terminal refund state can **Return shares to wallet** also become available; ordinary insufficient-liquidity remainders remain queued. Returning shares restarts the one-hour cooldown.

### Evaluate return with its liabilities

Before interpreting a gain or loss, ask:

1. Which tranche do the shares belong to?
2. Did pool trading revenue or trader payouts change during the period?
3. Was Senior being restored toward its high-water mark?
4. How much target coupon moved from Junior to Senior?
5. Does the interface have the complete deployment-matched seven-day history, and what do **7d return** and **7d realized APY** actually measure?
6. What trader liabilities and claims currently rank ahead of LP withdrawals?
7. How much of the accounting value is withdrawable after tranche priority, cooldown and any active temporary pricing fee?

Continue to [**Read your LP position and pool health**](read-your-lp-position-and-pool-health.md) for the operational metrics and [**LP risks and safeguards**](lp-risks-and-safeguards.md) for the failure modes behind them.

[^lp]: Liquidity provider, a participant that supplies USDC capital to the liquidity pool.
[^erc4626]: The Ethereum tokenized-vault standard used for Plether tranche shares.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^apy]: Annual percentage yield, an annualized return measure that includes compounding.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^vpi]: Virtual Price Impact, a separate USDC charge or rebate based on how a trade changes pool directional imbalance.
