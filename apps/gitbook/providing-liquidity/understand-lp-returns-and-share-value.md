# Understand LP returns and share value

> **LP return appears through the value of your vault shares. It is variable compensation for underwriting trader liabilities—not interest promised on a USDC deposit.**

When an LP[^lp] deposit becomes active, the selected vault issues ERC-4626[^erc4626] shares. Senior shares use the `psLP` symbol; Junior shares use `pjLP`.

Each share represents a proportional claim on one tranche[^tranche] of the HousePool. Its USDC[^usdc] value can rise or fall as the pool earns revenue, pays traders and allocates losses through the Senior–Junior waterfall.

> **Current interface status**
>
> The `Vaults` interface is under development and is not yet part of the published testnet application. Immediate deposits and synchronous withdrawals exist only on the current development branch; the pending lifecycle remains preview-only. The metrics described below belong to that in-progress frontend; historical APY[^apy] and performance data are not yet indexed.
>
> The Perps page's existing `Deposit` and `Withdraw` controls operate a trader's **Margin Account**, not an LP vault.

### What a vault share represents

Senior and Junior each have their own:

* accounting principal;
* share supply;
* share price;
* loss and revenue priority;
* frozen-oracle surcharge;
* withdrawal limit.

Conceptually:

```
Current position value
= vault shares held × current value per share
```

HousePool/LP-owned revenue and losses normally change tranche principal without changing the number of shares you hold. Your return therefore appears as a change in USDC value per share. There is no separate periodic interest payment to harvest.

The exact conversion follows ERC-4626 rounding and Plether's virtual-share protections. A share is an accounting claim on its tranche—not an unconditional claim on the same fraction of the HousePool's raw wallet balance.

### What can increase LP value

Potential sources of LP-owned value include:

| Source | How it affects LP economics |
| --- | --- |
| **Realized trader losses** | Become pool value only when physically collected |
| **Realized carry**[^carry] | Compensates LPs for bounded payout capacity committed through time |
| **Positive VPI**[^vpi] | A trader-to-HousePool charge for increasing directional imbalance |
| **Paid frozen-close spread** | Compensates LPs for eligible voluntary closes executed during `oracleFrozen` |
| **Frozen-oracle LP surcharge** | Value retained inside the affected tranche when entry or exit occurs under frozen-oracle rules |
| **Other authorized trading revenue** | Enters HousePool accounting before the tranche waterfall allocates it |

These sources are variable. They can be smaller than HousePool losses during the same period.

A frozen-close spread counts only when it is retained or collected. Any portion waived on an eligible terminal full close is uncollected revenue. It is not an LP receivable, protocol revenue or bad debt.

### What can reduce LP value

LP value can fall through:

* profits paid or owed to traders;
* negative VPI rebates funded by the HousePool;
* liquidation shortfalls and bad debt;
* losses caused by oracle, smart-contract, stablecoin, governance or operational failures;
* the Senior target coupon, when viewed from Junior;
* other losses applied through HousePool reconciliation.

Trader claims are liabilities of the HousePool and rank ahead of LP withdrawals. They are not LP revenue and cannot be reused as backing for another position.

High carry, VPI or spread revenue should never be read without the liability side of the pool. LPs earn because their capital stands behind trader payouts.

### What is not LP revenue

| Item | Economic destination or treatment |
| --- | --- |
| **Protocol execution fee** | Protocol treasury when physically cash-credited |
| **Order execution reward** | Order executor or clearer; certain liquidation cleanup can forfeit it to the treasury |
| **Liquidation bounty** | Successful liquidator |
| **Released trader margin** | Return of the trader's own collateral |
| **Recapitalization** | New capital introduced to repair backing; not trading return |
| **Unrealized trader loss** | Not an LP asset until value is physically collected |
| **Waived frozen-close spread** | Uncollected amount; not an LP receivable |
| **A new LP deposit** | New principal exchanged for shares; not yield earned by existing LPs |

The Senior target coupon also does not create new HousePool revenue. It reallocates existing value from Junior principal to Senior principal.

See [**Trading costs: fees, carry and VPI**](../how-plether-works/trading-costs-fees-carry-and-vpi.md) for the complete destination of each trader-side charge.

### How the waterfall allocates the result

HousePool reconciliation first determines the value economically distributable to LPs after protected obligations are accounted for. That result then moves through the tranche waterfall.

When the pool realizes a loss:

1. Junior principal absorbs the loss.
2. Senior absorbs only the remainder after Junior reaches zero.

When the pool realizes revenue:

1. Any Senior impairment is restored toward the Senior high-water mark.
2. Remaining ordinary revenue becomes Junior principal.

Separately, the configured Senior target coupon transfers available Junior principal to Senior. The transfer is capped by what Junior can fund. An unpaid amount does not accumulate as debt.

This is why the two share prices can move differently even though both vaults underwrite the same HousePool.

For the high-water-mark rules, coupon checkpointing and full allocation mechanics, see [**The HousePool and tranche waterfall**](../how-plether-works/the-housepool-and-tranche-waterfall.md).

### Conservative accounting is deliberately asymmetric

Plether does not treat unrealized trader gains and losses symmetrically from the LP perspective:

* Unrealized trader gains can reduce distributable LP value because they are potential pool liabilities.
* Unrealized trader losses do not increase LP value until they are physically collected.

This prevents LPs from withdrawing against money the HousePool does not yet possess. It can also temporarily understate tranche value, particularly in Junior.

Realization can therefore change share economics materially. A losing trader position is not spendable LP value merely because its current mark favors the pool.

Deposit pricing and withdrawal reconciliation also answer different questions. Plether does not offer new LPs discounted shares merely because conservative aggregate accounting temporarily reserves unrealized trader gains. When positions are open, ordinary entry instead uses a delayed deposit epoch so the batch price can be fixed later under the protocol's deposit rules.

### Read each `Vaults` metric as a different question

| Metric | What it tells you | What it does not tell you |
| --- | --- | --- |
| **Tranche TVL / NAV**[^nav] | Current accounting value assigned to the tranche | How much every holder can withdraw now |
| **Share price** | Current accounting value per `psLP` or `pjLP` share | A guaranteed future redemption price |
| **7d APY** | A historical annualized return display, once indexed and supported | A promised rate or forecast |
| **Return model** | Senior target coupon or Junior residual return | A guaranteed return amount |
| **Pool withdrawal cap** | Current pool-level free-liquidity constraint | Your personal maximum after tranche and holder checks |
| **Current value** | Current accounting value of your share balance under **Your position** | Wallet USDC or an unconditional cash claim |

Until indexed history is available, treat unavailable or placeholder performance fields as missing data—not as zero return and not as evidence of a specific APY.

Senior and Junior share prices are not directly comparable. A lower numerical price does not make one tranche “cheaper”; each vault has a separate supply, principal and risk position.

### Share value is not withdrawable USDC

Plether can remain solvent while having less free cash than LPs collectively want to withdraw.

Before permitting an LP exit, the withdrawal firewall reserves physical HousePool USDC for:

* maximum bounded liability on remaining trader positions;
* outstanding trader claims;
* other protected claimant buckets and explicit reserves.

Only the remaining free LP liquidity can leave. Senior receives first access within that amount; Junior can withdraw only above the complete Senior claim.

Your live withdrawal amount is then further limited by:

* your share balance;
* the holder cooldown;
* current oracle and protocol state;
* degraded-mode restrictions;
* any active tranche-specific frozen-oracle surcharge.

Consequently:

* Senior accounting value can exceed Senior's current withdrawal capacity.
* Junior can have positive share value and zero current withdrawal capacity.
* A positive `Pool withdrawal cap` does not guarantee that your requested amount will pass every check.
* Withdrawal capacity can improve or deteriorate as positions, claims and physical cash change.

Read [**Settlement liquidity and trader claims**](../how-plether-works/settlement-liquidity-and-trader-claims.md) for why traders rank first, then [**Withdraw liquidity**](withdraw-liquidity.md) for the holder-level exit flow.

### A deposit does not lock a rate or redemption value

The deposit preview estimates how many shares an amount of USDC may buy. It does not lock:

* a future APY;
* a future share price;
* an amount of USDC available on demand;
* the final shares for a pending deposit epoch;
* a frozen-oracle surcharge that may change before pending-epoch finalization.

For a pending deposit, the final batch share price is fixed at epoch finalization. Until then, the depositor has funded a request but does not hold active tranche shares or earn Senior coupon or Junior residual return.

After finalization, the depositor must claim the escrowed shares. Learn the complete lifecycle in [**Manage a pending deposit**](manage-a-pending-deposit.md).

### Evaluate return with its liabilities

Before interpreting a gain or loss, ask:

1. Which tranche do the shares belong to?
2. Did HousePool trading revenue or trader payouts change during the period?
3. Was Senior being restored toward its high-water mark?
4. How much target coupon moved from Junior to Senior?
5. Are historical performance data complete and indexed?
6. What trader liabilities and claims currently rank ahead of LP withdrawals?
7. How much of the accounting value is withdrawable after tranche priority, cooldown and any active surcharge?

Continue to [**Read your LP position and pool health**](read-your-lp-position-and-pool-health.md) for the operational metrics and [**LP risks and safeguards**](lp-risks-and-safeguards.md) for the failure modes behind them.

[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^erc4626]: The Ethereum tokenized-vault standard used for Plether tranche shares.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^apy]: Annual percentage yield, an annualized return measure that includes compounding.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^vpi]: Virtual Price Impact, a separate USDC charge or rebate based on how a trade changes HousePool directional imbalance.
[^nav]: Net asset value, the accounting value of a pool or tranche after assets and liabilities.
