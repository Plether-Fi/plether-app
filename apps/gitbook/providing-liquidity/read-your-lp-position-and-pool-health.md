# Read your LP position and pool health

An LP[^lp] position is a balance of active Senior or Junior vault shares. The shares record your proportional claim on one tranche[^tranche] of the HousePool; they are not a fixed USDC[^usdc] balance and they do not guarantee immediate redemption.

Read the position through two separate questions:

1. **What is my tranche claim worth?** Check shares, share price and current value.
2. **How much can safely leave now?** Check **Withdrawable now**, the cooldown, free LP liquidity and protocol state.

Those values can differ substantially without either one being an interface error.

> **Current interface status**
>
> The `Vaults` interface is under development and is not yet part of the published testnet application. The in-progress navigation has **Overview**, **Performance**, **Risk** and **Your position** tabs. **Your position** reads the active onchain share balance and token, **Current value**, **Share price**, **Withdrawable now**, and funded deposit epochs.
>
> It does not yet show a holder cooldown countdown, while historical performance and 7-day or 30-day APY[^apy] are not indexed. Pending requests show their epoch, amount, activation time, batch accounting, status, and the currently available cancel, finalize, recovery, or claim action.
>
> Treat the page below as guidance for the completed LP interface, and verify all live values onchain before acting.
>
> The Perps page’s Margin Account balance and its `Deposit` or `Withdraw` controls are trader-account values. They are not your LP position.

> **Screenshot placeholder — LP position overview**
>
> Add a screenshot of **Vaults → Your position** showing the selected tranche, active shares, share token, current value, share price, withdrawable amount, cooldown and pending items after those fields are connected to live data.

### Start with the selected tranche

Senior and Junior are separate ERC-4626[^erc4626] vaults with separate share supplies, share prices, frozen-oracle surcharges and withdrawal limits.

| | Senior | Junior |
| --- | --- | --- |
| **Return profile** | Target coupon funded from Junior principal | Residual HousePool return |
| **Loss order** | Absorbs losses after Junior reaches zero | Absorbs losses first |
| **Revenue order** | Restored toward its high-water mark first | Receives residual revenue after Senior priority |
| **Withdrawal priority** | First access to free LP liquidity | Access only above the complete Senior claim |

Always confirm which tranche the position panel is showing. Shares in one vault cannot be read using the other vault’s price or withdrawal limit.

### Read your active shares

**Active shares** are the vault tokens currently held for your position. Conceptually:

```text
Tranche share price
= tranche accounting principal ÷ tranche share supply
```

```text
Current value
≈ active shares × current tranche share price
```

The exact conversion follows ERC-4626 rounding and the protocol’s virtual-share protections, so the interface or contract preview is authoritative for a transaction.

The share token identifies the vault whose accounting claim you hold. Senior and Junior shares are not interchangeable, even though both ultimately reference the same HousePool.

Pending deposit USDC is not included in active shares. Finalized-but-unclaimed shares also remain in vault escrow until you submit the separate claim transaction.

### Read the share price

LP economics appear through value per share. There is no periodic interest payment that must be harvested.

Senior share value can change through:

* coupon transferred from Junior;
* restoration toward the Senior high-water mark;
* losses that reach Senior after Junior is exhausted; and
* tranche-retained oracle-frozen[^oracle] surcharges.

Junior share value can change through:

* residual realized HousePool revenue;
* the Senior target coupon paid from Junior principal;
* first-loss absorption; and
* tranche-retained oracle-frozen surcharges.

Potential pool revenue includes collected trader losses, positive VPI[^vpi] and realized carry[^carry]. Trader profits, VPI rebates, liquidation shortfalls and bad debt can reduce LP value. Protocol execution fees and order-execution rewards are not direct LP yield.

Plether prices LP value conservatively:

* unrealized trader profits can reduce distributable LP value as liabilities;
* unrealized trader losses do not increase LP value until collected; and
* trader claims rank ahead of both LP tranches.

A share price can therefore fall. A historical increase or displayed target rate does not guarantee a future return.

For the complete allocation rules, see [The HousePool and tranche waterfall](../how-plether-works/the-housepool-and-tranche-waterfall.md#the-waterfall) and [Trading costs: fees, carry and VPI](../how-plether-works/trading-costs-fees-carry-and-vpi.md#what-lps-receive).

### Current value is not withdrawable USDC

**Current value** estimates your share of the tranche’s accounting value. **Withdrawable now** applies the live cash, priority and safety constraints to your own share balance.

| Value | What it answers | What it does not promise |
| --- | --- | --- |
| **Active shares** | How many vault tokens do I hold? | A fixed number of USDC per share |
| **Share price** | What is one share worth under current tranche accounting? | That the same price will persist |
| **Current value** | What is my complete active tranche claim worth now? | That all of it can leave immediately |
| **Withdrawable now** | What does the vault currently allow this holder to withdraw? | That the amount will remain available until submission |

Before LP capital can leave, Plether reserves USDC for bounded live trader liabilities, outstanding trader claims and other protected amounts.

Conceptually:

```text
Free LP liquidity
= physical HousePool assets
− withdrawal reserves
```

Senior has first access to that free liquidity:

```text
Senior pool withdrawal cap
= min(free LP liquidity, Senior principal)
```

Junior can access only the amount above the complete Senior claim:

```text
Junior pool withdrawal cap
= min(
    Junior principal,
    max(free LP liquidity − Senior principal, 0)
  )
```

Your personal maximum is then constrained further by your share balance, cooldown, oracle state, protocol state and any active tranche surcharge.

This is why:

* Senior withdrawable USDC can be below Senior current value.
* Junior withdrawable USDC can be zero while Junior shares retain positive value.
* Withdrawal capacity can improve when positions close, liabilities are released or additional pool value becomes physically available.
* A large HousePool asset figure does not mean the same amount is free for LP withdrawal.

Trader claims are reserved ahead of Senior as well as Junior. “Senior” describes priority inside the LP stack, not priority over traders. See [Settlement liquidity and trader claims](../how-plether-works/settlement-liquidity-and-trader-claims.md#how-claims-affect-lp-withdrawals).

### Read the tranche and pool metrics together

No single headline number describes pool health. Use the **Overview** and **Risk** views together.

Read the data-status badge before the values. **Live onchain** means the required HousePool and vault reads completed; **Syncing** means the refresh is still in progress. **Partial onchain data** or **Onchain data unavailable** means at least part of the financial view is missing, so do not infer a zero balance or submit an action from the incomplete display.

| Metric | How to read it |
| --- | --- |
| **Tranche TVL[^tvl] / NAV[^nav]** | Current ERC-4626 accounting assets for the selected vault. It can rise or fall and is not cumulative deposits or an immediate-redemption promise. |
| **Pool withdrawal cap** | Current pool-level ceiling for the selected tranche before applying your share balance and holder cooldown. Junior’s cap is already subordinated behind the complete Senior claim. |
| **HousePool assets** | Capital backing the system. Protocol safety checks use conservative physical backing rather than assuming every visible or unsolicited token is LP-owned. It is not the LP withdrawal limit. |
| **Withdrawal reserve** | Capital protected for maximum modeled live liability, aggregate trader claims and other explicit reserves. A higher reserve leaves less cash available to LPs. |
| **Free LP liquidity** | Physical cash remaining after the withdrawal reserve. This is a pool-level limit before Senior/Junior priority and holder limits. |
| **Pending trading revenue** | Trading-derived value awaiting protocol ownership assignment. Do not count it as ordinary tranche principal or free withdrawal liquidity until reconciliation assigns it. |
| **Pending recapitalization** | Recovery capital awaiting recapitalization assignment. It is not trading return and should not be treated as ordinary tranche NAV or free liquidity before assignment. |
| **Senior principal** | Current accounting value assigned to Senior. It receives withdrawal priority over Junior but remains subordinate to trader obligations. |
| **Senior high-water mark** | The protected Senior reference used to measure impairment and restoration. It is not a separate pile of USDC. |
| **Senior impairment** | Active when Senior principal is below its high-water mark. Deposits into both tranches stop, and future revenue restores Senior before Junior receives residual value. |
| **Junior principal** | Current accounting value assigned to the first-loss tranche. It can reach zero before Senior is impaired. |
| **Oracle mark** | Fresh-or-stale status of the price observation used to reconcile live trader liabilities under the active market-state policy. The current prototype does not show a numeric mark in this row. |
| **Oracle frozen** | Indicates that the onchain `oracleFrozen` state is active, not merely that the market is in its scheduled close-only runway. Tranche surcharges and extended freshness rules may apply. |

If an interface labels a figure simply **Pool liquidity**, do not assume it means total LP capital. In the current trader interface, that label represents free HousePool USDC after protected reserves.

> **Screenshot placeholder — Live HousePool state**
>
> Add a screenshot of the **Overview** tab showing HousePool assets, protected withdrawal reserve, free LP liquidity, pending trading revenue, pending recapitalization, oracle state and the selected tranche's protection-account metrics once the data is live.

### Check your cooldown before planning an exit

An immediate deposit starts a fixed one-hour withdrawal cooldown for the active shares. During that cooldown:

* the shares cannot be withdrawn;
* the shares cannot be transferred to bypass the restriction; and
* the vault’s maximum-withdraw and maximum-redeem views return zero.

Depositing more into the same vault refreshes the applicable cooldown. A share transfer propagates the relevant cooldown timestamp to the receiver.

A successful withdrawal or redemption also restarts the cooldown for the remaining shares. Multiple partial withdrawals therefore normally require another one-hour wait between transactions.

The current prototype does not show a cooldown countdown. Its live **Withdrawable now** value already reflects the holder cooldown, so a zero maximum can be a cooldown result even when the pool is otherwise healthy. When a countdown is added, read it alongside **Withdrawable now**.

### Keep pending items separate

The **Your position** view should distinguish active shares from deposit requests that have not completed their lifecycle.

For each pending item, monitor:

* selected tranche;
* funded USDC amount;
* epoch ID;
* activation time;
* whether cancellation is still available;
* finalization status;
* claimable shares; and
* the next available action.

| Pending state | Included in active **Current value**? | What to do |
| --- | --- | --- |
| Requested, before activation | No | Wait or cancel |
| Active, awaiting finalization | No | Monitor the finalization gate |
| Finalized, unclaimed | No | Claim the shares |
| Claimed | Yes | Verify the active share balance |

Do not add the request-time estimated shares to your active balance. The batch price and exact share quantity are fixed only at finalization.

See [Manage a pending deposit](manage-a-pending-deposit.md) for cancellation, finalization, impairment recovery and claim instructions.

### Read impairment and wipeout warnings

Senior impairment is defined as:

```text
Senior principal < Senior high-water mark
```

During impairment:

* ordinary deposits into both tranches are blocked;
* Senior shares remain claims on reduced Senior principal;
* future HousePool revenue restores Senior toward the high-water mark before Junior receives residual value; and
* withdrawal availability still depends on free cash and runtime state.

Junior can be completely exhausted without Senior yet being impaired. Senior can also be fully wiped out if losses continue after Junior reaches zero.

A tranche with shares outstanding and zero accounting assets is terminally wiped. An ordinary new deposit cannot silently revive it or transfer existing holders’ recovery rights to a new depositor.

Treat an impairment or wipeout indicator as an accounting loss warning, not merely a temporary withdrawal warning.

### Check the current market and protocol state

| State | LP implication |
| --- | --- |
| **Scheduled close-only, oracle live** | Normal LP freshness, cooldown and liquidity rules continue; no frozen-market surcharge applies solely because of the schedule |
| **Oracle frozen, mark still eligible** | Entry and exit may continue under the selected tranche’s live surcharge; retained value stays in that tranche |
| **Oracle data over-stale** | Deposit finalization and withdrawals can be blocked; public withdrawal capacity can fall to zero |
| **HousePool paused** | New deposits are blocked; the pause alone does not necessarily block protective withdrawals |
| **Degraded mode** | LP withdrawals and new trader risk are blocked while closes, liquidations and recovery paths remain available |
| **Senior impaired** | Ordinary deposits into both tranches stop; restoration takes priority over Junior residual return |

The current interface uses an asset-denominated withdrawal: the amount entered is the target USDC receipt. During `oracleFrozen`, the surcharge increases the shares burned to deliver that target. The preview does not yet itemize the rate or share-cost calculation, so verify both from the decoded call and onchain configuration. A separate share-denominated redemption would return less USDC and is not currently exposed. In either path, retained value remains in the tranche, not the treasury.

The extended frozen-market freshness window is finite. **Oracle frozen** does not mean exits stay available indefinitely.

### Common readings

| What you see | How to interpret it |
| --- | --- |
| Positive current value, zero withdrawable | Check cooldown, reserves, tranche priority, oracle freshness and degraded mode |
| Junior value is positive, Junior withdrawable is zero | Check whether free LP liquidity exceeds the complete Senior claim, then check the holder cooldown, oracle freshness, degraded mode and other holder or state gates |
| Senior value is positive, but only part is withdrawable | Free LP liquidity is below the complete Senior claim or a holder/state limit applies |
| Share price declined | Trader payouts, rebates, bad debt or waterfall losses reduced tranche principal |
| Current value rose, but withdrawable did not | Accounting value increased while withdrawal reserves or holder limits still constrain cash |
| Senior realized return is below the target | The coupon is limited by available Junior principal and is not guaranteed |
| Epoch is finalized, but active shares did not increase | The batch allocation still requires a separate claim transaction |
| Oracle frozen is active | Review the tranche-specific surcharge, mark freshness and live withdrawal maximum |
| HousePool assets exceed your current value, but exit is unavailable | HousePool assets also back traders, claims, the other tranche and explicit reserves |

### A practical LP monitoring routine

Before depositing:

1. Confirm Senior or Junior and its place in the waterfall.
2. Review share price, tranche principal and Senior impairment.
3. Compare HousePool assets, withdrawal reserve and free LP liquidity.
4. Check the oracle and protocol state.
5. Confirm whether entry is immediate or pending.

While invested:

1. Check active shares and current value.
2. Keep pending requests separate and claim finalized shares.
3. Compare current value with **Withdrawable now**.
4. Monitor Senior impairment and Junior loss absorption.
5. Check outstanding withdrawal reserves and free LP liquidity.
6. Review cooldown and oracle state before planning an exit.
7. Treat performance history and the Senior coupon as variable, not guaranteed.

When you are planning an exit, continue to [Withdraw liquidity](withdraw-liquidity.md). If an amount, epoch or transaction does not match the expected state, use [LP troubleshooting](lp-troubleshooting.md) before retrying.

The distinction to remember is simple: **shares measure your accounting claim; withdrawable USDC measures how much cash can safely leave now.**

[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^erc4626]: The Ethereum tokenized-vault standard used for Plether tranche shares.
[^vpi]: Virtual Price Impact, a separate USDC charge or rebate based on how a trade changes HousePool directional imbalance.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^apy]: Annual percentage yield, an annualized return measure that includes compounding.
[^oracle]: A service that supplies external market data to smart contracts; Plether uses Pyth price feeds.
[^tvl]: Total value locked, the value reported as held by a protocol or vault.
[^nav]: Net asset value, the accounting value of a pool or tranche after assets and liabilities.
