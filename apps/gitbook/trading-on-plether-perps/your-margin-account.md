# Your Margin Account

Every Plether trade settles through a USDC Margin Account associated with the **Trading Account**.

The connected owner wallet signs for the Trading Account, but the Trading Account owns the positions, orders, Margin Account and trader claims. Deposits credit the Margin Account. Orders reserve parts of it. Position margin supports open exposure. Fees, VPI, carry and realized PnL update it. Eligible USDC can then be withdrawn to the owner wallet.

```
Owner-wallet USDC, when used
→ Trading Account USDC
→ Margin Account USDC
→ available, assigned or reserved USDC
→ trade settlement
→ withdrawable USDC
→ owner wallet
```

### Three places USDC can appear

| Balance                  | Where it exists                                                                        | What it can do                                                        |
| ------------------------ | -------------------------------------------------------------------------------------- | --------------------------------------------------------------------- |
| **Owner-wallet USDC**    | At the connected owner-wallet address, outside the Trading Account and Plether         | Can be held normally or authorized for transfer into a Trading Account |
| **Trading Account USDC** | At the Trading Account address, outside Plether’s Margin Account                       | Can fund a sponsored deposit; it is not yet trading collateral        |
| **Margin Account USDC**  | In Plether’s internal clearinghouse accounting under the Trading Account address       | Can become available, assigned or reserved collateral                 |

With a same-address EIP-7702 account, owner-wallet USDC and Trading Account USDC are the same token balance because both roles use one address. With a separate smart account, they are balances at two different addresses.

The Margin Account has no separate wallet address. Sending USDC to a Plether contract does not credit it; use the deposit flow.

### Reading your account

The interface presents several values drawn from the same account:

| Value                         | Meaning                                                                |
| ----------------------------- | ---------------------------------------------------------------------- |
| **Owner-wallet balance**      | USDC held by the connected owner wallet outside Plether                |
| **Trading Account balance**   | USDC held by the Trading Account outside its Margin Account            |
| **Available to Trade**        | Free, unreserved USDC inside Plether                                   |
| **Position margin**           | USDC assigned to the open position                                     |
| **Pending order margin**      | USDC committed to an opening or increase order                         |
| **Reserved execution reward** | USDC set aside for the account that processes an order                 |
| **Unrealized PnL**            | Directional profit or loss under the latest usable mark                |
| **Portfolio value**           | Current account equity after PnL, carry and applicable VPI treatment   |
| **Maintenance margin**        | The account-equity threshold used to determine liquidation eligibility |
| **Withdrawable**              | The maximum amount that can currently reach the verified owner wallet  |
| **Trader claim**              | A separately recorded USDC amount owed to the Trading Account          |

These values answer different account questions. Portfolio value measures current risk equity, while Withdrawable measures the amount eligible to leave Plether now.

> **Screenshot placeholder — Margin Account overview**
>
> Capture the Margin Account and Position panels with **Available to Trade**, **Position margin**, **Portfolio value**, **Maintenance margin** and **Withdrawable** visible.

### How account USDC is allocated

Plether’s clearinghouse records the account’s total USDC and divides it into several buckets:

```
Margin Account USDC
= Available to Trade
+ Position margin
+ Pending order margin
+ Reserved settlement
```

Reserved settlement includes execution rewards associated with pending orders.

Assigning margin or committing an order moves USDC between these buckets. The account’s total USDC changes when funds enter or leave through:

* Deposits and withdrawals
* Trading fees and VPI
* Realized carry
* Realized trading losses
* HousePool payouts
* Settled trader claims
* Execution reward payments

### Portfolio value

**Portfolio value** is the interface label for current account equity.

For an account with an open position, the simplified relationship is:

```
Portfolio value
≈ reachable account collateral
+ unrealized PnL
− accrued carry
− provisional VPI rebate clawback, when applicable
```

The calculation uses the latest mark accepted by the protocol.

Eligible free USDC, assigned position margin and committed order margin remain terminally reachable and can contribute to account equity. Reserved execution rewards are excluded from account health because they are already committed to terminal order processing.

An unsettled trader claim also remains outside Portfolio value. It enters the Margin Account after successful claim settlement.

Portfolio value can change as the accepted mark changes or carry accrues. Available to Trade may remain unchanged during the same period because unrealized PnL has not yet settled into account USDC.

### Depositing USDC

To deposit:

1. Open the **Margin Account** section.
2. Select `Deposit`.
3. Enter the amount.
4. Review the owner wallet, Trading Account and USDC source.
5. Complete any requested USDC authorization.
6. Authorize the sponsored deposit operation.

When USDC is already held by the Trading Account, the sponsored operation grants the exact approval needed and deposits it into the Margin Account.

When USDC starts in a separate owner wallet and the token supports signed transfers, the wallet first signs a limited USDC transfer authorization. The sponsored Trading Account operation then receives the USDC, approves the clearinghouse and deposits the amount as one batch. If the batch reverts, the Margin Account is not credited.

After confirmation, the deposited amount enters **Available to Trade**, subject to any carry collected during the same sponsored operation.

#### Deposits and accrued carry

An account with an open position checkpoints and realizes carry when its settlement balance changes.

Plether first credits the deposit and then applies accrued carry. A deposit can therefore increase the displayed Margin Account balance by less than the amount transferred from the funding balance.

For example:

```
Deposit:                         500 USDC
Carry collected in operation:    40 USDC

Net account increase:            460 USDC
```

The complete `500 USDC` entered Plether. The account used `40 USDC` to settle carry that had already accrued.

Deposits have no market-state, oracle-freshness or degraded-mode restriction. They remain available as a protective account action.

> **Screenshot placeholder — Deposit Margin**
>
> Capture the deposit window with owner-wallet balance, Trading Account balance, deposit amount, `Max`, authorization status and sponsored-operation status.

### What happens when you commit an order

A committed order reserves account USDC before it enters the pending queue.

#### Opening and increasing

An opening or increase reserves:

* The requested order margin
* The execution reward

The margin enters the **Pending order margin** bucket. The execution reward enters reserved settlement.

Both amounts reduce **Available to Trade** and **Withdrawable** while the order remains pending.

Immediately before a successful execution, the committed margin is released for the engine to apply the order. The resulting position margin then reflects the submitted margin, execution fee, VPI and any carry realized on an existing position.

#### Reducing and closing

A reduction or close reserves its execution reward without reserving new order margin.

Plether uses free account USDC for this reward first. When free USDC is insufficient, a bounded amount can be moved from position margin after the relevant risk checks pass. This allows a risk-reducing close to be committed during stale-market conditions.

#### When the order reaches a final state

The reservation outcome depends on how the order finishes:

| Result                                  | Account effect                                                                                                                     |
| --------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| **Executed open or increase**           | Committed margin is applied to the position; the execution reward is paid                                                          |
| **Executed reduction or close**         | Released margin and net settlement return to the account; the execution reward is paid                                             |
| **Failed, expired or outside slippage** | Remaining committed margin normally returns to Available to Trade; the execution reward is paid for terminal processing            |
| **Still pending**                       | Margin and reward remain reserved                                                                                                  |
| **Liquidation**                         | Pending execution rewards are forfeited to the protocol treasury; remaining order reservations are resolved during account cleanup |

Pending orders are binding and have no trader cancellation action. Failed or expired orders may require onchain cleanup before their margin becomes available again.

Terminal account settlement has wider reach than an ordinary partial reduction. A full close or liquidation may consume committed margin from other orders when free USDC and position margin cannot cover the account obligation. Partial reductions leave committed order margin protected.

> **Screenshot placeholder — Pending reservations**
>
> Pair the **Margin Account** and **Open Orders** panels. Show the change in Available to Trade after order margin and the execution reward have been reserved.

### Position margin

Position margin is USDC assigned directly to the open position.

It determines the leverage displayed for that position. Free USDC elsewhere in the account can also support account health, although it remains outside the assigned position-margin amount.

This can produce two different views of the same account:

* **Position leverage** reflects the margin assigned to the position.
* **Account buffer** also includes eligible free USDC.

#### Adding position margin

To add margin:

1. Select the pencil icon next to **Leverage**.
2. Open `Edit Position Margin`.
3. Enter the additional amount.
4. Review the resulting position margin and leverage.
5. Select `Add Margin`.

Plether realizes accrued carry first. The requested USDC then moves from **Available to Trade** into **Position margin**.

The immediate account effect is:

```
Position margin increases
Available to Trade decreases
Exposure remains unchanged
```

Total account USDC stays unchanged apart from carry collected during the transaction.

Additional position margin can:

* Lower displayed leverage
* Increase distance from liquidation
* Reduce the position’s LP-backed borrow base
* Reduce future carry accrual

Adding margin is immediate and bypasses the delayed order queue. It remains available during stale, frozen and degraded market conditions and requires no current oracle mark.

Position margin returns to the free account bucket as exposure is reduced. A partial reduction releases a proportional share, while a complete close releases the remaining amount.

> **Screenshot placeholder — Edit Position Margin**
>
> Capture available USDC, current position margin, amount being added, resulting position margin and resulting leverage.

### Available to Trade and Withdrawable

The account follows this relationship:

```
Withdrawable
≤ Available to Trade
≤ total Margin Account USDC
```

**Available to Trade** is free, unreserved account USDC. A new order must still pass the protocol’s margin, capacity, solvency and market-state checks.

**Withdrawable** is the current wallet-out limit after withdrawal-specific checks.

#### Flat accounts

When the account has no open position, its free, unreserved USDC is generally withdrawable.

A stale mark or degraded mode does not block a flat account withdrawal. Pending order margin and reserved execution rewards continue to limit the available amount.

#### Accounts with an open position

An open-position withdrawal requires:

* A usable stored mark
* Sufficient post-withdrawal account equity
* Carry realization before funds leave
* An account outside degraded mode
* A position above the applicable liquidation threshold
* Preservation of all pending reservations

The post-withdrawal requirement is stricter than the ordinary liquidation threshold. Equity must remain above the effective margin requirement, normally at least initial margin. During the FAD window, the higher applicable FAD requirement can control.

During a scheduled oracle closure, Plether may use the stored mark within the frozen-market validity window. Once that mark exceeds the permitted age, Withdrawable falls to zero until an eligible mark becomes available.

Withdrawing while a position remains open reduces the account buffer supporting that position. Exposure and assigned position margin remain unchanged.

The displayed Withdrawable value is an estimate based on the current state. Carry, accepted marks and account reservations can change before the withdrawal operation confirms.

#### Withdrawing to the owner wallet

To withdraw:

1. Open the **Margin Account** section.
2. Select `Withdraw`.
3. Enter an amount no greater than **Withdrawable**.
4. Review the Trading Account, verified owner-wallet recipient and remaining account buffer.
5. Authorize the sponsored withdrawal operation.

For a separate smart account, the operation atomically:

1. Withdraws eligible USDC from the Margin Account to the Trading Account.
2. Transfers the same USDC from the Trading Account to its verified owner wallet.

If either step fails, neither step is applied. The USDC is not left behind in the separate Trading Account.

For a same-address Trading Account, the owner wallet and Trading Account use one address, so the withdrawn USDC reaches that address directly.

> **Screenshot placeholder — Withdraw Margin**
>
> Capture the withdrawal window with Withdrawable, requested amount and the resulting Margin Account balance.

### Reductions, closes and account settlement

A reduction releases the proportional position margin associated with the exposure being closed.

The account then receives the net close settlement:

```
Net close economics
= realized PnL
− execution fee
− signed VPI
− accrued carry
− frozen-close spread, when applicable
```

Released position margin is added separately.

```
Account movement
≈ released position margin
+ net close economics
```

A profitable close is credited immediately when the HousePool has enough unreserved cash to pay the complete fresh payout. When full payment is unavailable, the fresh payout becomes a trader claim.

### Trader claims

A trader claim records a fixed USDC obligation owed to the Trading Account by the HousePool.

While unsettled, the claim remains outside:

* Available to Trade
* Position margin
* Portfolio value
* Account health
* Withdrawable

Claim settlement becomes available once the HousePool has enough physical cash to cover aggregate trader claims. Settlement credits the account’s complete claim balance; partial claim servicing is unavailable.

The flow is:

```
Trader claim
→ Owner wallet authorizes Settle Claim
→ Sponsored settlement confirms
→ Margin Account USDC
→ Sponsored withdrawal
→ Owner wallet
```

Claim settlement and wallet withdrawal are separate sponsored operations.

An existing claim can also offset a later uncovered loss from the same account during a close or liquidation. The remaining claim balance reflects any such netting.

> **Screenshot placeholder — Trader claim**
>
> Show the claim amount, settlement status, `Settle Claim` action and Margin Account destination.

### Example: how account USDC moves

Assume a flat account deposits `5,000 USDC`.

```
Margin Account USDC:    5,000.00
Available to Trade:     5,000.00
Withdrawable:           5,000.00
```

The trader commits an opening order with:

```
Order margin:           1,200.00
Execution reward:           0.20
```

While the order is pending:

```
Margin Account USDC:    5,000.00
Available to Trade:     3,799.80
Pending order margin:   1,200.00
Reserved reward:            0.20
```

When the order executes, pending order margin is replaced by the resulting position margin. The execution reward leaves the account, and the execution fee and VPI are applied.

If the trader later adds `300 USDC` of position margin:

```
Position margin:          +300
Available to Trade:       −300
Exposure:             unchanged
```

Any carry accrued before the margin addition is collected first.

When the position closes, its remaining margin is released. Net close settlement is then credited to the Margin Account or recorded as a trader claim, depending on HousePool settlement liquidity.

### Common account situations

| Situation                                                                | Likely explanation                                                           |
| ------------------------------------------------------------------------ | ---------------------------------------------------------------------------- |
| Deposit increased Available to Trade by less than the transferred amount | Accrued carry was collected during the deposit                               |
| Available to Trade fell after committing an order                        | Order margin and the execution reward were reserved                          |
| Portfolio value exceeds Withdrawable                                     | Part of the value comes from unrealized PnL or supports the open position    |
| Free USDC is visible but Withdrawable is zero                            | Check mark freshness, degraded mode, carry and post-withdraw margin headroom |
| Adding margin left Portfolio value nearly unchanged                      | Existing account USDC moved into the position-margin bucket                  |
| A failed order still affects the account                                 | Terminal processing or cleanup may still be required                         |
| A trader claim appears without increasing Available to Trade             | The claim awaits settlement into the Margin Account                          |
| A pending close exists while health continues to decline                 | Exposure and carry remain active until the close executes                    |
