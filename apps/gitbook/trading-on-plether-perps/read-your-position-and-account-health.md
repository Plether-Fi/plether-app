# Read your position and account health

The **Current Position** panel shows executed exposure and price performance. The **Margin Account** shows the collateral supporting that position.

Plether checks price equity, carry coverage and reserve backing separately. Position margin and same-account claims support price losses. Free USDC[^usdc] covers carry; committed-order funds and reserves do not increase price equity.

A useful reading order is:

![Recommended reading order, not a calculation or state transition: market regime, position, equity and margin, liquidation context, pending obligations and spendable balances.](../.gitbook/assets/diagrams/account-health-reading-order.svg)

### Check the market state first

Before relying on a position estimate, check:

* Current Plether Dollar Index mark
* Mark timestamp
* Live, FAD[^fad] or `oracleFrozen` state
* Protocol degraded-mode status
* Pending orders on the account

Current PnL[^pnl] and health use Plether’s latest stored mark. The health calculation can still display a result when that mark is stale, so the timestamp and oracle[^oracle] state matter.

A new eligible observation may change PnL, maintenance margin and liquidation status.

During a FAD window, Plether applies the active FAD margin requirement. This can reduce account health without any change in the index.

Degraded mode is a protocol-wide containment state. It blocks new exposure and position-backed withdrawals. Closes, liquidations, mark updates and recapitalization remain available.

### Read the Current Position panel

The position panel contains:

| Field                   | Meaning                                                               |
| ----------------------- | --------------------------------------------------------------------- |
| **Direction**           | Whether the position is Long or Short plDXY Perp                      |
| **plDXY Perp exposure** | Current dollar-oriented exposure at the displayed index               |
| **Entry notional**      | Contract notional recorded at the average entry price                 |
| **Entry price**         | Average execution price of the remaining position                     |
| **Leverage**            | Current contract notional relative to assigned position margin        |
| **Liquidation price**   | Price-based boundary estimate; pending carry is shown separately      |
| **Unrealized PnL**      | Price PnL between entry and the current mark                          |
| **Cost of carry**       | Persisted unpaid carry plus carry accrued since the last checkpoint   |

![Current Position fields](../.gitbook/assets/screenshots/storybook-perps-account-panel--connected-position.png)

### Direction

A **LONG USD** position gains when the displayed Plether Dollar Index rises:

```
LONG USD unrealized PnL
= contract quantity × (current index − entry index)
```

A **SHORT USD** position gains when the displayed index falls:

```
SHORT USD unrealized PnL
= contract quantity × (entry index − current index)
```

PnL remains bounded by the protocol’s fixed `0.00–2.00` settlement range.

### Exposure and entry notional

The interface presents the dollar-oriented index:

```
D = 2.00 − B
```

Where:

* `D` is the Plether Dollar Index shown to traders.
* `B` is the underlying basket value used by contract accounting.
* `q` is the position’s contract quantity.

The two position values are derived differently:

```
Displayed plDXY Perp exposure
= q × Dcurrent
```

```
Current contract notional
= q × Bcurrent
```

```
Entry notional
= q × Bentry
```

Displayed exposure follows the public LONG USD and SHORT USD view. Contract notional[^notional] is used for maintenance margin, leverage, execution fees and liquidation-bounty calculations.

Entry notional stays unchanged between size-changing executions. An increase recalculates the average entry price. A partial reduction lowers the remaining entry notional proportionally while leaving the average entry price unchanged.

### Entry price

Entry price is the size-weighted average execution price of the remaining position.

For an increase, `size` means contract quantity rather than displayed USDC exposure:

```
New average entry
=
(existing size × existing entry
+ added size × added execution price)
÷ new total size
```

A pending increase does not alter the entry price. The value changes only after execution.

A partial reduction preserves the entry price of the remaining exposure.

### Current mark

The current mark is the latest accepted oracle observation stored by Plether.

It is used to estimate:

* Current exposure and contract notional
* Unrealized PnL
* Position equity
* Maintenance margin
* Current liquidation status
* Withdrawal headroom

The displayed mark is a valuation reference. Order execution occurs later through the FIFO[^fifo] queue and uses the eligible execution-time oracle observation.

Live and FAD-only executions may include the adverse Pyth confidence adjustment. Voluntary closes during `oracleFrozen` use the validated unshifted price and charge the separate frozen-close spread.

### Unrealized PnL

Unrealized PnL reflects price movement between entry and the current mark.

It excludes:

* Pending carry[^carry]
* A future close execution fee
* Future close VPI[^vpi]
* The frozen-close spread
* The execution reward
* A potential liquidation bounty

Opening fees and opening VPI have already been applied to the account when the position was created or increased.

The eventual close result may differ because the position remains exposed while the close waits for execution.

For the full calculation, see [**How PnL is calculated**](../how-plether-works/how-pnl-is-calculated.md).

### Cost of carry

**Cost of carry** shows persisted unpaid carry from earlier checkpoints plus carry accrued since the position’s latest checkpoint.

Pending carry:

* Reduces account equity as it accrues
* Continues during stale and frozen oracle periods
* Can consume eligible free USDC when realized; position margin remains protected
* Reduces the settlement result of a close
* Can move an account toward liquidation without a price change

A deposit, withdrawal, order reservation, margin adjustment or position change can checkpoint and realize carry.

A partial reduction settles carry accrued by the entire position through execution. The remaining position then begins a new carry period.

### Position margin and leverage

Position leverage is calculated from the contract notional and USDC assigned to the position:

```
Position leverage
=
current contract notional ÷ position margin
```

This is the leverage shown beside the position.

The leverage tooltip may also show equity leverage:

```
Equity leverage
=
current contract notional ÷ Position equity
```

Equity leverage uses position margin, same-account claims and exact price PnL. Free USDC and carry do not enter its denominator.

Two accounts with the same position leverage can therefore have different liquidation buffers.

#### Adding position margin

Select the edit control beside **Leverage** to move free Margin Account USDC into the position-margin bucket.

Adding position margin:

* Leaves position size unchanged
* Reduces displayed position leverage
* Reduces the LP-backed[^lp] carry base
* Can lower future carry accrual

This moves free settlement into price-risk backing and increases the price buffer. The maximum subtracts projected carry first. Keep free funds for future carry, since position margin cannot pay it.

Depositing adds free settlement and improves carry coverage. Assigning funds to position margin increases the price-loss buffer.

Direct removal of assigned position margin is unavailable. A reduction releases position margin proportionally, and a full close releases the remainder.

![Position-margin form](../.gitbook/assets/screenshots/storybook-perps-account-panel--edit-position-margin.png)

### Read the Margin Account

The `Margin Account` card in the trade ticket currently shows four values:

| Field                  | Meaning                                                                |
| ---------------------- | ---------------------------------------------------------------------- |
| **Settlement balance** | All clearinghouse USDC, including locked margin and reserves; excludes price PnL and claims |
| **Position equity**    | Signed position margin + same-account claim + exact price PnL |
| **Unrealized PnL**     | Price-only PnL of the open position                                    |
| **Maintenance margin** | Current equity requirement for avoiding liquidation                    |
| **Withdrawable**       | Amount currently permitted to leave the protocol                       |

`Available to Trade` is a separate context row above the exposure input. Aggregate pending-order margin and execution-reward reserves are not shown as rows in the current card; use **Open Orders** to identify active commitments. Assigned position margin appears in `Edit Position Margin`, and a claim appears in a separate **Trader claim** card when one exists.

### Settlement balance and Position equity

**Settlement balance** is all USDC credited to the clearinghouse account, including free funds, assigned margin and locked reserves. It excludes unrealized PnL and unsettled claims.

For an open position:

```
Position equity = assigned position margin + same-account trader claim + exact price PnL
```

The UI preserves negative signed position equity. This figure excludes free settlement, pending-order funds and reserves. Carry is paid from free settlement and checked separately; negative VPI requires its dedicated reserve.

When no position exists, Position equity is omitted. Settlement balance remains visible. A failed or invalidated account snapshot makes current risk metrics unavailable until a successful refresh.

### Available to Trade

Available to Trade is unencumbered settlement USDC:

```
Available to Trade
=
Margin Account balance
− position margin
− committed-order margin
− reserved settlement
```

It can fund:

* New order margin
* Execution rewards
* Trading costs
* Loss settlement
* Withdrawals that pass the withdrawal checks

Unrealized profit increases Position equity but does not increase Available to Trade until it is realized and credited.

A pending opening order reduces Available to Trade immediately by reserving margin and its execution reward. Live position size remains unchanged until execution.

Pending carry may still be collected when the next account action checkpoints the position. The usable amount after that checkpoint can therefore be lower than the preceding display.

### Withdrawable

Withdrawable is the maximum amount currently permitted to leave the Margin Account.

For a flat account, it generally equals free USDC after active reservations.

With an open position, Plether also checks:

* Pending carry
* Current account equity
* Initial-margin headroom after withdrawal
* Active FAD requirements
* Mark availability and freshness
* Degraded mode
* Existing account reservations

The contract returns the full free settlement remaining after projected carry only if the position clears the stricter of initial margin and the active maintenance/FAD requirement and the other withdrawal checks pass. Otherwise it returns zero. Free settlement does not enter position equity, so withdrawing free funds does not move the price threshold.

Withdrawable can be lower than Available to Trade. It becomes zero for an account with an open position when:

* The required mark is unavailable or too stale
* The withdrawal would breach the post-withdraw margin requirement
* The protocol is in degraded mode

Closing or reducing exposure remains available through its separate rules.

### Pending-order margin

Margin committed to an opening or increase remains locked until the order executes or reaches a terminal outcome.

While pending:

* It is unavailable for another order or withdrawal.
* It is separate from price-risk backing.
* It creates no additional live exposure.
* Terminal settlement can use eligible commitments for action obligations under its separate rules.

After execution, the required amount moves into the active position-margin bucket.

### Execution-reward reserves

Every queued order reserves an execution reward.

Once reserved, that USDC:

* Leaves Available to Trade
* Remains separate from price equity and cannot cover carry
* Pays the account that performs terminal processing
* Remains payable after execution, terminal failure or expiry

A close uses free USDC first. When permitted by close-path risk checks, it can source the reward from assigned position margin.

The current ticket still checks the quoted reward against `Available to Trade` before review. Position-margin sourcing is a protocol fallback rather than a selectable interface option.

Position-margin sourcing lowers position margin and account health immediately while the full exposure remains open.

A failed close still pays the execution reward. Review health again before submitting a replacement.

### Trader claims

A same-account trader claim is included in position equity and can offset price losses. It is not free buying power, deposited settlement or withdrawable cash.

Settling a claim while a position is open turns it into position margin, preserving the combined margin-plus-claim price backing. Settling while flat credits account funds. Settlement and withdrawal are separate actions.

### Maintenance margin

Maintenance margin is the current equity requirement for the position:

```
Maintenance margin
=
current contract notional × active maintenance margin rate
```

The active onchain parameters determine the rate.

During a FAD window, the FAD margin rate replaces the ordinary maintenance rate. A position can become liquidatable when FAD begins even if its size, collateral and mark remain unchanged.

Use the active value shown by the interface rather than relying on a previously quoted percentage.

### Read account health

The interface does not display a named health percentage. For a practical screen check, compare **Position equity** with **Maintenance margin**. The underlying relationship can be expressed using signed net equity:

```
Health ratio
=
position equity ÷ maintenance margin
```

The same value expressed as a percentage is:

```
Health percentage
=
position equity ÷ maintenance margin × 100%
```

| Health         | Meaning                                           |
| -------------- | ------------------------------------------------- |
| Above `100%`   | Price equity exceeds maintenance; separate carry/reserve checks still apply |
| Exactly `100%` | Position is liquidatable                          |
| Below `100%`   | Position is liquidatable                          |

For positive maintenance, this ratio describes price risk only. With zero maintenance, the ratio is undefined; compare signed equity directly with zero. The contract’s account-liquidatable flag remains authoritative.

The price-risk test is:

```
Liquidatable when
position equity ≤ maintenance margin
```

There is no grace period after the condition is reached. An eligible keeper[^keeper] can submit a liquidation.

The absolute buffer is:

```
Liquidation buffer
=
position equity − maintenance margin
```

This is the amount by which equity currently exceeds the requirement. Both sides of the equation can change: Price PnL, pledged margin and claims move position equity; price and market state can move maintenance margin. Carry coverage is checked separately.

### Liquidation price

The liquidation price estimates the displayed index level at which equity reaches maintenance margin.

For the public Plether Dollar Index:

* **LONG USD** becomes liquidatable at or below its liquidation price.
* **SHORT USD** becomes liquidatable at or above its liquidation price.

The boundary is inclusive.

The price threshold changes when position margin, same-account claims, exact entry cost, size or the active maintenance/FAD rate changes. It does not move merely because the central market mark changes or free funds are deposited.

Carry and reserve deficiencies are independent conditions. The **Liquidatable** notice reports contract status and shows those reasons when verified. A failed refresh displays **Unavailable**, and stop-loss changes wait for current risk data.

#### “Not in range”

**Not in range** means the current calculation finds no liquidation threshold inside the fixed `0.00–2.00` settlement range.

A changed position, claim balance or FAD rate can create a price threshold. Carry and reserve deficiencies can independently cause liquidation even without a price boundary. **Unavailable** means required risk data is missing or awaiting refresh; it must not be read as **Not in range**.

#### Execution-time liquidation price

Actual liquidation uses an eligible Pyth observation with the liquidation-specific adverse confidence adjustment:

* LONG USD is evaluated at a lower dollar-oriented price.
* SHORT USD is evaluated at a higher dollar-oriented price.

The central displayed mark may therefore appear short of the projected threshold when the confidence-adjusted liquidation price has already crossed it.

Near the boundary, compare Position equity with Maintenance margin and check the account’s liquidatable status. The liquidation-price display remains a projection.

A liquidatable reading based on a stale stored mark does not guarantee immediate keeper execution. The keeper must still provide oracle data eligible under the current market state.

### How pending orders affect health

#### Pending open or increase

Before execution:

* Position size and entry price remain unchanged.
* Unrealized PnL remains based on live exposure.
* Committed margin remains part of terminal collateral.
* The execution reward is excluded from health.
* Available to Trade is lower.

The order preview estimates order-level price, margin and cost fields. Earlier FIFO orders, carry and the final execution price can change the result, and the current preview does not show a complete hypothetical post-execution account.

#### Pending reduction or close

Before execution:

* The complete position remains exposed.
* Carry continues to accrue.
* Liquidation remains possible.
* The close execution reward remains reserved.
* Position margin may already be lower if it funded the reward.

A pending close does not reduce live exposure.

#### Liquidation before execution

If liquidation happens first:

* The position is closed through liquidation.
* Account-local pending orders are cleared.
* Pending execution rewards are forfeited under liquidation cleanup.
* Committed-order funds are released through liquidation cleanup; they do not back price losses.

### Worked example

Suppose settlement contains 3,000 USDC: 1,500 assigned margin, 500 committed-order margin, 10 dedicated VPI reserve, 20 liquidation reserve and 0.20 execution reserve. The same account also has a 250 USDC claim, a 600 USDC price loss, and 40 USDC pending carry.

```
Free settlement = 3,000 − 1,500 − 500 − 10 − 20 − 0.20 = 969.80 USDC
Position equity = 1,500 + 250 − 600 = 1,150 USDC
Maintenance margin = 750 USDC
Price health ratio = 1,150 ÷ 750 = 153.3%
Price liquidation buffer = 1,150 − 750 = 400 USDC
Free settlement after carry = 969.80 − 40 = 929.80 USDC
```

If position equity also passes the applicable initial-margin withdrawal check, the mark is eligible and protocol state permits withdrawal, 929.80 USDC can be withdrawable. Use the contract-returned amount; withdrawal is not computed by subtracting maintenance from all account funds.

Settling the 250 USDC claim while this position stays open increases assigned margin to 1,750 USDC and reduces the claim to zero. Position equity remains 1,150 USDC before further price changes. Depositing free funds improves carry coverage but does not move the price threshold.

### Common readings

| What you see                                            | Likely explanation                                                                                |
| ------------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| Position equity is higher than Available to Trade       | Pledged margin, claims and price PnL differ from spendable free settlement |
| Available to Trade is higher than Withdrawable          | Withdrawal must preserve initial-margin headroom and pass mark/state checks                       |
| Position leverage stays unchanged after depositing USDC | The deposit entered free account collateral rather than assigned position margin                  |
| Health improves while position leverage stays unchanged | Claims or price PnL improved price equity; free USDC only improves carry coverage                                                            |
| Position margin fell after submitting a close           | A permitted close reward used position margin; carry only uses free settlement                     |
| Pending close is visible but exposure is unchanged      | Reductions take effect at execution                                                               |
| Liquidation price shows “Not in range”                  | No threshold exists inside `0.00–2.00` under the current inputs                                   |
| A claim exists beside low Position equity               | Same-account claims already support position equity, but are not free settlement                                      |
| Withdrawable is zero despite positive Position equity   | Mark freshness, degraded mode or post-withdraw margin checks are blocking withdrawal              |

### A practical monitoring routine

1. Check the mark timestamp and market state.
2. Confirm direction and current exposure.
3. Review Unrealized PnL and Cost of carry.
4. Compare Position equity with Maintenance margin.
5. Check the liquidation price and distance.
6. Review pending orders and remember that each one has a reserved execution reward.
7. Read Available to Trade and Withdrawable separately.
8. Account for continued exposure while a close is pending.
9. Deposit additional USDC or reduce exposure before reaching the maintenance boundary.
10. Recheck the account after every order reaches a terminal state.

[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^fad]: Friday Afternoon Deleverage, Plether’s wider scheduled close-only window around the weekly FX closure.
[^pnl]: Profit and loss, the financial result of market-price movement on a position.
[^oracle]: A service that supplies external market data to smart contracts; Plether uses Pyth price feeds.
[^notional]: The face value of a position’s market exposure, not the amount of collateral posted.
[^fifo]: First in, first out; orders at the front of the queue are processed before later orders.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^vpi]: Virtual Price Impact, a separate USDC charge or rebate based on how a trade changes pool directional imbalance.
[^lp]: Liquidity provider, a participant that supplies USDC capital to the liquidity pool.
[^keeper]: A permissionless actor or bot that submits order-finalization or protocol-maintenance transactions.
