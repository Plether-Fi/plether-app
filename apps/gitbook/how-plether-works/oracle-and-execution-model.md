# Oracle & execution model

Plether derives its dollar index from Pyth foreign-exchange price feeds. Its execution rules address two questions: is the data valid for this trade, and how should uncertainty in that data affect the price?

**Data that is stale or invalid under the active oracle policy blocks execution. It does not, by itself, mark a committed order as failed.** Accepted data can still carry uncertainty, which Plether accounts for in the execution price.

### From price uncertainty to an execution price

**Confidence describes the uncertainty Pyth reports alongside each price.** A wider confidence interval means a less precise estimate. Plether combines the uncertainty of its six FX components into a basket confidence measure.

The liquidity pool takes the other side of each trade. Treating an uncertain price estimate as exact could give traders favourable fills at the pool's expense. During live execution, Plether reduces that risk to liquidity providers by shifting the execution price against the trader. This is the **adverse confidence adjustment**.

Consider a LONG USD opening during a live market:

1. The eligible observation gives a neutral index price of `1.0000`, with confidence inside the accepted limits.
2. Suppose the applicable confidence adjustment is `0.0002` index points. The execution price becomes `1.0002`: a higher entry is worse for the buyer.
3. An **Execution limit** of `1.0003` passes the price check; a limit of `1.0001` causes the order to fail, even though the oracle data is valid.

These values are illustrative. The adjustment varies with reported uncertainty and the configured multiplier, and passing the price check still leaves the order subject to other execution checks.

### How the adjustment works in each direction

Confidence has two roles: **reject excessive uncertainty**, then **price accepted uncertainty conservatively**. During live and live-oracle close-only execution, the adjustment moves the displayed index price as follows:

| Action | Execution price relative to the neutral index price |
| --- | --- |
| Open or increase LONG USD | Higher |
| Reduce or close LONG USD | Lower |
| Open or increase SHORT USD | Lower |
| Reduce or close SHORT USD | Higher |

The adjustment is embedded in the execution price, rather than charged as a separate USDC fee. Because account valuation uses the neutral mark, a new position can show an immediate unrealized loss even if the index has not moved.

The Execution limit applies to the adjusted price. Separate costs, including the execution fee, virtual price impact (VPI), carry and execution reward, are covered in [Trading costs: fees, carry and VPI](trading-costs-fees-carry-and-vpi.md).

### Which observation prices the order?

Traders commit binding, non-cancellable orders before the final price is known. During live operation, a keeper submits proof of the first eligible Pyth observation strictly after commitment and inside the settlement window. The keeper cannot skip that observation and choose a later, more favourable price.

All six component feeds must pass the applicable validity, publication-time, timestamp-alignment and confidence checks. A recent update for one component does not make an old component acceptable. The [Plether Dollar Index guide](../welcome/understanding-the-plether-dollar-index.md) explains how the validated feeds become the index.

**Pricing time and finalization time are different.** A keeper may submit an eligible historical observation later while the order remains unexpired. A retry still targets the required observation; it does not reprice the order at the latest market price. See [How orders execute](how-orders-execute.md) for settlement windows, queue ordering and order lifetimes.

### What happens when prices are stale or unavailable?

An oracle check can reject a finalization attempt while leaving the order and its reservations pending. The displayed quote does not substitute for valid execution data.

| Condition | Effect on a committed order |
| --- | --- |
| Required historical data is unavailable | Execution waits; retries must prove the required observation. |
| Data exceeds the active age limit, confidence is too wide or component times are too far apart | The data is rejected; the order remains pending. |
| A valid execution price crosses the trader's acceptable-price limit | The order fails and is removed from the queue. |
| The order exceeds its maximum lifetime | It becomes eligible for cleanup; processing it marks the order failed and releases committed opening margin. |

**A reverted finalization attempt is not the same as a failed order.** Check the onchain order status. [Why is my order pending or failed?](../trading-on-plether-perps/why-is-my-order-pending-or-failed.md) covers retries, expiry cleanup and execution rewards.

A pending close leaves the position open: price exposure, carry and liquidation risk continue until it executes.

### Scheduled frozen markets use a different policy

**`oracleFrozen` is a calendar state, not an automatic response to an oracle outage.** Unexpectedly missing live data does not activate frozen pricing. The scheduled close-only shoulders retain live post-commit observation rules.

| Execution-time state | New or increased exposure | Voluntary reductions and closes |
| --- | --- | --- |
| Open, live oracle | Permitted subject to all checks | Live observation rules and adverse confidence adjustment |
| Close-only, live oracle | Blocked | Live observation rules and adverse confidence adjustment |
| `oracleFrozen` | Blocked | Latest validated basket within the extended age limit; unshifted price and a separate frozen-close spread |

During `oracleFrozen`, voluntary reductions and closes waive the post-commit observation requirement and the adverse confidence price shift. Confidence limits, bounded staleness, component alignment and acceptable-price checks remain active.

The frozen-close spread is a separate USDC charge paid to liquidity providers; normal signed VPI also remains active. [Market states and oracle closures](market-states-and-oracle-closures.md) covers the schedule, spread and settlement rules.

[Liquidations](margin-leverage-and-liquidation.md) retain their own adverse confidence and freshness policy, including during frozen operation, and do not pay the voluntary frozen-close spread.

Frozen pricing is still bounded. Once the available basket is too old even for that policy, execution remains blocked.
