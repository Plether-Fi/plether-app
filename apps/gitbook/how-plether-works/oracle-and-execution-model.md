# Oracle & execution model

Plether derives its dollar index (`plDXY`) from six Pyth foreign-exchange price feeds. Its execution rules answer two questions: **is the oracle data valid for this action**, and **how does price uncertainty affect the execution price?**

> **Stale or invalid oracle data blocks execution — it does not, by itself, mark a committed order as failed.** Accepted data can still carry uncertainty, which Plether prices conservatively through an adverse confidence shift.

## Validation limits on every update

Before accepting a Pyth basket update, `PletherOracle` verifies all six component feeds together:

* **Valid & fresh:** Every component price must be positive and within the active staleness limit. A fresh update for one pair cannot rescue a stale pair.
* **Cross-feed alignment:** All six component publish timestamps must fall within **`5 seconds`** of one another.
* **Confidence ceiling:** Combined raw-basket confidence cannot exceed **`0.10%` (`10 bps`)** of the raw basket price. If uncertainty breaches that ceiling, the update is rejected rather than used.

See [Understanding the Plether Dollar Index](../welcome/understanding-the-plether-dollar-index.md) for the six feeds, weights and arithmetic basket formula.

## Adverse confidence adjustment (`0.25×`)

Pyth reports a confidence interval alongside every price. Because the liquidity pool takes the other side of every trade, treating an uncertain estimate as exact would expose LPs to adverse selection.

During live and live-oracle close-only (FAD) execution, Plether shifts the execution price against the trader by **`0.25×` (`2,500 bps`)** of the raw basket confidence:

| Action | Execution price relative to neutral `plDXY` mark |
| --- | --- |
| **Open or increase LONG USD** | Shifted **higher** |
| **Reduce or close LONG USD** | Shifted **lower** |
| **Open or increase SHORT USD** | Shifted **lower** |
| **Reduce or close SHORT USD** | Shifted **higher** |

**Example:** If the neutral `plDXY` mark is `1.0000` and the `0.25×` confidence shift is `0.0002`, a LONG USD open executes at `1.0002`. An **Execution limit** of `1.0003` passes; a limit of `1.0001` fails the order.

This shift is built into the execution price — not charged as a separate USDG fee. Because account valuation uses the neutral mark, a newly opened position can show a small immediate unrealized loss even if the central index has not moved. Separate USDG costs (fees, VPI, carry and execution rewards) are covered in [Trading costs: fees, carry and VPI](trading-costs-fees-carry-and-vpi.md).

## Which observation prices a committed order?

Traders commit binding, non-cancellable orders before the fill price is known. During live and FAD-only sessions, a keeper submits proof of the **first eligible Pyth observation strictly after commitment** (`executionBlock > commitBlock` and `publishTime > commitTimestamp`) inside the settlement window. Keepers cannot skip ahead to a later, more favorable price.

**Pricing time and finalization time are separate:** A keeper can finalize an order later while it remains within its `30-second` execution deadline, still proving that exact historical post-commit tick. Retries never reprice an order to the latest market quote. See [How orders execute](how-orders-execute.md).

## When oracle data is stale, wide or unavailable

| Condition | Effect on a pending order |
| --- | --- |
| **Historical Pyth payload temporarily unavailable** | Keeper attempt reverts; order stays **Pending** for retry on the required historical tick |
| **Stale data, confidence `> 0.10%` or timestamp divergence `> 5s`** | Observation rejected; order stays **Pending** |
| **Valid execution price crosses your Execution limit** | Terminal failure (`Slippage exceeded`); removed from queue |
| **Order exceeds `30-second` execution deadline** | Eligible for keeper cleanup (`Expired`); committed opening margin released |

A reverted keeper transaction is **not** a failed order — always check onchain status in **Open Orders** and **Order History** (see [Why is my order pending or failed?](../trading-on-plether-perps/why-is-my-order-pending-or-failed.md)). Remember that a pending close leaves your full position exposed to PnL, carry and liquidation until it actually executes.

## Scheduled frozen markets (`oracleFrozen`)

**`oracleFrozen` is a calendar state, not an automatic fallback for a live oracle outage.** Unexpectedly missing live data never switches the protocol into frozen pricing, and scheduled FAD close-only shoulders still require live post-commit ticks.

| Execution-time state | Opens & increases | Voluntary reductions & closes |
| --- | --- | --- |
| **Open (live oracle)** | Allowed (subject to risk checks) | Live post-commit tick + `0.25×` adverse confidence shift |
| **FAD close-only (live oracle)** | Blocked | Live post-commit tick + `0.25×` adverse confidence shift |
| **`oracleFrozen`** | Blocked | Latest validated frozen basket (up to `3-day` staleness); **unshifted** price + separate **`50 bps` frozen-close spread** |

During `oracleFrozen`, voluntary closes waive the post-commit ordering requirement and the `0.25×` adverse price shift, while retaining confidence-width (`0.10%`), timestamp-alignment (`5s`), staleness (`3-day` ceiling), acceptable-price and normal signed VPI rules. The `50 bps` frozen-close spread goes 100% to LPs.

[Liquidations](margin-leverage-and-liquidation.md) always use their own adverse confidence and freshness policy — even during `oracleFrozen` — and never pay the voluntary frozen-close spread. See [Market states and oracle closures](market-states-and-oracle-closures.md) for the full FX calendar and closure rules.
