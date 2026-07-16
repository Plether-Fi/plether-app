# Market states and oracle closures

Plether runs on a 24/7 chain. The FX market underlying the Plether Dollar Index does not.

The protocol does not manufacture a weekend price. Instead, it changes which actions are permitted and how recent the oracle data must be.

There are three important conditions:

* **Scheduled close-only:** new risk is blocked, but the oracle is still treated as live.
* **Oracle frozen:** new FX observations are not expected, so reductions use a bounded stale-data policy.
* **Oracle data unavailable:** the available data is too old or invalid even for the active policy, so price-dependent actions stop.

These conditions are related, but they are not interchangeable.

> **Oracle frozen is a calendar state, not an outage detector.** It does not automatically activate whenever Pyth stops publishing.

### The weekly schedule

Plether’s regular market calendar is defined in UTC and does not move with daylight saving time.

```
OPEN
    Sunday 22:00 → Friday 19:00

CLOSE-ONLY · LIVE ORACLE
    Friday 19:00 → Friday 22:00

CLOSE-ONLY · ORACLE FROZEN
    Friday 22:00 → Sunday 21:00

CLOSE-ONLY · LIVE ORACLE
    Sunday 21:00 → Sunday 22:00

OPEN
    From Sunday 22:00
```

| Time in UTC               | Public market state | Oracle policy | New risk |
| ------------------------- | ------------------- | ------------- | -------- |
| Sunday 22:00–Friday 19:00 | Open                | Live          | Allowed  |
| Friday 19:00–22:00        | Close-only          | Live          | Blocked  |
| Friday 22:00–Sunday 21:00 | Close-only          | Frozen        | Blocked  |
| Sunday 21:00–22:00        | Close-only          | Live          | Blocked  |

The market is not fully open at Sunday 21:00. Frozen pricing ends then, but the close-only runway continues until Sunday 22:00.

The contracts refer to the wider close-only period as the **FAD window**, short for Friday Afternoon Deleverage. The public interface simply calls it **Close-only**.

### Why closure has two stages

Plether begins close-only operation before fresh FX observations are expected to stop.

This first stage provides time to:

* Reduce leverage
* Close positions
* Add margin
* Liquidate positions that no longer satisfy the closure margin requirement
* Stop new risk from entering immediately before the weekend gap

The oracle rules remain strict during this runway. A close still needs the first eligible post-commit observation, just as it would during ordinary trading.

Only when the calendar enters the narrower oracle-frozen interval does Plether relax the post-commit timing requirement and permit bounded stale data.

This preserves normal execution rules while the reference market may still be publishing, without making risk reduction impossible once it stops.

### Action matrix

The following assumes the protocol is active and all other account, liquidity, queue and acceptable-price checks pass.

| Action                                    | Open market                                 | Close-only, live oracle                 | Oracle frozen, data within limit                          | Data unavailable or over-stale                          |
| ----------------------------------------- | ------------------------------------------- | --------------------------------------- | --------------------------------------------------------- | ------------------------------------------------------- |
| Open or increase LONG USD                 | Allowed                                     | Blocked                                 | Blocked                                                   | Cannot execute                                          |
| Open or increase SHORT USD                | Allowed                                     | Blocked                                 | Blocked                                                   | Cannot execute                                          |
| Execute a queued opening                  | Allowed                                     | Blocked; remains pending                | Blocked; remains pending                                  | Blocked                                                 |
| Commit a reduction or close               | Allowed                                     | Allowed                                 | Allowed                                                   | May be accepted using a stored mark, but cannot execute |
| Execute a reduction or close              | Live historical pricing                     | Live historical pricing                 | Frozen-market pricing plus the 0.50% frozen-close spread  | Blocked                                                 |
| Liquidate                                 | Live liquidation policy                     | Live policy with closure margin         | Frozen policy with closure margin; no frozen-close spread | Blocked                                                 |
| Add margin                                | Allowed                                     | Allowed                                 | Allowed                                                   | Allowed                                                 |
| Withdraw USDC with an open position       | Requires fresh mark and sufficient headroom | Requires live mark and closure headroom | Requires mark within frozen limit                         | Blocked                                                 |
| Withdraw from an account with no position | Subject to free and reserved balances       | Same                                    | Same                                                      | Same                                                    |

Data availability is an overlay rather than a separate calendar phase. An oracle problem can occur while the calendar says **Open**, and valid bounded data can remain available while the calendar says **Oracle frozen**.

### Open market

During the open state, traders can:

* Open LONG USD or SHORT USD
* Increase an existing position
* Reduce or close a position
* Add margin
* Finalize eligible pending orders

Order execution uses the first eligible Pyth basket observation strictly after commitment and inside the settlement window.

Normal protections apply:

* No same-block live execution
* Strict post-commit publication
* Component freshness and timestamp alignment
* Confidence limits
* Adverse confidence adjustment
* Acceptable-price checks

Liquidation uses its own, stricter live-market freshness requirement.

The **Open** state means the calendar permits new risk. It does not guarantee that an individual order will pass oracle, margin, capacity or solvency checks.

### Scheduled close-only with a live oracle

Scheduled close-only begins every Friday at 19:00 UTC and returns for the Sunday 21:00–22:00 reopening shoulder.

During these intervals:

* New positions cannot be committed
* Existing positions cannot be increased
* Reductions and closes remain available
* Liquidations remain available
* The higher market-close margin requirement applies
* Live-market oracle protections remain active
* Normal signed VPI and its lifetime rebate clamp remain active
* The frozen-close spread is not active
* Frozen LP entry and exit surcharges are not active

A close submitted during this period still requires a valid post-commit observation. Plether does not accept an older price merely because the market is close-only.

If fresh observations have already stopped, the close remains pending. It does not automatically switch to stale pricing until the calendar enters the actual oracle-frozen interval.

### Oracle-frozen operation

The oracle-frozen calendar state covers the period in which continuous FX updates are not expected.

New risk remains blocked. Plether relaxes the oracle policy for reductions and liquidations, while a separate frozen-close spread applies only to voluntary reductions and closes.

A close or reduction uses the latest validated Pyth basket available within the frozen-market age limit.

The protocol no longer requires:

* Publication strictly after the order commitment
* A new observation in the commitment block’s settlement window
* A different block from the commitment block

Those requirements would make weekend execution impossible when no new FX observation exists.

The following protections remain:

* Every component price must be positive and valid
* Confidence must remain within its limit
* Component timestamps must remain sufficiently aligned
* The basket must remain within the frozen staleness limit
* The execution price remains inside the fixed `0.00–2.00` range
* The adverse confidence adjustment still applies
* The trader’s acceptable-price boundary still applies

Frozen execution does not necessarily use “Friday’s closing price.” It uses the latest basket that Pyth can validate within the active policy. That basket may have been updated after the initial closure observation.

### VPI and the frozen-close spread

Normal signed VPI and its existing lifetime rebate clamp apply to every voluntary close and reduction, including during `oracleFrozen`.

Frozen operation does not switch to a one-way VPI curve:

* A close that increases directional imbalance can incur positive VPI
* A close that reduces directional imbalance can receive a bounded VPI rebate
* The existing lifetime rebate clamp remains unchanged

Separately, a voluntary close or reduction executed while `oracleFrozen` is assessed a fixed spread on the USDC value of the notional being reduced:

```
Frozen-close spread
= reduced position notional × 0.50%
```

For example, reducing `$100,000` of notional produces a `$500` frozen-close spread.

The spread:

* Applies only to voluntary closes and reductions executed while `oracleFrozen`
* Is currently **50 bps**, or **0.50% of reduced notional**
* Does not vary with pool skew, VPI, elapsed closure time or mark staleness
* Is separate from VPI, the execution fee, accrued carry and Pyth’s adverse confidence adjustment
* Does not apply during the open market
* Does not apply during the live-oracle shoulders of the close-only window
* Does not apply to liquidations
* Belongs entirely to LPs and never becomes protocol treasury revenue

The state at execution determines whether the spread applies. A close committed before `oracleFrozen` but executed after the boundary is assessed the spread.

#### How the spread is settled

When collectible account value is limited, settlement follows this priority:

```
Execution fee
→ Base close obligation
→ Frozen-close spread
```

The base close obligation is the ordinary close settlement before the additional frozen spread.

A partial reduction must settle its complete obligation, including the full frozen-close spread. If it cannot, the reduction does not execute.

A terminal full close is not trapped solely because the entire spread cannot be collected. Plether collects the available spread and waives only the uncollectible portion.

The waived amount:

* Does not become bad debt
* Does not become a trader claim
* Does not become an LP receivable or LP revenue

Any uncovered base trading-loss obligation continues through the protocol’s ordinary bad-debt accounting. Only the waived spread receives this special treatment.

Close previews expose the result separately:

* `frozenSpreadUsdc` — spread assessed
* `frozenSpreadPaidUsdc` — spread collected for LPs
* `frozenSpreadWaivedUsdc` — spread waived on a terminal full close

For a valid close:

```
spread assessed
= spread paid
+ spread waived
```

All three values are zero outside `oracleFrozen`.

A successful close with a nonzero assessment emits:

```
FrozenCloseSpreadSettled(
    account,
    assessedUsdc,
    paidUsdc,
    waivedUsdc
)
```

“Fixed” means that the percentage does not respond dynamically to VPI, skew or staleness. It does not mean that the parameter is immutable. Changes require the protocol’s timelocked risk-configuration process.

The fixed spread reduces the stale-price risk borne by LPs. It does not guarantee that LPs are fully compensated for an external FX move during the closure.

Frozen mode preserves a bounded path for reducing risk. It does not guarantee a cost-free exit.

### When oracle data is unavailable

Oracle data is unavailable when Plether cannot construct a valid basket under the relevant policy.

Possible causes include:

* One or more components are too old
* A component price is invalid
* Confidence is too wide
* Component timestamps are too far apart
* Required historical data cannot be proven
* Pyth updates are temporarily unavailable

When that happens, Plether does not:

* Invent a weekend price
* Extrapolate the previous movement
* Substitute an arbitrary later observation
* Accept data older than the configured maximum

Close execution and liquidation revert rather than settle against an unbounded price.

The affected order normally remains pending until:

* Valid data becomes available
* The order expires and is cleared
* Another terminal condition applies

A close intent may still be committed if the protocol has a stored mark and can reserve its execution reward. Commitment does not imply that the close can currently execute.

### A live-market oracle outage is not frozen mode

The oracle-frozen flag is determined by the market calendar and configured closure days.

If Pyth stops publishing unexpectedly during ordinary market hours, Plether does not automatically relax into frozen pricing.

Instead:

* The calendar may still show **Open**
* New order commitments may still reach preflight
* Execution remains blocked without valid post-commit data
* Liquidation remains blocked without data satisfying its live freshness limit
* Existing orders remain pending until recovery or expiry

This avoids turning an unexpected oracle failure into permission to trade against stale data.

### Margin requirements rise before closure

The close-only window activates a higher margin requirement for existing positions.

This happens as soon as the window begins—not when the oracle later becomes frozen.

A position that was healthy immediately before Friday 19:00 UTC can become liquidatable after the boundary if its equity falls below the closure requirement.

No position is closed automatically. A keeper must still submit a valid liquidation using an eligible oracle price.

Before the close-only window, traders should consider:

* Adding margin
* Reducing contract size
* Closing the position
* Leaving enough headroom for fees and carry
* Avoiding reliance on a last-minute pending close

The current protocol setting uses a **3.00% market-close margin requirement**. This is a timelocked risk parameter, not part of the fixed index formula.

> **Screenshot placeholder:** Margin Call Simulator — show the active maintenance requirement and warning that it becomes stricter at market closure.

### Carry does not pause

Carry accrues according to elapsed time.

It continues during:

* Scheduled close-only operation
* Oracle-frozen periods
* Weekends
* Holiday closures
* Temporary oracle outages

A market that is not publishing a new price is not a position with no cost.

If a close remains pending through the weekend, the position remains open and carry continues to accrue until settlement.

### What happens to queued orders?

Plether’s global FIFO queue does not reorder itself when the market state changes.

#### Queued opening or increase

An opening committed before close-only begins cannot execute after the boundary.

It remains pending at the queue head until it:

* Becomes eligible again
* Expires and is cleared
* Fails another terminal check
* Is removed after account liquidation

Later orders cannot jump ahead, including risk-reducing orders.

The configured maximum order age bounds how long an old order can occupy the queue.

#### Queued reduction or close

A pending close uses the policy active when it is finalized.

For example:

* A close committed at Friday 21:59 may use live historical execution and pay no frozen-close spread if finalized before 22:00.
* The same close uses frozen-market execution and is assessed the frozen-close spread if finalized after 22:00.

The spread is determined by the state at execution, not the state at commitment.

The acceptable-price boundary continues to constrain the oracle-derived execution price in either case. It does not cap the execution fee, VPI, carry or frozen-close spread.

A pending close is not a completed close. The position remains exposed and liquidatable until execution succeeds.

### Liquidations during closures

Liquidation remains available during scheduled close-only and oracle-frozen operation, provided the oracle satisfies the active policy.

During the live-oracle shoulders:

* The higher closure margin requirement applies
* Liquidation still requires live data
* The normal adverse liquidation confidence adjustment applies

During oracle-frozen operation:

* The higher closure margin requirement remains active
* The extended frozen staleness limit applies
* The liquidation price remains adverse to the account
* Full liquidation remains the only liquidation mode
* The frozen-close spread is not assessed

The spread protects LPs against voluntary stale-price exits without reducing the value available for keeper incentives or changing liquidation settlement.

If data becomes too old even for frozen policy, liquidation cannot execute.

This is a liveness risk, not debt forgiveness. The position remains open, carry continues, and liquidation can resume when acceptable data returns.

### Adding and withdrawing trader margin

Depositing USDC into the margin account and adding margin to an existing position remain available even when the oracle is stale.

These actions reduce risk and do not require a current index price.

Withdrawing USDC from an account that has an open position is different. It requires:

* A sufficiently recent mark for the active market state
* A non-degraded protocol
* A position that is not liquidatable
* Enough post-withdrawal equity to satisfy the stricter of the initial and active maintenance requirements

During close-only, the closure margin requirement is included in this check.

If the mark is over-stale, withdrawal from an account with an open position is blocked. An account with no position may still withdraw unreserved USDC without requiring an oracle price.

### What closures mean for LPs

LP operations distinguish scheduled close-only from actual oracle-frozen operation.

#### Scheduled close-only, oracle still live

The normal LP rules remain in force:

* Ordinary freshness requirements
* Tranche liquidity limits
* Senior impairment rules
* Deposit cooldowns
* Pending deposit epochs
* No frozen-market surcharge

The fact that trader openings are blocked does not itself change LP share pricing.

#### Oracle frozen

LP entry and exit may remain available while the stored mark remains within the frozen-market limit.

The trader frozen-close spread and the tranche-specific LP action surcharge are different charges:

* A collected trader spread becomes LP-owned HousePool revenue and never protocol treasury revenue.
* An LP action surcharge remains inside the tranche in which it was charged.
* A trader spread waived on a terminal full close is not LP revenue or bad debt.

A tranche-specific frozen surcharge applies to LP entry and exit:

* Depositors receive fewer shares
* Minting a target number of shares requires more USDC
* Redeemers receive less USDC
* Withdrawing a target amount requires more shares

The retained value stays in the same tranche for the benefit of its incumbent LPs.

The current settings are:

| Tranche | Frozen entry and exit surcharge |
| ------- | ------------------------------- |
| Senior  | 0.25%                           |
| Junior  | 0.75%                           |

These are timelocked pool parameters and should be read from the live contract or interface before acting.

Immediate deposits remain available only when no trader positions are open. When positions exist, pending deposit epochs remain the ordinary entry path.

If an epoch is finalized while oracle-frozen policy is active, its share calculation uses the frozen pricing rules active at finalization.

#### Oracle data over-stale

When trader liabilities require a mark and the available mark exceeds the frozen limit:

* Deposit finalization can be blocked
* Withdrawals and redemptions can be blocked
* Public withdrawal capacity can fall to zero

If there are no open trader liabilities requiring mark-to-market accounting, freshness may not be required solely for LP reconciliation. All other tranche, liquidity and lifecycle restrictions still apply.

Frozen mode keeps LP exits available under bounded conditions. It does not make liquidity unlimited or remove the HousePool’s withdrawal firewall.

### Holiday closures

Plether can register additional full-day FX closures through timelocked calendar configuration.

For a configured closure day:

* The entire UTC day is close-only
* Oracle-frozen policy is active
* A configurable close-only runway begins before the day starts

The override runway is a configured calendar parameter. The built-in Friday close-only shoulder is separate.

Holiday calendar changes are subject to a 48-hour timelock.

Because closure days use UTC calendar boundaries, their relationship to a local exchange holiday or daylight-saving transition may not be exact to the minute.

### Daylight saving time and reopening gaps

The weekly schedule does not move with daylight saving time.

This means the calendar and actual FX publication schedule can briefly diverge.

The most visible example is Sunday 21:00–22:00 UTC:

* Oracle-frozen policy has ended
* The market remains close-only
* Strict live oracle rules apply again
* Fresh FX updates may or may not have resumed yet

If no eligible live observation exists, closes and liquidations remain blocked despite the oracle-frozen flag being off.

The reverse can happen around the Friday close: updates may stop before the protocol’s 22:00 frozen boundary. During that gap, close-only remains active but live freshness is still required.

Calendar permission never overrides oracle validation.

### Other protective states

Calendar mode and oracle freshness are not the only controls. Other protocol overlays can exist at the same time.

| Overlay                         | Main effect                                                                                                                                     |
| ------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| Router paused                   | Blocks new opening and increase commitments; reductions, queued execution, liquidations and mark updates remain available where otherwise valid |
| HousePool paused                | Blocks new LP deposits; eligible LP withdrawals and trader operations are evaluated separately                                                  |
| Degraded mode                   | Blocks new trader risk and LP withdrawals while preserving reductions, liquidations, margin additions and recovery actions                      |
| Trading inactive or configuring | The market is not yet active for new trader risk                                                                                                |
| Oracle stale                    | Blocks actions requiring a valid current or bounded-frozen price                                                                                |

Degraded mode is a solvency-containment state. It does not mean the market has settled or that all positions have been terminated.

Plether Perps has no boundary-triggered **SETTLED** state. Reaching `0.00` or `2.00` does not end the market.

### What the interface shows

The current banner uses the public states:

* **Open**
* **Close-only**
* **Closed**
* **Degraded**

It shows messages such as:

> Market is **open** for another `…`. Then **close-only** for `…`.

or:

> Market is **close-only** for another `…`. Then **open** for `…`.

The interface combines the live-oracle shoulders and the actual frozen interval into the single public label **Close-only**.

That label is enough to determine whether new risk is permitted. It is not enough to determine which oracle policy will price a close.

Check the oracle freshness indicator as well:

* `Oracle fresh`
* `Oracle stale`
* `updated … ago`
* `checking backend for a fresh update`

> **Screenshot placeholder:** Market open banner — annotate the countdown to close-only.

> **Screenshot placeholder:** Close-only banner beside a Reduce-only order — explain that new risk is blocked while reductions remain available.

> **Screenshot placeholder:** Plether Dollar Index freshness indicator — show the last update age and stale state.

> **Screenshot placeholder:** Frozen close preview — show normal VPI and the 0.50% frozen-close spread as separate values.

The current weekly countdown follows the regular Friday-to-Sunday schedule. Around configured holiday closures, the onchain state is authoritative even if the displayed duration still reflects the ordinary week.

The interface may also allow an opening to reach the review screen during close-only. The contract will reject the commitment. A successful preview is not permission to open new risk.

### Current closure parameters

Unlike the fixed `2.00` index boundary, the following values are timelocked risk parameters.

| Parameter                       | Current setting           |
| ------------------------------- | ------------------------- |
| Market-close margin requirement | 3.00%                     |
| Maximum frozen oracle age       | 3 days                    |
| Additional closure-day runway   | Configured calendar value |
| Frozen-close spread             | 0.50% of reduced notional |
| Senior frozen LP surcharge      | 0.25%                     |
| Junior frozen LP surcharge      | 0.75%                     |

The frozen-close spread is separate from VPI. It is part of the 48-hour timelocked risk configuration, must remain nonzero.

Live onchain values are authoritative.

### Trader checklist before closure

Before Friday 19:00 UTC or an announced closure day:

* Check when close-only begins
* Recalculate health using the closure margin requirement
* Add margin or reduce exposure before the boundary
* Leave room for carry, execution fees, VPI and—if the close may execute while `oracleFrozen`—the frozen-close spread
* Do not treat a submitted close as completed
* Avoid leaving an opening order near the state transition
* Check the age of the latest oracle update
* Remember that carry continues through the closure

During oracle-frozen operation:

* Confirm the timestamp of the price being used
* Review the adverse confidence adjustment
* Review normal signed VPI and the separate 0.50% frozen-close spread
* If reducing only part of a position, confirm that the account can settle the complete close obligation
* Check the assessed, paid and waived spread amounts in the close preview or result
* Set an acceptable-price boundary
* Confirm that the order executed before treating the position as closed

For LPs:

* Check whether the protocol is actually oracle-frozen or only close-only
* Review the active tranche surcharge
* Check withdrawal capacity and cooldown
* Confirm whether a deposit will enter immediately or through a pending epoch
* Do not assume a frozen-window withdrawal is unlimited or guaranteed
* Count only collected frozen-close spread as LP revenue; waived spread is not an LP receivable

### The central distinction

A 24/7 chain does not create a 24/7 reference market.

Plether handles that boundary in stages:

```
Close-only prevents new weekend risk.

Oracle-frozen policy preserves bounded risk reduction
and adds a fixed LP-owned spread to voluntary exits.

Freshness limits stop execution when the available price
can no longer be defended.
```

The purpose is not to pretend the FX market remains open. It is to keep the protocol honest about when it has a price—and what it can safely do with it.
