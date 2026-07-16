# Why is my order pending or failed?

An action can fail before an order exists or after a confirmed order commitment. These are separate lifecycles and require different responses.

![Two-lane flowchart separating sponsored submission states from delayed-order execution and failure outcomes.](../.gitbook/assets/diagrams/sponsorship-vs-order-failure-lifecycles.svg)

The sponsored operation is **Confirmed** when the commitment call succeeds onchain and creates an order ID. The order then enters Plether’s global FIFO queue with its own **Pending** status. Margin and execution-reward reservations become active while the requested position change waits for execution.

A **Failed** order has reached a terminal state. The same order cannot execute later.

Committed orders cannot be cancelled, resized or given a new acceptable price.

### First identify what failed

| What you see                              | Does an order exist?        | Meaning                                                                  |
| ----------------------------------------- | --------------------------- | ------------------------------------------------------------------------ |
| **Wallet signature rejected**             | No                          | The owner wallet did not authorize the prepared action                   |
| **Sponsorship unavailable / rate-limited** | No                          | The sponsor did not approve gas funding for this attempt                 |
| **Bundler rejected**                      | No                          | The bundler refused the UserOperation before submission                  |
| **UserOperation Pending**                 | Not yet known               | The operation is waiting for inclusion; do not submit a duplicate        |
| **UserOperation dropped**                 | Usually no                  | The bundler stopped tracking it; check for a transaction receipt first   |
| **Sponsored operation failed**            | No                          | The included commitment call failed and reservations reverted            |
| Order appears under **Open Orders**       | Yes                         | The commitment succeeded and the order remains Pending                   |
| **Finalization transaction failed**       | Yes, usually still Pending  | The delayed execution attempt reverted                                   |
| **Order failed**                          | No longer active            | An onchain `OrderFailed` event made the order terminal                    |
| **Expired** under Open Orders             | Yes, awaiting cleanup       | Its lifetime passed, but cleanup is still required                       |
| **Expired / Cleaned up** in Order History | No longer active            | The expired order has been terminally removed                            |
| **Executed**                              | No longer active            | The requested position change completed                                  |

A confirmed UserOperation or transaction does not always mean the trade executed. A commitment only creates the order, and a later finalization transaction can confirm while emitting `OrderFailed`.

Check **Order History** for the terminal result.

![Four failure classes side by side](../.gitbook/assets/screenshots/storybook-documentation-trading-account-and-sponsorship--failure-state-comparison.png)

### UserOperation hash versus transaction hash

A sponsored smart-account action can expose two different identifiers:

| Identifier             | What it identifies                                                                                                  |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------- |
| **UserOperation hash** | The signed smart-account operation sent to the bundler; it can exist before any onchain transaction includes it     |
| **Transaction hash**   | The onchain transaction submitted by the bundler; it can contain one or more UserOperations                         |

Use the UserOperation hash to check sponsorship and bundler status. Once included, its receipt should identify the transaction hash. Use the transaction hash to inspect the block, EntryPoint events and Plether contract events.

A UserOperation hash is not interchangeable with a transaction hash. A dropped UserOperation may never receive a transaction hash. Conversely, a confirmed bundler transaction does not by itself prove that the specific UserOperation’s inner Plether call succeeded.

For an order commitment, the strongest confirmation is:

1. The UserOperation receipt shows successful inclusion.
2. The transaction receipt shows the successful Plether commitment event.
3. An order ID appears under **Open Orders** or **Order History**.

### A quick troubleshooting sequence

1. Check the sponsored operation status and UserOperation hash.
2. If included, open the linked transaction hash and check whether the commitment call succeeded.
3. Find the order ID under **Open Orders**.
4. Check its expiry countdown.
5. Check whether the market is live, FAD-only or `oracleFrozen`.
6. Read the latest finalization or oracle message.
7. Wait for automatic keeper processing.
8. Use **Finalize Trade** when manual finalization becomes available.
9. If the order has expired, use **Clean Up**.
10. If Order History shows a terminal failure, request a new preview and create a new order.

Do not submit a replacement until you know whether the original order is still Pending. A replacement creates another binding FIFO order.

### The sponsored commitment failed

Failures before successful inclusion include:

* Sponsorship unavailable
* Sponsor rate limit reached
* Invalid or expired wallet signature
* Invalid nonce
* Failed smart-account simulation
* Bundler policy rejection
* UserOperation dropped before inclusion

These failures create no order ID and no margin or execution-reward reservation.

If the UserOperation is included but the commitment call reverts, the attempted state changes revert atomically. No order or reservation remains. The sponsored submission can still consume network gas, but Plether pays that gas for an eligible sponsored operation rather than charging the owner wallet’s native-token balance.

The underlying commitment call can fail for:

| Commit error                     | Typical cause                                                                                   |
| -------------------------------- | ----------------------------------------------------------------------------------------------- |
| Insufficient free USDC           | Available to Trade cannot cover the margin and execution reward                                 |
| Too many pending orders          | The account has reached the active pending-order limit                                          |
| Position too small               | The opening or partial reduction is below the minimum supported size                            |
| Insufficient initial margin      | Size and margin would create excessive leverage                                                 |
| Opposing position                | The account already has exposure in the other direction                                         |
| Skew limit                       | The requested direction would exceed the current skew boundary                                  |
| Solvency limit                   | The HousePool cannot admit the requested maximum liability                                      |
| Close-only or frozen market      | New exposure is unavailable in the current market state                                         |
| Degraded mode                    | New risk is blocked                                                                             |
| Router pause or pool restriction | Risk-increasing commits are temporarily unavailable                                             |
| Invalid reduction                | No executed position exists, the side differs or earlier queued reductions already use the size |
| Close reward unavailable         | Eligible account collateral cannot safely back the execution reward                             |
| Pending-order limit              | Existing orders must execute or be cleaned before another can be committed                      |

Correct the displayed sponsorship, account or ticket issue and request a fresh operation. Use **Retry Commit** only after the previous UserOperation is confirmed failed or dropped and no order ID exists.

### Why an order remains Pending

#### It is behind an earlier global order

Plether processes all accounts through one global FIFO queue.

An order can execute after every earlier unresolved order has:

* Executed
* Failed
* Expired and been cleaned
* Been removed during liquidation cleanup

Your Open Orders list shows account-local orders. It does not show the complete global queue ahead of them.

A temporary problem at the global head delays every later order. FIFO does not prioritize reductions and closes. An opening order blocked by a close-only state can therefore delay a later close until the earlier order becomes executable or expires.

Expired-head cleanup is bounded per transaction. Several expired orders may require multiple keeper calls.

#### It is waiting for the post-commit price

During live and FAD-only execution, Plether requires:

```
execution block > commit block
```

and:

```
oracle publish time > commit time
```

The execution price comes from the unique first eligible Pyth basket observation inside the order’s historical settlement window.

A keeper cannot skip that observation and select a later, more favorable price.

Immediately after commitment, the interface may show:

* **Pending**
* **Waiting for reveal**
* **Pending reveal**
* **Final price not ready yet**

Wait for the next eligible observation and FIFO processing.

#### The historical oracle payload is unavailable

The keeper needs Pyth data covering the order’s specific execution window.

A finalization attempt can revert because of:

* Missing update data
* Historical data retrieval delay
* Hermes rate limiting
* Pyth update fee changing before submission
* Failure to identify a unique post-commit tick
* Confidence width above the permitted limit
* Basket component timestamps too far apart
* Invalid or out-of-order oracle data

These failures normally leave the order Pending.

The interface may show:

| Message                            | Suggested action                                         |
| ---------------------------------- | -------------------------------------------------------- |
| **Price data rate limited**        | Retry while the order remains Pending                    |
| **Historical price data required** | Use **Retry Finalizing** with newly fetched data         |
| **Historical price data rejected** | Retry; persistent failure may require waiting for expiry |
| **Final price not ready yet**      | Wait briefly and retry                                   |
| Pyth fee changed                   | Retry to obtain a fresh fee quote                        |
| Confidence too wide                | Retry when an eligible observation passes validation     |
| Component prices misaligned        | Retry after the basket feeds align                       |

Retries still target the order’s eligible historical observation. They do not move the order to the latest market price.

#### The market became close-only

An open or increase committed while live remains Pending if it reaches execution during:

* A FAD close-only window
* `oracleFrozen`

It can execute if risk-increasing trading resumes before expiry. Otherwise, it must expire and be cleaned.

| Market state                       | Open or increase                          | Reduce or close                           |
| ---------------------------------- | ----------------------------------------- | ----------------------------------------- |
| Live                               | Eligible under historical execution rules | Eligible under historical execution rules |
| FAD-only                           | Blocked and remains Pending               | Eligible under live historical rules      |
| `oracleFrozen`                     | Blocked and remains Pending               | Eligible under frozen-market rules        |
| Frozen data beyond its allowed age | Blocked                                   | Waits for eligible data                   |

A voluntary frozen close uses the validated unshifted oracle price, retains slippage and normal signed VPI, and pays the separate frozen-close spread.

#### A finalization attempt was too early

Same-block execution and live observations published at or before commitment are rejected.

The finalization transaction reverts and the order stays Pending. Wait for a later block and retry.

#### The finalization transaction lacked enough gas

Insufficient forwarded gas prevents the execution attempt from reaching the engine.

Retry with a higher gas limit. The order and its reservations remain unchanged.

#### Keeper or network delay

A confirmed commitment still requires a keeper or another account to submit the finalization transaction.

RPC interruptions, congestion, keeper downtime and delayed Pyth caching can extend the wait.

The trade modal gives automatic finalization a short grace period. It then exposes **Finalize Trade**.

![Finalization modal details](../.gitbook/assets/screenshots/storybook-perps-final-reveal-modal--manual-finalization-ready.png)

### Manual finalization

Manual finalization is permissionless and follows the same rules as keeper execution.

Order-commitment sponsorship does not automatically cover manual finalization. Unless the interface explicitly marks **Finalize Trade** as **Sponsored**, the manual finalizer submits a conventional transaction.

The finalizer pays:

* Network gas
* The required Pyth update fee in native ETH

The reserved USDC execution reward is credited to the finalizing address’s Margin Account when the order reaches a terminal state.

If the finalizing address is the same Trading Account that owns the order, the reward is credited back to that Trading Account’s Margin Account. A separate owner EOA is a different address and therefore a different Plether account. A trader who explicitly chooses an unsponsored manual-finalization route pays that route’s network gas and Pyth update fee; normal sponsored order commitment does not require owner-wallet ETH.

The current Open Orders panel provides monitoring and expired-order cleanup. Manual finalization is available through the active trade modal. Closing or reloading that modal may leave keeper processing as the remaining path until expiry.

#### Finalization transaction failed

A wallet rejection or reverted finalization transaction usually leaves:

* Order status: Pending
* Committed margin: reserved
* Execution reward: reserved
* Position: unchanged by that attempt

Use **Retry Finalizing** while the order remains under Open Orders.

#### Finalization confirmed without a result for your order

A transaction can advance or clean earlier FIFO orders without reaching the selected order.

If the transaction confirms without an `OrderExecuted` or `OrderFailed` event for your Order ID:

1. Refresh Open Orders.
2. Check whether earlier orders were removed.
3. Confirm whether the selected order is still Pending.
4. Retry only after checking the refreshed state.

### What remains reserved while Pending

#### Open or increase

A pending open or increase reserves:

* Submitted order margin
* Execution reward

Committed margin:

* Leaves Available to Trade
* Cannot be withdrawn or reused
* Remains part of terminally reachable account collateral
* Creates no live exposure before execution

The execution reward:

* Leaves Available to Trade
* Stops contributing to account health
* Remains reserved for terminal processing

An existing position keeps its current size, entry price, PnL and carry exposure until the increase executes.

#### Reduce or close

A close normally reserves only its execution reward.

Plether uses free Margin Account USDC first. When permitted, it can source a bounded part from assigned position margin.

Position-margin funding can:

* Lower position margin at commitment
* Increase displayed leverage
* Reduce account health
* Leave the complete position exposed

Carry may also be checkpointed while the reward is reserved.

A pending close provides no liquidation protection.

### Expired orders and Clean Up

The active onchain maximum order age determines expiry. Use the countdown shown by the interface.

Passing the expiry time makes the order ineligible for trade execution, but reservations remain until terminal cleanup.

An expired order continues to:

* Hold committed opening margin
* Hold its execution reward
* Count toward the pending-order limit
* Leave a pending close’s position fully exposed

The interface changes the status to **Expired** and displays `Clean Up`. Select it to process the expiry and release the reserved margin.

Cleanup:

* Emits `OrderFailed` with reason `Expired`
* Removes the order from the queue
* Releases committed opening margin
* Pays the execution reward to the cleaner
* Records **Expired / Cleaned up** in Order History

Cleanup remains subject to global FIFO. An earlier unexpired order cannot be skipped. If cleanup fails for that reason, wait for the earlier head to progress.

The interface can reach zero slightly before the contract considers the order strictly older than its maximum age. If cleanup reverts exactly as the countdown reaches zero, wait for the next block and retry.

![Pending and expired Open Orders](../.gitbook/assets/screenshots/storybook-perps-account-panel--open-orders-pending-and-expired.png)

### What Failed means

A terminal failure emits `OrderFailed`, removes the order from the queue and makes another execution attempt impossible.

The requested trade is not applied. Earlier orders and separate carry checkpoints may already have changed the account since commitment.

For an ordinary terminal failure:

* Remaining committed opening margin is released.
* The execution reward is paid to the terminal processor.
* The proposed execution fee and VPI are not charged.
* A new order is required.
* Existing position exposure continues unless another action changed it.

### Failure reasons in Order History

#### Failed: Slippage exceeded

The execution price fell outside the committed acceptable-price boundary.

Using the dollar-oriented index shown by the application:

| Action                     | Required execution condition                   |
| -------------------------- | ---------------------------------------------- |
| Open or increase LONG USD  | Price at or below the maximum acceptable price |
| Open or increase SHORT USD | Price at or above the minimum acceptable price |
| Reduce or close LONG USD   | Price at or above the minimum acceptable price |
| Reduce or close SHORT USD  | Price at or below the maximum acceptable price |

A target of **Unlimited** disables this price check.

Live and FAD-only checks use the adverse confidence-adjusted execution price. A frozen voluntary close uses the unshifted validated price; its frozen-close spread is charged separately.

Slippage failure is terminal for that observation. A later favorable price cannot revive the same order.

Before resubmitting:

* Check the current index.
* Review the applicable confidence adjustment.
* Set a new boundary deliberately.
* Request a fresh preview.

#### Failed: Engine rejected

The engine found the order invalid against execution-time state.

An open or increase can be rejected because:

* The account still holds an opposing position.
* The resulting position is below minimum size.
* Fees, VPI or accrued carry drain the usable margin.
* Initial margin is insufficient.
* Current skew exceeds the admitted limit.
* HousePool solvency capacity is insufficient.
* The protocol entered degraded mode.
* Earlier terminal settlement consumed margin reserved for the order.

A reduction or close can be rejected because:

* Requested size exceeds the current position.
* Earlier orders changed the available exposure.
* The residual position would be too small.
* Remaining margin would fall below the applicable minimum.
* A partial reduction cannot fully settle its losses and costs.
* A frozen partial reduction cannot pay its complete frozen-close spread.

Refresh the position, account and preview before submitting again.

#### Failed: Account liquidated

The account became liquidatable before the order executed.

Plether then:

* Liquidates the live position
* Clears the account’s pending orders
* Marks them failed due to liquidation
* Uses eligible committed margin during terminal settlement
* Releases any remaining committed reservation
* Forfeits pending execution rewards to the protocol treasury

Review the liquidation result and current Margin Account before creating new exposure.

#### Failed: Engine panic

The engine encountered an unexpected internal failure.

Save:

* Order ID
* Commit transaction hash
* Reveal transaction hash
* Block number
* Current market state

Contact the team if the failure persists.

#### Expired / Cleaned up

The order passed its lifetime and was removed without executing.

Committed opening margin is released. The execution reward is paid to the cleaner.

Submit a fresh order if the trade is still required.

### Reservation outcomes

| Outcome                          | Committed opening margin                         | Execution reward               |
| -------------------------------- | ------------------------------------------------ | ------------------------------ |
| Pending                          | Remains reserved                                 | Remains reserved               |
| Executed                         | Becomes active position margin as applicable     | Paid to finalizer              |
| Slippage or engine failure       | Released to free Margin Account USDC             | Paid to finalizer              |
| Expired and cleaned              | Released to free Margin Account USDC             | Paid to cleaner                |
| Finalization transaction reverts | Unchanged                                        | Unchanged                      |
| Liquidation happens first        | May be consumed before any remainder is released | Forfeited to protocol treasury |

For a failed close whose reward came from position margin, that margin remains spent because the reward is paid to the processor.

### Why a valid preview can still fail

The preview uses current account and protocol state. Execution occurs later, after earlier global FIFO activity.

The result can change through:

* Price movement
* Carry accrual
* Execution fees and VPI at the final price
* Earlier orders changing account exposure
* Earlier orders changing market skew
* HousePool liability or solvency changes
* Margin being consumed during terminal settlement
* FAD or frozen-market activation
* Degraded mode
* Liquidation

For example, an opposite-direction open may be queued behind a full close. If the close fails its slippage check, the later opening order reaches execution while the original position still exists and is rejected.

### Why Available to Trade has not fully recovered

After failure, possible causes include:

* The order is expired but has not yet been cleaned.
* Another pending order still reserves margin.
* Another execution reward remains reserved.
* The failed close reward was paid from position margin.
* Carry was checkpointed or realized.
* Terminal close or liquidation settlement consumed committed margin.
* The interface is waiting for the history indexer to catch up.

Open Orders are read from onchain state, while Order History is indexed separately. A processed order may briefly disappear from Open Orders before appearing in history.

### Common interface situations

| What you see                                                      | What to do                                                                                    |
| ----------------------------------------------------------------- | --------------------------------------------------------------------------------------------- |
| Sponsorship unavailable or rate-limited                            | Wait for the displayed retry time; no order exists                                             |
| UserOperation dropped and no transaction hash exists               | Request a fresh sponsored operation                                                            |
| Bundler transaction confirmed but no order appears                 | Inspect the UserOperation receipt and Plether commitment event                                 |
| Finalization transaction failed, but order remains in Open Orders | Retry finalization                                                                            |
| Order disappeared and position did not change                     | Check Order History for a terminal failure                                                    |
| Order History shows Type: Commit and Status: Failed               | Commitment succeeded; later execution failed before producing trade activity                  |
| Failed row shows Not executed for price and size                  | The order reached a terminal state without applying the trade                                 |
| Expired cleanup reverts                                           | Check FIFO progress and retry after the strict expiry boundary                                |
| Maximum pending-order message                                     | Execute or clean the oldest eligible order                                                    |
| Close remains Pending while position health falls                 | Deposit collateral if appropriate and monitor liquidation; the close has not reduced exposure |
| History has not appeared yet                                      | Refresh after the indexer updates and verify the transaction onchain                          |

### Before creating a replacement

1. Confirm the original order is absent from Open Orders.
2. Check its terminal status in Order History.
3. Refresh the current position and Margin Account.
4. Review other pending orders.
5. Check live, FAD and frozen-market state.
6. Request a new preview.
7. Review carry and account health.
8. Set a new acceptable price.
9. Confirm the new execution reward.
10. Monitor the replacement until it reaches a terminal state.
