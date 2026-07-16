# Trader troubleshooting

Plether actions can stop at different stages. A rejected wallet request, a reverted commitment, a pending order and a terminally failed order require different responses.

Start by checking the current onchain state before submitting another transaction.

### Check these first

1. Confirm the connected wallet.
2. Confirm the supported network.
3. Keep enough native token for gas.
4. Check the market state and oracle timestamp.
5. Open **Open Orders** and **Order History**.
6. Inspect any available transaction hash in the block explorer.
7. Refresh the application.

Do not commit a replacement order until the original is absent from **Open Orders**. A second commitment creates another binding order with its own margin and execution-reward reservations.

> **Screenshot placeholder:** Market state, oracle freshness, connected wallet and the Open Orders and Order History tabs.

### Identify the current state

| What you see                      | Onchain result                      | Next step                                     |
| --------------------------------- | ----------------------------------- | --------------------------------------------- |
| Wallet request rejected           | Nothing was submitted               | Correct the wallet issue and retry            |
| Commit transaction reverted       | No order was created                | Read the error and adjust the order           |
| Order appears in Open Orders      | Order is Pending                    | Wait for execution conditions or expiry       |
| Finalization transaction reverted | Order usually remains Pending       | Refresh its status before retrying            |
| Order appears as Failed           | Order is terminal                   | Create a new order after addressing the cause |
| Order appears as Executed         | Position settlement is final        | Refresh account and position data             |
| App timed out after submission    | Result may still be pending onchain | Check the transaction hash before retrying    |

A successful commitment only creates an order. Position size and entry price change after that order executes.

```
Commit confirmed
→ Order becomes Pending
→ Order reaches execution
→ Position changes
```

### I cannot review or commit an open or increase

Common causes include:

* The wallet is disconnected or on the wrong network.
* The plDXY price or oracle publish time is unavailable.
* The order size is zero or below the minimum.
* Available to Trade cannot cover the margin and execution reward.
* The requested margin fails the initial-margin requirement.
* The account already holds a position in the opposite direction.
* The account has reached its pending-order limit.
* The requested direction exceeds the current skew limit.
* The HousePool cannot admit the additional maximum liability.
* The market is close-only.
* The protocol is in degraded mode.
* New risk commitments are paused.

The interface may show messages such as:

* **Deposit … USDC more before committing this order**
* **Minimum order size is …**
* **Max Long/Short exposure is … before hitting the market skew cap**
* **Reduce or close the current position first**
* **Trade preview is unavailable**
* **You already have … pending orders**

Adjust the size, leverage or margin according to the displayed reason. Market-state, skew and solvency limits may require waiting for protocol conditions to change.

A reverted commitment rolls back the complete transaction. No order, margin reservation or execution-reward reservation remains.

> **Screenshot placeholder:** Disabled Review button with an insufficient-margin, minimum-size or skew-cap message.

See **Open or increase a position**, **Market states and oracle closures** and **Solvency at a glance**.

### I cannot reduce or close

Check the following:

* An executed position exists.
* The selected direction matches that position.
* Earlier pending reductions have not already reserved the requested size.
* The reduction does not exceed the remaining unreserved position.
* The residual position will remain above the minimum size.
* The account can reserve the complete close execution reward.
* A partial reduction can fully pay its losses and costs.

Exposure from a pending open cannot be reduced yet. Wait for the open order to execute.

#### Another order is already closing the position

Pending close orders reserve their requested position size. If existing orders already cover the full position, the interface blocks another reduction.

Execute the earlier order or wait until it expires and becomes eligible for **Clean Up**.

#### The remaining position would be too small

Reduce a smaller amount or submit a full close.

#### The partial reduction would be underfunded

A partial reduction must fully cover its obligation, including:

* Trading loss
* Execution fee
* Signed VPI
* Carry
* Frozen-close spread, when applicable

Add collateral, reduce a smaller amount or submit a terminal full close.

A full close can consume all reachable collateral and record a genuine uncovered trading obligation as bad debt. During `oracleFrozen`, only an uncollectible frozen-close spread may be waived.

Full-close treatment does not bypass slippage, expiry, oracle validation or execution-reward backing.

Close commitments remain available at contract level during degraded mode and while new risk commitments are paused.

See **Reduce or close a position**.

### My order remains Pending

An order can remain pending because:

* Earlier orders are ahead in the global FIFO queue.
* The first eligible post-commit Pyth observation is not available yet.
* The order is still protected by same-block execution rules.
* A keeper has not finalized it.
* Historical oracle data could not be fetched or validated.
* Oracle confidence is too wide.
* Basket component timestamps are not sufficiently aligned.
* An open order reached execution during a close-only state.
* An earlier expired order needs cleanup.
* The execution attempt supplied insufficient gas.
* The order has not yet crossed its expiry time.

A close-only block on a previously committed open leaves the order pending under the current contracts. It may execute after the market reopens or expire first.

While an order is pending:

* Committed opening margin remains reserved.
* The execution reward remains reserved.
* A pending open creates no position exposure.
* A pending close removes no position exposure.
* The executed position continues accruing PnL and carry.
* The executed position remains liquidatable.

Pending orders cannot be cancelled or repriced. They end through execution, terminal failure or expiry cleanup.

Use **Finalize Trade** when the active order modal makes it available. Once an order expires, use **Clean Up** to release eligible reservations and remove it from the queue.

> **Screenshot placeholder:** Pending reveal state with the finalization countdown, Finalize Trade and an expired Open Orders row showing Clean Up.

See **How orders execute** and **Why is my order pending or failed?**

### My finalization transaction failed

A reverted finalization transaction usually leaves the order pending.

Possible causes include:

* Finalization was attempted too early.
* Eligible historical Pyth data was unavailable.
* The Pyth update expired before confirmation.
* The Pyth fee changed.
* Oracle confidence was too wide.
* Basket component publish times diverged.
* An earlier FIFO order blocked execution.
* The transaction supplied insufficient gas.
* The network, RPC or oracle-data service was unavailable.

Open **Open Orders** and check whether the order is still present.

If it remains pending and has not expired, retry after the transient condition changes. The app will fetch new eligible price data where available.

If the finalization transaction confirmed, also check **Order History**. A confirmed transaction can produce either `OrderExecuted` or a terminal `OrderFailed` result.

After reloading the current application, manual finalization may no longer be available from the Open Orders table. The order can still be executed by a keeper. If it expires, use **Clean Up**.

### My order Failed

A Failed order cannot execute later. Creating another trade requires a new commitment.

Terminal reasons include:

* Expiry
* Slippage exceeded
* Execution-time engine rejection
* Account liquidation
* Unexpected engine failure

After an ordinary failure:

* The requested position change is not applied.
* Any remaining committed opening margin is released.
* The reserved execution reward is paid to the account that finalized or cleaned up the order.
* The account’s pending-order count decreases.

The execution reward is spent even though the position change failed.

#### Slippage exceeded

The eligible execution price, including the oracle confidence adjustment, crossed the acceptable-price boundary.

Use a new preview. Increase slippage only after reviewing how much execution-price movement you are prepared to accept.

#### Order expired

Expiry is terminal. Use **Clean Up** if the expired order remains visible.

Cleanup releases committed opening margin and pays the reserved execution reward to the cleanup caller. A new order is required.

#### Account liquidated

Liquidation removes the position and marks all of that account’s pending orders as Failed.

See **Why is my order pending or failed?** for the detailed failure lifecycle.

> **Screenshot placeholder:** Order History showing a failed order, its reason and commit/finalization transaction links.

### My transaction succeeded, but the position did not change

Check which transaction succeeded.

A successful deposit changes the Margin Account. A successful commit creates a pending order. A successful cleanup removes an expired order. Only successful order execution changes the position.

Review:

* Transaction events
* Order ID
* Open Orders
* Order History
* Position size and entry price

If the order reached a terminal failure, the position remains unchanged.

### My deposit failed

Check:

* Wallet USDC balance
* Deposit amount
* Connected account
* Network
* Native gas balance
* USDC allowance

The first deposit may require two wallet confirmations:

1. Approve USDC.
2. Deposit USDC.

Approval alone does not move USDC into the Margin Account.

A reverted deposit leaves wallet and Margin Account balances unchanged.

#### The deposit succeeded, but Available to Trade increased by less

Depositing into an account with an open position checkpoints carry. Some of the deposited USDC may be collected against accrued carry.

Review:

* Margin Account balance
* Available to Trade
* Cost of carry
* Position margin
* Transaction events

A deposit increases free Margin Account USDC. To assign part of it directly to the current position, use **Edit Position Margin** and **Add margin**.

See **Your Margin Account**.

### I cannot add position margin

Adding position margin requires:

* The claim-owning wallet to be connected
* An existing open position
* Sufficient free Margin Account USDC
* An amount greater than zero

Carry is realized before the margin is assigned. If free USDC is insufficient after that carry collection, reduce the amount or deposit additional USDC first.

Position margin can be added directly. Releasing assigned position margin requires reducing or closing the position.

### Withdrawable is zero or my withdrawal failed

Withdrawable can be lower than the Margin Account balance or Available to Trade.

Common causes include:

* Position margin is locked.
* Pending orders reserve margin.
* Execution rewards remain reserved.
* Carry reduced the available balance.
* An open position does not have enough post-withdraw margin.
* The live mark required for an open-position withdrawal is stale.
* The protocol is in degraded mode.

An account with an open position must preserve the higher of the applicable initial-margin and current maintenance or FAD requirement after withdrawal.

A flat account can still have funds reserved by pending orders and execution rewards.

Try:

* Reducing the withdrawal amount
* Finalizing or cleaning up pending orders
* Waiting for a fresh mark
* Adding collateral
* Reducing or closing the position

Trader withdrawals are paid from the Margin Clearinghouse. HousePool payout liquidity and trader-claim coverage do not determine ordinary Margin Account withdrawals.

A reverted withdrawal leaves the account balance unchanged.

> **Screenshot placeholder:** Withdraw Margin modal showing Withdrawable and an amount-exceeds-withdrawable message.

See **Your Margin Account** and **Read your position and account health**.

### The execution price differs from the preview

The preview uses the current market and account state. Execution uses the eligible oracle observation reached later through FIFO.

Differences can come from:

* Market movement while the order waits
* Adverse oracle-confidence adjustment
* Earlier orders changing market skew
* A change in market state
* Rounding

For the dollar-oriented price shown in the interface:

| Action                     | Acceptable-price condition                         |
| -------------------------- | -------------------------------------------------- |
| Open or increase LONG USD  | Execution at or below the maximum acceptable price |
| Open or increase SHORT USD | Execution at or above the minimum acceptable price |
| Reduce or close LONG USD   | Execution at or above the minimum acceptable price |
| Reduce or close SHORT USD  | Execution at or below the maximum acceptable price |

A voluntary close during `oracleFrozen` uses the validated frozen basket and the fixed `50 bps` frozen-close spread. The usual adverse confidence price shift is removed for this path, while confidence-width validation and signed VPI remain active.

See **How orders execute** and **Market states and oracle closures**.

### My realized result differs from Unrealized PnL

Unrealized PnL reflects the price movement from entry to the current mark.

Final settlement can also include:

* Execution fee
* Signed VPI
* Carry
* Execution-price confidence adjustment
* Frozen-close spread
* Rounding

The execution reward is reserved separately when the order is committed.

Compare the final execution result with the close preview and transaction details rather than the displayed Unrealized PnL alone.

See **How PnL is calculated** and **Fees, VPI and cost of carry**.

### Available to Trade is lower than expected

Available to Trade excludes:

* Assigned position margin
* Committed-order margin
* Reserved execution rewards

Carry can also reduce the balance when an account action checkpoints it.

Unrealized profit can contribute to Portfolio value before it becomes free Margin Account USDC. Realization may produce an immediate Margin Account credit or a trader claim, depending on HousePool settlement liquidity.

### My liquidation price or account health changed

Check for changes in:

* Current mark
* Unrealized PnL
* Carry
* Position margin
* Free Margin Account USDC
* Pending-order reservations
* Execution-reward reservations
* Current FAD margin requirement

Account health uses physically reachable Margin Account collateral and excludes reserved execution rewards. Pending carry reduces equity before it is collected.

Actual liquidation uses an adverse confidence-adjusted oracle price. A position close to the boundary can therefore become liquidatable before the central displayed mark reaches the estimated liquidation price.

If **Liquidation price** shows **Not in range**, the projected threshold is outside the protocol’s bounded settlement range under the current position state.

See **Margin, leverage and liquidation** and **Read your position and account health**.

### My close is pending while liquidation risk is increasing

The complete position remains active until the close executes.

During the wait:

* PnL continues moving.
* Carry continues accruing.
* The position remains liquidatable.
* The close execution reward has already been reserved.
* Position margin may already have been used to back that reward.

Depositing additional USDC can improve account-wide health while leaving position size unchanged.

If liquidation executes first:

* The position is removed.
* Every pending order for the account becomes Failed.
* Reserved execution rewards are forfeited to the protocol treasury.
* Committed-order margin may be consumed by terminal settlement.
* Any remainder is released.
* A positive residual is paid immediately or recorded as a trader claim.

Liquidation applies no new VPI delta and no frozen-close spread.

### What each market state means for troubleshooting

| Market state                | Trader effect                                                                                                                                                |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **Open**                    | Opens, increases, reductions and closes are available subject to normal checks                                                                               |
| **FAD close-only**          | Opens are blocked; closes use live post-commit pricing, the FAD margin requirement and no frozen-close spread                                                |
| **Oracle frozen**           | Opens are blocked; voluntary closes use the validated frozen basket, signed VPI and the fixed `50 bps` spread                                                |
| **Degraded**                | Opens and withdrawals backed by an open position are blocked; deposits, margin additions, closes, liquidations and flat-account withdrawals remain available |
| **Risk commitments paused** | New opens are blocked; close commitments and existing order execution remain available at contract level                                                     |

An oracle-frozen close can still become unavailable if the stored basket exceeds the extended staleness limit.

Carry continues accruing through FAD, oracle-frozen and stale periods.

See **Market states and oracle closures**.

### I have a trader claim but cannot settle it

Settlement requires:

```
Recognized HousePool assets
≥
Total outstanding trader claims
```

The condition applies to aggregate claims. Cash sufficient for one individual claim does not make that claim serviceable during an aggregate shortfall.

Claim settlement processes the account’s complete balance. Retrying while coverage remains insufficient only spends gas.

Successful settlement credits the Margin Account. Moving the USDC to the wallet requires a separate withdrawal.

An existing claim may also be consumed against a shortfall from a losing terminal full close or liquidation.

See **Check and settle a trader claim** and **Settlement liquidity and trader claims**.

### Open Orders and Order History disagree

**Open Orders** reads the current onchain queue. **Order History** and **Transaction History** depend on indexed events and may update later.

If an order disappears from Open Orders:

1. Check its commit or finalization receipt.
2. Look for `OrderExecuted` or `OrderFailed`.
3. Refresh Order History.
4. Check the current position and Margin Account.

A history API error does not change the onchain order state.

Avoid submitting a replacement while the result remains unclear.

### Information to provide when reporting a problem

Include:

* Wallet address
* Network
* Order ID
* Commit transaction hash
* Finalization or cleanup transaction hash
* Approximate time
* Market state
* Exact error message
* Screenshot of Open Orders
* Screenshot of Order History
* Screenshot of the relevant Margin Account or position field

Never share a seed phrase, private key or wallet recovery code.
