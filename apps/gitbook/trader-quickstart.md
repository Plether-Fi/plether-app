# Trader quickstart

> **Deposit USDC. Choose your dollar view. Commit now; price later.**

Plether Perps lets you take a leveraged **LONG USD** or **SHORT USD** position against the Plether currency basket.

This is not a spot swap. You do not receive a LONG or SHORT token in your wallet. You open an onchain position backed by USDC in your Plether Perps DEX margin account.

### The flow in one line

`Wallet MockUSDC → Margin account → Committed order → Open position → Closed settlement → Margin account → Wallet`

Closing a position does not send funds directly to your wallet. Settlement first returns to your Plether Perps DEX margin account. You withdraw separately.

### Before you begin

You need:

* A compatible self-custody wallet
* Arbitrum Sepolia selected in your wallet
* Arbitrum Sepolia ETH for transaction fees
* MockUSDC for collateral
* Enough time to monitor your order until it executes or fails

Use only the official Plether application. Never send tokens directly to a Plether contract or share your seed phrase with anyone.

### 1. Connect your wallet and get test funds

Open Plether Perps DEX and select `Connect Wallet`.

Confirm that your wallet is connected to **Arbitrum Sepolia**, chain ID **421614**.

Two assets are required:

* **Arbitrum Sepolia ETH** pays network transaction fees.
* **MockUSDC** acts as testnet collateral.

The welcome window lets you enter your wallet address and select `Get 100,000 mock USDC`. It also links to an Arbitrum Sepolia ETH faucet.

If you previously closed the welcome window, select `Get mock USDC` in the testnet notice bar to open it again.

MockUSDC is test collateral. It is not issued by Circle and cannot be redeemed for real dollars.

<figure><img src=".gitbook/assets/Zrzut ekranu 2026-07-14 o 17.04.41.png" alt="" width="375"><figcaption></figcaption></figure>

### 2. Deposit USDC into your margin account

Find the **Margin Account** section in the trade ticket and select `Deposit`.

Enter the amount of MockUSDC you want to deposit and confirm the transaction.

If your existing token allowance is insufficient, the application will first request approval to use the specified MockUSDC. In that case, depositing requires two wallet confirmations:

1. Approve MockUSDC.
2. Deposit MockUSDC.

Wait for both transactions to confirm.

Depositing does not open a position. It moves MockUSDC from your wallet into your Plether margin account, where it becomes available for trading.

The interface separates several balances:

| Balance                | Meaning                                            |
| ---------------------- | -------------------------------------------------- |
| **Wallet balance**     | MockUSDC held outside Plether                      |
| **Available to Trade** | Account collateral currently available for orders  |
| **Position margin**    | USDC assigned to an open position                  |
| **Withdrawable**       | Free USDC that can currently return to your wallet |
| **Portfolio value**    | Current account equity, including position PnL     |

These values do not need to be equal. Open positions, pending orders, carry and margin requirements can make your withdrawable balance lower than your portfolio value.

Keep some USDC free rather than committing the entire account to one position.

<figure><img src=".gitbook/assets/Zrzut ekranu 2026-07-14 o 17.06.22.png" alt="" width="375"><figcaption></figcaption></figure>

### 3. Check the market state

Before configuring the order, read the **Market State** panel above the trade ticket.

* **Open** means new positions and increases are available, subject to live risk limits.
* **Close-only** means you may reduce or close exposure but cannot add new exposure.
* **Closed** or **Paused** means orders cannot execute normally.
* **Degraded** means additional protocol restrictions may apply.

The panel shows how long the current state is expected to last. Always follow the live validation shown in the trade ticket.

### 4. Choose LONG USD or SHORT USD

Choose the direction that matches your view:

| Position      | Your view                  | Benefits when                                 |
| ------------- | -------------------------- | --------------------------------------------- |
| **LONG USD**  | The dollar will strengthen | USD gains against the Plether currency basket |
| **SHORT USD** | The dollar will weaken     | USD loses against the Plether currency basket |

The raw foreign-currency basket moves inversely to dollar strength:

* **LONG USD** benefits when the raw basket falls.
* **SHORT USD** benefits when the raw basket rises.

The displayed perps price is dollar-oriented, so the interface behaves conventionally: LONG benefits from a rising displayed price, while SHORT benefits from a falling displayed price.

On the current testnet interface, the direction buttons may appear as `Long plDXY Perp` and `Short plDXY Perp`. These correspond to **LONG USD** and **SHORT USD** respectively.

Plether supports one live direction per wallet. You can increase a position in the same direction, but you cannot reverse it in one transaction. To change from LONG USD to SHORT USD—or the other way around—you must close the existing position first and wait for that close to execute.

For a new position:

* Leave `Reduce only` disabled.
* Leave `Margin Call Simulator` disabled. It is a boundary-testing mode that can place a position extremely close to liquidation.

### 5. Set your exposure and leverage

Enter your intended size in the `plDXY Perp exposure` field.

This is your market exposure, not the amount of USDC being spent. The **Leverage** control determines how much position margin supports that exposure.

For the same exposure:

* Lower leverage assigns more margin and provides more room before liquidation.
* Higher leverage assigns less margin and makes fees, carry and small price movements more consequential.

The interface’s maximum leverage is a limit, not a recommendation.

Next, review `Max slippage`. Plether uses it to calculate your execution limit—the worst price at which the order may execute.

A tighter limit provides stronger price protection but makes failure more likely if the market moves before execution. An unlimited setting removes that protection and is not appropriate for a first trade.

### 6. Read the preview

The preview is the most important part of the ticket. Review at least:

* Direction
* plDXY Perp exposure
* Contract notional
* Initial margin
* Maintenance margin
* Resulting leverage
* Execution limit
* Liquidation price
* Estimated protocol execution fee
* VPI or price impact
* Adverse oracle confidence spread
* Estimated execution reward

These costs are different:

* The **protocol execution fee** is charged when the trade executes.
* **VPI** adjusts for trade size, available pool depth and directional imbalance. It can be a cost or a rebate.
* The **oracle confidence spread** adjusts the execution price for oracle uncertainty. It is not a separate USDC fee.
* The **execution reward** is reserved for whoever finalizes the order.
* **Carry** accrues after the position is open. Either direction can pay it.

A preview is an estimate, not an executable quote. The final price comes from eligible oracle data published after commitment.

If the preview is invalid, do not proceed. The ticket may require you to reduce exposure, deposit more margin, adjust slippage or wait for market conditions to change.

### 7. Review and commit the order

Select `Review Long` or `Review Short`.

The **Commit Preview** repeats the order terms. Check the direction, exposure, leverage, execution limit, liquidation price and total funding requirement one final time.

If everything matches your intent, select `Confirm Commit` and approve the wallet transaction.

You can close the review window before committing. Once the commitment confirms onchain, the rules change:

* The order becomes binding.
* It cannot be cancelled.
* Its margin and execution reward are reserved.
* It enters the global first-in, first-out queue.
* It is not yet an open position.

Plether does not let the trader or keeper choose a favorable future price. Execution uses the first eligible Pyth observation strictly after the order was committed, adjusted for oracle confidence, VPI and your execution limit.

If that price exceeds your slippage limit, the order fails rather than executing outside it.

### 8. Wait for finalization

After commitment, the application displays `Finalizing execution price`.

A keeper gets the first opportunity to finalize the order. If automatic finalization does not happen during the initial grace period, the interface exposes `Finalize Trade`.

Manual finalization requires another wallet transaction. It does not let you choose a different price; it submits the data needed to settle the already committed order.

Monitor the order until it reaches a terminal state:

| Status             | Meaning                                                   |
| ------------------ | --------------------------------------------------------- |
| **Pending reveal** | Waiting for an eligible oracle update or finalization     |
| **Executed**       | The position was opened, increased or reduced             |
| **Failed**         | The order will not execute                                |
| **Expired**        | Its execution window ended before successful finalization |

The **Open Orders** tab shows the current countdown and explicitly displays `Cancel unavailable`.

Do not submit a duplicate order simply because the first remains pending. Global FIFO ordering means earlier orders must be resolved first.

If an order expires, use `Clean Up` when the action becomes available. If it fails, check **Order History** for the reason before submitting another order. Failed and expired orders are not retried automatically.

### 9. Check and manage the position

After execution, open the **Position** tab and verify:

* Direction
* plDXY Perp exposure
* Entry notional
* Entry price
* Leverage
* Liquidation price
* Unrealized PnL
* Cost of carry

The preview and final result can differ. Use the executed position—not the original preview—as the record of what you own.

Plether uses a shared-collateral account. Position leverage is calculated from the margin assigned to that position, but free USDC elsewhere in the account can also contribute to account equity and protect it from liquidation.

Carry continues to accrue while the position is open. It reduces account equity and can move a position toward liquidation even when the market price changes very little.

A pending close order does not protect you from liquidation before that close executes.

#### Add margin

To strengthen the position without increasing its size:

1. Select the pencil icon next to **Leverage**.
2. Open `Edit Position Margin`.
3. Enter an amount under `Add margin`.
4. Review the resulting margin and leverage.
5. Select `Add Margin`.

Adding margin is immediate and does not enter the delayed order queue. It reduces position leverage but does not change exposure.

Direct position-margin removal is not supported. Margin is released proportionally when the position is reduced or closed.

#### Increase the position

To increase exposure, submit another order in the same direction. It follows the same delayed, binding order process as the original open.

Review the resulting combined position rather than evaluating the increase in isolation.

### 10. Reduce or close the position

Use the trade ticket to exit. The Position panel does not have a separate close button.

Enable `Reduce only`, then enter the exposure you want to close.

* For a partial close, enter part of the current exposure. The action becomes `Review Reduce`.
* For a full close, select `Current Position` or `Max` to fill the available position size. The action becomes `Review Close`.

Review the close preview, including execution price, realized PnL, VPI and execution fee. Then commit and monitor the order exactly as you did when opening.

A reduction or close is still:

* Delayed
* Binding after commitment
* Non-cancellable
* Subject to oracle confidence and slippage
* Processed through the global FIFO queue

Partial reductions must satisfy the current minimum-order and remaining-position rules. A complete residual close may be permitted even when the remaining amount is below the ordinary minimum.

When the close executes, released margin and settlement return to your Plether margin account. They do not go directly to your wallet.

> **Trader claims**
>
> In an exceptional cash-shortfall scenario, part of a profitable close can become a **trader claim** instead of immediately withdrawable USDC.
>
> A trader claim is a protocol liability associated with your address. It is not wallet USDC and cannot be treated as available margin until settled. See **Trader claims** for the complete settlement process and liquidity conditions.

### 11. Withdraw USDC

In the **Margin Account** section, select `Withdraw`.

Enter an amount no greater than the displayed **Withdrawable** balance and confirm the transaction.

Withdrawable USDC excludes collateral or funds required for:

* Position margin
* Pending-order margin
* Reserved execution rewards
* Accrued carry
* Maintenance requirements
* Other active protocol safeguards

You can withdraw free USDC while a position remains open, but doing so can reduce the account buffer protecting that position. Review the position’s health and liquidation price before confirming.

After a successful withdrawal, MockUSDC moves from your Plether margin account back to the connected wallet.

### Common problems

| Problem                                              | What to check                                                                     |
| ---------------------------------------------------- | --------------------------------------------------------------------------------- |
| Wallet is connected but trading is disabled          | Switch to Arbitrum Sepolia                                                        |
| Deposit does not proceed                             | MockUSDC balance, token approval and ETH for gas                                  |
| Order preview is invalid                             | Minimum size, deposited margin, side capacity, market state and fresh oracle data |
| Order remains pending                                | Open Orders countdown, earlier FIFO orders and oracle availability                |
| Order expired                                        | Use `Clean Up`, then submit a new order                                           |
| Order failed                                         | Check Order History before changing slippage or resubmitting                      |
| Opposite direction is unavailable                    | Close the current position and wait for execution first                           |
| Withdrawal is below portfolio value                  | Position margin, pending reservations, carry and maintenance requirements         |
| Position health worsened without much price movement | Check accumulated carry and account equity                                        |

### First-trade checklist

Before selecting `Confirm Commit`:

* Start with a small test position.
* Confirm whether you are **LONG USD** or **SHORT USD**.
* Keep free USDC outside the position.
* Read the execution limit and avoid unlimited slippage.
* Review the liquidation price.
* Review the execution fee, VPI, confidence spread and execution reward.
* Accept that the committed order cannot be cancelled.
* Monitor the order until it executes, fails or expires.
* Verify the final position after execution.

### Continue reading

* **How delayed orders execute**
* **Margin and liquidation**
* **Fees, VPI and cost of carry**
* **Managing and closing a position**
* **Trader claims**
* **Market hours and closures**
