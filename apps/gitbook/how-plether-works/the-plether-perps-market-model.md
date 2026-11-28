# The Plether Perps market model

> **The price comes from the market. The liability sits onchain.**

Plether is an oracle-priced[^oracle], cash-settled perpetual market for leveraged **LONG USD** and **SHORT USD** exposure.

Traders do not buy foreign currencies, receive a position token or borrow notional[^notional] dollars. Instead, they open a contract settled in USDG[^usdg] whose value tracks the Plether Dollar Index (`plDXY`). Realized PnL[^pnl], VPI[^vpi], execution fees, carry[^carry] and any frozen-close spread settle directly in USDG.

| Function | Plether mechanism |
| --- | --- |
| **Reference price** | Six-currency external FX[^fx] basket from Pyth |
| **Order sequencing** | Binding, delayed global FIFO[^fifo] queue |
| **Trade-cost adjustments** | Adverse oracle confidence, VPI and frozen-close spread |
| **Trader collateral** | Trading Account's USDG Margin Account |
| **Settlement capital** | Shared USDG liquidity pool |
| **LP[^lp] risk allocation** | Senior (`psLP`) and Junior (`pjLP`) tranche[^tranche] vaults |

## One market, two directions

The Plether Dollar Index starts from a normalized arithmetic basket of six foreign currencies priced in USD, which moves inversely to dollar strength. The trading interface displays its bounded dollar-oriented complement:

```text
Displayed dollar price = 2.00 − bounded raw basket
```

| Position | Raw basket | Displayed `plDXY` price | Benefits when |
| --- | --- | --- | --- |
| **LONG USD** | Falls | Rises | USD strengthens |
| **SHORT USD** | Rises | Falls | USD weakens |

Contracts account against the raw basket while the interface shows the dollar-oriented complement — both represent the exact same payoff. See [Understanding the Plether Dollar Index](../welcome/understanding-the-plether-dollar-index.md).

## Neither an order book nor an AMM

* **Not an order book:** Plether does not match bids and asks or require equal LONG USD and SHORT USD open interest. Large opens, closes or liquidations never move the Plether chart — only external FX feeds move it.
* **Not an AMM:**[^amm] The liquidity pool supplies **settlement capacity**, not price discovery. Trading changes open interest, pool liability, skew[^skew], VPI, carry and LP withdrawal capacity without distorting the oracle mark.

> **The oracle sets the market price. VPI, carry and capacity limits price the risk a trade places on the pool.**

## How an order becomes a position

Plether executes trades via a binding **commit-first, price-later** pipeline:

1. Deposit USDG into your Trading Account's Margin Account.
2. Authorize a sponsored LONG USD or SHORT USD commit in your owner wallet.
3. **Opens and increases** reserve submitted margin and an opening execution reward (`0.01–0.25 USDG`). **Reductions and full closes** (including TP/SL closes) reserve **zero** execution reward and no new margin; carry still checkpoints from active position margin first, then eligible free USDG.
4. The binding order enters the global FIFO queue (committed orders cannot be cancelled or repriced).
5. A keeper[^keeper] finalizes it using the first eligible post-commit Pyth basket observation (during live or FAD-only[^fad] sessions, with the `0.25×` adverse confidence shift) or the validated frozen basket (for voluntary closes during `oracleFrozen`, unshifted plus the `50 bps` frozen-close spread).
6. If execution-limit and risk checks pass, the engine updates the position and settles balances.

See [How orders execute](how-orders-execute.md).

## The liquidity pool as economic counterparty

Every trader faces the shared **liquidity pool** — never an individual LP or peer trader:

* Collectible trader losses, realized carry, positive VPI, paid frozen-close spread and `50%` of collected liquidation charges flow into LP economics.
* Profitable trader payouts and negative VPI rebates are funded by the liquidity pool.
* Protocol execution fees (`0.04%`) belong to the protocol treasury, not LPs; opening execution rewards come from trader collateral, not pool capital.

Both Senior and Junior vaults back the same pool. **Junior** takes first loss and earns residual upside; **Senior** receives a Junior-funded target coupon and absorbs losses only after Junior reaches zero.

## Why the `0.00–2.00` range makes solvency measurable

Plether clamps the settlement mark to **`0.00 <= raw basket <= 2.00`**:

* **LONG USD** max profit occurs as the raw basket approaches `0.00` (displayed price → `2.00`).
* **SHORT USD** max profit occurs as the raw basket reaches `2.00` (displayed price → `0.00`).

Because every position's worst-case payout is finite and known at entry, Plether enforces pre-trade solvency before admitting new risk:

```text
maxLiability = max(aggregate LONG USD max profit, aggregate SHORT USD max profit)
maxLiability + ceil(maxLiability × 0.25% settlement buffer) <= effective pool backing
```

where `effective pool backing = max(0, recognized pool assets − total outstanding trader claims)`. If a new open would violate this invariant (or the `40%` net-pool skew cap), it is rejected.

The `2.00` ceiling is a fixed solvency boundary, not a market forecast, stop-loss or principal guarantee. If external FX moves beyond `2.00`, Plether PnL stops extending — creating basis risk[^basis-risk] against unrestricted spot FX.

## Isolated price collateral vs account settlement

A Trading Account separates USDG into distinct buckets:

| Account balance | Role |
| --- | --- |
| **Free Margin Account USDG** | Available for new orders, carry overflow, action charges or withdrawal (**does not** back position price losses) |
| **Position margin** | Active collateral backing position price risk |
| **Committed-order margin** | Reserved for a pending open/increase (**does not** back position price losses) |
| **Execution-reward / TP-SL reserves** | Reserved for opening-order execution or TP/SL triggering |

Price-risk backing equals **position margin (after carry) + same-account trader claims**. Carry collects from **active position margin first, then eligible free USDG** (never from trader claims). Plether can never debit assets sitting outside the protocol in your owner wallet.

## How value moves through the system

| Event | Trader side | Pool side |
| --- | --- | --- |
| **Deposit margin** | USDG enters Trading Account's Margin Account | Unchanged |
| **Commit order** | Opens reserve margin + execution reward; closes reserve zero reward and checkpoint carry | Unchanged |
| **Open / increase executes** | Position margin locks; execution fee & VPI settle | Pool underwrites bounded payout liability |
| **Mark price moves** | Unrealized PnL updates | Pool Terminal NAV & liability views update; no cash moves |
| **Losing close** | Price loss nets same-account claim first, then position margin; action charges collect separately | Collectible LP portion enters pool; `0.04%` execution fee goes to treasury |
| **Profitable close** | Released margin settles; complete fresh payout credits position margin (partial close), free USDG (full close) or becomes a trader claim | Pool pays full fresh profit or records it in full as a trader claim |
| **Carry checkpoint** | Collected from active position margin first, then eligible free USDG | Realized carry enters LP revenue |
| **LP deposit / withdrawal** | Unchanged | Enters or leaves via hourly tranche vault epochs |

## Profitable closes, trader claims, bad debt and no ADL

* **Unrealized PnL is not cash:** Marked trader profits reduce LP NAV immediately, while marked trader losses are capped at collectible collateral and never count as spendable LP withdrawal cash until realized.
* **All-or-nothing profit payouts & trader claims:** If unreserved pool cash covers a fresh profit payout in full, Plether credits it immediately (to **position margin** on a partial close, or **free Margin Account USDG** on a full close). Otherwise, the **entire** fresh payout becomes a senior **trader claim** settled all-or-nothing once `recognized pool assets >= total outstanding trader claims`.
* **Losses & bad debt:** Partial closes must fund their losses and action charges completely without waiver. Full closes net same-account claims first, then position margin against price loss (any uncovered price loss becomes explicit LP bad debt), while uncollectible action charges (including frozen-close spread) are waived rather than booked as bad debt.
* **No counterparty auto-deleveraging (ADL):** Profitable traders are never forcibly closed because other traders defaulted. Instead, Plether relies on bounded payouts, pre-trade solvency, skew caps, VPI, carry, LP withdrawal firewalls, trader claims, bad-debt accounting and full position liquidation when an account's own equity hits maintenance margin.

## How Plether compares to common perpetuals

| Feature | Plether Perps | Common perpetual venue |
| --- | --- | --- |
| **Price source** | Six-currency external Pyth FX basket | Order book or AMM |
| **Counterparty** | Tranched USDG liquidity pool | Peer traders or market makers |
| **Execution** | Delayed, binding global FIFO | Immediate match or swap |
| **Settlement range** | Bounded `0.00–2.00` | Usually unbounded |
| **Ongoing cost** | Carry paid for LP-backed payout capacity (both sides can pay simultaneously) | Peer-to-peer funding rate |
| **Stress containment** | Bounded solvency, LP waterfall, trader claims, no ADL | Insurance fund, socialized losses or ADL |

## The three-part mental model

1. **The oracle defines the market.**
2. **The delayed FIFO queue defines execution.**
3. **The liquidity pool underwrites the bounded outcome.**

[^oracle]: A service that supplies external market data to smart contracts; Plether uses Pyth price feeds.
[^notional]: The face value of a position’s market exposure, not the amount of collateral posted.
[^usdg]: USDG is used for margin and settlement. The testnet uses MockUSDG, a test token with no real-world value.
[^pnl]: Profit and loss, the financial result of market-price movement on a position.
[^vpi]: Virtual Price Impact, a separate USDG charge or rebate based on how a trade changes pool directional imbalance.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^fx]: Foreign exchange, the market for trading one currency against another.
[^fifo]: First in, first out; orders at the front of the queue are processed before later orders.
[^lp]: Liquidity provider, a participant that supplies USDG capital to the liquidity pool.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^amm]: Automated market maker, an onchain liquidity mechanism that prices trades using a pool and formula.
[^skew]: The imbalance between aggregate LONG USD and SHORT USD exposure.
[^keeper]: A permissionless actor or bot that submits order-finalization or protocol-maintenance transactions.
[^fad]: Friday Afternoon Deleverage, Plether’s wider scheduled close-only window around the weekly FX closure.
[^basis-risk]: The risk that a hedge and the exposure it is intended to offset do not move together.
