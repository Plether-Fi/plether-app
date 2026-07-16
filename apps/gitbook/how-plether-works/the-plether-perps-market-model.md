# The Plether Perps market model

> **The price comes from the market. The liability sits onchain.**

Plether is an oracle-priced, cash-settled market for leveraged **LONG USD** and **SHORT USD** exposure.

Traders do not buy currencies, receive a position token or borrow a notional amount of dollars. They enter a contract whose value changes with the Plether Dollar Index. Only the resulting difference is settled in USDC.

Plether separates the functions that many trading venues combine:

| Function                  | Plether mechanism                          |
| ------------------------- | ------------------------------------------ |
| **Reference price**       | External FX oracle data                    |
| **Order sequencing**      | Binding, delayed FIFO queue                |
| **Trade-cost adjustment** | Oracle confidence and virtual price impact |
| **Trader collateral**     | Trading Account’s USDC Margin Account      |
| **Settlement capital**    | USDC-funded HousePool                      |
| **LP risk allocation**    | Senior and Junior tranches                 |

The oracle supplies the market price. The HousePool supplies the balance sheet behind it.

### One market, two directions

The Plether Dollar Index begins with a normalized basket of six foreign currencies priced in US dollars.

The raw basket therefore moves inversely to dollar strength:

* When USD strengthens, the raw basket falls.
* When USD weakens, the raw basket rises.

For the trading interface, Plether converts that raw basket into a dollar-oriented price:

```
Displayed dollar price
= 2.00 − bounded raw basket
```

This makes LONG and SHORT behave conventionally in the application:

| Position      | Raw basket | Displayed dollar price | Trader benefits |
| ------------- | ---------- | ---------------------- | --------------- |
| **LONG USD**  | Falls      | Rises                  | USD strengthens |
| **SHORT USD** | Rises      | Falls                  | USD weakens     |

The contracts account against the raw basket. The interface displays its fixed complement. Both describe the same economic position.

For the basket construction itself, see [Understanding the Plether Dollar Index](../welcome/understanding-the-plether-dollar-index.md).

### Not an order book

Plether does not match bids and asks.

A LONG USD trader does not need to wait for another trader to open an equivalent SHORT USD position. There is no requirement for directional open interest to balance.

That means Plether has:

* No order-book spread set by market makers
* No queue of resting limit orders
* No peer-to-peer position matching
* No local trade price produced by the last matched order

The Plether chart does not move because someone opens, closes or liquidates a position. It moves because the external FX feeds move.

A large oracle move can still liquidate many positions at once. But those liquidations do not mechanically sell into a Plether order book and push the next execution price farther.

### Not an AMM

The HousePool holds liquidity, but it is not an AMM.

Traders do not swap one asset for another through a reserve curve. LPs do not quote the index price, and opening a position does not remove a LONG or SHORT token from pool inventory.

The HousePool provides **settlement capacity**. It does not provide price discovery.

Trading can still change:

* Directional open interest
* Available LONG USD and SHORT USD capacity
* Pool liability
* Virtual price impact
* Carry
* LP withdrawal availability

Plether separates price discovery from risk pricing:

> The oracle supplies the market price. VPI, carry and capacity limits price the burden a position places on the pool.

### How an order becomes a position

A Plether order follows a commit-now, price-later process:

1. The trader deposits USDC into the Trading Account’s Margin Account.
2. The owner wallet authorizes a LONG USD or SHORT USD commitment, and Plether submits the eligible sponsored operation.
3. Margin and an execution reward are reserved.
4. The order enters the global FIFO queue.
5. Plether resolves the first eligible Pyth observation strictly after commitment.
6. During live or FAD-only execution, the protocol applies an adverse oracle-confidence adjustment. Frozen voluntary closes use the validated unshifted price and the separate frozen-close spread.
7. The order’s execution limit and risk checks are evaluated.
8. If valid, the engine records the position and locks its margin.

The order is binding once committed. The trader cannot cancel because the market moved, and the keeper cannot choose a later, more favourable oracle observation.

Virtual price impact is applied separately to the trade’s economics. It does not replace the oracle or become the new market price.

The full process is covered in [How orders execute](how-orders-execute.md).

### The HousePool is the economic counterparty

A Plether trader does not face an individual LP or an opposite-direction trader.

The **HousePool** is the economic counterparty to every position:

* When traders realize losses, collectible value enters pool economics.
* When traders realize profits, the pool funds those profits.
* When traders pay carry, realized carry becomes LP revenue.
* When a trader receives a VPI rebate, the pool funds it.
* When a trader pays positive VPI, the non-protocol portion strengthens the pool.

The HousePool is funded through the Senior and Junior LP vaults. Both tranches back the same market. One tranche does not back LONG USD while the other backs SHORT USD.

Their role is to decide how pool returns and losses are allocated:

* Junior absorbs losses first and receives residual upside.
* Senior receives a Junior-funded target coupon and absorbs losses after Junior is exhausted.

The HousePool is not an emergency insurance fund added behind another counterparty. It is the primary balance sheet underwriting trader settlement.

### Why the settlement range is fixed

For execution and PnL accounting, Plether uses a fixed settlement range:

```
0.00 ≤ raw basket ≤ 2.00
```

If the external basket observation exceeds 2.00, Plether uses 2.00. The lower boundary is zero.

This creates finite endpoints for both directions:

| Position      | Maximum modeled profit                                      |
| ------------- | ----------------------------------------------------------- |
| **LONG USD**  | Raw basket approaches 0.00; displayed price approaches 2.00 |
| **SHORT USD** | Raw basket reaches 2.00; displayed price reaches 0.00       |

Because those endpoints are known, Plether can calculate every position’s maximum modeled profit before accepting it.

At the market level, the engine tracks the aggregate maximum-profit envelope for each direction. In simplified form, new exposure is accepted only while:

```
Effective HousePool backing after the trade
≥ max(
    aggregate LONG USD maximum profit,
    aggregate SHORT USD maximum profit
  )
```

Effective backing accounts for physical pool assets and existing trader-claim liabilities.

If the post-trade liability cannot be supported, the order is rejected. The protocol does not accept unlimited exposure and hope that enough losing traders appear later.

> **Solvency before volume.**
>
> Plether accepts new exposure only while its bounded liability remains supportable.

#### What the boundary does not mean

The 2.00 settlement ceiling is not:

* A market forecast
* A claim that external FX relationships cannot move farther
* A trader stop-loss
* An automatic position close
* A guarantee that every profit is paid immediately
* A guarantee that LP principal cannot decline
* A guarantee that no bad debt can occur

If the external basket moves beyond the settlement range, Plether PnL stops extending beyond the boundary. This creates basis risk relative to the unrestricted external FX market.

The boundary makes liability measurable. It does not make risk disappear.

### Margin is collateral, not the purchase price

Opening a leveraged position does not transfer a large notional loan to the trader.

Instead, Plether records market exposure and locks USDC collateral against it. Leverage describes the relationship between that exposure and the collateral supporting it.

A trader account separates USDC into several states:

| Account balance              | Purpose                                       |
| ---------------------------- | --------------------------------------------- |
| **Free settlement balance**  | Available for new orders, costs or withdrawal |
| **Position margin**          | Assigned to the live position                 |
| **Committed-order margin**   | Reserved for a pending order                  |
| **Execution-reward reserve** | Reserved for whoever finalizes the order      |

Reserved balances are not free buying power.

Although the interface displays margin assigned to the position, Plether uses account-level collateral. Free USDC inside the same Plether account can contribute to position health.

This means a trader’s loss is not necessarily limited to the margin initially assigned on the trade ticket. A full close or liquidation can consume other economically reachable USDC held inside the Plether account.

Plether cannot debit assets sitting outside the protocol in the trader’s wallet.

### How value moves through the system

The simplified value flow is:

| Event                 | Trader side                                      | HousePool side                                    |
| --------------------- | ------------------------------------------------ | ------------------------------------------------- |
| **Deposit margin**    | USDC enters the Trading Account’s Margin Account | No change                                         |
| **Commit order**      | Margin and execution reward are reserved         | No change                                         |
| **Open position**     | Position margin is locked and trade costs settle | Pool assumes a bounded payout liability           |
| **Price movement**    | Unrealized PnL changes                           | Liability views change; no cash necessarily moves |
| **Losing close**      | Reachable trader USDC is collected               | Realized value enters pool economics              |
| **Profitable close**  | Margin is released and profit is credited        | Pool funds the profit or records a trader claim   |
| **Carry realization** | Carry is collected from reachable collateral     | Realized carry becomes LP revenue                 |
| **LP deposit**        | No change to trader margin                       | USDC enters through a tranche vault               |

Protocol execution fees belong to the treasury rather than LPs. Order-execution rewards are funded from trader collateral rather than HousePool capital.

### Unrealized PnL is not cash

Plether distinguishes between a mathematical gain or loss and USDC that has physically moved.

For conservative pool accounting:

* Unrealized trader profits are treated as liabilities.
* Unrealized trader losses are not treated as spendable LP assets.
* A trader loss becomes pool value only when it is physically collected.
* A trader profit becomes a margin credit or an explicit trader claim.
* An uncovered realized loss becomes bad debt.

This avoids treating money owed by a losing trader as if the pool already possessed it.

> **A number on a ledger is not cash in a contract. Plether accounts for the difference.**

### When a trader closes at a profit

A profitable close accounts for:

* Realized directional PnL
* VPI
* Execution fee
* Accrued carry
* Released position margin

If sufficient unreserved HousePool cash is available, the net result is credited to the Trading Account’s Margin Account.

It is not sent directly to the wallet. The trader withdraws separately.

If the pool cannot fund the profit immediately, the position can still close. The unpaid amount becomes a **trader claim** owned by the Trading Account.

The claim:

* Remains a senior HousePool liability
* Is excluded from LP-withdrawable value
* Is not placed in a FIFO queue
* Requires authorization from the Trading Account’s owner wallet
* Settles through an eligible sponsored operation into the Margin Account
* Requires aggregate trader claims to be fully cash-covered before settlement

Bounded solvency and immediate liquidity are different questions. A liability can be fully recorded even when it cannot yet be paid out as free USDC.

### When a trader closes at a loss

Plether collects the loss from physically reachable collateral inside the trader’s account.

A partial close cannot externalize an uncovered loss to LPs while leaving the remainder of the position protected. If the partial close cannot leave the account valid, it is rejected.

A full close can consume the terminally reachable collateral defined by the protocol. Any remaining shortfall becomes explicit bad debt and is absorbed by LP capital through the tranche waterfall.

### Liquidation and counterparty auto-deleveraging

Plether does not use **counterparty auto-deleveraging**.

A profitable trader’s position is not forcibly reduced simply because another trader is losing or the pool is under stress.

Instead, Plether uses:

* Bounded maximum payouts
* Pre-trade solvency checks
* Directional capacity limits
* VPI and carry
* Conservative LP withdrawal reserves
* Trader claims
* Explicit bad-debt accounting
* Degraded-mode containment
* Recapitalization when required

No counterparty auto-deleveraging does not mean no liquidation.

If a trader’s own account equity falls to or below the active maintenance requirement, the complete position can be liquidated. Plether liquidations are full rather than partial.

The protocol can also apply higher margin requirements around FX-market closures.

### How Plether prices imbalance

Because traders are not matched against one another, LONG USD and SHORT USD open interest can diverge.

Plether manages that imbalance through three mechanisms.

#### Capacity

The protocol limits how much additional liability the HousePool may accept in each direction. An order can be rejected even when the trader has sufficient margin if the pool cannot safely underwrite it.

#### Virtual price impact

VPI is a one-time charge or rebate based on factors including:

* Trade direction
* Current market skew
* Trade size
* Available pool depth
* The protocol’s VPI factor

VPI affects the economics of the trade. It does not set the oracle index or move the chart price.

#### Carry

Carry is the ongoing cost of using LP capital to support a position’s bounded payout.

It is not a payment transferred from LONG USD traders to SHORT USD traders, or vice versa. Both directions can pay carry at the same time when both use HousePool capital.

Realized carry becomes LP revenue.

### How Plether differs from common perpetual markets

Perpetual designs vary, but the main structural differences are:

|                              | Plether                                                       | Common perpetual model                                          |
| ---------------------------- | ------------------------------------------------------------- | --------------------------------------------------------------- |
| **Reference price**          | Six-currency external FX basket                               | Exchange index, order book or AMM reference                     |
| **Counterparty**             | Tranched USDC HousePool                                       | Other traders, market makers or pooled AMM liquidity            |
| **Execution**                | Delayed, binding FIFO orders                                  | Commonly immediate matching or pool execution                   |
| **Price discovery**          | External oracle                                               | Often influenced by venue trading                               |
| **Price impact**             | Separate VPI adjustment                                       | Often produced by order-book or AMM liquidity                   |
| **Settlement range**         | Fixed between 0.00 and 2.00                                   | Often not bounded by an equivalent protocol-wide range          |
| **Ongoing cost**             | Carry paid for LP-backed capital                              | Commonly side-to-side funding                                   |
| **Net directional exposure** | HousePool can warehouse imbalance within limits               | Often balanced through traders or market makers                 |
| **Stress handling**          | Capacity limits, LP waterfall, claims and bad-debt accounting | May use insurance funds, socialized losses or auto-deleveraging |
| **Liquidation execution**    | Settles against an external bounded mark                      | May require selling into an order book or AMM                   |

### The shortest useful mental model

Plether separates three things:

1. **The oracle defines the market.**
2. **The delayed queue defines execution.**
3. **The HousePool underwrites the result.**

Traders post USDC margin and choose LONG USD or SHORT USD. The protocol calculates the position’s maximum possible profit inside the fixed settlement range. It accepts the order only if the HousePool can support the resulting liability.

When the position settles, value moves between the Trading Account’s Margin Account and the HousePool. Junior and Senior LP capital determine how the pool absorbs the outcome.

That is the Plether market model: **oracle-priced, margin-backed and bounded by design.**

### Continue reading

Next: [How orders execute](how-orders-execute.md)
