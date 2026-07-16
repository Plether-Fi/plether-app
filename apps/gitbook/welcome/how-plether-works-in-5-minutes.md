# How Plether works in 5 minutes

> **Traders take a view on the dollar. LPs underwrite it. Plether keeps the obligation measurable and accounts for everything in USDC.**

Plether is an oracle-priced perpetual market, not an order book.

Traders are not matched against other traders. They open **LONG USD** or **SHORT USD** positions against a USDC liquidity pool called the **HousePool**.

Four components connect them:

1. A currency basket measures the direction of the dollar.
2. Traders deposit USDC as margin.
3. Delayed orders determine when and how positions execute.
4. Senior and Junior LP capital backs trader payouts.

### 1. One market, two directions

Plether derives dollar exposure from six major currencies:

* Euro
* Japanese yen
* British pound
* Canadian dollar
* Swedish krona
* Swiss franc

Pyth supplies the underlying exchange rates. Plether combines them using fixed, DXY-inspired weights to produce a transparent basket-derived market.

It is not raw DXY, a wrapped futures contract, or a claim on an offchain index.

| Position      | Your view                  | Profits when                                 |
| ------------- | -------------------------- | -------------------------------------------- |
| **LONG USD**  | The dollar will strengthen | The dollar gains against the currency basket |
| **SHORT USD** | The dollar will weaken     | The dollar loses against the currency basket |

The raw oracle basket prices foreign currencies in dollars. It therefore falls when the dollar strengthens and rises when the dollar weakens. Plether expresses positions from the dollar’s perspective: LONG USD or SHORT USD.

The market price has a hard upper bound. This limits the maximum possible payout and makes the protocol’s worst-case obligation calculable before accepting a trade.

### 2. Traders deposit margin

Every position starts with USDC margin recorded under a Trading Account. The connected owner wallet authorizes that account’s actions.

Margin absorbs losses and determines how far the market can move against a position before it becomes liquidatable. Adding more margin reduces effective leverage and moves the liquidation threshold farther away.

Each Trading Account can hold one live direction at a time. A trader can:

* Open a position
* Increase its size
* Add margin
* Partially reduce it
* Close it completely

To switch from LONG USD to SHORT USD, or the other way around, the existing position must be closed first.

Positions have no scheduled expiry. They remain open until the trader closes them or their remaining equity is no longer sufficient.

### 3. Orders commit first and price later

Plether does not execute an order in the same operation in which it is committed.

Instead:

1. The owner wallet authorizes the Trading Account action, and Plether submits the eligible sponsored operation.
2. Required margin and an execution reward are reserved.
3. The order enters a global first-in, first-out queue.
4. A permissionless keeper supplies the required Pyth data.
5. While the FX market is live, execution uses the first eligible oracle update published after the order was committed.
6. The protocol applies the active confidence policy, VPI and the trader’s acceptable-price limit. Frozen voluntary closes waive the adverse confidence price shift and use the separate frozen-close spread.
7. The position executes or the order fails according to protocol rules.

The trader cannot cancel an order after commitment.

This prevents queued orders from becoming free options. It also reduces front-running and keeper price-selection risk: neither the trader nor the keeper can simply choose a later, more favorable price update.

Delayed execution is still execution risk. The market may move between commitment and settlement, and an order may fail because of slippage, expiry, missing oracle data, or protocol state.

### 4. What determines a position’s result?

A position has several separate economic components:

| Component           | What it does                                                             |
| ------------------- | ------------------------------------------------------------------------ |
| **Directional PnL** | Gains or loses value as the dollar moves relative to the basket          |
| **Margin**          | Supports the position and absorbs losses                                 |
| **Execution fee**   | Protocol fee charged when position size changes                          |
| **Price impact**    | Applies a separate USDC charge or rebate based on HousePool imbalance     |
| **Carry**           | Time-based cost for using LP-backed capital                              |
| **Execution reward** | Pays the account that processes the delayed order                        |

#### Price impact

Plether uses virtual price impact to respond to directional imbalance.

A trade that adds to the pool’s existing imbalance can pay a VPI charge. A trade that reduces that imbalance can receive a bounded VPI rebate during normal market conditions.

VPI changes the trade’s USDC economics. It does not change the oracle execution price recorded on the position or move the external oracle price.

#### Carry instead of funding

Plether does not transfer funding payments between LONG and SHORT traders.

Carry is charged on the portion of a position economically financed by LP capital. It varies with utilization and accrues continuously over time.

Both LONG USD and SHORT USD positions can pay carry simultaneously. Carry can also continue accruing while the FX oracle is stale or frozen.

When collected, realized carry becomes HousePool trading revenue.

#### Liquidation

When a position’s carry-adjusted equity falls to or below the applicable maintenance requirement, it becomes eligible for full liquidation.

Available collateral pays the trading loss and liquidation bounty. Any positive residual remains attributable to the trader. If collateral cannot cover the full loss, the shortfall is absorbed by the HousePool.

### 5. LPs provide the counterparty capital

The HousePool holds the USDC that backs trader payouts. LPs enter through two tranches with different positions in the loss waterfall.

| Tranche    | Return profile                                      | Loss position                                 |
| ---------- | --------------------------------------------------- | --------------------------------------------- |
| **Senior** | Targets a coupon funded from available Junior value | Losses reach Senior after Junior is exhausted |
| **Junior** | Receives residual upside after Senior obligations   | Absorbs losses first                          |

The Senior target is not fixed or guaranteed. Senior capital can still be impaired.

Junior takes more risk because it absorbs bad debt first. In return, it receives the residual economics after Senior has been accounted for.

**Liability is the product. Return is what LPs receive for underwriting it.**

LP entry and exit are not always immediate:

* While trader positions are open, ordinary deposits can enter through pending deposit epochs.
* Withdrawals are subject to a cooldown.
* Capital reserved for trader payouts or claims cannot be withdrawn.
* Available withdrawals depend on free pool cash, solvency, oracle state and protocol status.

### 6. Solvency is checked before exposure grows

The bounded market price lets Plether calculate the maximum aggregate payout for each direction.

Before accepting a trade that increases risk, the protocol compares physically backed HousePool assets—after existing senior claims—with the worst-case directional liability after the trade.

If the pool cannot support that obligation, the trade is rejected.

This is **solvency before volume**. It does not mean that LP principal is guaranteed or that bad debt is impossible. It means the protocol measures the bounded obligation before choosing to accept it.

Plether does not forcibly reduce unrelated profitable positions to cover another trader’s loss. There is no counterparty auto-deleveraging.

Released position margin follows separately. The complete fresh HousePool-funded payout is either credited immediately or, when sufficient cash is unavailable, recorded in full as a senior trader claim. Plether never splits one fresh payout between an immediate credit and a new claim. The claim remains an obligation of the pool and can later be settled into the Trading Account’s Margin Account when sufficient cash is available.

If a terminal settlement reveals insolvency, the protocol enters degraded mode. New risk is blocked while closes, liquidations and recapitalization remain available.

### 7. FX market hours affect available actions

The dollar market does not trade like crypto. Plether changes behavior around FX-market closures.

| State                   | What changes                                                                                                 |
| ----------------------- | ------------------------------------------------------------------------------------------------------------ |
| **Live market**         | Normal opens, increases, closes and liquidations                                                             |
| **Market-close runway** | New risk is blocked and a higher margin requirement applies; closes and liquidations continue                |
| **Oracle frozen**       | Opens remain blocked; eligible closes and liquidations use conservative frozen-market rules; carry continues |
| **Paused**              | New trader risk or LP deposits may be blocked while protective actions remain available                      |
| **Degraded**            | New risk and affected withdrawals are blocked; closes, liquidations and recapitalization continue            |

Frozen-market execution prioritizes risk reduction over normal live-price guarantees. It uses special pricing rules and an LP-protection surcharge.

### Five things to remember

1. **LONG and SHORT refer to the dollar**, not the raw currency basket.
2. **Orders are delayed, binding and non-cancellable.**
3. **The HousePool—not another trader—is the economic counterparty.**
4. **Carry pays for LP-backed exposure; it is not trader-to-trader funding.**
5. **Bounded liability makes risk measurable. It does not make trading or LPing risk-free.**

### Where to go next

* [**Understanding the Plether Dollar Index**](understanding-the-plether-dollar-index.md)
* [**Trader quickstart**](../trader-quickstart.md)
* [**Liquidity provider quickstart**](../liquidity-provider-quickstart.md)
* [**Fees, carry and price impact**](../how-plether-works/trading-costs-fees-carry-and-vpi.md)
* [**Margin and liquidation**](../how-plether-works/margin-leverage-and-liquidation.md)
* [**Market states and closures**](../how-plether-works/market-states-and-oracle-closures.md)
* [**Risk and security**](risks-you-should-understand-first.md)
