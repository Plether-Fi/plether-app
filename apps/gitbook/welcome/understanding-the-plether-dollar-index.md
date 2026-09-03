# Understanding the Plether Dollar Index

> **The dollar has no standalone price. It only has exchange rates.**
>
> Plether combines six of them into one onchain market for taking a view on USD strength.

The Plether Dollar Index measures the dollar against a basket of major currencies.

Instead of trading six FX[^fx] pairs separately, traders can express one position:

* **LONG USD** when they expect the dollar to strengthen.
* **SHORT USD** when they expect the dollar to weaken.

The index is DXY-inspired[^dxy], but it is not raw DXY. Plether uses the familiar six-currency composition and starting coefficients, then calculates its own normalized basket from Pyth price feeds.

Same currencies. Different math.

### What is inside the basket?

The current basket contains:

| Currency        | Reference coefficient |
| --------------- | --------------------- |
| Euro            | 57.6%                 |
| Japanese yen    | 13.6%                 |
| British pound   | 11.9%                 |
| Canadian dollar | 9.1%                  |
| Swedish krona   | 4.2%                  |
| Swiss franc     | 3.6%                  |

The coefficients total 100%.

The euro has the largest influence at the basket’s reference level. If EUR moves while every other component remains unchanged, the basket initially responds more than it would to an equal percentage move in SEK or CHF.

These are **reference coefficients**, not permanently fixed effective weights. As currencies move away from their reference rates, their actual contribution to the basket changes.

This creates broad dollar exposure. It is not a perfect hedge against any single currency. Someone whose expenses are entirely in euros will still retain EUR-specific risk after hedging the wider basket.

### DXY-inspired, not raw DXY

Plether does not wrap an offchain DXY future or reproduce an external dollar index price tick for tick.

Plether differs in several ways:

* It constructs the basket from six individual Pyth FX feeds.
* Every component is normalized against a configured reference rate.
* It uses an arithmetic calculation rather than a geometric one.
* It does not automatically rebalance effective currency weights.
* The resulting level is native to Plether rather than expressed in traditional index points.
* The settlement price is bounded for protocol accounting.

The dollar-oriented Plether level shown by the interface and external dollar indices can move in the same general direction while producing different percentage returns. The protocol’s raw foreign-currency basket moves in the opposite direction, as described below.

That difference is **basis risk[^basis-risk]**. Anyone using Plether as a hedge should account for it.

### Same currencies. Different math.

Traditional DXY methodology uses a weighted geometric calculation.

Plether uses a weighted arithmetic mean of normalized FX rates.

The difference is not cosmetic. It determines how each currency contributes to the index and how that contribution changes over time.

#### Plether’s arithmetic calculation

Plether first converts every component into the value of one unit of foreign currency in U.S. dollars. Feeds quoted in the opposite direction are inverted.

It then calculates a normalized price relative for each currency:

`Normalized rate = Current FX rate ÷ Reference FX rate`

Each normalized rate is multiplied by its reference coefficient and added to the basket:

`Raw basket mark = Σ(Reference coefficient × Normalized rate)`

When every component equals its reference rate, every normalized rate equals 1.00 and the complete basket equals 1.00.

The resulting basket level is an accounting reference. It is not the dollar price of a token.

#### Geometric calculation

A weighted geometric index uses multiplication rather than addition:

`Geometric index = ∏(Normalized rate ^ Reference weight)`

In a geometric formula, each component retains constant percentage sensitivity to the index:

* A 1% EUR move has approximately 57.6% of the index-level effect.
* That percentage sensitivity remains tied to the formula weight.
* Component moves compound multiplicatively.

Plether’s arithmetic basket behaves differently. Its configured coefficients stay fixed, but each currency’s effective influence changes as its normalized value changes.

| Property              | Plether arithmetic basket               | Geometric basket                                       |
| --------------------- | --------------------------------------- | ------------------------------------------------------ |
| Calculation           | Weighted sum                            | Weighted product                                       |
| Reference level       | 1.00                                    | 1.00                                                   |
| Component influence   | Drifts as normalized rates diverge      | Percentage sensitivity remains tied to formula weights |
| Automatic reset       | None                                    | Constant-weight sensitivity is built into the formula  |
| Large divergent moves | Outperforming components gain influence | Moves compound using fixed exponents                   |
| Component-to-index relationship | Linear in normalized component levels  | Nonlinear and multiplicative                           |

The two calculations remain close when currency moves are small. They can diverge materially over longer periods or during large relative moves.

### Why effective weights drift

The configured coefficients remain fixed inside the deployed oracle[^oracle]. EUR remains configured at 57.6%, JPY at 13.6%, and so on.

Their **effective weights**, however, can change.

For each currency:

`Effective weight = Component contribution ÷ Current basket mark`

A currency that appreciates relative to its reference rate becomes a larger part of the arithmetic basket. A currency that depreciates becomes a smaller part.

Plether does not periodically reset those effective weights to their starting composition.

There are no underlying currencies being bought or sold. “Rebalancing” here refers to resetting the index formula’s effective composition—not trading a portfolio of FX assets.

Plether performs no such automatic reset.

#### Example: EUR appreciates by 10%

Assume EUR rises 10% from its reference rate while every other component remains unchanged.

Its contribution becomes:

`57.6% × 1.10 = 63.36%`

The complete arithmetic basket becomes:

`63.36% + 42.4% = 1.0576`

EUR’s new effective weight is therefore:

`0.6336 ÷ 1.0576 ≈ 59.9%`

The configured EUR coefficient remains 57.6%, but its effective contribution has drifted to approximately 59.9%.

Using a geometric calculation, the same isolated EUR move would produce:

`1.10 ^ 57.6% ≈ 1.0564`

Plether’s arithmetic result is:

`1.0576`

The difference is small after a 10% move. It can grow as component prices move farther apart.

If EUR continues outperforming the rest of the basket, its effective weight continues increasing. Plether does not automatically rebalance it back to 57.6%.

This drift is a property of the index methodology, not an implementation error.

### Why the raw basket moves opposite to the dollar

The oracle measures the value of foreign currencies in dollars.

That creates an inverse relationship:

* When foreign currencies strengthen against USD, the raw basket rises.
* When USD strengthens against foreign currencies, the raw basket falls.

Plether names positions from the trader’s economic exposure, not from the direction of the raw basket number.

| Dollar move     | Raw basket move | Position that benefits |
| --------------- | --------------- | ---------------------- |
| USD strengthens | Basket falls    | **LONG USD**           |
| USD weakens     | Basket rises    | **SHORT USD**          |

**LONG USD does not mean long the raw basket. It means long the dollar.**

This is the central relationship behind every Plether position.

The interface makes that relationship conventional by displaying the fixed complement of the bounded raw basket:

`Displayed dollar index = 2.00 − bounded raw basket`

| Dollar move     | Raw basket move | Displayed index move | Position that benefits |
| --------------- | --------------- | -------------------- | ---------------------- |
| USD strengthens | Basket falls    | Displayed index rises | **LONG USD**           |
| USD weakens     | Basket rises    | Displayed index falls | **SHORT USD**          |

The contracts account against the raw basket; the interface displays its dollar-oriented complement. Both representations describe the same position.

### Two simple position examples

#### The dollar strengthens

Suppose the raw basket moves from **1.00 to 0.96**.

Foreign currencies have lost 4% against USD on a weighted basis.

* **LONG USD** profits.
* **SHORT USD** loses.

For a position of 10,000 index units—approximately 10,000 USDC[^usdc] of market exposure at a 1.00 entry—the directional PnL[^pnl] is approximately **400 USDC** before fees, carry[^carry], virtual price impact and oracle adjustments.

#### The dollar weakens

Suppose the raw basket moves from **1.00 to 1.03**.

Foreign currencies have gained 3% against USD on a weighted basis.

* **SHORT USD** profits.
* **LONG USD** loses.

Using the same 10,000-unit position, the directional PnL is approximately **300 USDC** before costs.

These examples isolate movement in the basket. Actual settlement also reflects execution price, fees, carry and virtual price impact.

### How Pyth prices the market

Pyth supplies the six underlying FX prices.

It does not supply a ready-made Plether index and does not determine the basket composition.

When Plether processes an oracle update, it:

1. Converts every component into the same quote direction.
2. Normalizes each price to the same decimal precision.
3. Confirms that every price is positive and sufficiently recent.
4. Checks that the component publication times are close enough together.
5. Rejects feeds whose reported uncertainty is too wide.
6. Calculates the weighted arithmetic basket.
7. Produces one basket price, confidence value and publication time.

Pyth publishes a confidence range alongside every component price. Plether combines those values into a basket-level measure of uncertainty.

During live markets, including the market-close runway before the oracle freezes, execution is adjusted toward the side adverse to the trader:

* A LONG USD open receives a conservative entry.
* A SHORT USD open receives a conservative entry.
* Closing either position uses the corresponding conservative exit.

Confidence is not an additional fee. It is a risk control.

If uncertainty exceeds the configured limit, the protocol rejects the price instead of pretending it is precise.

Oracle-frozen voluntary closes are the exception to the adverse price shift: they retain confidence-width validation, use the validated unshifted basket price and are assessed the separate fixed frozen-close spread. A terminal full close can waive any uncollectible portion of that spread. Liquidations retain their own adverse-confidence policy.

During live markets, delayed orders use the first eligible basket update published after commitment. The keeper[^keeper] cannot choose a later, more favorable update.

### The mark and execution economics can differ

The raw basket mark is the normalized value produced by the oracle.

A trader’s execution and final USDC economics can differ from that reference because of:

* The active oracle-confidence policy
* Virtual price impact
* Whether the eligible oracle price satisfies the trader’s acceptable-price limit
* The protocol execution fee
* The execution reward

These are separate controls and economic effects. A charted index level should not be interpreted as a guaranteed execution price.

### Why the index is bounded

Plether clamps the raw basket to a settlement mark between **0.00 and 2.00**.

`Settlement mark = min(Raw oracle basket, 2.00)`

If the oracle basket rises above 2.00, Plether continues using 2.00 for execution and PnL accounting.

The 2.00 cap is fixed in the protocol. It cannot be changed through governance.

#### Why 2.00?

The basket is normalized around a reference level of **1.00**. Setting the upper boundary at 2.00 creates a symmetric settlement range around that starting point:

* **LONG USD** reaches its maximum gross directional profit when the raw basket falls toward 0.00.
* **SHORT USD** reaches its maximum gross directional profit when the raw basket rises toward 2.00.

For a position opened at 1.00, either direction has a maximum favorable price movement of 1.00 per index unit.

Positions opened away from 1.00 use the remaining distance to their corresponding boundary:

* LONG USD maximum: entry mark minus 0.00
* SHORT USD maximum: 2.00 minus entry mark

#### What the cap does

The cap makes every position’s maximum gross directional payout calculable.

Before accepting additional exposure, Plether can determine the largest amount the liquidity pool could owe if the market moves fully in the trader’s favor. If the pool cannot support that obligation, the trade is rejected.

This is the foundation of Plether’s bounded-liability model.

#### What the cap does not do

The 2.00 cap is not:

* A market forecast
* A claim that FX rates cannot move farther
* A trader stop-loss
* Protection from liquidation
* A guarantee of immediate profit settlement
* A guarantee that LP[^lp] capital cannot be impaired

It is a settlement and solvency boundary.

#### Basis risk above 2.00

The external currency basket can theoretically move above 2.00. Plether’s settlement mark cannot.

If that happens:

* SHORT USD stops accumulating additional directional profit above 2.00.
* LONG USD stops accumulating additional directional loss above 2.00.
* Plether’s return diverges from the unbounded external basket.

That divergence is part of the product’s basis risk. It is the trade-off that makes maximum liability knowable before a position is accepted.

The cap is a protocol constant rather than a live risk parameter. It defines the product and cannot be changed through ordinary parameter updates.

### Why the market follows FX hours

Crypto trades continuously. Global FX does not.

Plether does not treat a stale weekend quote as a live market. New exposure is restricted as the FX market approaches closure and remains blocked while the oracle is frozen.

Risk-reducing closes and liquidations can remain available under separate conservative rules.

Carry continues accruing while the market is closed because existing positions continue using LP-backed capital.

Scheduled holidays and exceptional market closures can also move the protocol into its market-close or frozen-oracle state.

The exact schedule and available actions are documented under **Market hours and closures**.

### Why LPs should understand the index

Liquidity providers do not choose LONG USD or SHORT USD. The liquidity pool backs the aggregate positions opened by traders.

The direction and size of those positions determine which dollar moves create liabilities for the pool:

* A market skewed toward LONG USD owes more if the dollar strengthens.
* A market skewed toward SHORT USD owes more if the dollar weakens.
* A balanced market can still owe profitable traders on either side.

Effective currency-weight drift also changes which underlying FX moves have the greatest influence on future trader PnL.

The bounded index lets Plether calculate the maximum directional obligation. It does not prevent LP losses.

### Can the basket be changed or rebalanced?

There is no automatic rebalancing schedule.

Inside each deployed oracle, the following are fixed:

* Currency feed identifiers
* Reference coefficients
* Reference FX rates
* Quote-direction settings

These values cannot be edited in place.

Governance can deploy a new oracle and move the protocol to it through a timelocked configuration change. A new oracle could introduce different feeds, coefficients or reference rates.

That would be a disclosed methodology change—not routine automatic rebalancing.

Users should rely on the active oracle configuration and deployment reference rather than assume that every future Plether deployment uses identical parameters.

### Remember

1. The index measures USD relative to six currencies.
2. It is DXY-inspired, not a raw DXY tracker.
3. Plether uses an arithmetic calculation rather than a geometric one.
4. Reference coefficients remain fixed, but effective weights drift.
5. The basket is not automatically rebalanced.
6. **LONG USD benefits when the raw basket falls.**
7. **SHORT USD benefits when the raw basket rises.**
8. Pyth supplies the component prices; Plether calculates and bounds the basket.
9. The basket provides broad dollar exposure, not an exact hedge for every local currency.

[^fx]: Foreign exchange, the market for trading one currency against another.
[^dxy]: The U.S. Dollar Index; Plether uses its six-currency composition as inspiration but does not track raw DXY.
[^basis-risk]: The risk that a hedge and the exposure it is intended to offset do not move together.
[^oracle]: A service that supplies external market data to smart contracts; Plether uses Pyth price feeds.
[^pnl]: Profit and loss, the financial result of market-price movement on a position.
[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^keeper]: A permissionless actor or bot that submits order-finalization or protocol-maintenance transactions.
[^lp]: Liquidity provider, a participant that supplies USDC capital to the liquidity pool.
