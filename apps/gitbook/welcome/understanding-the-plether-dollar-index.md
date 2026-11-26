# Understanding the Plether Dollar Index

> **The dollar has no standalone price — only exchange rates.** Plether combines six major FX[^fx] pairs into one onchain index for trading USD strength (**LONG USD**) or weakness (**SHORT USD**).

The Plether Dollar Index (`plDXY`) is **DXY-inspired**[^dxy], not a raw tracker of ICE's DXY. It uses the same six currencies and starting weights, then calculates its own normalized, bounded arithmetic basket from six live Pyth feeds.

## What is inside the basket?

On the active Core v2.0.0 oracle (`PletherOracle`), each component is normalized to `foreign currency per USD` (`FX/USD`, inverting feeds quoted as `USD/FX`) against a fixed 8-decimal reference base price:

| Currency | Pyth feed | Inverted | Reference coefficient | Reference base price (`FX/USD`) |
| --- | --- | --- | --- | --- |
| **Euro (EUR)** | `EUR/USD` | No | `57.6%` | `1.17500000` |
| **Japanese yen (JPY)** | `USD/JPY` | Yes | `13.6%` | `0.00638000` |
| **British pound (GBP)** | `GBP/USD` | No | `11.9%` | `1.34480000` |
| **Canadian dollar (CAD)** | `USD/CAD` | Yes | `9.1%` | `0.72880000` |
| **Swedish krona (SEK)** | `USD/SEK` | Yes | `4.2%` | `0.10860000` |
| **Swiss franc (CHF)** | `USD/CHF` | Yes | `3.6%` | `1.26100000` |

Coefficients sum to `100.0%`. Because EUR starts at `57.6%`, euro moves have the largest initial effect. The basket expresses broad USD strength rather than a 1:1 hedge for any single currency.

## Arithmetic basket vs geometric DXY — and why weights drift

Traditional DXY uses a weighted **geometric** product (`∏(Normalized rate ^ weight)`), which keeps constant percentage sensitivity to each currency. Plether uses a weighted **arithmetic** sum of normalized rates:

```text
Normalized rate_i = Current FX/USD rate_i ÷ Reference base price_i
Raw basket mark   = Σ(Reference coefficient_i × Normalized rate_i)
```

At reference base prices, every normalized rate is `1.00` and `Raw basket mark = 1.00000000`.

| Property | Plether arithmetic basket | Traditional geometric DXY |
| --- | --- | --- |
| **Formula** | Weighted sum | Weighted product |
| **Starting level** | `1.00000000` | Traditional index points |
| **Effective weights** | Drift as components diverge | Fixed percentage sensitivity |
| **Auto-rebalancing** | None | Built into geometric exponents |
| **Settlement range** | Clamped to `0.00–2.00` | Unbounded |

### Why effective weights drift

Inside a deployed oracle[^oracle], the configured coefficients never change, but each currency's **effective weight** (`Component contribution ÷ Current raw basket`) shifts as rates move.

* **Example (EUR +10% alone):** EUR's contribution rises from `0.576` to `0.6336`. The raw arithmetic basket moves from `1.00` to `1.0576`, so EUR's effective weight becomes `0.6336 ÷ 1.0576 ≈ 59.9%` (versus `1.10^0.576 ≈ 1.0564` geometrically).
* Plether never automatically resets effective weights; the difference against raw DXY is part of the index's **basis risk**[^basis-risk].

## Why the raw basket moves opposite to the dollar

Because each component measures foreign currency priced in USD, the raw basket moves **inversely** to USD strength. Plether names positions by your economic view on the dollar, and the trading interface displays the dollar-oriented complement:

```text
Displayed plDXY price = 2.00 − bounded raw basket
```

| Dollar move | Raw basket move | Displayed `plDXY` price | Position that profits |
| --- | --- | --- | --- |
| **USD strengthens** | Falls | Rises | **LONG USD** |
| **USD weakens** | Rises | Falls | **SHORT USD** |

**LONG USD does not mean long the raw basket — it means long the dollar.** On a `10,000 plDXY` position entered at `1.00`:

* If the raw basket falls from `1.00` to `0.96` (displayed `plDXY` rises `1.00 → 1.04`), **LONG USD** gains `400 USDG`[^usdg] gross before fees, carry[^carry] and VPI.
* If the raw basket rises from `1.00` to `1.03` (displayed `plDXY` falls `1.00 → 0.97`), **SHORT USD** gains `300 USDG` gross before costs.

## How Pyth feeds become the index

Pyth supplies the six raw FX feeds (not a prebuilt Plether index). On every update, `PletherOracle`:

1. Normalizes all six feeds to 8-decimal `FX/USD` prices.
2. Verifies positive prices, freshness, cross-feed timestamp alignment (within `5 seconds`) and aggregate basket confidence (at most `0.10%` of the raw basket price).
3. Computes the weighted arithmetic basket, combined confidence interval and publication time.

During live and FAD-only sessions, Plether shifts the execution price against the trader by **`0.25×` the raw basket confidence** (waived on frozen-oracle voluntary closes, which pay the separate `50 bps` spread instead). See [Oracle & execution model](../how-plether-works/oracle-and-execution-model.md).

## The mark and execution economics can differ

The charted `plDXY` mark is the neutral oracle level. Your actual fill and USDG outcome can differ from that mark because of:

* The `0.25×` adverse oracle confidence shift (or `50 bps` frozen-close spread)
* Virtual Price Impact (VPI)
* Your acceptable-price limit (Execution limit)
* The `0.04%` protocol execution fee and any accrued carry
* Opening-order execution rewards (`0.01–0.20 USDG`; `0` on closes)

Never read a charted index level as a guaranteed fill price.

## Why settlement is bounded to `0.00–2.00`

Plether clamps the raw basket to **`0.00 <= Settlement mark <= 2.00`** (`CAP_PRICE = 2.00000000`, immutable in the contract):

* **LONG USD** max profit per unit = `entry raw basket − 0.00`
* **SHORT USD** max profit per unit = `2.00 − entry raw basket`

This finite cap makes every position's worst-case liability calculable before entry, letting the liquidity pool enforce strict solvency backing. However, the cap is **not** a market forecast, stop-loss or protection against liquidation — if external FX ever drives the raw basket above `2.00`, SHORT USD stops accruing additional profit and LONG USD stops accruing additional loss, creating basis risk relative to unrestricted spot FX.

## FX market hours, LPs and oracle governance

* **FX market schedule:** Global FX closes on weekends and holidays. Rather than trading on stale quotes, Plether switches to **FAD close-only** before Friday close and **`oracleFrozen`** while FX is shut (carry continues accruing). See [Market states and oracle closures](../how-plether-works/market-states-and-oracle-closures.md).
* **LP[^lp] exposure:** LPs back the opposite side of net trader skew across this exact index; effective weight drift also changes which underlying currency moves impact pool PnL[^pnl] most.
* **No silent edits:** Feed IDs, coefficients and base prices are immutable inside a deployed `PletherOracle`. Changing them requires deploying a new oracle contract and switching via a **`72-hour` timelocked** governance change. Verify active parameters in [Deployments & live parameters](../reference/deployments-and-parameters.md).

[^fx]: Foreign exchange, the market for trading one currency against another.
[^dxy]: The U.S. Dollar Index; Plether uses its six-currency composition as inspiration but does not track raw DXY.
[^oracle]: A service that supplies external market data to smart contracts; Plether uses Pyth price feeds.
[^basis-risk]: The risk that a hedge and the exposure it is intended to offset do not move together.
[^usdg]: USDG is used for margin and settlement. The testnet uses MockUSDG, a test token with no real-world value.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^lp]: Liquidity provider, a participant that supplies USDG capital to the liquidity pool.
[^pnl]: Profit and loss, the financial result of market-price movement on a position.
