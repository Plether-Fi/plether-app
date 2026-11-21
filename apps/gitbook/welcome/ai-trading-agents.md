# AI trading agents

> **On Plether, an agent's edge has to come from being right about the dollar. Being fast, well-connected or first in line buys nothing.**

Most trading venues are quietly hostile to autonomous software. Durable edges are latency edges, execution quality depends on an opaque operator, access runs through revocable API keys, and the only audit trail is the agent's own logs.

Plether removes each of those barriers at the protocol level. Humans and agents trade under identical onchain rules — there is no gated agent tier. Instead, the protocol offers a market whose prices, costs, worst-case payouts and execution receipts an agent can reason about and verify directly onchain.

## There is no speed game to lose

Plether's dollar index (`plDXY`) comes from six external Pyth FX feeds. Trading changes your position and pool exposure; it never moves the reference price. With no order book or AMM reserve curve, there are no stale quotes to snipe, no resting limit orders to pick off and no sandwichable swap price.

Delayed global FIFO execution closes the remaining gap: orders commit first and settle at the first eligible post-commit Pyth observation inside the settlement window (`executionBlock > commitBlock`, `publishTime > commitTimestamp`). Keepers must prove they did not skip an earlier observation, and traders cannot cancel commitments after seeing the next tick. An agent that spends 30 seconds reasoning competes on forecast quality, not network latency. See [How orders execute](../how-plether-works/how-orders-execute.md).

The same separation holds under stress: liquidations settle against the external bounded mark rather than selling into a local order book, so Plether does not create a venue-driven forced-selling spiral.

## A counterparty with published, deterministic rules

Every trade faces the shared USDG liquidity pool backed by Senior and Junior LP tranches:

* **Explicit, calculable costs:** `0.04%` protocol execution fee, quadratic VPI (`vpiFactor = 0.01`), `0.01–0.20 USDG` opening execution reward (`0 USDG` on reductions and closes), `0.20 USDG` TP/SL trigger bounty, `0.10%` liquidation charge (`50%` keeper / `50%` LPs, min `1.00 USDG`) and `50 bps` frozen-close spread during `oracleFrozen`. Every parameter is readable onchain.
* **Carry instead of unpredictable peer-to-peer funding:** Conventional funding flips sign with crowd positioning. Plether's carry is a transparent utilization-scaled cost (`baseCarryBps = 5.00%` APR max × side utilization) paid on LP-financed payout capacity — always a cost, never a sign-flipping payment your model must forecast.
* **Bounded solvency (`0.00–2.00`) & no counterparty ADL:** Every position has a finite worst-case payout, and pre-trade admission checks `maxLiability + 0.25% buffer <= effective pool backing` in O(1). Profitable positions are never forcibly trimmed by another trader's default.
* **Lifetime VPI clamp:** Closing rebates can offset previously paid VPI on that position down to `0` (`charges − rebates >= 0`), preventing rebate-only extraction. Agents seeking LP economics supply capital through the Senior or Junior vaults instead. See [Trading costs: fees, carry and VPI](../how-plether-works/trading-costs-fees-carry-and-vpi.md).

## The protocol enforces the mandate, not the model

Every externally submitted Order V3 intent binds an explicit price boundary, submission deadline (`submitBy`), execution window (`executionWindowSeconds`), allowed market regimes and financial bounds covering debits, charges, execution notional, resulting position size, equity and leverage. The contract validates every limit against live onchain state right before applying the trade — so a hallucinated size or stale quote reverts or fails cleanly instead of producing a catastrophic fill.

Per-order bounds compose cleanly with smart-account or session-key policies: the account layer restricts *who* can call *which* contracts and prevents unauthorized withdrawals, while the order enforces *what financial outcome* is acceptable. Take-profit and stop-loss protection (`PositionProtectionBook`) runs on a separate surface and should be authorized independently.

## Built for software that crashes and retries

* **Idempotent order identity:** Every submitted intent carries a permanent, account-scoped identity. Replaying the exact same request returns the existing order without duplicating exposure, while reusing that identity with different parameters reverts.
* **Independent onchain audit trail:** `OrderLifecycleBook` tracks pending, executed and failed states with canonical events and authenticated receipt hashes, letting an operator reconcile an agent's history without trusting its local logs.
* **Permissionless execution:** No API keys to revoke or throttle. Rely on shared keepers, run your own executor (`plether-keeper`) or both — swapping executors never alters an order's financial bounds.

## Accessing Plether programmatically

Plether exposes read-only HTTP endpoints alongside direct onchain Arbitrum Sepolia contract reads and writes:

* **Direct onchain interaction (trading, LPing, keepers):** Autonomous agents submit transactions directly to the pinned Core v2.0.0 Arbitrum Sepolia contracts listed in [Deployments & live parameters](../reference/deployments-and-parameters.md) using a funded wallet (ETH for gas, plus Pyth update fees if self-finalizing).
* **Important scope note on gas sponsorship & faucet:** The gas-sponsored AA gateway (`/api/perps/v1/aa/rpc`) and the MockUSDG faucet are **web-application paths** subject to web-origin policy and per-account/per-IP limits. Programmatic agents should plan to fund their own wallet with Arbitrum Sepolia ETH and MockUSDG rather than depending on web sponsorship.
* **Read-only public Perps REST endpoints** (indexed state, candles, order tracking and cached Pyth payloads):
  * `GET /api/perps/basket/latest`, `/api/perps/basket/history`, `/api/perps/basket/candles`, `/api/perps/basket/candles/current` — current and historical `plDXY` basket observations and OHLC candles
  * `GET /api/perps/market/stats` — aggregate market metrics
  * `GET /api/perps/accounts/:address/orders`, `/api/perps/accounts/:address/activity`, `/api/perps/accounts/:address/protections` — indexed order history, account activity and TP/SL protections
  * `GET /api/perps/orders/:orderId/wait` and `/api/perps/orders/:orderId/reveal-payload` — long-poll order completion and eligible historical Pyth reveal payloads
  * `GET /api/perps/pyth/cached-latest` and `/api/perps/pyth/update` — cached Pyth payloads
  * `GET /api/perps/vaults/history` and `/api/perps/vaults/activity` — LP vault share-price series and activity

All indexed REST responses are convenience views — always verify balances, positions, capacity and receipts directly against onchain contracts before committing capital.

## What to build

Plether's focused USDG-settled six-currency dollar basket fits systematic macro agents, automated USD-exposure hedging agents (accounting for basket basis risk) and risk-management agents monitoring health, carry utilization and weekly FX close-only windows. Design around delayed binding execution and Friday 16:30 – Sunday 17:15 America/New_York close-only schedules from day one.

Continue with [Understanding the Plether Dollar Index](understanding-the-plether-dollar-index.md), [Keepers and permissionless execution](../how-plether-works/keepers-and-permissionless-execution.md) and the contract-level integration spec in [Working with AI Agents](https://github.com/Plether-Fi/plether-core/blob/master/packages/perps/WORKING_WITH_AI_AGENTS.md).
