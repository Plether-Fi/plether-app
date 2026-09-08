# AI trading agents

> **On Plether, an agent's edge has to come from being right about the dollar. Being fast, well-connected or first in line buys nothing.**

Most trading venues are quietly hostile to autonomous software. The durable edges are latency edges, so a strategy that reasons for ten seconds loses to one that reacts in ten milliseconds. Execution quality depends on an operator you cannot inspect. Access runs through an API key that can be throttled or revoked. And when something goes wrong, the only record of what your agent actually did is its own logs.

Plether removes each of those problems at the protocol level. Humans and agents trade under identical rules; there is no agent API and no agent tier. What the protocol offers instead is a market whose mechanics an agent can fully reason about: where prices come from, what execution will cost, what the worst case is, and how to prove afterwards what happened.

### There is no speed game to lose

Plether's dollar index is derived from external Pyth FX data. Trading changes your position and the pool's exposure; it never moves the reference price. There is no local reserve curve or order book, which means there is also no stale quote to snipe, no resting order to pick off, and no price to push around a victim in a sandwich.

Execution closes the remaining gap. Orders commit first and price later: once committed, an order is binding, sits in a first-in-first-out queue, and executes at the first eligible oracle observation published after commitment. Keepers must prove they did not skip an observation, and a trader cannot watch the next price land and cancel only the commitments that turned unfavourable. Nobody on either side of the trade gets to see the price and then decide. [How orders execute](../how-plether-works/how-orders-execute.md) walks through the full policy, including validated oracle confidence and the separate frozen-market rules.

For a model-driven agent this is the whole ballgame. The market can still move against you between commitment and execution — that is ordinary market risk, and you should size for it. What cannot happen is another participant using superior speed to take the difference from you at execution time. An agent that spends thirty seconds thinking competes on the quality of its forecast, not on the quality of its network connection.

The same separation holds during stress. Closing a liquidated position does not mechanically push the next account's reference price toward liquidation, so Plether's liquidations do not feed a local forced-selling spiral. A large external FX move can still make many accounts liquidatable at once; the point is that the venue itself does not amplify it.

### The counterparty is a pool with published rules

You do not need an opposing trader to open a position. The USDC liquidity pool is the counterparty to every trade: collected trader losses flow into it, and it funds trader gains. LPs supply that capital through Senior and Junior tranches and are paid for underwriting it — carry accrues on LP-financed exposure over time, and Virtual Price Impact (VPI) charges trades that deepen the pool's directional imbalance while rebating trades that reduce it. Keepers earn explicit execution rewards; the protocol takes explicit fees. Every cost your agent will pay is one of those named items, and each is calculable from onchain state.

Carry deserves a direct comparison, because it replaces the cost that is hardest to model everywhere else. On a conventional perps venue, holding a position means paying — or hoping to receive — a funding rate that swings with crowd positioning and premium, flips sign without warning, and can spike exactly when your trade is most crowded. Forecasting it means forecasting other traders. Plether has no trader-to-trader funding at all. Carry is a time-based charge on the portion of your position financed by LP capital, it varies only with pool utilization, and it is always a cost — never a payment your strategy depends on receiving. An agent can read utilization onchain and project its holding cost over any horizon before committing, instead of carrying a funding-rate model as a second source of error.

The pool's obligations are bounded by construction. Settlement prices stay within a fixed 0.00–2.00 range, so every position has a calculable maximum payout, and the protocol measures its aggregate worst-case liability before accepting more. An open or increase that would leave the pool unable to cover the resulting liability plus a settlement buffer is rejected. Capacity is therefore not a vague liquidity estimate your agent has to guess at — it is a concrete number to read and size against.

Two consequences matter for strategy design:

* **No counterparty auto-deleveraging.** Plether never forcibly trims an unrelated profitable position to cover another trader's loss. A hedge you put on stays on unless your own account fails its margin requirement. That makes PnL modelable in a way venues with ADL cannot offer.
* **Solvency is one read away.** Because payouts are bounded, the pool's health reduces to a single comparison: effective backing against worst-case directional liability plus the settlement buffer. Your agent can monitor the same O(1) condition the protocol checks before accepting risk — no reconstructing counterparty exposure from thousands of open positions, the way assessing an opaque exchange or a mutualized insurance fund would require.

One cost deserves special attention because agents keep trying to farm it. VPI can rebate a trade that improves the pool's balance, but a lifetime rule clamps every exposure to `charges − rebates ≥ 0`: if a position paid 30 USDC of VPI on the way in, its closing rebate is capped at 30 USDC no matter what the formula would otherwise pay. Reducing imbalance can zero out your VPI costs; it cannot become an income stream. If your agent wants the pool's liquidity economics, it commits capital as an LP. The full cost model is in [Trading costs: fees, carry and VPI](../how-plether-works/trading-costs-fees-carry-and-vpi.md).

### The protocol enforces the mandate, not the model

A forecast can be uncertain. The authority you hand your agent does not have to be.

Every externally submitted order binds an explicit price boundary, a deadline, allowed execution regimes and the execution-critical configuration, plus financial limits covering debits, charges, execution notional, resulting position size, equity and leverage. The protocol checks those limits against authoritative onchain state immediately before applying the trade — not against whatever the agent believed when it submitted. Use a preview to choose the limits; if the market moves while the order waits in the queue, the limits still hold.

This inverts the usual trust model for autonomous trading. On a conventional venue, the code around the model is the only thing standing between a bad inference and a bad fill. Here, a hallucinated size or a stale price assumption produces a rejected order, because the mandate is enforced by the contract, not by the agent's own guardrails.

Per-order limits compose with account-level controls. A smart account or session policy decides *who* may act — which contracts, cumulative exposure, withdrawals, expiry, revocation — while the order decides *what financial result is allowed*. An agent permitted to trade need not be capable of moving the owner's capital anywhere else. Take-profit and stop-loss protection runs through a separate surface with its own execution envelope; authorize it separately from ordinary orders.

### Built for software that crashes and retries

Agents lose network connections, restart mid-submission, and double-fire. Plether's order surface assumes this. Every submitted intent gets a permanent, account-scoped identity: replaying the exact same request returns the existing order rather than opening a second position, and reusing that identity for a different request is rejected. An agent recovering from an uncertain submission can retry blindly without duplicating exposure.

Afterwards, the record is independent of the agent. The lifecycle book distinguishes pending, executed and failed orders, and canonical events with authenticated receipt hashes prove both the authorized intent and the terminal result. An operator can reconcile everything the agent did without trusting its memory or its logs — which is the difference between running an agent on your own capital and being able to run one on someone else's.

Access cannot be taken away, either. Execution is permissionless: there is no API key to revoke, no account review, no rate limit tied to your standing with an operator. Use shared keepers, run your own executor, or both — swapping executors changes nothing about an order's financial limits. Keepers still need valid oracle data, gas and transaction inclusion, so delays and a blocked queue head remain part of the timing model.

### What to build

The market is deliberately narrow: USDC-margined, USDC-settled exposure to the dollar against a six-currency basket, LONG or SHORT.

That suits a few shapes well. A systematic macro agent translates a dollar-strength model into sized exposure, pricing carry, VPI and execution costs into its expected result. A hedging agent uses SHORT USD to partially offset weakening-dollar risk in a dollar-heavy portfolio, accepting that a bounded basket index is not a substitute for any specific currency pair. A risk-management agent watches position health, utilization, carry and market-close rules, and adjusts within its mandate.

All of them must be designed around the venue's honest constraints: orders are delayed and binding, and FX market hours bring close-only windows. Those belong in the strategy's evaluation from day one, not as exception handling bolted on later.

For index construction and direction conventions, read [Understanding the Plether Dollar Index](understanding-the-plether-dollar-index.md). For contract interfaces, financial bounds, account policies, order identity and receipt verification, continue with [Working with AI Agents](https://github.com/Plether-Fi/plether-core/blob/master/packages/perps/WORKING_WITH_AI_AGENTS.md).
