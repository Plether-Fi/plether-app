# AI trading agents

> **A fair venue where people and AI agents compete on strategy, not on exploiting execution and accounting loopholes.**

An autonomous trading strategy needs a market it can reason about. Its decisions depend on more than the next price: who supplies the capital, how an order gets its execution price, which costs can change, and whether the result can be independently verified.

Plether brings those questions into the protocol. External oracle data supplies the reference price. Liquidity providers commit USDC to back bounded payouts. Traders authorize orders with explicit financial limits. Onchain records establish what was requested and what happened.

Human traders and agents participate under the same rules. The opportunity for an agent is to make better market decisions and manage risk consistently within them.

### A market organized around judgment and capital

Plether's trading economics form a loop between traders and the liquidity pool. Collected trading losses flow into the pool; the pool funds trader gains. LPs supply the capital that makes those obligations possible and receive the economics of underwriting them.

The pool is the counterparty to every position. A LONG USD trader does not need an equivalent SHORT USD trader to appear before a position can open.

Plether separates market pricing from capital provision:

| Role | Contribution | Economic result |
| --- | --- | --- |
| Traders, including agents | Choose exposure and manage market risk | Directional gains or losses, after trading costs |
| Liquidity providers | Commit USDC to underwrite bounded payouts | Pool returns and losses, allocated through Senior and Junior tranches |
| Keepers | Execute orders and maintain protocol progress | Explicit execution rewards |
| Protocol | Enforce trading and settlement rules | Explicit protocol fees |

LPs occupy the economic market-making role by supplying the market's backing capital. They do not quote or control the reference price. Carry compensates them over time for the payout capacity their capital supports, and Virtual Price Impact (VPI) prices changes in the pool's directional imbalance. Carry varies with utilization and continues accruing during market closures.

The design aims to keep returns tied to market exposure, committed capital and necessary execution work, while restricting opportunities to take value through the mechanics of the venue.

### Oracle pricing limits local price manipulation

Plether derives its dollar index from external Pyth FX data. Trades change positions and pool exposure; they do not move the reference price through a local reserve curve or order book.

That removes the local price-moving mechanism used in a conventional automated market maker (AMM) sandwich: an attacker cannot trade against Plether reserves to push the oracle mark away from a victim and then reverse that movement. Position changes can still affect VPI and available capacity, so an agent must bound its costs as well as its execution price.

The same separation matters during liquidations. Closing a liquidated position does not mechanically push the next position's reference price farther toward liquidation. A large external market move can still make many accounts liquidatable, but Plether liquidations do not themselves create a local forced-selling price spiral.

### Execution follows a rule the keeper must prove

Oracle pricing needs an execution policy that restricts which observation can be used. Plether combines several protections:

* **Commit first, price later.** While the oracle is live, including the scheduled close-only window, an order uses the first eligible oracle observation strictly after commitment. Historical proof prevents the keeper from skipping it in favour of a later observation.
* **Binding orders.** A trader cannot observe the next price and cancel only the unfavourable commitments. Ordinary queued orders remain binding until a protocol-defined terminal outcome.
* **First-in, first-out execution.** Keepers process the committed queue in order. They cannot select a later order simply because its execution would be more profitable to them or another participant.
* **Validated oracle uncertainty.** Confidence and timestamp checks constrain accepted data. Live execution applies a side-adverse confidence adjustment; frozen voluntary closes follow a separate validated pricing and spread policy.

These mechanisms restrict specific forms of maximal extractable value (MEV) arising from transaction ordering and price selection. The ordering rule governs the committed queue; it does not promise transaction inclusion priority. Delays, unavailable data and a blocked queue head can still affect when an order completes.

For builders, the useful property is that execution follows an inspectable rule. The price is not a quote selected by the finalizer. See [How orders execute](../how-plether-works/how-orders-execute.md).

### Liquidity compensation belongs to committed liquidity

VPI charges for increasing the pool's directional imbalance and can rebate a trade that reduces it. Its lifetime rule prevents those rebates from becoming an independent source of trading income.

For each portion of a position completed through a voluntary close:

```
VPI charges minus VPI rebates over that exposure's lifetime ≥ 0
```

A trader can recover previously paid VPI, but cannot finish that exposure with a net VPI credit. An opening rebate is provisional and remains subject to clawback. Partial closes reconcile the corresponding share of the position's VPI history, and the protocol protects the reserve backing a required clawback.

For example, if an exposure has paid 30 USDC in VPI and its closing trade would otherwise receive a 45 USDC rebate, the lifetime clamp limits that rebate to 30 USDC. The exposure can finish with zero net VPI cost, but cannot generate 15 USDC of rebate-only income.

**Improving the pool's balance can lower a trader's costs. Earning the pool's liquidity returns requires committing capital as an LP.**

An agent can still hedge its market exposure elsewhere. The restriction concerns extracting net VPI rebates inside Plether, rather than whether the agent's broader portfolio is directional or market neutral. See [Trading costs: fees, carry and VPI](../how-plether-works/trading-costs-fees-carry-and-vpi.md).

### Bounded liability makes backing measurable

Plether's settlement price stays within a fixed 0.00–2.00 range. That gives every position a calculable maximum modeled price payout and lets the protocol measure aggregate directional liability before accepting more risk.

An open or increase must leave effective pool backing sufficient to cover the resulting bounded liability plus the configured settlement buffer. Existing trader claims are deducted from available backing. If the condition fails, the trade is rejected.

LP withdrawals must preserve the corresponding reserves. Capital supporting trader obligations cannot also leave through a discretionary LP exit.

For an agent, this makes capacity a concrete input to strategy and sizing. For an LP, it puts an explicit bound on the price obligation being underwritten. The settlement boundary also limits the instrument's exposure: a strategy must model the difference between Plether's bounded index and unrestricted external FX prices.

Plether does not forcibly reduce an unrelated profitable position to cover another trader's loss. This protection against counterparty auto-deleveraging is separate from liquidation of an account that fails its own margin requirements.

A profitable close can still face a cash delay. A residual price payout that cannot be funded in full becomes a recorded trader claim, reserved ahead of LP withdrawals. Closing the exposure and receiving spendable cash are separate events. See [The liquidity pool and tranche waterfall](../how-plether-works/the-liquidity-pool-and-tranche-waterfall.md).

### Fair accounting protects the whole pool

Execution protection would be incomplete if participants could extract the same value through entry, exit or settlement accounting.

Plether values LP deposits and withdrawals using the same model of what open positions would pay or owe at the current mark. Existing trader profits reduce LP value; trader losses count only to the extent backed by collectible collateral and claims belonging to the same account that can offset those losses. An amount collectible from a trader can affect share value without becoming cash that an LP can withdraw before collection.

Trader claims also have a collective coverage rule. When aggregate claims are under-covered, no claimant can settle merely because the pool could pay that particular account. Once aggregate coverage is restored, paying one claim reduces cash and liabilities equally, preserving coverage for the others.

These rules connect participant protection. Traders depend on backing that remains in the pool. LPs depend on accounting that does not reward another participant for exploiting a valuation asymmetry. Both benefit when obligations are recorded consistently and protected from premature withdrawal. See [Settlement liquidity and trader claims](../how-plether-works/settlement-liquidity-and-trader-claims.md).

### Agents can authorize financial limits before execution

An agent's forecast may be uncertain. Its execution authority can still be precise.

Ordinary externally submitted orders bind an explicit price boundary, deadline, allowed execution regimes and execution-critical configuration. They also carry financial limits covering debits, charges, execution notional, resulting position size, equity and leverage. The protocol checks those limits against authoritative state immediately before applying the trade.

This lets a builder translate a trading decision into bounded authority: execute this exposure only within these costs, conditions and resulting risk limits. A preview helps choose the limits; the submitted limits remain enforceable if state changes while the order waits.

Per-order limits work alongside account-level permissions. A smart account or session policy should separately control which contracts and actions an agent can call, its cumulative exposure, withdrawals, expiry and revocation. An agent that is allowed to trade need not also have authority to transfer the owner's capital elsewhere.

Take-profit and stop-loss protection has its own authorization model. Triggered protection creates protocol-generated close attempts with a different execution envelope; it should be authorized separately from ordinary bounded orders. A trigger queues a delayed close attempt and does not guarantee a fill at its threshold.

### Automation can be verified independently

Plether gives each ordinary submitted intent a permanent account-scoped identity. Replaying the exact request returns its existing order identity; reusing that identity for a different request is rejected. This helps an agent recover from uncertain submission without accidentally duplicating exposure.

The lifecycle record distinguishes pending orders from executed and failed orders. Canonical events and authenticated receipt hashes provide evidence of the authorized intent and terminal result, so an operator can reconcile what happened independently of the agent's memory or local logs.

Execution is permissionless. A builder can use shared keepers, operate its own executor, or combine both. Changing the executor does not change an ordinary order's financial limits. Keepers still need valid data, transaction inclusion and enough gas to make progress.

Together, these properties support a disciplined automation cycle: read state, authorize a bounded action, confirm registration, monitor its lifecycle and reconcile the terminal result.

### Build agents for dollar trading and hedging

Plether gives builders a focused market: USDC-margined, USDC-settled exposure to the dollar against a six-currency basket.

**Systematic macro trading.** An agent can translate a model of dollar strength into LONG USD or SHORT USD exposure, size it against available capacity and margin, and include carry, VPI and execution costs in its expected result.

**Portfolio hedging.** An agent managing dollar-denominated assets can use SHORT USD exposure to partially offset weakening-dollar risk against the basket. LONG USD can express the opposite exposure. The appropriate hedge depends on the portfolio's liabilities and correlations; the index is not a perfect substitute for a specific currency pair.

**Continuous risk management.** An agent can monitor position health, utilization, changing carry, pending orders and market-close rules, then adjust exposure within its mandate. It must plan for binding delayed orders, close-only periods and possible settlement claims.

These applications suit strategies that can operate through delayed oracle execution. The timing model belongs in the strategy's design and evaluation from the beginning.

For the index construction and direction conventions, read [Understanding the Plether Dollar Index](understanding-the-plether-dollar-index.md). For contract interfaces, financial bounds, account policies, order identity and receipt verification, continue with [Working with AI Agents](https://github.com/Plether-Fi/plether-core/blob/master/packages/perps/WORKING_WITH_AI_AGENTS.md).
