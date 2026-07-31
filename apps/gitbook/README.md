# What is Plether Perps DEX?

> **Trade and hedge dollar index perpetuals. Onchain.**
>
> Margined and settled in USDC[^usdc]. Bounded payouts. Solvency-checked entry. Delayed oracle[^oracle] execution.

Holding dollars feels neutral. It isn’t.

Much of onchain finance is already denominated in dollars. If you hold USDC but earn, spend, or report in another currency, changes in the dollar affect your purchasing power. That exposure is a position, whether you chose it or not.

Plether makes that position explicit.

Plether Perps[^perps] DEX[^dex] is an onchain perpetual market for trading the strength of the U.S. dollar against a transparent basket of six major currencies. It is built for **USDC-first, gas-sponsored trading**: eligible perps actions use wallet authorization and a sponsored operation instead of requiring the trader to hold the network’s native gas token. See [Gas-sponsored trading and your Plether Trading Account](trading-on-plether-perps/gas-sponsored-trading-and-your-plether-trading-account.md) for the account model, eligible actions and availability limits.

Traders use USDC margin to take either side:

| Position      | Your view                  | Profits when                        |
| ------------- | -------------------------- | ----------------------------------- |
| **LONG USD**  | The dollar will strengthen | The dollar gains against the basket |
| **SHORT USD** | The dollar will weaken     | The dollar loses against the basket |

Positions have no scheduled expiry. They remain open until you close them or a keeper liquidates them after the account no longer satisfies its maintenance requirement.

The market is basket-derived. It is not a wrapped futures contract or a tokenized claim on an offchain index.

### How a trade works

1. Deposit USDC into your Margin Account.
2. Authorize the Trading Account action; Plether submits the eligible sponsored operation that commits the binding order.
3. A keeper[^keeper] executes the order against the eligible Pyth observation under the active market-state policy. Live execution uses the first eligible post-commit observation.
4. The HousePool takes the other side of the position.
5. Profit, loss, fees, and remaining margin are accounted for in USDC.

Orders enter a global first-in, first-out queue and cannot be cancelled after commitment.

During live execution, the price comes from a post-commit oracle update—not a price selected by the trader or keeper. Frozen voluntary closes use Plether’s bounded frozen-market policy. Both paths reduce keeper price-selection risk while preserving the applicable oracle safeguards.

### Solvency before volume

Plether uses a bounded market price. This makes the maximum modeled payout of every position calculable before the protocol accepts it.

Before a trade can increase risk, the protocol checks whether the HousePool has enough physically backed assets, after existing trader-claim liabilities, to cover the resulting worst-case aggregate trader payout. If it does not, the trade is rejected.

Plether does not forcibly reduce an unrelated profitable position to cover another trader’s loss. There is no counterparty auto-deleveraging.

Released position margin follows separately. The complete fresh HousePool-funded payout is either credited immediately or, when sufficient cash is unavailable, recorded in full as a senior trader claim. Plether never splits one fresh payout between an immediate credit and a new claim. The claim is not erased, but its settlement may be delayed until sufficient cash is available.

Bounded liability does not remove risk. Traders can still be liquidated, and liquidity providers can still lose capital. It makes the obligation measurable before the protocol takes it on.

### Where liquidity comes from

Liquidity providers deposit USDC into the HousePool through two tranches[^tranche]:

* The **Senior tranche** targets a coupon funded from available Junior value and has last-loss priority. The target is not guaranteed, and Senior capital can still be impaired after Junior capital is exhausted.
* The **Junior tranche** absorbs losses first and receives the residual upside after Senior obligations have been accounted for.

LP[^lp] capital backs trader payouts and absorbs bad debt. Realized carry[^carry] becomes pool trading revenue.

**Liability is the product. Return is what LPs receive for underwriting it.**

LP withdrawals are subject to cooldowns, reserved obligations, available pool liquidity, oracle state, and solvency checks. Capital already required to support trader liabilities cannot leave the pool.

To provide liquidity, start with the [Liquidity provider quickstart](liquidity-provider-quickstart.md). The task-oriented guides under **Providing liquidity on Plether** cover tranche selection, deposits, pending epochs, position monitoring and withdrawals.

### Market hours and risk

The underlying FX[^fx] market does not operate like crypto markets.

Plether remains onchain during weekends and market closures, but new risk is blocked during the configured market-close runway and frozen-oracle period. Once the oracle is frozen, eligible closes and liquidations follow special frozen-market rules. Positions can continue accruing carry throughout these periods.

Before using Plether, understand that:

* Orders are delayed, binding, and non-cancellable.
* Leveraged positions can be liquidated.
* Profitable settlements can temporarily become trader claims.
* LP deposits can lose value or become temporarily unavailable for withdrawal.
* Smart-contract, oracle, USDC, keeper, liquidity, and governance risks remain.

### Onchain macro starts here

Onchain finance already runs on dollars. Plether makes the dollar itself a market.

[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^oracle]: A service that supplies external market data to smart contracts; Plether uses Pyth price feeds.
[^perps]: Perpetual contracts, derivatives with no scheduled expiry.
[^dex]: Decentralized exchange, an onchain venue for trading without a traditional centralized intermediary.
[^keeper]: A permissionless actor or bot that submits order-finalization or protocol-maintenance transactions.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^fx]: Foreign exchange, the market for trading one currency against another.
