# Risks you should understand first

> **Plether can bound a modeled obligation. It cannot remove risk.**
>
> Traders can lose their collateral. Liquidity providers can lose principal. Either may be unable to access USDC when expected.

Plether is designed to measure counterparty liabilities before accepting them. That is different from making capital safe.

Before trading or providing liquidity, understand what the protocol’s safeguards do—and what they do not do.

This page is a summary, not an exhaustive list. Smart contracts and financial systems can fail in ways that have not been anticipated or documented.

### Read Plether’s claims precisely

| Protocol property                    | What it provides                                                  | What it does not provide                                           |
| ------------------------------------ | ----------------------------------------------------------------- | ------------------------------------------------------------------ |
| **Fixed 0.00–2.00 settlement range** | A calculable maximum modeled payout                               | A guarantee that external FX markets remain inside that range      |
| **Entry solvency check**             | Rejects new exposure the pool cannot support at entry             | A guarantee that bad debt or later insolvency is impossible        |
| **No counterparty ADL**              | Another trader’s failure does not reduce your profitable position | Protection from liquidation of your own under-margined position    |
| **Delayed oracle execution**         | Reduces front-running and keeper price-selection risk             | Guaranteed execution, no slippage, or freedom from all MEV         |
| **Gas-sponsored Trading Account**    | Eligible actions without owner-wallet native gas                       | Guaranteed sponsor or bundler availability, or permission to act without a wallet signature |
| **Withdrawal firewall**              | Prevents encumbered LP capital from leaving the pool              | Immediate or unconditional LP withdrawals                          |
| **Non-upgradeable logic**            | Prevents the owner from replacing deployed code                   | Proof that the deployed code is correct or free of vulnerabilities |
| **Senior tranche priority**          | Junior absorbs losses before Senior                               | Principal protection or guaranteed yield                           |

### Bounded liability is not guaranteed payment

Plether’s raw basket settlement mark is permanently bounded between **0.00 and 2.00**.

The 2.00 ceiling is a protocol constant. It cannot be changed through governance.

This lets Plether calculate the maximum gross directional payout of every position:

* LONG USD cannot accrue directional profit below a raw basket mark of 0.00.
* SHORT USD cannot accrue directional profit above a raw basket mark of 2.00.

Before accepting a trade that increases risk, Plether checks whether physically backed HousePool assets can cover the resulting worst-case aggregate directional liability.

This is an admission rule. It does not guarantee that:

* Every trader profit will be immediately withdrawable
* LP principal cannot be impaired
* Bad debt cannot occur
* USDC will remain worth one dollar
* Pyth prices will always be correct or available
* Keepers will always execute promptly
* The contracts contain no defects
* Governance will always make good decisions

The solvency check depends on correct code, correct accounting, valid oracle data and functioning external infrastructure.

A profitable close can complete while some or all of the amount owed becomes a trader claim. The liability remains recorded, but settlement may be delayed until sufficient HousePool cash is available.

### No counterparty ADL does not mean no forced exit

Plether does not reduce or close an unrelated profitable position to cover another trader’s loss.

There is no counterparty auto-deleveraging between traders.

Your own position can still be fully liquidated if its equity falls below the applicable maintenance requirement.

That requirement rises around FX-market closures. A position that satisfies normal margin rules can become liquidatable under the stricter market-close requirement.

The distinction is simple:

* Another trader’s failure does not force your profitable position to shrink.
* Your own insufficient collateral can still force your position to close.

### How losses move through the system

| Event                                    | First affected                         | What happens next                                                                      |
| ---------------------------------------- | -------------------------------------- | -------------------------------------------------------------------------------------- |
| A trader loses                           | The trader’s reachable USDC collateral | Any remaining deficit becomes bad debt absorbed by HousePool capital                   |
| A trader profits                         | Available HousePool cash               | Any unpaid amount becomes a senior trader claim                                        |
| The HousePool incurs a loss              | Junior tranche                         | Senior is impaired after available Junior value is exhausted                           |
| Senior earns its target coupon           | Available Junior value                 | The coupon is limited by available Junior principal                                    |
| A terminal transition exposes insolvency | Protocol availability                  | Degraded mode blocks new risk and affected withdrawals while protective actions remain |

This waterfall determines who bears economic losses. It does not guarantee when sufficient cash will be available.

## Risks for traders

### Directional and leverage risk

LONG USD profits when the dollar strengthens against the Plether basket. SHORT USD profits when the dollar weakens.

If the market moves in the opposite direction, the position loses value. Leverage magnifies that loss relative to the collateral posted.

High leverage leaves less room for:

* Adverse price movement
* Carry accrual
* Execution fees
* Virtual price impact
* Oracle-confidence adjustments
* Liquidation bounties

A directionally correct position can still lose money if its gain does not exceed its accumulated costs.

### Full-liquidation risk

Plether uses full liquidation rather than partial liquidation.

Once a position becomes liquidatable, the entire position can be closed. Reachable collateral pays the trading loss and liquidation bounty. Any positive residual remains attributable to the trader.

There is no partial-liquidation process that reduces an oversized position and leaves the remainder open.

### Account-collateral risk

Do not assume that the margin figure displayed beside a position is always the maximum USDC that can be reached during terminal settlement.

Plether uses account-level USDC accounting. Position health is calculated from its assigned margin, but full-close and liquidation paths can reach additional eligible balances or reservations belonging to the same account before passing a deficit to the HousePool.

A trader claim is not generic collateral and cannot normally be reused as immediately spendable margin.

The detailed rules for free balance, locked position margin, committed-order reservations and terminally reachable collateral should be understood before using leverage.

### Carry risk

Carry accrues on the portion of a position economically financed by LP capital.

Carry can:

* Reduce position equity over time
* Bring a position closer to liquidation
* Continue accruing while the oracle is stale or frozen
* Change with HousePool utilization
* Change after timelocked risk-parameter updates
* Affect both LONG USD and SHORT USD simultaneously

Carry is not trader-to-trader funding. It is the cost of using LP-backed capital.

A position does not need to move against the trader to become less healthy. Carry alone can reduce equity over time.

### Binding-order risk

Plether orders are delayed, binding and non-cancellable.

After commitment:

* The market can move before execution.
* The order enters a global first-in, first-out queue.
* Missing or stale oracle data can leave the order pending.
* Slippage, expiry or certain invalid states can fail it terminally.
* A failed order is not automatically retried.
* Depending on the failure reason, the reserved execution bounty may still be paid to the keeper.

A blocked queue head must be resolved before later orders can execute. Heavy queue cleanup can require more than one keeper transaction.

Do not commit an order unless you accept its delayed and non-cancellable lifecycle.

### Execution-price risk

The price visible when an order is committed is not a guaranteed execution price.

Actual trade economics can include:

* The first eligible post-commit Pyth update
* An adverse oracle-confidence adjustment
* Virtual price impact
* The acceptable-price limit chosen by the trader
* The protocol execution fee
* The keeper execution bounty

Virtual price impact depends on HousePool depth and directional imbalance. It can improve or worsen execution during live markets.

During frozen-oracle closes, virtual price impact becomes a one-way LP-protection surcharge. There is no rebate path in that state.

Plether’s execution model reduces specific forms of price-selection MEV. It does not eliminate congestion, censorship, transaction-ordering effects or information leakage after an order is committed.

### Oracle and market-closure risk

Live execution depends on Pyth publishing timely and correct prices for all six basket components.

Incorrect, stale, inconsistent or unavailable data can:

* Distort the basket
* Delay an order
* Leave the global queue blocked
* Cause an order to expire
* Block new exposure
* Affect liquidation timing
* Move the protocol into conservative close-only behavior

During genuine FX-market closures, eligible closes and liquidations use relaxed freshness rules and conservative pricing. Those rules prioritize risk reduction, but they do not provide the same price guarantees as a live market.

Scheduled holidays, daylight-saving transitions and exceptional closures can create periods in which expected actions are unavailable.

### Market-close liquidation risk

Plether raises its applicable margin requirement before scheduled FX-market closures.

This reduces the amount of highly leveraged risk carried into a stale-price window. It also means a position can become liquidatable without a large immediate price move if it does not satisfy the higher requirement.

Carry continues accruing while the market is closed.

Traders are responsible for reducing exposure or adding collateral before the stricter regime begins.

### Delayed-profit risk

A profitable close does not always produce immediately withdrawable USDC.

If the HousePool lacks sufficient free cash, the unpaid amount becomes a trader claim.

A trader claim is:

* A senior liability recorded by the protocol
* Associated with a specific beneficiary
* Settled only when aggregate claims are sufficiently cash-covered
* Credited into the trader’s margin account rather than directly to their wallet

It is not:

* USDC already held in the trader’s wallet
* Freely reusable position collateral
* A payment with a guaranteed date
* A first-in, first-out claim on incoming cash

### Basis risk

Plether’s index is DXY-inspired, not raw DXY.

It uses:

* A weighted arithmetic basket
* Fixed reference coefficients
* Effective weights that drift without automatic rebalancing
* Pyth component prices
* A fixed 0.00–2.00 settlement range

Its returns can diverge from DXY futures, another dollar index, or a user’s personal currency exposure.

A SHORT USD position may reduce broad dollar exposure without perfectly hedging rent, payroll or expenses in one particular currency.

The fixed 2.00 ceiling creates additional basis risk if the external currency basket rises beyond 2.00. Plether stops recognizing further price movement even if the external market continues moving.

## Risks for liquidity providers

### LPs are the economic counterparty

The HousePool backs trader payouts.

LP return is compensation for underwriting that liability. It is not risk-free yield and does not come from a guaranteed external revenue source.

LP economics are affected by:

* Trader profits and losses
* Bad debt
* Realized carry
* Virtual price impact
* Directional skew
* Pool utilization
* Senior coupon transfers
* Outstanding trader claims

Protocol execution fees belong to the protocol treasury, not automatically to LPs.

### Junior tranche risk

Junior is first-loss capital.

Junior can be impaired or wiped out by:

* Profitable trader settlement
* Bad debt beyond reachable trader collateral
* Senior coupon transfers
* Oracle or accounting failures
* Other losses affecting HousePool backing

Junior receives residual upside because it occupies the first-loss position.

A wiped Junior tranche cannot be silently restored through ordinary deposits. Recovery requires explicit recapitalization or realized protocol revenue.

### Senior tranche risk

Senior has priority over Junior, not immunity from loss.

The Senior target coupon is funded from available Junior value and capped by available Junior principal.

It is not:

* A guaranteed APY
* Fixed income in the legal sense
* Principal protection
* A claim on revenue that does not exist

Senior capital can be impaired after Junior value is exhausted.

“Senior” describes its position in the loss waterfall. It does not mean risk-free.

### Directional-skew risk

LPs do not choose LONG USD or SHORT USD, but the HousePool takes the other side of aggregate trader exposure.

If open positions become concentrated:

* A LONG USD-heavy market creates greater pool liability when USD strengthens.
* A SHORT USD-heavy market creates greater pool liability when USD weakens.
* A balanced market can still owe profitable traders on either side.

The entry solvency check limits modeled exposure relative to backing. It does not prevent LP capital from being used to pay traders when the covered scenario occurs.

Drift in the Plether basket’s effective currency weights can also change which underlying FX moves have the greatest influence on future pool liability.

### Withdrawal-liquidity risk

An LP share is not the same as immediately withdrawable USDC.

Withdrawals depend on:

* Holder cooldowns
* Available HousePool cash
* Reserved trader liabilities
* Outstanding trader claims
* Current solvency
* Oracle freshness
* Tranche impairment
* Protocol lifecycle and pause state

Withdrawals can be reduced, delayed or blocked.

During frozen-oracle periods, withdrawals may remain available under stale-price rules but incur a tranche-specific surcharge.

Capital already required to support trader obligations cannot leave the pool.

### Pending-deposit risk

When trader positions are open, ordinary LP deposits enter through pending deposit epochs rather than receiving active shares immediately.

The user funds the request before shares are minted. Cancellation is available only before the activation period begins. After activation, finalization is permissionless and determines the batch share price.

Transaction ordering can affect entry economics. For example, a matured epoch may be finalized shortly before another transaction realizes a large trader loss.

Pending does not mean risk-free or freely cancellable.

### NAV and accounting risk

Plether deliberately does not treat unrealized trader losses as immediately withdrawable LP profit.

This reduces the risk of LPs withdrawing against assets the pool has not yet received. It can also temporarily understate LP value, particularly in Junior.

Value can change materially when trader PnL becomes realized.

Deposit pricing, withdrawal pricing and conservative liability accounting answer different questions. They should not be expected to produce identical values at all times.

A displayed share price or historical APY is not a guarantee of future redemption value.

## Risks shared by everyone

### Smart-contract risk

A defect in the contracts, integrations or economic assumptions can cause:

* Loss of funds
* Incorrect accounting
* Delayed settlement
* Unavailable withdrawals
* Incorrect liquidation
* Permanent protocol disruption

The perps contracts are non-upgradeable. This limits the owner’s ability to replace deployed logic, but it also means a discovered defect cannot be patched in place.

A material fix can require a new deployment and user migration.

Immutability makes behavior harder to change. It does not make behavior correct.

### Smart-account risk

Sponsored trading depends on the Trading Account’s smart-account execution path in addition to Plether’s perps contracts.

Depending on the account model, this can include:

* Smart-account code and signature validation
* EntryPoint compatibility
* Nonce and replay protection
* Owner-wallet recovery and key security
* EIP-7702 delegation state
* Correct account initialization and ownership checks

A defect or incompatible account state can reject an otherwise valid Plether action, lock the account out of the sponsored path or require a deliberate migration. Changing or removing an EIP-7702 delegation can invalidate an outstanding sponsored operation that has not yet been submitted.

The Trading Account owns the positions, orders, Margin Account and trader claims. Losing control of its owner wallet can therefore affect the ability to manage that complete protocol state.

### Sponsor-service and bundler availability risk

Eligible trader actions depend on a sponsor approving network-gas funding and a bundler accepting and submitting the signed UserOperation.

An action can be delayed or rejected because of:

* Sponsor downtime or depleted budgets
* Per-account or protocol-wide rate limits
* Gas-price or action-policy limits
* Failed operation simulation
* Bundler policy rejection
* Bundler, RPC or EntryPoint outages
* A UserOperation being dropped before inclusion
* An expired signature, invalid nonce or changed account delegation

Before the sponsored commitment confirms, these failures normally mean no order exists. After a commitment confirms, the order remains governed by the delayed FIFO execution rules even if sponsorship later becomes unavailable.

A sponsorship outage can be especially consequential for a position that needs margin, reduction or closure. The position remains active, carry continues to accrue and liquidation rules continue to apply while the action is delayed.

Sponsorship failure does not authorize Plether to transact for the user. The sponsor decides whether to pay gas; it cannot create the owner-wallet signature, replace the signed instruction with another action or withdraw funds without the required authorization. Plether also does not silently fall back to charging the owner wallet or submitting from a different address.

### Audit and deployment risk

Plether Perps has completed an external pre-audit security consultation.

It has not completed a formal production audit.

The pre-audit reviewed an earlier code snapshot. It should not be presented as formal coverage of every later change or of the complete current deployment.

The active release is an Arbitrum Sepolia testnet deployment using MockUSDC. Testnet operation does not demonstrate mainnet safety, economic durability or production readiness.

Every release should be assessed against its exact:

* Source commit
* Deployed bytecode
* Oracle configuration
* Risk parameters
* Admin addresses
* Audit scope

### Oracle risk

Plether assumes Pyth provides accurate and timely component prices.

Confidence filters, freshness checks, publication-time checks and conservative execution adjustments reduce specific failure modes.

They cannot eliminate:

* Feed compromise
* Incorrect prices
* Publication outages
* Historical-data unavailability
* Pyth contract failures
* Errors in Plether’s basket construction
* Unexpected behavior during market closures

Frozen-oracle rules deliberately prioritize close and liquidation liveness over normal live-price guarantees.

### Keeper and queue risk

Keepers are permissionless, but execution still requires someone to submit the correct transaction and oracle data.

If keeper infrastructure is unavailable:

* Orders can remain queued
* Liquidations can be delayed
* Pending LP epochs can remain unfinalized
* Users cannot cancel committed orders
* Restoring service may not clear the entire queue immediately

Permissionless execution removes an exclusive operator. It does not guarantee timely execution.

### USDC risk

The protocol accounts and settles in USDC.

Users remain exposed to:

* Loss of dollar parity
* Issuer blacklisting or freezing
* Token-contract upgrades
* Collateral centralization
* Disruption to transfers or redemptions

Plether has no internal mechanism that converts impaired USDC into an unimpaired dollar.

The current testnet uses MockUSDC, which has no claim on real dollars.

### Chain and interface risk

The current deployment depends on Arbitrum Sepolia and its surrounding infrastructure.

Sequencer interruption, congestion, transaction-ordering effects, RPC failure or wider Ethereum and Arbitrum disruption can delay actions or make the protocol temporarily unavailable.

Interfaces, APIs and indexers can display stale or incorrect information. The deployed contracts remain authoritative, but direct interaction still depends on the chain, oracle data, smart-account services and correct operation construction.

Wallet approvals, signed authorizations and confirmed operations are generally irreversible. Users are responsible for verifying the network, owner wallet, Trading Account, contract addresses and action details.

### Governance and pause risk

Core runtime logic is non-upgradeable, and the **2.00 settlement ceiling is immutable**. Governance cannot alter either through parameter changes.

Governance still controls important economic and operational parameters, including:

* Margin and carry settings
* Execution fees
* Market-close calendars
* Oracle freshness limits
* Senior target rate
* Router and keeper-bounty configuration
* The active oracle contract
* Emergency pause state

Most risk-sensitive changes currently use a 48-hour timelock. Emergency pauses can be applied immediately.

Timelocks provide notice. They do not prevent:

* Poor parameter choices
* Key compromise
* Operational error
* Disruption from a pause
* Economic terms changing during a position or LP deposit

Users should monitor pending governance actions and current protocol state.

### Model risk

Plether’s controls rely on assumptions about:

* The arithmetic currency basket
* Effective weight drift
* The fixed 0.00–2.00 settlement range
* Trader behavior
* Liquidation incentives
* Keeper availability
* Market hours
* The relationship between modeled liability and realizable cash

Extreme or previously unseen conditions can expose weaknesses that are not apparent in normal operation or historical testing.

Code can enforce a model exactly and the model can still be wrong.

## Before you use Plether

### Trader checklist

Do not open a position unless you can answer yes to each question:

* Do I understand that LONG and SHORT refer to USD while the raw basket moves in the opposite direction?
* Do I understand which same-account USDC balances can become reachable during settlement?
* Can I afford to lose that collateral?
* Can the position survive carry, fees and the higher market-close margin requirement?
* Am I willing to submit a delayed order that cannot be cancelled?
* Have I set an acceptable-price limit I understand?
* Have I verified the active Trading Account and the action covered by sponsorship?
* Can I tolerate sponsor or bundler unavailability while my position remains active?
* Can I tolerate delayed execution during an oracle, keeper or chain outage?
* Can I tolerate profitable settlement becoming a trader claim rather than immediate USDC?
* Does Plether’s arithmetic basket actually hedge the exposure I intend to hedge?
* Have I verified the network, contracts, current parameters and protocol state?

### LP checklist

Do not deposit unless you can answer yes to each question:

* Do I understand that LP capital pays profitable traders and absorbs bad debt?
* Have I chosen Senior or Junior based on its actual place in the loss waterfall?
* Can I tolerate partial or total loss of principal?
* Can I tolerate cooldowns, blocked withdrawals and frozen-market surcharges?
* Do I understand that trader claims rank ahead of LP withdrawals?
* Do I understand pending-deposit activation, cancellation and finalization?
* Have I reviewed current directional skew, open liability, free cash and tranche impairment?
* Am I relying on a target coupon or projected return as if it were guaranteed?
* Can I hold the LP position through an extended period of reduced liquidity?

### Everyone

Before interacting:

* Confirm that you are using the intended network and verified contracts.
* Read the current deployment and audit disclosures.
* Review live parameters rather than relying on examples in documentation.
* Understand the dependencies on Pyth, USDC, smart accounts, sponsors, bundlers, keepers, the deployment chain and governance.
* Use only capital you can afford to lose or have temporarily unavailable.

## The short version

1. The fixed 0.00–2.00 settlement range makes maximum directional liability measurable. It does not guarantee immediate payment.
2. No counterparty ADL protects positions from other traders’ failures. It does not prevent liquidation of your own position.
3. Delayed oracle execution reduces price-selection risk. It does not remove delay, slippage or infrastructure risk.
4. Junior absorbs LP losses first. Senior can still be impaired.
5. Trader claims are senior obligations, but they may not be settled immediately.
6. LP withdrawals depend on free, unencumbered HousePool cash.
7. Non-upgradeable contracts can still contain defects.
8. Testnet operation and a pre-audit consultation are not substitutes for a formal production audit.
9. Gas sponsorship can fail or be delayed, but it never gives Plether authority to act without the owner wallet’s authorization.

## Where to go next

* **Trader quickstart**
* **Liquidity provider quickstart**
* **Margin and liquidation**
* **Fees, carry and virtual price impact**
* **Market hours and closures**
* **Security, audits and governance**
