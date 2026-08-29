# LP risks and safeguards

> **LP return is compensation for underwriting trader liabilities. It is not risk-free yield.**
>
> Senior and Junior shares can lose value. A positive share balance may not be immediately withdrawable, and either tranche can be completely wiped out.

Plether uses solvency checks, a Senior–Junior waterfall, conservative accounting and a withdrawal firewall to limit specific risks. These controls change when and how losses can occur. They do not guarantee principal, return, liquidity or correct protocol operation.

> **Current interface status**
>
> The `Vaults` interface is under development and is not yet part of the published testnet application. Immediate deposits, pending-deposit lifecycle actions and synchronous withdrawals are available on the current development branch. Full activity history and APY history are not enabled. The `Senior Vault` and `Junior Vault` controls referenced in this section must not be treated as published until the deployed application exposes them.
>
> LP actions are outside the current trader gas-sponsorship promise. An LP should expect to use the connected owner wallet and pay native network gas for each approval and vault transaction.

### Start with the economic risk

The HousePool is the USDC[^usdc] balance sheet behind Plether Perps[^perps]. LP[^lp] capital can be used to pay profitable traders, fund VPI[^vpi] rebates and absorb bad debt. Collected trader losses, collected carry[^carry] and positive VPI can add value to the pool.

This creates a direct tradeoff:

* More HousePool/LP-owned revenue can increase tranche value.
* Trader profits and other pool losses can decrease tranche value.
* The order of loss differs between Senior and Junior, but neither tranche is protected from every loss.

Read [The HousePool and tranche waterfall](../how-plether-works/the-housepool-and-tranche-waterfall.md) for the canonical accounting model.

### Safeguards are controls, not guarantees

| Safeguard | What it is designed to do | What it does not guarantee |
| --- | --- | --- |
| **Fixed `0.00–2.00` settlement range** | Makes the maximum modeled directional payout of the Plether index calculable | That component FX prices are bounded, or that non-directional losses and failures are bounded |
| **Entry solvency check** | Rejects new trader exposure when effective backing would not cover the resulting maximum modeled liability | That LP principal cannot decline, that bad debt is impossible or that every obligation is immediately payable |
| **Physical-asset accounting** | Uses the lower of raw and accounted HousePool assets and quarantines unassigned transfers | That USDC remains worth one dollar or that the token, chain and contracts cannot fail |
| **Conservative reconciliation** | Counts unrealized trader gains as liabilities without counting unrealized trader losses as spendable assets | That every displayed NAV[^nav] is an exact future redemption value |
| **Junior first-loss position** | Makes Junior absorb pool losses before Senior | Principal protection for Senior, or a limit on Junior loss |
| **Senior restoration priority** | Routes future realized revenue toward an impaired Senior tranche before new residual value reaches Junior | That Senior will be restored, or when restoration will occur |
| **Withdrawal firewall** | Keeps cash reserved for trader liabilities and other protected amounts inside the HousePool | Immediate or unconditional LP withdrawals |
| **Pending deposit epochs** | Avoids immediate share issuance while open positions make deposit pricing incomplete | A fixed entry price, immediate activation, prompt finalization or cancellation after activation |
| **Frozen-oracle surcharge** | Retains value inside the affected tranche when an LP enters or exits under bounded stale-price rules | Full compensation for an external FX move, or continued action availability once data becomes too old |
| **Pause and degraded mode** | A HousePool pause blocks new deposits; degraded mode blocks LP withdrawals while risk-reducing trader actions remain available | A loss-free recovery or uninterrupted access to capital |
| **Non-upgradeable perps logic and timelocks** | Limit in-place code replacement and provide notice for most risk-parameter changes | Correct code, safe governance decisions or immutability of every external dependency |

The fixed settlement range and solvency check are admission controls. If the covered market outcome occurs, LP capital is still expected to pay traders.

### Principal-loss risk differs by tranche

Senior and Junior are tranche[^tranche] claims on the same HousePool, not isolated pools.

| | Senior Vault | Junior Vault |
| --- | --- | --- |
| **Loss position** | Absorbs losses after Junior is exhausted | Absorbs losses first |
| **Return position** | Receives a target coupon funded from available Junior value | Receives residual realized pool revenue after Senior obligations |
| **Withdrawal position** | First LP claim on free cash | Withdraws only from cash remaining above the complete Senior claim |
| **Severe outcome** | Can be impaired or completely wiped out | Can lose value or be completely wiped out before Senior loses value |

The Senior target coupon is an allocation from Junior principal. It is not external yield, a guaranteed APY[^apy] or a debt claim against future revenue. If Junior cannot fund the coupon, the unpaid portion does not accumulate as an amount owed to Senior.

Junior receives residual upside because it funds that coupon and bears first loss. A wiped tranche cannot be silently revived by an ordinary deposit; recovery requires realized revenue allocated through the waterfall or an explicit recapitalization path.

Use [Choose Senior or Junior](choose-senior-or-junior.md) to compare the two risk positions before depositing.

### Share value is not withdrawal liquidity

An ERC-4626[^erc4626] share represents a proportional claim on one tranche's accounting value. It is not an unconditional claim on the same fraction of the HousePool's raw USDC balance.

Before LP cash can leave, Plether reserves value for:

* Maximum bounded trader liability
* Outstanding trader claims
* USDC already set aside for trader claims and other protected obligations
* Unassigned assets and explicit protocol reserves

Trader claims rank ahead of both LP tranches. After those trader and protocol obligations determine free LP liquidity, Senior receives priority over Junior within the remaining amount. The complete Senior claim is a tranche-allocation priority, not an additional HousePool withdrawal-reserve deduction.

Withdrawal capacity can also be limited by:

* The holder's cooldown
* Insufficient physical HousePool cash
* Senior priority over Junior
* Stale or unavailable oracle data
* Pause, lifecycle or degraded-mode restrictions
* A tranche-specific frozen-oracle surcharge

The current vault design has no withdrawal queue. A permitted withdrawal settles synchronously; an amount above the live cap cannot be withdrawn. There is no guaranteed date on which a lower cap, including a zero Junior cap, will increase.

Read [Settlement liquidity and trader claims](../how-plether-works/settlement-liquidity-and-trader-claims.md) for why a protocol can remain modeled as solvent while free cash is temporarily unavailable.

### Pending deposits introduce commitment and pricing risk

When trader positions are open, ordinary LP entry uses a pending deposit epoch rather than issuing active shares immediately.

After a request is funded:

* USDC leaves the owner wallet and sits in tranche-vault escrow.
* Before finalization, the depositor holds no shares and does not participate in tranche returns.
* Cancellation is normally available only before the activation epoch begins.
* The request normally becomes binding at activation.
* Permissionless finalization fixes one batch price and creates claimable shares in vault escrow; their value then moves with the tranche share price.
* A separate claim transfers those escrowed shares to the owner wallet.

The final share amount can differ from the request-time estimate. Pool economics and the oracle-frozen surcharge can change before finalization, and transaction ordering can affect which realized events are included in the batch price.

Finalization is permissionless, but the current protocol does not assign it a separate bounty. Permissionless does not mean immediate. If Senior impairment prevents an active epoch from finalizing, a special cancellation path becomes available so escrowed USDC can be recovered.

See [Manage a pending deposit](manage-a-pending-deposit.md) before funding a request.

### Accounting and pricing can move at different times

Plether deliberately applies asymmetric LP accounting:

* Unrealized trader gains can reduce distributable LP value because they are potential pool liabilities.
* Unrealized trader losses do not increase LP value until the USDC is physically collected.

This reduces the risk of LPs withdrawing against money the protocol does not possess. It can also temporarily understate tranche value, particularly in Junior.

Deposit pricing and conservative withdrawal reconciliation answer different questions. A deposit preview, current share price, position value and current withdrawal capacity should not be expected to remain equal or move together. ERC-4626 rounding and virtual-share protections also affect exact share conversion.

Read [Understand LP returns and share value](understand-lp-returns-and-share-value.md) before treating any displayed performance figure as a forecast.

### Directional and model risk remain with LPs

LPs do not select LONG USD or SHORT USD, but the HousePool takes the economic other side of aggregate trader exposure.

* A LONG USD-heavy market creates greater pool liability when the dollar index strengthens.
* A SHORT USD-heavy market creates greater pool liability when the index weakens.
* A more balanced market can still owe profitable traders on either side.

Plether admits new exposure only when effective backing covers the resulting maximum modeled directional liability. That check depends on correct code, accounting, oracle data and model assumptions. It does not prevent capital from being paid out when an admitted liability is realized.

The index is DXY-inspired, uses a weighted arithmetic basket with fixed reference coefficients and clamps settlement to `0.00–2.00`. Effective currency weights can drift. External FX behavior, trader concentration and liquidation incentives can therefore produce outcomes that differ from a simple historical-return assumption.

### Oracle and market-state risk

Plether relies on Pyth[^pyth] data for every basket component. Freshness, confidence and timestamp-alignment rules reject some unsafe observations, but they cannot eliminate incorrect data, feed compromise, publication outages or integration defects.

During the scheduled oracle-frozen state, an LP deposit or withdrawal can remain available under an extended freshness policy and a tranche-specific surcharge. The retained amount stays in the same tranche for incumbent LPs; it is not protocol-treasury revenue.

The surcharge reduces a specific stale-price timing risk. It does not guarantee that:

* The retained amount covers the next external FX move
* The available basket is correct
* Data remains young enough for the frozen policy
* An LP action will execute

A scheduled close-only runway does not by itself activate the surcharge. An unexpected live-market oracle outage does not automatically switch the protocol into frozen pricing. See [Market states and oracle closures](../how-plether-works/market-states-and-oracle-closures.md).

### Technical, operational and external risks

| Risk | Possible LP impact | What to verify or monitor |
| --- | --- | --- |
| **Smart contract** | Incorrect accounting, lost funds, blocked actions or permanent disruption | Exact source commit, deployed bytecode, contract addresses and security disclosures |
| **Immutability** | A defect in non-upgradeable perps logic can require a new deployment and migration | Whether the deployment and audit scope cover the code actually in use |
| **Audit and testnet** | Undiscovered defects or untested economic behavior | The active release's audit status; testnet use does not demonstrate production safety |
| **Governance and pause** | Changed economic terms, blocked deposits or operational disruption | Timelocked parameter changes, emergency state and admin addresses |
| **Keeper/finalizer availability** | Delayed order processing or pending-epoch finalization | Pending epochs, protocol operations and whether a permissionless action still needs submission |
| **Chain and RPC** | Delayed, reordered or unavailable transactions | Correct network, confirmed receipts and an independent block explorer |
| **USDC** | Depeg, issuer freeze, token upgrade or transfer disruption | Exact token contract and collateral status; MockUSDC has no claim on real dollars |
| **Wallet and approval** | Unauthorized token use or an unrecoverable transaction | Owner wallet, selected vault, exact spender, allowance and network before signing |
| **Interface/indexer** | Stale or incorrect balances, previews or statuses | Onchain state and transaction receipts; the contracts are authoritative |

The core perps contracts are non-upgradeable, but that property does not prove they are correct. The current Arbitrum Sepolia release uses MockUSDC and has not completed a formal production audit. Treat it as a test environment, not evidence of mainnet safety.

### What to review before depositing

At minimum, check:

* The official deployment and selected Senior or Junior Vault address
* Tranche principal, share price and Senior impairment status
* Senior high-water mark and available Junior value
* Physical HousePool assets and maximum live trader liability
* Aggregate trader claims and other withdrawal reserves
* Free liquidity and the tranche-specific withdrawal cap
* Directional exposure, utilization and current market state
* Immediate or pending deposit route
* Pending-epoch timing and cancellation boundary
* Current oracle state and frozen-oracle surcharge
* Pause, degraded-mode and governance state
* The exact transactions and native gas required to manage and eventually exit the position

No single metric, including historical return or current free liquidity, summarizes all of these risks.

> **Screenshot placeholder — final Vaults risk overview**
>
> Add the production `Vaults` overview showing `HousePool assets`, `Free liquidity`, `Withdrawal reserve`, `Deposit route`, `Market state` and the Senior and Junior Vault cards after the interface and field definitions are finalized. Do not substitute the documentation prototype.

### Decide whether the position is suitable

Do not provide liquidity if you require:

* Guaranteed principal or yield
* A guaranteed withdrawal date
* A position whose value cannot depend on trader performance
* A deposit that can always be cancelled
* A protocol action that is always gas-sponsored

If these risks are acceptable, continue with [Deposit liquidity](deposit-liquidity.md) and verify every address and preview field before signing.

For the broader protocol risk model, read [Risks you should understand first](../welcome/risks-you-should-understand-first.md).

[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^perps]: Perpetual contracts, derivatives with no scheduled expiry.
[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^vpi]: Virtual Price Impact, a separate USDC charge or rebate based on how a trade changes HousePool directional imbalance.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^nav]: Net asset value, the accounting value of a pool or tranche after assets and liabilities.
[^apy]: Annual percentage yield, an annualized return measure that includes compounding.
[^erc4626]: The Ethereum tokenized-vault standard used for Plether tranche shares.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^pyth]: The oracle network that supplies Plether's external FX price feeds.
