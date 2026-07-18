# Keepers and permissionless execution

Keepers are independent actors who submit transactions that move Plether from one valid onchain state to the next.

A keeper may be a person running a script, a professional automation operator or a trader finalizing an eligible action. There is no keeper allowlist, registration process or special onchain role. If an action is permissionless and an address submits a transaction that satisfies the protocol's rules, that address acts as the keeper for that transaction.

Keepers do not custody trader or LP[^lp] funds. They also do not decide whether a transaction should be accepted. Plether's smart contracts independently verify every submitted action and revert or record a terminal failure when its requirements are not met.

### What keepers do

Plether uses delayed execution. Committing an order records a binding instruction, but it does not immediately create, change or close a position. A later transaction must finalize that order.

For normal order execution, a keeper:

1. Watches for committed orders.
2. Starts with the head of the global FIFO[^fifo] queue.
3. Waits until the post-commit and oracle conditions are satisfied.
4. Supplies the eligible Pyth[^pyth] update data and pays the required oracle update fee.
5. Calls the order router to execute one order or a consecutive batch.
6. Clears an expired queue head when it can no longer execute.

Keepers can also perform other permissionless protocol actions. The most important protective example is liquidation: when an account is at or below the applicable maintenance requirement, any address may submit a valid liquidation with the required oracle data.

The current Plether reference keeper automates the perps order queue. It indexes order events, re-reads pending orders from the contracts, selects eligible historical Pyth payloads, batches consecutive executable orders and clears expired queue heads. Liquidation and other permissionless maintenance may be operated separately; they are not currently automated by this keeper binary.

### Why keepers matter

Keepers provide **liveness**. Smart contracts validate transactions, but they do not wake up and submit transactions by themselves. Without a finalizer, a committed order remains pending until someone executes or clears it.

Permissionless keeper participation is important because it:

* avoids making one operator the protocol's only execution path;
* allows another operator or the trader to act when the usual automation is unavailable;
* creates an open incentive to process orders and liquidations;
* helps remove expired orders that would otherwise block the FIFO queue; and
* enables timely liquidation of unsafe accounts before losses can grow.

Keeper availability is still an operational dependency. Network congestion, RPC failures, unavailable oracle data or all active keepers going offline can delay execution. Permissionless access reduces reliance on a designated operator, but it does not guarantee that a profitable keeper will always be available.

### What a keeper can and cannot choose

Being permissionless does not give keepers discretion over protocol outcomes.

For queued orders, the contracts enforce:

* **FIFO ordering:** a keeper cannot skip a valid queue head to execute a later order;
* **post-commit pricing:** live execution uses the first eligible Pyth observation after commitment;
* **the settlement window:** the submitted observation must belong to the order's permitted time window;
* **market-state policy:** live, protective and frozen-market execution follow their respective rules;
* **the trader's execution limit:** an order fails rather than executing beyond its accepted boundary; and
* **account, margin and solvency checks:** a keeper cannot bypass protocol risk controls.

A keeper can choose when to submit a valid transaction and what gas fee to offer. It cannot choose an arbitrary execution price, reorder valid orders or force the contracts to accept invalid data. This separation preserves open participation without trusting keepers to price trades.

Read [How orders execute](how-orders-execute.md) for the complete pricing and queue rules.

### Keeper incentives and costs

An order reserves an execution reward in USDC[^usdc] when it is committed. After successful finalization, the reward is credited to the finalizer's Plether Margin Account. A successful liquidator receives the applicable liquidation bounty instead.

The operator pays the transaction costs required to attempt the action, including network gas and, where applicable, the Pyth update fee. A reward is not guaranteed profit. Costs can exceed rewards, and transactions can revert or lose a race to another keeper. Operators should model fees, failure rates, competition and infrastructure costs before running a live service.

#### Hermes access is a separate operating cost

The reference keeper does not calculate the six FX prices itself. Its basket worker retrieves signed Pyth update payloads from a Hermes-compatible API and caches them for order execution.

Hermes data access and the onchain Pyth update fee are different costs:

* **Hermes access** is an offchain API dependency. It may require an API key and paid data plan. Pricing, request quotas and feed coverage are set by Pyth or the selected provider and may change.
* **The Pyth update fee** is paid in the network's native token when the keeper submits update data onchain. It is paid in addition to transaction gas.

The Plether basket worker defaults to one batched six-feed Hermes request every five seconds and backs off after a rate-limit response. Hermes availability or an insufficient API quota can therefore delay finalization. Check Pyth's current [Hermes access guidance](https://docs.pyth.network/price-feeds/core/how-pyth-works/hermes), [data-plan information](https://docs.pyth.network/price-feeds/core/upgrade/preparing) and [onchain update fees](https://docs.pyth.network/price-feeds/core/current-fees) when estimating keeper costs.

#### Is the public Hermes endpoint mandatory?

No. Plether's contracts require a valid Pyth-signed update payload; they do not trust or identify the HTTP server from which the keeper obtained it. The reference implementation currently consumes the Hermes API format, but `PYTH_HERMES_URL` can point to Pyth's hosted service or another compatible Hermes provider. Pyth also publishes Hermes as open-source software.

This means the current reference keeper requires **Hermes-compatible delivery**, not one specific hosted endpoint. It cannot replace the Pyth payload with data from an unrelated FX API or another oracle network. That would require a protocol-level oracle change. Running another Hermes instance or choosing another provider may improve infrastructure independence, but it does not necessarily remove Pyth data-access terms or costs.

### How to become a keeper

At the protocol level, no enrollment is required. You need:

* a wallet dedicated to keeper operations;
* the network's native token for gas and Pyth update fees;
* a reliable RPC connection;
* software that identifies eligible actions and constructs valid calls; and
* monitoring for failed, reverted, delayed or replaced transactions.

You may build your own keeper against the public contracts or run Plether's open-source reference implementation:

* [`apps/backend/app/Keeper.hs`](https://github.com/Plether-Fi/plether-app/blob/master/apps/backend/app/Keeper.hs) is the executable entry point.
* [`apps/backend/src/Plether/Keeper.hs`](https://github.com/Plether-Fi/plether-app/blob/master/apps/backend/src/Plether/Keeper.hs) contains the queue, payload-selection, batching and transaction-submission logic.

The reference service requires GHC 9.4+, Cabal 3.0+, PostgreSQL and an RPC endpoint. It also depends on the Plether basket worker to cache the Pyth payloads used for execution.

From `apps/backend`, configure at least:

```bash
export DATABASE_URL=postgresql://USER:PASSWORD@HOST:PORT/DATABASE
export PERPS_RPC_URL=https://YOUR_RPC_ENDPOINT
export KEEPER_PRIVATE_KEY=0xYOUR_DEDICATED_KEEPER_PRIVATE_KEY
export CHAIN_ID=YOUR_CHAIN_ID
export PERPS_CHAIN_ID=YOUR_CHAIN_ID
export PERPS_ORDER_ROUTER=0xYOUR_ORDER_ROUTER
export PERPS_PLETHER_ORACLE=0xYOUR_PLETHER_ORACLE
export PERPS_INDEXER_START_BLOCK=YOUR_DEPLOYMENT_START_BLOCK
export PYTH_HERMES_URL=https://YOUR_HERMES_ENDPOINT
export PYTH_API_KEY=YOUR_SERVER_SIDE_API_KEY
```

Use the current deployment addresses for the network you intend to serve. Never assume that example or testnet addresses are valid for another deployment.

Keep the Pyth cache populated in a separate process:

```bash
RPC_URL=https://YOUR_RPC_ENDPOINT \
CHAIN_ID=YOUR_CHAIN_ID \
DATABASE_URL=postgresql://USER:PASSWORD@HOST:PORT/DATABASE \
cabal run plether-basket-worker -- --latest-loop
```

Then validate the keeper without submitting transactions:

```bash
cabal run plether-keeper -- --once --dry-run
```

After checking the selected network, contract addresses, wallet and logs, start the continuous keeper:

```bash
cabal run plether-keeper
```

Use a dedicated key with only the funds needed for operation, protect it as production infrastructure and monitor its native-token balance. Multiple operators may compete to execute the same action; the first valid transaction included onchain wins the execution opportunity.

### Keepers are executors, not administrators

A keeper's authority begins and ends with submitting calls that anyone may submit. Keepers cannot change risk parameters, move user funds arbitrarily or override the oracle and queue rules.

That is the core design: **open execution, constrained by onchain verification.**

[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^fifo]: First in, first out; the oldest unresolved order must be handled before later orders.
[^pyth]: Plether's external price-data provider for the six currency feeds used to calculate its index.
[^usdc]: A US dollar-denominated stablecoin Plether uses for margin, rewards and settlement.
