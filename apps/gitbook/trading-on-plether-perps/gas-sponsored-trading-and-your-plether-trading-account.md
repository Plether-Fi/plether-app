# Gas-sponsored trading and your Plether Trading Account

The current Plether Perps deployment uses a separate smart account for trading. Your connected wallet is the owner and signature surface; the derived **Plether Trading Account** is the onchain account that submits actions and owns the resulting protocol state.

Plether sponsors network gas for eligible Trading Account actions, subject to availability and policy limits. There is no direct owner-wallet or self-funded fallback in the current interface.

Gas sponsorship covers network gas only. Protocol execution fees, VPI[^vpi], carry[^carry], execution rewards and the frozen-close spread remain USDC[^usdc] costs.

### Your wallet, Trading Account and Margin Account

| Term                 | Meaning                                                                                                  |
| -------------------- | -------------------------------------------------------------------------------------------------------- |
| **Owner wallet**     | The connected wallet used to control the Trading Account and authorize its actions                       |
| **Trading Account**  | A separate smart-account address that owns positions, orders, margin, token balances and trader claims   |
| **Margin Account**   | Plether’s internal USDC balance for the Trading Account; it has no separate wallet address                |

The owner wallet and Trading Account use different addresses on the current deployment:

```
Owner wallet:     0xOwner...
Trading Account:  0xTrading...
```

Your wallet’s signature rules control the Trading Account, but protocol state belongs to the Trading Account address. Use that address when funding the account, checking activity in a block explorer or contacting support.

The round activity button beside the connected wallet opens **Trading Account activity**. It shows both addresses and provides copy and explorer actions.

![Hierarchy showing the connected wallet signing for and controlling the Trading Account, which owns positions, orders, margin and trader claims.](../.gitbook/assets/diagrams/wallet-trading-account-ownership.svg)

### Which actions Plether sponsors

The current sponsorship allowlist covers:

| Action                    | What the Trading Account does                                                        |
| ------------------------- | ------------------------------------------------------------------------------------ |
| **Deposit margin**        | Approves an exact amount of Trading Account USDC and deposits it into the Margin Account |
| **Open or increase**      | Commits a risk-increasing order to the global FIFO[^fifo] queue                       |
| **Reduce or close**       | Commits a risk-reducing order to the same global FIFO queue                           |
| **Add position margin**   | Assigns free Margin Account USDC to an existing position                              |
| **Settle a trader claim** | Credits a serviceable claim to the Margin Account                                     |
| **Withdraw margin**       | Withdraws eligible USDC and transfers the exact amount to the verified owner wallet   |

All sponsored calls send zero native value. The sponsor validates the Trading Account, owner and exact call sequence before funding an operation. Arbitrary contract calls and unrelated token transfers are not sponsored.

Sponsorship does not give any order priority. Open, increase, reduce and close commitments enter the same global FIFO queue.

#### Order execution and expired-order cleanup

Sponsorship covers the action that commits an order. A separate keeper[^keeper] transaction later executes it with the required Pyth data or cleans it up after expiry.

In the current sponsored interface:

* The owner wallet is not asked to finalize an order.
* The owner wallet is not asked to clean up an expired order.
* The owner wallet does not pay native gas or the Pyth update fee for keeper processing.
* For ordinary execution, failure or expiry cleanup, the reserved USDC execution reward is paid to the account that processes the terminal order result. If liquidation clears the order first, the reward is forfeited to the protocol treasury.

An expired row under **Open Orders** therefore shows **Keeper cleanup in progress** and **Keeper processing**, not a trader-operated **Clean Up** action.

### What sponsorship pays

Plether pays the network gas required to include an eligible smart-account operation. The following still come from the Trading Account or the trade’s settlement:

* USDC deposited into the Margin Account
* Position margin
* Protocol execution fees
* VPI charges or credits
* Carry charges
* The order execution reward
* The current 50 bps[^bps] frozen-close spread, when applicable

Gas sponsorship does not change execution price, FIFO priority, slippage protection, margin requirements, liquidation rules or protocol solvency checks. It also does not guarantee that a committed order will execute; oracle[^oracle] availability, market state, FIFO and execution-time checks still apply.

### What you sign

For each eligible action, the owner wallet signs an authorization for the prepared Trading Account operation. Before signing, check:

* The network
* The owner wallet
* The Trading Account
* The action and USDC amount
* The withdrawal recipient, when applicable
* That network gas is shown as sponsored

Creating the signature is an offchain action and does not itself consume network gas. The sponsor and bundler[^bundler] can relay an authorized operation, but they cannot create the owner signature or replace its call data with a different action.

![Sequence showing the user reviewing and signing, Plether checking eligibility, submitting the authorized operation and paying eligible network gas.](../.gitbook/assets/diagrams/authorization-and-gas-sponsorship.svg)

### Fund and deposit

On the current Arbitrum Sepolia deployment, the sponsored deposit operation uses USDC held at the Trading Account address.

The testnet welcome flow sends MockUSDC directly to the derived Trading Account. The deposit modal also supports MockUSDC that has returned to the owner wallet after a withdrawal. It first transfers only the required shortfall to the verified Trading Account, then continues with the sponsored deposit.

To deposit:

1. Confirm that the combined owner-wallet and Trading Account token balance appears as **Available to deposit**.
2. In the trade ticket’s **Margin Account** section, select **Deposit**.
3. Enter an amount no greater than **Available to deposit**.
4. If the button says **Transfer & Deposit**, confirm the regular owner-wallet transfer. This requires Arbitrum Sepolia ETH for network gas.
5. Authorize the sponsored deposit operation in the owner wallet.

The Trading Account batches two exact calls:

1. Approve the Margin Clearinghouse for the deposit amount.
2. Deposit the same amount into the Margin Account.

When the Trading Account already holds the complete amount, the transfer step is skipped. If the transfer succeeds but the sponsored deposit fails, the USDC remains in the Trading Account and can be deposited on retry.

The batch is atomic. If either call fails, the Margin Account is not credited and the approval does not remain changed.

### Withdraw to the owner wallet

A withdrawal begins with the Trading Account’s withdrawable Margin Account balance. Open positions, maintenance requirements, pending orders, execution-reward reservations and carry can make this lower than the displayed Margin Account balance.

The sponsored operation performs two exact calls as one batch:

1. Withdraw eligible USDC from the Margin Account to the Trading Account.
2. Transfer the same USDC amount from the Trading Account to its verified owner wallet.

Plether’s sponsor rejects a withdrawal batch whose transfer recipient is not the verified owner or whose two amounts differ. Gas sponsorship does not increase the amount available for withdrawal.

![Sponsored withdrawal sequence from amount selection and recipient review to owner-wallet receipt.](../.gitbook/assets/diagrams/sponsored-withdrawal-flow.svg)

### Account continuity

Positions, pending orders, free and reserved margin, position margin, execution-reward reservations and trader claims all belong to the Trading Account address that created them.

They do not automatically move to another owner-wallet address or Trading Account. Connecting a different owner-wallet address derives a different Trading Account and therefore shows different protocol state. Before funding or trading, copy the active Trading Account from **Trading Account activity** and verify that it is the address you intend to use.

### Sponsorship availability and limits

Plether evaluates every prepared operation. Sponsorship can be unavailable because of:

* An unsupported network or incompatible Trading Account
* A call outside the current allowlist
* Per-account or per-IP request limits
* Gas or sponsorship-budget limits
* Sponsor, bundler or RPC[^rpc] availability
* A failed contract or smart-account simulation
* An invalid nonce or owner signature
* A deployment-manifest or account-verification failure

Limits and network conditions can change. The current operation state in **Trading Account activity** is authoritative for that attempt.

Common activity labels include:

| Activity label                        | Meaning                                                               |
| ------------------------------------- | --------------------------------------------------------------------- |
| **Preparing sponsored transaction**  | The app is building, estimating or requesting sponsorship             |
| **Confirm in wallet**                 | The prepared operation is waiting for the owner signature             |
| **Submitting**                        | The signed UserOperation[^useroperation] is being sent                 |
| **Pending onchain**                   | The operation is waiting for an inclusion receipt                      |
| **Confirmed**                         | The sponsored operation completed                                     |
| **Failed onchain**                    | The included smart-account execution reverted                         |
| **Dropped by bundler**                | The bundler stopped tracking the operation                             |
| **Replaced**                          | A replacement UserOperation superseded the displayed operation        |
| **Expired**                           | The prepared or submitted operation passed its validity window         |
| **Inclusion timeout**                 | The app did not observe a receipt within its waiting period            |
| **Cancelled locally**                 | The app stopped tracking the operation locally                         |
| **Failed**                            | Preparation, sponsorship or submission did not complete                |

**Pending onchain** describes the sponsored operation, not the delayed order. After a confirmed commit, the new order separately appears under **Open Orders** as **Pending reveal**.

### Retry safely

If sponsorship is rejected before submission, no onchain action has occurred. Read the displayed reason, wait if rate-limited and request a newly prepared operation.

If an operation was submitted but its result is uncertain:

1. Open **Trading Account activity**.
2. Check the UserOperation and transaction links.
3. Refresh the Trading Account.
4. For a trade commitment, check **Open Orders** for an order ID.
5. Request a new operation only after the previous result is known.

A retry can require a fresh gas estimate, sponsorship decision, nonce and owner signature. Do not duplicate a trade merely because inclusion or keeper execution takes longer than expected.

During a sponsorship outage, existing positions remain active, carry continues to accrue, pending orders remain in FIFO and liquidation rules continue to apply.

### No owner-wallet fallback

The owner wallet and Trading Account are different onchain callers. Submitting the same Plether call directly from the owner wallet would act on a different Margin Account and could create state under the wrong address.

The current application is therefore sponsorship-only. If sponsorship is unavailable, it keeps the selected Trading Account and does not:

* Change the sender to the owner wallet
* Ask the owner wallet to pay native gas for the Plether action
* Expose a self-funded Trading Account route

Wait and retry the same Trading Account action, or contact support if the problem persists. External wallet transfers and other applications are outside Plether’s sponsored flow and may still require the network’s native gas token.

### Frequently asked questions

#### Do I need a new wallet?

No. Continue using your compatible self-custody wallet. The separate Trading Account does not create another recovery phrase or private key; the connected owner wallet controls it.

#### Why do I see two addresses?

The owner-wallet address is used for ownership and signatures. The separate Trading Account address owns Plether positions, orders, Margin Account balances and claims. This is the only account model enabled in the current deployment.

#### Who controls my funds?

Your owner wallet controls the Trading Account through its signature rules. The sponsor decides whether to pay network gas, but it cannot sign trader actions or withdraw funds on your behalf.

USDC in the Margin Account remains subject to Plether’s margin, reservation and settlement rules. Never share your private key or recovery phrase with Plether support.

#### Do I need the network’s native gas token?

The owner wallet and Trading Account do not need native gas for eligible sponsored Plether operations. Keeper-operated order execution and expiry cleanup also do not charge the owner wallet native gas.

The owner wallet does need Arbitrum Sepolia ETH when the deposit flow must first transfer MockUSDC to the Trading Account. That transfer is a regular token transaction rather than a sponsored Trading Account operation.

#### Why is my sponsored order still pending?

Sponsorship covers the operation that commits the order. Execution is a separate keeper-operated step. After commitment confirms, the order enters the global FIFO queue and waits for the required oracle, market and execution conditions.

#### Does sponsorship cover my trading costs?

It covers eligible network gas. Execution fees, VPI, carry, execution rewards and the frozen-close spread continue to be accounted for in USDC.

[^vpi]: Virtual Price Impact, a separate USDC charge or rebate based on how a trade changes HousePool directional imbalance.
[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^fifo]: First in, first out; orders at the front of the queue are processed before later orders.
[^keeper]: A permissionless actor or bot that submits order-finalization or protocol-maintenance transactions.
[^bps]: Basis points; 100 bps equals 1%.
[^oracle]: A service that supplies external market data to smart contracts; Plether uses Pyth price feeds.
[^rpc]: Remote Procedure Call, an interface used to communicate with a blockchain node.
[^bundler]: A service that packages smart-account operations and submits them for onchain inclusion.
[^useroperation]: A signed smart-account instruction sent to a bundler for onchain inclusion.
