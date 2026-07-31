# Liquidity provider quickstart

> **Supply the balance sheet behind the market.**

Plether liquidity providers deposit USDC[^usdc] through the **Senior Vault** or **Junior Vault**. Their capital becomes part of the HousePool, which stands behind trader profits and other protocol liabilities.

In return, LPs[^lp] receive vault shares whose value reflects their tranche's share of pool revenue and losses. This is underwriting capital, not a savings balance: share value can rise or fall, and not all share value is necessarily withdrawable at once.

> **Current interface status**
>
> The `Vaults` interface is under development and is not yet part of the published testnet application. Immediate deposits and synchronous withdrawals exist only on the current development branch; the pending lifecycle remains preview-only. Labels, layouts and available actions can change. LP screenshots elsewhere in the documentation are prototypes, not evidence that a control is live.
>
> The work-in-progress pending lifecycle appears as a disabled `Lifecycle coming soon` preview. Follow only the actions enabled in the published application; do not bypass a disabled step.
>
> The `Deposit` control in the existing Perps trade interface funds the Trading Account's **Margin Account**. It does **not** provide liquidity to either LP vault.
>
> LP actions execute from the connected **owner wallet**, which must hold the USDC being deposited and enough ETH for transaction fees. Approval, deposit, pending-epoch request, cancellation, finalization, claim and withdrawal transactions are **not gas-sponsored unless that exact action is explicitly marked as Sponsored** in the live interface.

### The LP flow in one line

`Owner-wallet USDC → Senior or Junior Vault → immediate shares or pending deposit → active vault shares → withdrawal subject to available pool liquidity → owner wallet`

Never send USDC directly to the HousePool. Deposit only through the verified Senior or Junior Vault surfaced by the official application and active deployment metadata.

For the protocol-level model, read [The HousePool and tranche waterfall](how-plether-works/the-housepool-and-tranche-waterfall.md).

### Before you begin

You need:

* A compatible self-custody wallet
* Arbitrum Sepolia selected in your wallet
* MockUSDC held by the connected owner wallet
* Enough Arbitrum Sepolia ETH for every required transaction
* The official application and verified contract metadata for the active deployment
* Time to monitor a pending deposit through activation, finalization and claim, once that route is supported

The welcome faucet may fund the separate **Trading Account** rather than the owner wallet. A Trading Account balance cannot be approved for an owner-wallet LP deposit. Confirm which address holds the MockUSDC before you start.

MockUSDC is test collateral. It is not issued by Circle and cannot be redeemed for real dollars.

### 1. Choose Senior or Junior

Both tranches supply the same HousePool, but they occupy different positions in its waterfall.

|                         | Senior Vault                                      | Junior Vault                                                 |
| ----------------------- | ------------------------------------------------- | ------------------------------------------------------------ |
| **Return profile**      | Target coupon funded from Junior NAV[^nav]        | Residual pool revenue after Senior obligations               |
| **Loss priority**       | Absorbs losses after Junior is exhausted          | Absorbs losses first                                         |
| **Withdrawal priority** | First claim on free LP liquidity                  | Withdraws only from liquidity above the complete Senior accounting claim (Senior principal) |
| **Central risk**        | Coupon can stop; principal can still be impaired  | Share value can be partially or completely wiped out first   |

Senior is **last-loss**, not risk-free. Its coupon is a target rather than a guaranteed return and is limited by available Junior capital.

Junior takes losses first and funds the Senior target coupon, but receives the residual upside after Senior obligations are met.

Read [Choose Senior or Junior](providing-liquidity/choose-senior-or-junior.md) before selecting a tranche.

### 2. Know what your shares represent

Each tranche is an ERC-4626[^erc4626] vault. A deposit mints shares representing proportional ownership of that tranche.

Shares are not fixed at one USDC. Their value may increase through collected trader losses, collected carry[^carry], positive VPI[^vpi] and other LP-owned revenue. Their value may decrease when the pool pays trader profits or VPI rebates, or absorbs bad debt and other losses allocated through the waterfall.

Cash-credited protocol execution fees belong to the treasury and should not be counted as LP returns.

Plether treats unrealized trader profits as liabilities but does not treat unrealized trader losses as spendable LP assets. Consequently:

* Share value can change before a trader position closes.
* Displayed position value can exceed currently withdrawable USDC.
* A historical return does not predict the next period's return.

See [Understand LP returns and share value](providing-liquidity/understand-lp-returns-and-share-value.md) and [Trading costs: fees, carry and VPI](how-plether-works/trading-costs-fees-carry-and-vpi.md).

### 3. Check whether the deposit is immediate or pending

The protocol, not the depositor, determines the available path.

| Condition | Path | When you receive shares |
| --------- | ---- | ----------------------- |
| No trader positions are open and all deposit gates pass | Immediate | In the successful deposit transaction |
| One or more trader positions are open and the pending-request gates pass | Pending epoch | After activation, finalization and a separate claim |
| Deposits are paused, Senior is impaired or another gate fails | Unavailable | No shares are issued |

Immediate deposits are restricted while positions are open because unrealized trader losses are not collected LP assets and the exact losing-trader receivables remain unsettled. In a live market, pending deposits should be expected to be the normal route.

If the application shows `Lifecycle coming soon`, the pending path is informational and cannot yet be completed through that interface. Wait for the lifecycle controls to become available.

Read [Deposit liquidity](providing-liquidity/deposit-liquidity.md) for all deposit checks and [Manage a pending deposit](providing-liquidity/manage-a-pending-deposit.md) for the complete epoch lifecycle.

### 4. Review the tranche and pool

Open `Vaults`, then select the **Senior Vault** or **Junior Vault** card.

Before entering an amount, check the live values for:

* Tranche assets and share price
* Current Senior and Junior capital
* Senior impairment status
* Deposit availability and path
* Pool free liquidity and withdrawal availability
* Oracle[^oracle] and market state
* Any active LP surcharge

Do not select a tranche from its displayed return alone. Review [LP risks and safeguards](providing-liquidity/lp-risks-and-safeguards.md) and [Market states and oracle closures](how-plether-works/market-states-and-oracle-closures.md) first.

### 5. Make your first deposit

Proceed only when the selected vault's deposit action is enabled.

1. Select the `deposit` mode on the intended vault; the action panel should read `Deposit USDC`.
2. Enter the amount held by the connected owner wallet.
3. Select `Review deposit`.
4. Verify the tranche, USDC amount, estimated or indicative shares, share price, deposit path, network and surcharge state. Confirm the `Wallet balance` shown on the amount form separately.

If the preview says **Pending deposit epoch** and shows `Lifecycle coming soon`, stop here. The current work-in-progress interface is previewing the route, not accepting the request.

If the preview says **Immediate deposit**:

5. Confirm that the approval spender is the selected **Tranche Vault**, not the HousePool, Margin Clearinghouse or an unknown contract.
6. Select `Approve & deposit` when an exact allowance is required, or `Deposit USDC` when the current allowance is sufficient. Confirm each required owner-wallet transaction.
7. Keep the application open until it reports the resulting onchain state.

An estimate is not a guaranteed redemption value. Share price, surcharge state and available liquidity can change before a transaction confirms or a pending epoch finalizes.

#### If the deposit is immediate

The successful transaction should mint shares directly to your owner wallet. Verify the share balance and the position shown for the selected tranche.

#### If the deposit is pending

The requested USDC moves into vault escrow, but no shares exist for it yet. Record the tranche, amount and assigned epoch.

A pending deposit follows this lifecycle:

`Request → cancellation window → activation → finalization (claimable shares in escrow) → claim (shares in owner wallet)`

Do not expect wallet-held shares at request time. Finalization creates the claimable batch shares in vault escrow; a separate claim transfers them to the owner wallet. The final share amount may differ from the request preview because the batch is priced at finalization.

### 6. Verify the result

After an immediate deposit—or after claiming a finalized pending deposit—confirm all of the following:

* The transaction succeeded on Arbitrum Sepolia.
* The event names the intended Senior or Junior Vault.
* Owner-wallet USDC decreased by the expected amount.
* Vault shares appear for the same owner wallet and tranche.
* No active pending request remains for an amount already claimed.
* The displayed share value and withdrawable amount are treated as different values.

If USDC moved but no shares or pending request appear, stop before trying again. Use [LP troubleshooting](providing-liquidity/lp-troubleshooting.md) to distinguish a Margin Account deposit, pending epoch, failed claim or wrong-wallet issue.

To monitor the position, read [Read your LP position and pool health](providing-liquidity/read-your-lp-position-and-pool-health.md).

### First-deposit checklist

Before you approve anything:

* I am using the official application and the active deployment.
* My MockUSDC is in the connected owner wallet, not only the Trading Account.
* I have enough Arbitrum Sepolia ETH for every required LP transaction.
* I chose Senior or Junior based on its loss and withdrawal priority.
* I verified the selected Tranche Vault as the approval spender.
* I know whether this deposit is immediate, pending or unavailable.
* I understand when a pending request becomes non-cancellable.
* I reviewed the estimated shares, live share price and any surcharge.
* I accept that share value can fall and withdrawals can be constrained.
* I will verify shares or pending state before submitting another deposit.

### Continue from here

* [Choose Senior or Junior](providing-liquidity/choose-senior-or-junior.md)
* [Understand LP returns and share value](providing-liquidity/understand-lp-returns-and-share-value.md)
* [LP risks and safeguards](providing-liquidity/lp-risks-and-safeguards.md)
* [Deposit liquidity](providing-liquidity/deposit-liquidity.md)
* [Manage a pending deposit](providing-liquidity/manage-a-pending-deposit.md)
* [Read your LP position and pool health](providing-liquidity/read-your-lp-position-and-pool-health.md)
* [Withdraw liquidity](providing-liquidity/withdraw-liquidity.md)
* [LP troubleshooting](providing-liquidity/lp-troubleshooting.md)

Trader liabilities rank ahead of LP withdrawals; see [Settlement liquidity and trader claims](how-plether-works/settlement-liquidity-and-trader-claims.md) for the rationale.

[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^nav]: Net asset value, the accounting value of a pool or tranche after assets and liabilities.
[^erc4626]: The Ethereum tokenized-vault standard used for Plether tranche shares.
[^carry]: The time-based cost charged on the portion of a position financed by LP capital.
[^vpi]: Virtual Price Impact, a separate USDC charge or rebate based on how a trade changes HousePool directional imbalance.
[^oracle]: A service that supplies external market data to smart contracts; Plether uses Pyth price feeds.
