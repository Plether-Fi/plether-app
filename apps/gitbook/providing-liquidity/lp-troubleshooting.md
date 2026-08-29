# LP troubleshooting

Liquidity-provider (LP)[^lp] actions can stop before submission, during an owner-wallet transaction, in a pending deposit epoch or at a live withdrawal limit. Start by identifying which balance and contract the action belongs to before trying again.

> **Current interface status**
>
> The `Vaults` interface is under development and is not yet part of the published testnet application. Immediate deposits, pending-deposit lifecycle actions and synchronous withdrawals are available on the current development branch. The labels on this page are placeholders until the relevant interface is deployed.
>
> The current Perps-page `Deposit` and `Withdraw` actions operate the Trading Account's **Margin Account**. They do not provide liquidity to or withdraw liquidity from a tranche vault.
>
> LP approvals, deposits, pending-epoch actions, claims and withdrawals are not currently gas-sponsored. Keep enough Arbitrum Sepolia ETH in the owner wallet for every required transaction.

### Check these first

1. Confirm the connected **owner wallet** and Arbitrum Sepolia network.
2. Identify whether the balance is owner-wallet USDC[^usdc], Margin Account USDC, a pending deposit request or active tranche shares.
3. Verify whether the target is the official Senior Vault or Junior Vault from the active deployment's contract metadata.
4. Check the transaction hash and block-explorer result before submitting another transaction.
5. Check the deposit epoch, holder cooldown, oracle[^oracle] state and protocol lifecycle state relevant to the action.
6. Refresh the application and compare the interface with onchain balances and events.

Never send USDC directly to the HousePool. Never approve the HousePool, Margin Clearinghouse or an unknown contract when the intended spender is a tranche vault.

### Identify the balance you are looking at

| What you see | What it represents | Which flow applies |
| --- | --- | --- |
| MockUSDC in the owner wallet | Tokens available to approve and deposit into a tranche vault | LP deposit flow |
| MockUSDC at the Trading Account address | Tokens controlled through the separate Trading Account | Margin Account deposit flow, not an LP vault approval |
| Margin Account balance | Trader collateral held through the Margin Clearinghouse | Trader deposit or withdrawal flow |
| Pending Senior or Junior request | USDC held in tranche-vault escrow before shares are active | Cancel, finalize or claim according to epoch state |
| Active Senior or Junior shares | A proportional claim on that tranche | LP position and withdrawal flow |
| `Pool liquidity` on the Perps page | Free HousePool USDC after protected reserves | Neither total tranche value nor your personal withdrawal limit |

An LP deposit does not create Margin Account collateral. A Margin Account deposit does not create tranche shares. The protocol does not automatically move value between those systems.

### Quick symptom guide

| Symptom | Check first | Safe next action |
| --- | --- | --- |
| `Vaults` is missing | Whether the LP interface has been deployed | Do not use an unknown contract or the Perps-page `Deposit`; wait for the enabled verified interface or a separately documented direct-contract procedure |
| MockUSDC moved but no shares appeared | Target contract and immediate-versus-pending route | Identify whether the funds entered the Margin Account, vault escrow or an immediate tranche deposit |
| Approval fails | Owner-wallet balance, gas, network and spender | Fund gas, select the right network and approve only the verified selected tranche vault |
| Immediate deposit is unavailable | Open trader positions and deposit safety gates | Use the pending route if offered; otherwise wait until the displayed gate clears |
| Pending request cannot be cancelled | Whether its activation epoch has begun | Do not submit repeated cancellation attempts; follow the post-activation finalization path |
| Activation time passed, but no shares appeared | Finalization and claim status | Finalize the epoch if eligible, then submit the separate share claim |
| Final share amount differs from estimate | Batch price and frozen-oracle surcharge at finalization | Verify the finalized epoch details; the request-time amount was an estimate |
| `Withdrawable now` is zero | Cooldown, reserves, tranche priority, oracle and degraded mode | Address the identified gate or wait for protocol conditions to change |
| Junior value is positive but withdrawable is zero | Free liquidity relative to Senior, then holder and protocol-state gates | Check the Junior pool cap, cooldown, oracle freshness and degraded mode; positive value alone does not create withdrawal capacity |
| Withdrawal preview changed | Fresh reconciliation and live maximum | Request a fresh preview and do not submit the stale amount |
| More shares burn than an ordinary withdrawal quote | Frozen-oracle surcharge on the asset-denominated withdrawal | Compare the refreshed vault quote and configured tranche rate |
| Share value declined | Pool revenue, trader payouts and waterfall losses | Review the economics and transaction history; do not assume it is a display error |

### I used `Deposit`, but no LP shares appeared

First identify which `Deposit` action you used.

The visible Perps-page action funds the Trading Account's Margin Account. If that balance increased, the transaction succeeded as a **trader collateral deposit**. It did not provide HousePool liquidity and will not mint Senior or Junior shares.

If a tranche-vault transaction confirmed, identify its path:

* **Immediate deposit:** active shares are issued in the deposit transaction.
* **Pending deposit:** USDC moves into vault escrow. Finalization creates the claimable share allocation in escrow; a separate claim transfers those shares to the owner wallet.

Check:

1. Transaction target
2. Selected tranche
3. Owner and share recipient
4. Vault share balance
5. Pending request and epoch ID
6. Transaction events

Do not send another deposit until you know which state the first transaction created.

See [**Deposit liquidity**](deposit-liquidity.md) and [**Manage a pending deposit**](manage-a-pending-deposit.md).

### `Vaults` or an LP action is unavailable

The LP interface is not yet available in the published testnet application. Its absence is not an instruction to substitute the Perps-page Margin Account control.

When the interface is deployed, an action can still be unavailable because of protocol state. Immediate deposits require, among other gates:

* Trading to be activated
* Deposits not to be paused
* The applicable mark-freshness rule to pass
* Senior not to be impaired
* No unassigned assets awaiting ownership assignment
* No open trader positions

When trader positions are open, the pending-epoch route is the ordinary deposit path. If deposits are unavailable rather than pending, read the displayed gate and wait for it to clear; do not attempt to bypass it with a direct HousePool transfer.

### My approval or LP deposit failed

Check:

* MockUSDC balance in the connected owner wallet
* Arbitrum Sepolia ETH in that owner wallet
* Supported network
* Selected Senior or Junior tranche
* Verified tranche-vault spender address
* Allowance amount
* Whether the approval transaction actually confirmed
* Whether the next action is immediate deposit or funded pending request

The testnet welcome flow normally funds the separate Trading Account address. Tokens held there cannot be used directly for an owner-wallet tranche-vault approval.

Approval and deposit or request are separate owner-wallet transactions. A confirmed approval does not itself move USDC into the HousePool or mint shares.

### My pending request cannot be cancelled

A pending request can normally be cancelled only **before** its activation epoch begins.

Before activation, a successful cancellation:

* Removes the request
* Returns escrowed USDC to the owner wallet
* Issues no shares

After activation begins, the request normally becomes binding. Repeated cancellation attempts do not move the epoch forward. Wait for the epoch to be finalized, then claim the shares.

There is one protective exception: if Senior impairment prevents an active epoch from finalizing, cancellation becomes available again so the depositor can recover escrowed USDC.

### The activation time passed, but no shares appeared

Activation, finalization and claiming are separate states.

1. Confirm that the assigned activation epoch has begun.
2. Check whether the epoch has been finalized.
3. If it is eligible but unfinalized, submit finalization or wait for the application, a keeper[^keeper] or another user to do so.
4. After finalization, submit the separate `[Claim shares]` transaction.
5. Verify the vault-share balance and that the pending request cleared.

Finalization is permissionless and currently has no separate protocol bounty. It still requires an onchain transaction and gas from whoever submits it.

If finalization fails, check:

* Activation time
* Oracle state and accepted mark freshness
* Senior impairment
* Other deposit gates shown by the application

If Senior impairment is the blocker, check whether the special cancellation path has reopened.

> **Screenshot placeholder — Pending epoch status**
>
> Add the deployed pending-epoch status panel here. It should distinguish **Before activation**, **Ready to finalize**, **Finalized—claim shares** and **Cancellation reopened** without implying that a pending request already owns active shares.

### The claimed share amount differs from the request estimate

Pending requests use one final batch share price for the complete epoch. The final share count can differ from the request-time estimate because tranche economics and the frozen-oracle surcharge state can change before finalization.

Finalization:

* Reconciles the HousePool
* Fixes the batch share price
* Applies any active frozen-oracle surcharge
* Moves batch USDC into the HousePool
* Mints batch shares into vault escrow

Compare the finalized batch details with the claimed share amount. Do not treat the request-time estimate as a guaranteed conversion rate.

### My shares do not appear after claiming

Check the claim transaction before trying again:

1. Confirm that it succeeded onchain.
2. Confirm the owner and recipient addresses.
3. Confirm the selected tranche and epoch ID.
4. Read the Senior or Junior Vault share balance directly.
5. Refresh the application.
6. Confirm whether the pending request cleared.

A finalized epoch holds shares in vault escrow until each depositor claims. Finalization alone does not transfer shares to the owner wallet.

### `Withdrawable now` is zero

Check each independent restriction:

* **Share state:** you must hold active shares, not only a pending request or unclaimed epoch allocation.
* **Holder cooldown:** an immediate deposit or successful prior withdrawal can make `maxWithdraw` and `maxRedeem` return zero for one hour.
* **Withdrawal reserves:** trader liabilities, outstanding claims, USDC already set aside to fund claims and other protected amounts reduce free liquidity.
* **Tranche priority:** Junior withdraws only from free liquidity above the complete Senior principal.
* **Oracle state:** stale or over-stale data can restrict the action.
* **Degraded mode:** LP withdrawals are blocked until effective solvency is restored and the latched mode is explicitly cleared.

A HousePool pause blocks new deposits but does not, by itself, block protective withdrawals. If the withdrawal is unavailable, identify the separate restriction rather than assuming that a deposit pause explains it.

Withdrawal capacity may improve after positions close, liabilities are released, additional cash enters the HousePool or valid oracle data returns. There is no guaranteed recovery time.

See [**Withdraw liquidity**](withdraw-liquidity.md) and [**Settlement liquidity and trader claims**](../how-plether-works/settlement-liquidity-and-trader-claims.md#what-is-a-trader-claim).

### Junior has value but cannot withdraw

Junior is subordinate to the complete Senior claim for withdrawals.

Conceptually:

```
Junior maximum withdrawal
= min(
    Junior principal,
    max(free USDC − Senior principal, 0)
  )
```

If free USDC does not exceed Senior principal, Junior's pool-level cap is zero. This does not, by itself, mean the Junior shares have zero accounting value.

If the Junior pool-level cap is positive but the holder's `Withdrawable now` remains zero, check the cooldown, oracle freshness, degraded mode and other holder or protocol-state gates.

There is no queue position to claim and no action that bypasses Senior priority. Recheck `Free liquidity`, `Withdrawal reserve`, `Pool withdrawal cap` and the holder gates after protocol conditions change.

### My withdrawal was rejected

The in-progress `Vaults` flow accepts a USDC amount:

1. Select `withdraw`.
2. Enter `Amount to withdraw`.
3. Select `Review withdraw`.
4. Review `Withdrawal preview`.
5. Confirm `Withdraw USDC`.

It does not currently expose a share-count or **Redeem** control.

Check:

* The amount is no greater than the latest `Withdrawable now` or `maxWithdraw` value.
* The holder cooldown has expired.
* The current development frontend's ordinary withdrawal amount is at least `1 USDC`; deployed vault rules remain authoritative.
* A sub-`1 USDC` request is a complete residual exit, not a partial withdrawal.
* The current `Market state` permits LP exit.
* If the withdrawal requires a reconciliation mark, the accepted oracle data is not beyond the extended frozen-market window; no mark may be needed when there is no open liability to reconcile.
* The protocol is not in degraded mode.
* The owner wallet has enough Arbitrum Sepolia ETH.
* The transaction targets the verified selected tranche vault.

The vault reconciles accounting at withdrawal time. If the previewed amount is no longer permitted, request a fresh preview and reduce the amount or wait for conditions to change.

### I cannot make a second partial withdrawal

Every successful withdrawal or redemption resets the holder's one-hour cooldown. This includes a partial withdrawal.

The remaining shares stay invested, but `maxWithdraw` and `maxRedeem` normally return zero until the new cooldown expires. The current interface does not show a countdown; wait for the one-hour holder period to pass, then request a fresh live maximum.

Plan partial exits with this reset in mind. The protocol does not queue the remaining amount for automatic execution.

### My remaining balance is below the minimum

The current development frontend enforces `1 USDC` as its ordinary withdrawal minimum. It rejects a smaller partial withdrawal; the deployed interface and onchain vault rules remain authoritative.

A complete dust exit can still be permitted when your **entire remaining tranche claim** is below the minimum. Submit the complete residual amount after the holder cooldown, subject to the same withdrawal firewall, oracle and protocol-state checks.

The dust exception does not bypass a zero tranche cap or degraded mode.

### A frozen withdrawal burns more shares than expected

The current `Vaults` interface uses `withdraw(assets)`: the amount entered is the target USDC wallet receipt. Check whether the onchain `oracleFrozen` state was active when the withdrawal was submitted.

During `oracleFrozen`:

* A tranche-specific surcharge increases the shares required to deliver the entered USDC amount.
* The retained amount remains inside the same tranche for incumbent LPs.
* The retained amount does not go to the protocol treasury or the other tranche.

Senior and Junior can use different live rates. A scheduled close-only runway alone does not activate this surcharge.

Compare the confirmed share burn with the refreshed vault quote and onchain tranche rate. The current development preview shows whether the surcharge is active but does not itemize a numeric rate or share-cost decomposition. Do not assume a fixed rate from an earlier session.

A separate share-denominated `redeem(shares)` path would return less USDC while the surcharge is active, but the current interface does not expose that control.

### My share value declined

Vault shares are not fixed at `1 USDC`. Their value can rise or fall as the tranche earns revenue, pays trader obligations and moves losses through the waterfall.

Value can decline through:

* Profits paid or owed to traders
* VPI[^vpi] rebates paid by the pool
* Liquidation shortfalls and bad debt
* Oracle, smart-contract, stablecoin and operational failures
* The Senior target coupon, from Junior's perspective

Plether also treats unrealized trader gains as liabilities while refusing to treat unrealized trader losses as spendable LP assets before they are realized. This conservative accounting can make share value and withdrawable USDC differ.

Review [**Understand LP returns and share value**](understand-lp-returns-and-share-value.md), [**LP risks and safeguards**](lp-risks-and-safeguards.md) and the confirmed HousePool events. A declining value is not automatically an interface error.

### Senior return is below the target

The Senior coupon is a target funded from available Junior principal. It is not a guaranteed APY[^apy], external yield or separate debt claim.

If Junior cannot fund the complete coupon:

* Senior receives only the available amount.
* Junior cannot fall below zero.
* The shortfall does not become a debt that automatically accrues later.

Senior can also lose principal after Junior is exhausted. Check the current Senior principal, high-water mark and Junior capital before treating a lower-than-target result as a calculation failure.

### `Pool liquidity` does not match my LP position

The Perps page's `Pool liquidity` figure represents free HousePool USDC after protected reserves.

It is not:

* Total HousePool assets
* Total Senior or Junior NAV[^nav]
* Your tranche's share value
* Your personal `Withdrawable now`

In `Vaults`, read `Current value`, `Share price`, `Withdrawable now`, `Pool withdrawal cap`, `Free liquidity` and `Withdrawal reserve` as separate measurements.

### My LP transaction is pending or failed

LP actions are submitted from the owner wallet and are not currently gas-sponsored.

1. Check the transaction hash in the block explorer.
2. Confirm the connected owner wallet and network.
3. Confirm the selected tranche-vault target.
4. Check whether the transaction is still pending, confirmed or reverted.
5. Compare the onchain vault balance, request state or share balance with the intended action.
6. Submit a fresh transaction only after the first transaction's result is known.

A successful approval changes allowance only. A successful pending request creates escrowed USDC only. Successful finalization creates batch shares in vault escrow. Successful claiming transfers active shares. Successful withdrawal burns the quoted shares and sends the requested USDC amount. Identify which transition actually confirmed before repeating the next step.

### When reporting a problem

Collect:

* Transaction hash
* Connected owner-wallet address
* Network
* Senior or Junior Vault address
* Action attempted
* Amount and share balance
* Pending epoch ID, if relevant
* `Current value`, `Share price` and `Withdrawable now`
* `Pool withdrawal cap`, `Free liquidity` and `Withdrawal reserve`
* Holder cooldown expiry
* Oracle timestamp and `Market state`
* Exact interface or contract error

Never share a private key, seed phrase or an unrelated wallet signature.

For the complete operational path, return to [**Liquidity provider quickstart**](../liquidity-provider-quickstart.md). For the canonical mechanics, see [**The HousePool and tranche waterfall**](../how-plether-works/the-housepool-and-tranche-waterfall.md).

[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^oracle]: A service that supplies external market data to smart contracts; Plether uses Pyth price feeds.
[^keeper]: A permissionless actor or bot that submits order-finalization or protocol-maintenance transactions.
[^vpi]: Virtual Price Impact, a separate USDC charge or rebate based on how a trade changes HousePool directional imbalance.
[^apy]: Annual percentage yield, an annualized return measure that includes compounding.
[^nav]: Net asset value, the accounting value of a pool or tranche after assets and liabilities.
