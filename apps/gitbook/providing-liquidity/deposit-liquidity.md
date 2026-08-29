# Deposit liquidity

> **An LP deposit goes through a verified Senior or Junior Vault.**
>
> The existing trader `Deposit` action funds a Trading Account's Margin Account. It does not provide HousePool liquidity and does not issue LP shares.

Use this guide after deciding which tranche[^tranche] fits your risk tolerance. If you have not made that choice, start with [Choose Senior or Junior](choose-senior-or-junior.md) and [LP risks and safeguards](lp-risks-and-safeguards.md).

> **Current interface status**
>
> The `Vaults` interface is under development and is not yet part of the published testnet application. The current development branch supports immediate deposits, funded epoch requests, request discovery, cancellation, finalization, claims and synchronous withdrawals. These controls must not be treated as published until the deployed application exposes them.
>
> Historical APY and full vault-activity indexing are not enabled. A preview is still not a funded request: the approval and `Queue deposit` transaction must both confirm before USDC enters vault escrow.
>
> Do not attempt to reproduce the flow by sending USDC directly to a vault or the HousePool. Wait until the application explicitly enables the verified LP action or use a separately documented direct-contract procedure from the deployment operator.

### Before you start

For the current Arbitrum Sepolia test environment, prepare:

* A compatible self-custody owner wallet
* Arbitrum Sepolia selected in both the application and wallet
* MockUSDC[^usdc] at the connected **owner-wallet address**
* Arbitrum Sepolia ETH for the approval and every subsequent LP transaction
* Enough time to monitor a pending request if the deposit does not qualify for immediate entry

LP approvals, deposits, requests, cancellations, finalizations and share claims are not covered by the current trader gas-sponsorship promise. Treat an action as sponsored only if that specific LP action is explicitly marked **Sponsored** in a later interface.

### 1. Open the LP deposit—not the trader deposit

Open `Vaults`, select `Senior Vault` or `Junior Vault`, and select the `deposit` mode. The action-panel heading should read `Deposit USDC`.

The two deposit routes have different destinations and outcomes:

| | LP vault deposit | Trader Margin Account deposit |
| --- | --- | --- |
| **Where it starts** | `Vaults` → `Senior Vault` or `Junior Vault` → `deposit` | The Perps or welcome deposit flow |
| **USDC source** | Connected owner wallet | Trading Account balance, with an owner-wallet shortfall transfer when needed |
| **Destination** | Selected tranche vault and, through its protocol method, the HousePool | Trading Account's Margin Account |
| **What the user receives** | Active tranche shares or a funded pending-deposit request | Spendable or reserved trading-account balance |
| **Purpose** | Underwrite HousePool liabilities as an LP[^lp] | Fund trading collateral and fees |
| **Gas policy today** | LP pays native network gas | Eligible Trading Account actions can be sponsored |

Depositing to the Margin Account does not later convert the balance into LP shares. Withdrawing from the Margin Account and completing a separate LP vault deposit would be required.

Read [Your Margin Account](../trading-on-plether-perps/your-margin-account.md) if you are unsure which balance the current trader interface displays.

### 2. Confirm the owner wallet, network and balance

The owner wallet and Plether Trading Account are separate onchain addresses.

The current testnet welcome flow funds the derived Trading Account. MockUSDC held there cannot be used directly by an owner-wallet approval for a tranche vault. The deposit form's **Wallet balance** is the connected owner-wallet balance and must cover the requested amount.

If it does not:

1. Obtain test MockUSDC at the owner-wallet address from the deployment operator or by a direct transfer.
2. Verify the recipient is the connected owner wallet, not the Trading Account.
3. Wait for the transfer to confirm before reopening the deposit preview.

Also confirm that the owner wallet holds enough Arbitrum Sepolia ETH for more than one transaction. A new allowance and the deposit or funded request are separate onchain transactions. A pending flow can later require cancellation, finalization and claim transactions as well.

### 3. Verify the selected tranche vault

Never rely on the label `Senior Vault` or `Junior Vault` alone. The vault page header links its abbreviated address to the block explorer. Open that link, then compare the complete address with the active deployment's official contract metadata.

Verify all of the following:

* **Network:** Arbitrum Sepolia for the current test deployment
* **Token:** the official MockUSDC contract for that deployment
* **Selected tranche:** Senior or Junior
* **Vault address:** the official address for that exact tranche
* **Approval spender:** the selected tranche vault

The spender must not be:

* The HousePool
* The Margin Clearinghouse
* The Trading Account
* The owner wallet
* An address supplied only through a message, search result or unverified link

Do not make a plain MockUSDC transfer directly to the HousePool or selected vault. Use the vault's deposit or funded-request method so the protocol can account for the deposit and issue the correct claim.

### 4. Enter the amount and review the preview

Enter the MockUSDC amount and select `Review deposit`. The current development frontend enforces a minimum deposit of `1 USDC`; the deployed interface and onchain vault rules remain authoritative.

The in-progress `Deposit preview` shows:

* **Selected tranche** and **Relative risk**
* **USDC deposited**
* **Estimated shares**, or **Current indicative shares** for a pending route
* **Current share price**
* **Deposit path:** `Immediate deposit`, `Pending deposit epoch` or unavailable
* **Expected activation:** in this transaction for an immediate deposit, or dependent on epoch eligibility
* **Frozen-oracle surcharge:** inactive, included in the quote where supported or state unavailable
* **Network** and **Quote refreshed** time
* An onchain-action notice and a final `Approve & deposit`, `Deposit USDC`, `Approve & queue`, `Queue deposit` or `Unavailable` state

The owner-wallet balance appears on the amount form rather than in the modal. The current preview does not show the balance after deposit, a numeric surcharge rate or an exact pending activation epoch. Calculate the expected remaining balance yourself, use the vault quote and onchain configuration for the active surcharge, and do not infer a pending deadline that is not displayed.

The full vault address is available through the explorer link in the vault-page header, not as a preview row. Verify it before signing. A final `Approve & deposit` button means an exact MockUSDC approval is required; `Deposit USDC` means the current allowance is sufficient.

The share estimate is not a guaranteed redemption value. Share value can rise or fall after deposit. A pending request is repriced at epoch finalization, so its final shares can differ from the preview.

When an oracle-frozen surcharge applies, the depositor receives fewer shares. The retained value remains in the selected tranche for incumbent LPs; it does not go to the protocol treasury. Use the live preview rather than assuming a fixed rate.

> **Screenshot placeholder — final Deposit preview**
>
> Add the production `Deposit preview` together with the vault-page address and amount form after the interface is finalized. The capture should show the selected tranche, owner-wallet balance, verified vault link, estimated or indicative shares, deposit path, activation status, surcharge state, network, quote time and final action state. Do not embed the current documentation prototype.

### 5. Approve the exact deposit amount

If the owner wallet already has sufficient allowance for the selected vault, the preview uses `Deposit USDC` and can proceed without another approval. Otherwise, `Approve & deposit` should first request a conventional ERC-20[^erc20] approval transaction and then advance to the separate deposit transaction.

The decoded approval must be equivalent to:

```solidity
MockUSDC.approve(selectedTrancheVault, depositAmount)
```

Check the wallet request carefully:

| Approval field | Expected value |
| --- | --- |
| **Transaction target** | Official MockUSDC token contract |
| **Function** | `approve(address spender, uint256 amount)` |
| **Spender** | Official selected Senior or Junior Vault |
| **Amount** | Exact MockUSDC deposit amount |
| **Caller** | Connected owner wallet |
| **Network** | Arbitrum Sepolia for the current deployment |

The transaction is sent to the MockUSDC contract. The tranche vault appears as the decoded **spender**. These are intentionally different fields.

Reject the request if it grants an unlimited allowance, names the wrong tranche, uses an unfamiliar token contract or changes networks. The approval only creates an allowance; it does not move USDC, issue shares or fund a pending request.

Wait for the approval receipt before confirming the deposit transaction. Both transactions consume owner-wallet native gas even when the interface presents them as one guided flow.

### 6. Confirm the route selected by the protocol

The protocol determines which deposit route is allowed at execution.

| Protocol condition | Deposit route | Result when confirmed |
| --- | --- | --- |
| No trader positions are open and every deposit gate passes | **Immediate deposit** | Active tranche shares are issued in the deposit transaction |
| One or more trader positions are open and the pending-request gates pass | **Pending deposit epoch** | USDC enters vault escrow; active shares arrive only after activation, finalization and claim |
| Deposits are paused, Senior is impaired, required pricing is unavailable or another safety gate fails | **Unavailable** | No deposit or request is accepted and no shares are issued |

Immediate deposits additionally require trading to be activated, an eligible mark, no unassigned assets awaiting ownership assignment and no Senior impairment. The contract rechecks its gates when the transaction executes. A route shown in an earlier preview is not permission to bypass a later failure.

#### Immediate deposit

When the preview shows `Immediate deposit`:

1. Confirm the exact approval if required.
2. Confirm the separate vault deposit transaction.
3. Wait for the deposit receipt.
4. Verify the tranche shares in the LP position.

Active shares begin participating in tranche economics after confirmation. An immediate deposit starts a one-hour withdrawal cooldown. During that cooldown, the shares cannot be withdrawn. Transferring shares does not bypass the restriction because the receiver inherits the relevant cooldown timestamp. A later deposit into the same vault refreshes the applicable cooldown.

#### Pending deposit epoch

When the preview shows **Pending deposit epoch**, the second transaction funds a request rather than issuing shares.

After it confirms:

* The MockUSDC leaves the owner wallet and enters selected-vault escrow.
* The depositor does not yet own active shares.
* Escrowed USDC does not yet earn the Senior coupon or Junior residual return.
* The request is assigned to a future activation epoch.
* Cancellation is normally available only before activation.
* Finalization fixes the batch share price, and the depositor must then claim the shares.

The current contracts use one-hour epoch identifiers and assign requests two epochs ahead, producing an approximate one-to-two-hour wait before activation. This is not a guaranteed finalization time.

The in-progress frontend submits this route through the verified vault's `requestDeposit` method and then displays the request under **Your position**. Do not send USDC directly to the vault; only the funded-request method creates the epoch accounting needed for cancellation and claiming.

Read [Manage a pending deposit](manage-a-pending-deposit.md) before funding this route, including its cancellation boundary and recovery behavior.

### 7. Verify what confirmed

Do not treat a wallet signature, approval receipt or preview screen as proof of an LP deposit.

For an immediate deposit, verify:

* The vault deposit transaction succeeded
* The correct tranche received the USDC through its deposit method
* The expected Senior or Junior shares appear in the owner wallet's LP position
* The owner-wallet MockUSDC balance changed by the deposited amount
* `Withdrawable now` or the onchain `maxWithdraw` reflects any active cooldown; the current interface does not show a countdown

For a pending request, verify:

* The funded-request transaction succeeded
* The requested USDC amount is shown in vault escrow
* The correct tranche and assigned epoch are recorded
* The activation time and cancellation deadline are visible
* No active shares or returns are attributed to the request yet

Save every applicable transaction hash: the approval, when required, and the deposit or funded request. Use an independent Arbitrum Sepolia explorer to confirm the contract addresses, decoded calls and emitted events.

### If the result is not what you expected

| Symptom | Most likely explanation | What to do |
| --- | --- | --- |
| Approval confirmed, but the owner-wallet USDC balance did not change | Approval creates allowance only | Return to the verified vault flow and confirm the deposit or funded-request transaction |
| USDC moved, but no LP shares appeared | The route may be pending, or the trader Margin Account deposit may have been used | Check the transaction target and pending-epoch record; a Margin Account credit is not an LP position |
| `Deposit preview` showed pending but no request exists | Only the approval confirmed, the request failed, or event discovery is still refreshing | Check the funded-request receipt, refresh **Your position**, and do not send funds manually |
| Immediate deposit became unavailable | A position opened or another execution-time gate changed | Refresh the preview and use the protocol-selected route; do not bypass the gate |
| Wallet shows an unfamiliar spender | The approval is not for the verified selected vault | Reject it and re-check the official deployment metadata |
| Approval or deposit is stuck | Network, RPC or fee conditions may have changed | Check the transaction hash before retrying; avoid creating multiple funded requests |
| Final shares differ from the estimate | Pending batches are priced at finalization, with any active surcharge applied then | Verify the finalized batch price and any active frozen-oracle surcharge rather than the request-time estimate |

See [LP troubleshooting](lp-troubleshooting.md) for recovery paths and [Read your LP position and pool health](read-your-lp-position-and-pool-health.md) for the fields to monitor after shares become active.

### Deposit checklist

Before the final deposit or request confirmation:

* Confirm the owner wallet—not only the Trading Account—holds the MockUSDC.
* Confirm the connected network and official MockUSDC contract.
* Confirm Senior or Junior and understand its loss position.
* Match the complete vault address with official deployment metadata.
* Approve only the exact amount to the selected tranche vault.
* Distinguish the approval receipt from the deposit/request receipt.
* Review immediate versus pending routing.
* Check the share estimate, active surcharge and expected remaining owner-wallet balance.
* Keep native gas for every remaining LP transaction.
* Accept that share value and future withdrawal capacity can both decrease.

[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement; the current testnet uses MockUSDC with no claim on real dollars.
[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^erc20]: The Ethereum token standard used by USDC and MockUSDC, including token allowances through `approve`.
