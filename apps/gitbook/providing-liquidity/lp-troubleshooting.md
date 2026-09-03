# LP troubleshooting

Liquidity-provider (LP)[^lp] actions can stop before submission, during an owner-wallet transaction or while an hourly deposit or withdrawal request is waiting. Start by identifying the balance, request and contract involved before trying again.

> The Perps-page `Deposit` and `Withdraw` controls operate the Trading Account's **Margin Account**. They do not provide liquidity to or withdraw liquidity from a tranche vault.
>
> Vault approvals, requests, cancellations and claims use the connected owner wallet. The current Vaults flow does not sponsor them, so keep enough Arbitrum Sepolia ETH for every required transaction.

### Check these first

1. Confirm the connected **owner wallet** and Arbitrum Sepolia network.
2. Identify whether the value is owner-wallet USDC[^usdc], Margin Account USDC, queued deposit USDC, wallet-held vault shares, shares locked in a withdrawal or claimable USDC.
3. Verify the official Senior or Junior Vault address from the active deployment.
4. Check the transaction hash and Arbiscan result before submitting another transaction.
5. Check **Expected processing**, the final-five-minute cutoff, holder cooldown, oracle[^oracle] state, **Hourly processing paused**, **Safety restrictions** and **New withdrawal funding**.
6. Refresh the application and compare its values with onchain balances, events and request state.

Never send USDC directly to the liquidity pool. Never approve the liquidity pool, Margin Clearinghouse or an unknown contract when the intended spender is a Tranche Vault.

### Identify the balance or request

| What you see | What it represents | Which flow applies |
| --- | --- | --- |
| MockUSDC in the owner wallet | Tokens available to approve and queue into a vault | LP deposit flow |
| MockUSDC at the Trading Account address | Tokens controlled through the separate Trading Account | Margin Account flow, not an LP vault approval |
| Margin Account balance | Trader collateral held through the Margin Clearinghouse | Trader deposit or withdrawal flow |
| **Pending deposits** | USDC held by the selected vault for hourly processing | Cancel before processing, then move ready shares or return refundable USDC |
| Wallet-held `psLP` or `pjLP` | An active proportional claim on the selected tranche | LP position and withdrawal-request flow |
| **Pending withdrawals** | Shares locked by the vault while funding is pending | Cancel before the boundary, move ready USDC, or return a zero-value share remainder when offered |
| **USDC ready for wallet** | Funded withdrawal assets still held by the vault | **Move USDC to wallet** |

An LP deposit does not create Margin Account collateral. A Margin Account deposit does not create vault shares. The protocol does not automatically move value between those systems.

### Quick symptom guide

| Symptom | Check first | Safe next action |
| --- | --- | --- |
| `Vaults` is missing | Application hostname, cached build and route | Open the official Arbitrum Sepolia application, reload, and do not substitute the Perps-page Margin Account controls |
| USDC moved but no shares appeared | **Pending deposits** and transaction target | Confirm that **Queue deposit** succeeded; wait for processing, then use **Move shares to wallet** |
| Approval confirmed but no deposit exists | Whether the second wallet step confirmed | Return to the deposit preview and queue once; approval changes allowance only |
| **Review deposit** is disabled | Wallet balance, minimum, capacity and displayed deposit-closure reason | Correct the displayed issue or wait for the stated reopening condition |
| Deposit is past **Expected processing** | **Hourly processing paused**, pricing and safety state | Leave it queued; the holder does not submit a processing transaction |
| **Cancel deposit** is missing | Whether the request has reached its processing epoch | Wait for **Shares ready** or **Refund available** |
| **Shares ready** but wallet balance did not increase | The separate share claim | Select **Move shares to wallet** and confirm the transaction |
| **Shares available to withdraw** is zero | Wallet share balance and cooldown countdown | Wait for **Available in** to reach zero or return/move the required shares first |
| **Review withdrawal** is disabled | Share estimate, request limit and live data | Correct the displayed issue; separately remember that a sub-minimum partial request can pass review but still revert onchain |
| Withdrawal is past **Expected processing** | **New withdrawal funding**, Senior priority and processing status | Leave it queued; **Waiting for USDC** can persist beyond the displayed time |
| **Cancel withdrawal** is missing | Whether processing time has arrived | Monitor for **USDC ready** or **Shares ready to return** |
| **USDC ready** but wallet balance did not increase | The separate asset claim | Select **Move USDC to wallet** |
| **Shares ready to return** | A remaining share amount quoted to zero assets and entered refund state | Select **Return shares to wallet**; the one-hour cooldown restarts |
| Junior value is positive but its request waits | Senior-first funding and available liquidity | Monitor available liquidity; positive Junior value does not create funding ahead of Senior |
| Temporary fee is active | `oracleFrozen` and the tranche rate | Refresh the quote and wait for live pricing when the withdrawal is not urgent |
| **Older activity is unavailable** | Block-explorer request discovery | Select **Retry history**; the app still checks recent request IDs through the public lens |
| Share value declined | Pool revenue, trader payouts and the waterfall | Review performance and confirmed pool events; do not assume a display error |

### I used `Deposit`, but no vault shares appeared

First identify which `Deposit` action you used. The Perps-page action funds the Trading Account's Margin Account. A successful trader collateral deposit does not create Senior or Junior shares.

Every vault deposit is queued:

`Approve USDC when needed → Queue deposit → Pending → eligible processing → Shares ready → Move shares to wallet`

Check:

1. The transaction target is the selected Senior or Junior Vault.
2. **Approve USDC** and **Queue deposit** are separate wallet steps; both required steps confirmed.
3. **Pending deposits** shows the deposit reference and expected processing time.
4. Whether the request transaction was included onchain strictly before the five-minute cutoff or at/after it. Inclusion at or after the cutoff targets the following hour, even if the transaction was signed or sent earlier; use the confirmed request record.
5. **Hourly processing paused**, price freshness or a safety gate is not delaying processing.
6. **Shares ready** is followed by a successful **Move shares to wallet** transaction.

Do not queue another deposit until you know which transition the first transaction reached. See [**Deposit liquidity**](deposit-liquidity.md) and [**Manage a pending deposit**](manage-a-pending-deposit.md).

### `Review deposit` is disabled

The action panel shows a specific reason whenever possible. Common causes are:

* **Safety pause active** — new deposits are paused; withdrawal requests remain available when otherwise eligible.
* The live FX market is closed and fresh pricing is unavailable.
* The shared pool has an unresolved shortfall.
* **Safety restrictions** are active.
* The latest market price is too old.
* Senior has unrecovered losses.
* The selected vault has no current deposit capacity.
* The amount exceeds the owner-wallet balance or current vault limit.
* The amount is below the displayed vault deposit minimum.
* Required live data or the latest share estimate is unavailable.

Read both the reason and **Available again** message. Do not bypass a disabled action by transferring USDC directly to the vault or liquidity pool.

### My approval or deposit request failed

Check the owner wallet's MockUSDC and ETH balances, Arbitrum Sepolia network, selected tranche, verified vault spender, exact allowance, and each transaction receipt.

A deposit can require two wallet confirmations:

1. **Approve USDC** grants the selected vault the exact allowance.
2. **Queue deposit** transfers the requested USDC into vault custody and creates the request.

If approval succeeds and queueing fails, the USDC normally remains in the owner wallet while allowance remains. Inspect the failed receipt and latest deposit status before retrying. Do not approve an unfamiliar spender.

### I cannot cancel a pending deposit

**Cancel deposit** appears only while the request is still before its processing epoch. A successful cancellation returns the held USDC to the owner wallet and issues no shares.

After the expected processing epoch begins, the status becomes **Waiting for processing** and ordinary cancellation is unavailable. When LP settlement is enabled, a healthy keeper submits eligible work through the permissionless path; the current interface exposes no user processing action.

If the processed batch's aggregate deposit quote rounds to zero shares, the epoch is rejected and the status becomes **Refund available**. Select **Return USDC to wallet** and verify the receipt. Pause, stale pricing, impairment, caps and shortfalls ordinarily keep the deposit waiting or enable an exceptional mature cancellation instead of creating a refund.

### The processing time passed, but no shares appeared

The expected time is not a completion guarantee. Check:

* whether the request says **Waiting for processing**;
* whether **Hourly processing paused** is displayed;
* market-price freshness and live-pricing availability;
* safety restrictions, an unresolved pool shortfall or Senior impairment; and
* keeper and network availability.

There is no user processing button. Monitor the request and its eligibility gates. When the automated LP worker is enabled and healthy, it submits eligible processing; otherwise progress requires another permissionless caller. Once processing succeeds, **Shares ready** appears. Those shares already participate in vault performance while held by the vault, but they are not wallet-held until **Move shares to wallet** confirms.

### The share amount differs from the request estimate

**Estimated shares** is calculated before processing. The final batch result can differ because share price and pool economics can change while the request waits.

Compare the processed request and final share amount. Do not treat the preview or pending-card estimate as a guaranteed conversion rate.

### My shares do not appear after `Move shares to wallet`

1. Confirm that the transaction succeeded onchain.
2. Confirm the owner and recipient addresses.
3. Confirm the selected tranche and deposit reference.
4. Read the `psLP` or `pjLP` balance directly.
5. Refresh the application.
6. Confirm that the pending deposit cleared.

Moving shares to the wallet starts or restarts the one-hour withdrawal cooldown for the wallet's complete position in that vault. A zero **Shares available to withdraw** value immediately afterward is expected; use the displayed **Available in** countdown.

### My withdrawal request is pending

A confirmed **Queue withdrawal** transaction locks the estimated shares. It does not send USDC to the wallet.

![Pending deposit and withdrawal records with their current actions](../.gitbook/assets/screenshots/storybook-documentation-vaults--pending-activity.png)

Before the expected processing time, the request shows **Pending** and can show **Cancel withdrawal**. After that time:

* **Waiting for USDC** means the expected processing epoch has arrived or passed but no USDC has been allocated; another settlement gate may still be blocking funding.
* **USDC ready** means funded assets can be moved with **Move USDC to wallet**; a zero-value share remainder may also be returnable.
* **Shares ready to return** means a remaining share amount quoted to zero assets and can be reclaimed with **Return shares to wallet**.

Ordinary insufficient-liquidity remainders stay queued for later funding. A partially funded request can expose both wallet-move actions, with **USDC ready** shown as the status while any assets are claimable.

Do not submit a duplicate request for shares already locked in the vault.

### `Shares available to withdraw` is zero

Check these independent conditions:

* **Wallet share state:** queued or returnable shares are not wallet-held shares.
* **Cooldown:** receiving deposit shares, cancelled-withdrawal shares or a returned zero-value remainder starts or restarts the one-hour countdown for the complete tranche position.
* **Vault request limit:** current vault rules may make fewer shares eligible than the wallet balance.
* **Live data:** unavailable wallet or vault reads disable the preview.

The page shows **Available in** while the cooldown is active and **Withdrawal cooldown active** in the action panel. Wait for the countdown rather than estimating from transaction time.

Pool liquidity is a separate question. A positive share limit lets the holder queue a request; it does not guarantee that USDC will be allocated at the first eligible hour.

### Junior is waiting for USDC

Trader obligations and the liability-scaled settlement buffer are protected first. Among LP requests, matured Senior demand is funded before Junior. Once that queue is clear, Junior remains capped by free cash, Junior principal and the governed Senior-share ratio.

This can leave a Junior request at **Waiting for USDC** while Junior shares still have positive value. Monitor **Available liquidity**, **Reserved for trader withdrawals**, **Available withdrawal liquidity** and **New withdrawal funding**. There is no control that bypasses Senior priority and no guaranteed funding time.

### The network-switch control appears, `Review withdrawal` is disabled or the quote changed

On the wrong network, the primary control is **Switch to Arbitrum Sepolia**, not a disabled **Review withdrawal** button. Switch first; the review control returns only after the connected wallet is on the required network.

The withdrawal form accepts a target USDC amount and converts it to **Estimated shares used**. Check:

* the amount is positive and valid;
* the estimate is available;
* estimated shares do not exceed **Shares available to withdraw**;
* the cooldown has expired;
* the required action data is available.

A partial withdrawal request must estimate to at least the vault's live minimum, currently `1 USDC`. The contract allows a smaller dust amount only when the request exits all remaining requestable shares. The interface may not catch this minimum before submission, so a reviewable amount can still revert onchain.

The quote is refreshed before the preview opens. If it changes or cannot be refreshed, use the latest estimate and do not rely on an older preview.

### I cannot cancel a withdrawal

**Cancel withdrawal** is available only while locked shares remain pending before their processing epoch. Once the expected processing epoch has begun, ordinary cancellation is unavailable even when the status is **Waiting for USDC**.

If cancellation succeeds, **Return shares to wallet** is not needed: the cancellation transaction returns the shares and restarts the one-hour cooldown. Verify the wallet share balance and **Available in** countdown before attempting another request.

### `USDC ready` did not change my wallet balance

Eligible funding and wallet delivery are separate. Select **Move USDC to wallet**, confirm the owner-wallet transaction, then verify:

* the Arbiscan receipt targets the selected vault;
* the wallet USDC balance increased;
* **USDC ready for wallet** decreased; and
* the corresponding pending request cleared or shows only a remaining portion.

Do not infer wallet receipt from **USDC ready** alone.

### `Shares ready to return` is shown

The remaining shares quoted to zero assets during settlement and entered the terminal refund state. Select **Return shares to wallet** and confirm the owner-wallet transaction. A remainder that merely lacks current liquidity stays queued instead.

Returning shares restarts the one-hour cooldown for every share in the wallet's position in that tranche. The returned shares remain exposed to subsequent vault gains and losses.

### A frozen-pricing withdrawal uses more shares than expected

When `oracleFrozen` is active, the action panel shows **Temporary withdrawal surcharge active** with the tranche's current rate. The preview includes that current fee in the share quote.

For the same target USDC, the frozen-pricing quote can require more shares than an ordinary quote. The request locks that quoted share amount. Those shares remain exposed to share-price and fee-state changes while queued, so final USDC is set at processing and can differ from the target; the vault does not pull extra shares later. Senior and Junior can use different rates.

A scheduled close-only period does not activate the surcharge by itself. If pricing becomes too old for the bounded frozen policy, the request can wait for valid pricing. When the withdrawal is not urgent, wait for live pricing and request a fresh quote.

### A pause or safety restriction is active

Do not assume every pause disables every vault action:

* **Hourly processing paused:** new deposit and withdrawal requests can still be submitted when other limits permit; pre-boundary cancellations and already-ready move or return actions remain available. New deposits do not start earning and withdrawals receive no new funding until processing resumes.
* **Safety pause active:** new deposits are disabled, but otherwise eligible withdrawal requests remain available.
* **Safety restrictions active** (degraded mode): deposits are disabled; otherwise eligible withdrawal requests remain available, but no new withdrawal USDC is allocated until effective solvency recovers and the protocol owner explicitly clears degraded mode. Already-funded actions remain usable.
* **Live pricing unavailable:** deposits are disabled; withdrawal requests can remain available with a temporary surcharge under the bounded frozen-pricing rules.

Read **New withdrawal funding** and the individual request status to distinguish request submission from eligible funding.

### `Older activity is unavailable`

Request discovery uses block-explorer history plus recent onchain request IDs. If explorer discovery fails, the page warns that older unfinished activity is unavailable but continues checking recent requests through the public lens.

Select **Retry history**. Also verify the wallet address, selected tranche, request reference and transaction directly onchain before assuming an older request is gone.

### My share value declined

Vault shares are not fixed at one USDC. Value can fall through trader profits and rebates, liquidation shortfalls, bad debt, operational failures and losses assigned through the waterfall. Junior also funds the Senior target return and pays its configured annual vault fee through share issuance.

Plether uses one signed Terminal NAV snapshot: marked trader gains reduce LP value, while marked trader losses can increase it only up to a collateral- and claim-capped collectible amount. That receivable is not physical cash until collected, so share value and available withdrawal liquidity can differ.

Review [**Understand LP returns and share value**](understand-lp-returns-and-share-value.md), [**LP risks and safeguards**](lp-risks-and-safeguards.md) and confirmed pool events before treating a decline as an interface error.

### Senior return is below the target

The Senior return is targeted, not guaranteed. It is funded from available Junior capital. If Junior cannot fund the complete amount, Senior receives only what is available, and an unpaid portion does not become accumulating debt.

Senior can also lose value after Junior is exhausted. Check **Current Senior capital**, **Protected balance**, **Amount still to recover** and **Junior capital**.

### Pool values do not match my position

Read each metric as a separate question:

* **Total pool funds** is canonical physically backed pool depth: the smaller of raw pool assets and accounted assets, excluding quarantined excess; it is not necessarily the literal token balance.
* **Available liquidity** is cash remaining after protected amounts.
* **Reserved for trader withdrawals** is set aside ahead of LP funding.
* **Current value** is the accounting value of the wallet-held vault shares.
* **Shares available to withdraw** is the share request limit after holder-level rules.
* **Available withdrawal liquidity** is the selected tranche's estimated pool-level funding capacity.
* **USDC ready for wallet** has already been allocated to processed withdrawals.

None of these values is interchangeable with another.

### My vault transaction is pending or failed

1. Check the transaction hash in Arbiscan.
2. Confirm the connected owner wallet, Arbitrum Sepolia network and selected vault.
3. Identify the exact step: **Approve USDC**, **Queue deposit**, **Queue withdrawal**, cancellation, **Move shares**, **Move USDC**, or a return action.
4. Compare the onchain allowance, request state, wallet balances and vault balances with the intended transition.
5. Submit a fresh transaction only after the first transaction's result is known.

A successful approval changes allowance only. A successful request creates a queued balance only. Eligible processing creates ready shares or USDC and can terminally refund a zero-value remainder; the separate holder action moves that value to the wallet.

### When reporting a problem

Collect:

* transaction hash;
* connected owner-wallet address;
* network;
* Senior or Junior Vault address;
* action and amount;
* deposit or withdrawal reference;
* **Expected processing** and current status;
* **Current value**, **Shares available to withdraw** and **USDC ready for wallet**;
* **Available liquidity**, **Reserved for trader withdrawals**, **Available withdrawal liquidity** and **New withdrawal funding**;
* cooldown expiry;
* oracle state and temporary fee;
* exact interface or contract error.

Never share a private key, seed phrase or unrelated wallet signature.

For the complete operational path, return to [**Liquidity provider quickstart**](../liquidity-provider-quickstart.md). For the canonical mechanics, see [**The liquidity pool and tranche waterfall**](../how-plether-works/the-liquidity-pool-and-tranche-waterfall.md).

[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^lp]: Liquidity provider, a participant that supplies USDC capital to the liquidity pool.
[^oracle]: A service that supplies external market data to smart contracts; Plether uses Pyth price feeds.
