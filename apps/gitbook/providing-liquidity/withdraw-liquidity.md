# Withdraw liquidity

Withdrawing liquidity exchanges active Senior or Junior Vault shares for USDC[^usdc]. It is a tranche-vault action, not a withdrawal from the Trading Account's Margin Account.

Every LP[^lp] withdrawal uses the hourly vault queue. The request locks the quoted number of vault shares, but it does not send USDC to the owner wallet. When LP settlement is enabled, a healthy keeper can process eligible requests; Senior receives funding priority, and the holder moves funded USDC to the wallet in a separate transaction.

> **Share value and withdrawal liquidity are different**
>
> A vault position can have positive accounting value while no shares are currently eligible for a new request or no USDC is available to fund it. Trader obligations rank ahead of both vaults, and Senior ranks ahead of Junior when the remaining LP liquidity is allocated.

> The `Withdraw` action on the Perps page withdraws from the Trading Account's **Margin Account**. It does not redeem Senior or Junior Vault shares.
>
> Vault actions use the connected owner wallet. The current Vaults flow does not sponsor them, so keep enough Arbitrum Sepolia ETH for the request and any later claim, cancellation or share-return transaction.

### 1. Check what you are withdrawing

Before opening the withdrawal flow, confirm that you hold wallet shares in the intended tranche.

| What you hold | Can it fund a new withdrawal request? |
| --- | --- |
| Wallet-held Senior shares (`psLP`) | Yes, up to **Shares available to withdraw** |
| Wallet-held Junior shares (`pjLP`) | Yes, up to **Shares available to withdraw** |
| USDC in a queued deposit | No; it must be processed into shares or returned |
| **Shares ready** from a processed deposit | Not until you select **Move shares to wallet** and the resulting cooldown ends |
| Shares already locked in a queued withdrawal | No; monitor that request under **Pending withdrawals** |
| USDC in the Trading Account's Margin Account | No; use the separate trader withdrawal flow |
| MockUSDC in the owner wallet | It is already in the wallet and is not an LP position |

On the vault page, read these values separately:

* **Current value** estimates the USDC accounting value of the wallet-held shares.
* **Shares available to withdraw** is the share amount currently eligible for a new request after the holder cooldown and vault limits.
* **Estimated withdrawal liquidity** in the vault header and **Available withdrawal liquidity** in **Overview** estimate how much USDC the selected tranche can receive at the next processing time.
* **USDC ready for wallet** is already funded and can be claimed from a processed request.

Positive **Current value** does not guarantee positive **Shares available to withdraw**, and neither value means USDC is already allocated.

### 2. Read the live request limit

The withdrawal form accepts a target USDC amount. The vault quote converts that amount into **Estimated shares used**, and the interface checks the estimate against **Shares available to withdraw**.

The form rejects an amount when:

* it is zero or cannot be parsed;
* the latest share estimate is unavailable;
* the estimated shares exceed the wallet's current request limit; or
* required pool, vault or wallet data is unavailable.

A partial withdrawal request must estimate to at least the vault's live minimum, currently `1 USDC`. A complete exit of all remaining requestable shares may use the contract's smaller dust-exit exception. The current interface may not prevalidate this minimum, so an enabled review action is not by itself proof that a smaller partial request will succeed onchain.

The limit is a request ceiling, not a promise of funding at the next hour. Share price, protected reserves and pool liquidity can change after the request is queued.

### 3. Understand hourly processing

The vault assigns every request to an hourly processing time.

| Onchain inclusion time | Expected processing |
| --- | --- |
| Strictly before the five-minute cutoff | The next hourly processing time |
| At or after the five-minute cutoff | The following hourly processing time |
| Processing paused or a required gate blocked | Delayed until processing can resume |

The displayed **Expected processing** time is an estimate, not a guaranteed payout time. A request can remain **Waiting for USDC** after that time if the pool cannot yet fund it.

The contract uses the request transaction's block-inclusion timestamp. Signing or sending before the cutoff is not enough if inclusion lands at or after it; treat the confirmed request record as authoritative.

From the holder's perspective, the lifecycle is:

`Review withdrawal → Queue withdrawal → Pending → eligible hourly settlement → USDC ready → Move USDC to wallet`

The holder does not submit a separate processing transaction. While the request waits, its locked shares continue to gain or lose value, so the final USDC can differ from the amount shown in the request preview.

### 4. Understand the withdrawal firewall and tranche priority

Plether reserves cash for trader obligations before allocating funds to LP withdrawals.

Conceptually:

```
Available LP liquidity
= physical pool assets
− protected trader and protocol reserves
```

Protected amounts include maximum bounded trader liability, its configured liability-scaled settlement buffer, existing trader claims, USDC already set aside for trader claims, unassigned assets and any explicit protocol reserve.

The `Vaults` page surfaces these inputs through **Total pool funds**, **Available liquidity**, **Reserved for trader withdrawals**, **Estimated withdrawal liquidity** and **Available withdrawal liquidity**. None of those values is the complete amount every LP can withdraw.

After protected obligations are accounted for:

1. Matured Senior withdrawal requests receive funding first, up to their demand, Senior principal and available USDC.
2. Once no matured Senior backlog remains, Junior is capped by remaining free cash, Junior principal and the governed maximum Senior share of protected tranche capital. Dormant Senior principal is not fully reserved.

This is why Junior can show positive share value while a matured request remains **Waiting for USDC**. Senior priority is relative, not a guaranteed Senior payout at the first eligible hour.

For the complete accounting model, see [**The liquidity pool and tranche waterfall**](../how-plether-works/the-liquidity-pool-and-tranche-waterfall.md#the-withdrawal-firewall).

### 5. Check the one-hour cooldown

The vault's **Move shares to wallet**, **Cancel withdrawal** and **Return shares to wallet** actions start or restart a one-hour withdrawal cooldown for the wallet's complete position in that tranche. This includes:

* selecting **Move shares to wallet** after a deposit is processed;
* cancelling a queued withdrawal and returning its shares; and
* selecting **Return shares to wallet** for a zero-value withdrawal remainder.

During the cooldown, **Shares available to withdraw** is zero. The vault page shows **Available in** with a live countdown, and the action panel shows **Withdrawal cooldown active**.

An ordinary wallet-to-wallet share transfer is possible only after the sender's cooldown and propagates that timestamp rather than starting a fresh one-hour period. Wait for the displayed countdown to reach zero before entering a new withdrawal amount. Queuing a withdrawal or moving funded USDC to the wallet does not itself return shares and should not be treated as a cooldown restart.

### 6. Read pauses, degraded mode and frozen pricing correctly

Several states affect submission and processing differently.

| State | New withdrawal request | Funding at hourly processing | Existing actions |
| --- | --- | --- | --- |
| **Hourly processing paused** | Still allowed when the holder limit and quote permit | No new USDC is allocated until processing resumes | Ready funds can be moved; eligible requests can be cancelled; returnable funds or shares can be claimed |
| **Liquidity-pool safety pause** | Still allowed when the holder limit and quote permit | Follow **New withdrawal funding** and the request status | The pause blocks new deposits, not the protective withdrawal controls by itself |
| **Safety restrictions active** (degraded mode) | Still allowed by the interface when the holder limit and quote permit | No new USDC is allocated until effective solvency recovers and the protocol owner explicitly clears degraded mode | Deposit requests are blocked; already-funded withdrawal actions remain available |
| **Live pricing unavailable** (`oracleFrozen`) | Can remain available under the bounded frozen-pricing rules | The current quote can include a temporary tranche-specific fee; later fee or price changes affect the final USDC from the fixed queued shares | Deposits are unavailable; wait for live pricing when the withdrawal is not urgent |

The action panel shows **Temporary withdrawal surcharge active** when frozen pricing is active. Its current percentage is tranche-specific, and the current quote uses more shares for the same target USDC. The request then locks that quoted share amount. If the fee or share price changes before processing, the final USDC changes; the vault does not pull additional shares from the wallet.

A close-only trading schedule does not by itself activate this fee. If accepted price data becomes too old even for the frozen-pricing rules, quoting or processing can wait until valid data returns.

### 7. Submit the withdrawal request

1. Open `Vaults` and select **Explore Senior Vault** or **Explore Junior Vault**.
2. Confirm **Current value**, **Shares available to withdraw**, **Estimated withdrawal liquidity**, **Available withdrawal liquidity** and the pool status.
3. Select the `withdraw` mode. The panel heading becomes **Withdraw USDC**.
4. Enter **Amount to withdraw**.
5. Check **Estimated shares used**, **Position value**, **Estimated USDC you’ll receive**, **Processing** and any active temporary pricing fee.
6. Select **Review withdrawal**.
7. In the preview, verify **USDC to withdraw**, **Estimated shares used**, **Current share price**, **Processing** and **Expected processing**. When complete history is available, it also shows **7d realized APY**.
8. Select **Confirm withdrawal**, then confirm **Queue withdrawal** in the owner wallet.
9. Keep the modal open until it reports **Withdrawal submitted**, then select **View activity** or open **Your position**.

![Withdrawal preview](../.gitbook/assets/screenshots/storybook-documentation-vaults--withdrawal-preview.png)

The request transaction targets the selected Tranche Vault and locks the quoted shares. Do not send shares to the liquidity pool, Margin Clearinghouse or an unknown contract.

### 8. Monitor or cancel the queued request

Under **Your position → Pending withdrawals**, each request shows a reference, requested shares, **Expected processing** and **Estimated USDC**.

Before its processing time, the status is **Pending** and **Cancel withdrawal** is available. Cancelling returns the locked shares to the owner wallet and restarts the one-hour cooldown for the complete tranche position.

Once the request reaches its processing time, ordinary cancellation is no longer available. If USDC has not been allocated, the status becomes **Waiting for USDC** even when a pause, pricing or health gate means funding is not yet eligible. Leave the request queued and monitor the pool; do not submit a duplicate request for the same locked shares.

### 9. Move funded USDC or returned shares

Eligible processing can produce these actionable states:

| Status | Meaning | Action |
| --- | --- | --- |
| **USDC ready** | USDC has been allocated to all or part of the request; a zero-value remainder can also be returnable | **Move USDC to wallet** |
| **Shares ready to return** | A remaining share amount quoted to zero assets and entered the terminal refund state | **Return shares to wallet** |

These are separate owner-wallet transactions and can coexist after partial funding. **USDC ready** takes status precedence when any assets are claimable. Ordinary insufficient-liquidity remainders stay FIFO-queued for later funding; only a zero-value remainder becomes returnable. **USDC ready** is not part of the wallet balance until **Move USDC to wallet** confirms. Returning shares restarts the one-hour cooldown; moving USDC does not.

After each action, verify the transaction on Arbiscan, the remaining request state, the wallet's USDC or share balance, and the vault position.

### If funding takes longer than expected

Do not cancel and resubmit after the request has become **Waiting for USDC**; the ordinary cancellation action is no longer available. Funding can improve as trader liabilities are released, cash enters the liquidity pool or valid pricing returns. Paying a trader claim from existing pool cash reduces cash and the claim together; it does not by itself create net LP withdrawal liquidity.

There is no guaranteed funding date. Junior requests can wait longer because Senior is funded first. Use [**LP troubleshooting**](lp-troubleshooting.md) if the request status or available action does not match the expected lifecycle.

### Exit checklist

Before queuing:

* Confirm this is a Senior or Junior Vault action, not a Margin Account withdrawal.
* Confirm the owner wallet and Arbitrum Sepolia network.
* Check the cooldown countdown and **Shares available to withdraw**.
* Compare the target USDC with **Estimated shares used**.
* Review available pool liquidity, protected reserves and tranche priority.
* Check **New withdrawal funding**, hourly-processing status and safety restrictions.
* Review frozen-pricing state and any temporary surcharge.
* Verify the Tranche Vault address.
* Keep enough ETH for the request and later claim or return transaction.
* Remember that the preview is an estimate and the request can wait beyond its expected processing time.

LP withdrawals can be delayed or partially funded; a zero-value remainder can be returned, and vault shares can lose value while a request waits. Review [**LP risks and safeguards**](lp-risks-and-safeguards.md) before treating tranche value as available cash.

[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^lp]: Liquidity provider, a participant that supplies USDC capital to the liquidity pool.
