# Manage a pending deposit

When trader positions are open, Plether routes ordinary LP[^lp] deposits through a **pending deposit epoch**. The request moves USDC[^usdc] into the selected tranche[^tranche] vault, but it does not immediately create wallet-held shares or an immediately withdrawable LP position.

A pending deposit has five distinct stages:

```text
Request → Escrow → Activation → Finalization (claimable shares in escrow) → Claim (shares in owner wallet)
```

No shares exist for the request before finalization. Finalization mints the batch shares into vault escrow, where the claimable allocation is already exposed to later tranche share-price movements. Until you claim, however, those shares are not in your owner wallet and are not available for withdrawal.

> **Current interface status**
>
> The `Vaults` interface is under development and is not yet part of the published testnet application. Immediate deposits and synchronous withdrawals exist only on the current development branch; the pending lifecycle remains preview-only. That preview can identify **Pending deposit epoch** as the available path and show an **Epoch preview** with indicative shares and a generic note that activation depends on epoch-request eligibility. It does not yet escrow USDC or discover requests, cancel them, finalize epochs or claim shares. The submission control is disabled and marked **Lifecycle coming soon**, with a **Pending lifecycle not enabled** warning.
>
> Control names in square brackets on this page describe the intended flow. Do not try to reproduce them with the Margin Account’s existing `Deposit` control; that funds a trader account, not a tranche vault.
>
> LP approvals, requests, finalization, cancellation and claims are not currently gas-sponsored. When the controls become available, keep enough network ETH for every transaction you may need.

> **Screenshot placeholder — pending deposit lifecycle**
>
> Add a screenshot of **Vaults → Your position** showing the epoch ID, requested amount, activation time, lifecycle status and the next available action. Do not capture the interface until the pending lifecycle is enabled.

### When Plether uses a pending epoch

The protocol allows an immediate deposit only when no trader positions are open and every deposit safety gate passes. Once one or more positions are open and the pending-request gates pass, the normal route is `[Request deposit]`.

This separation prevents new shares from being priced against an incomplete view of unrealized trader losses. Plether can reserve unrealized trader profits as liabilities, but it does not treat unrealized trader losses as spendable LP assets before collecting them.

| Market and pool condition | Deposit path | When you receive shares |
| --- | --- | --- |
| No trader positions are open and every safety gate passes | Immediate | In the deposit transaction |
| One or more trader positions are open and the pending-request gates pass | Pending epoch | After activation, finalization and claim |
| Deposits are paused, Senior is impaired or another deposit gate fails | Unavailable | No shares are issued |

For the broader pricing rationale, see [The HousePool and tranche waterfall](../how-plether-works/the-housepool-and-tranche-waterfall.md#why-deposit-and-withdrawal-pricing-differ).

### Read the lifecycle correctly

| Stage | Where the value is | Can you normally cancel? | What happens next |
| --- | --- | --- | --- |
| **Requested, before activation** | USDC in vault escrow | Yes | Wait for activation or cancel |
| **Active, awaiting finalization** | USDC in vault escrow | No | Anyone may finalize the epoch |
| **Finalized, unclaimed** | Claimable batch shares in vault escrow; their USDC value moves with the tranche share price | No | You claim your proportional shares |
| **Claimed** | Shares in your owner wallet | Not applicable | The wallet-held shares can enter the normal withdrawal flow |

Senior impairment creates one important exception: if it prevents an active epoch from finalizing, the special cancellation path becomes available again.

### 1. Request the deposit

Before requesting, confirm:

* The selected vault is **Senior** or **Junior**, as intended.
* The spender in any USDC approval is the verified tranche vault.
* The preview says **Pending deposit epoch** rather than **Immediate deposit**.
* Your owner wallet has the deposit USDC and enough network ETH for the approval and request.
* You understand the estimated activation time and the point after which ordinary cancellation stops.

If the vault allowance is insufficient, approving USDC and submitting `[Request deposit]` are separate transactions.

After a successful request:

* The USDC leaves your wallet immediately.
* The selected vault holds it in escrow.
* The request receives an epoch ID.
* You do not yet hold active vault shares.
* The escrowed USDC does not yet earn the Senior coupon or Junior residual return.
* The request-time share estimate remains provisional.

The current contracts use one-hour epoch identifiers and assign requests two epoch IDs ahead. Depending on when you submit during the current hour, activation is roughly one to two hours later. Treat the displayed activation time as the operational reference.

Record the request transaction and epoch ID. They are the fastest way to distinguish a valid pending request from an approval that succeeded without a request.

### 2. Use the cancellation window

Before the activation epoch begins, the request owner can normally select `[Cancel request]`.

A successful cancellation:

* removes the request;
* returns the complete escrowed USDC amount to the owner wallet; and
* issues no tranche shares.

Check the transaction result and wallet balance before submitting a replacement request.

The cancellation boundary is the activation epoch, not finalization. Waiting until the batch is ready to finalize is too late for ordinary cancellation.

### 3. Understand activation

When the activation epoch begins, the request normally becomes binding:

* ordinary cancellation becomes unavailable;
* the USDC remains in vault escrow;
* the batch price is still not fixed; and
* no active shares have been delivered.

Activation only makes the epoch eligible for finalization. Time passing does not complete the remaining lifecycle for the depositor: the epoch must still be finalized, then the depositor must claim.

### 4. Finalize the epoch

Epoch finalization is permissionless. The application, a keeper[^keeper], the depositor or any other user may submit it once the epoch is eligible and the safety checks pass.

Finalization performs the batch transition:

1. Reconcile the HousePool and tranche accounting.
2. Fix one share price for the complete epoch.
3. Apply the tranche’s active oracle-frozen[^oracle] surcharge, if any.
4. Move the batch USDC from vault escrow into the HousePool.
5. Mint the batch shares into vault escrow.

Every depositor in the epoch receives shares from the same batch calculation, proportional to their funded request.

Finalization does not send those shares directly to each wallet. It only makes the depositor’s share allocation claimable.

#### Finalization is permissionless, not guaranteed to be immediate

The current design does not assign a separate protocol bounty to epoch finalization. Permissionless means that anyone *can* finalize; it does not promise that a particular actor will do so at the first eligible block.

If an activated epoch has not finalized:

* do not submit a second deposit merely because the first one is still waiting;
* check whether the activation time has actually passed;
* check the oracle and protocol state;
* check for Senior impairment; and
* use `[Finalize epoch]` only when the interface enables it and its preview passes.

#### The final price can differ from the request estimate

The request commits USDC before its exact share amount is known. The final number of shares can change because:

* tranche accounting changes before finalization;
* trader outcomes alter the value reconciled into the waterfall;
* the Senior coupon or restoration state changes; or
* an oracle-frozen surcharge is active at finalization.

Transaction ordering is therefore a residual risk. Events included before the finalization transaction can affect the batch price; events ordered after it are not part of that finalization checkpoint. For example, an epoch could be finalized immediately before a transaction realizes a large trader loss.

There is no request-time price lock and no request-time lock on the frozen-oracle surcharge.

### 5. Claim the finalized shares

After the onchain epoch has finalized—and once the future interface exposes the claimable allocation:

1. Open the pending epoch under **Vaults → Your position**.
2. Confirm the funded amount and claimable share quantity.
3. Select `[Claim shares]`.
4. Confirm the transaction.
5. Verify that the shares appear under your active tranche position.
6. Confirm that the pending request no longer shows an unclaimed balance.

Claiming does not create a separate interest payment. Senior coupon, realized HousePool/LP-owned revenue and losses are reflected through the USDC value of the active vault shares.

If the epoch is finalized but your active share balance is unchanged, first check for a separate claim action. Finalization and claim are intentionally different transactions.

### Cancellation after Senior impairment

Senior is impaired when its accounting principal is below its protected high-water mark. Ordinary deposits into both tranches are blocked during impairment.

If Senior impairment prevents an already active epoch from finalizing, depositors regain a special cancellation route. This exception allows the escrowed USDC to be recovered even though the ordinary pre-activation window has closed.

Use the impairment cancellation only when the interface identifies that condition. A delayed finalizer, an over-stale oracle or another failed safety gate does not by itself imply that post-activation cancellation is available.

After a successful impairment cancellation:

* verify that the request has cleared;
* verify that the escrowed USDC returned to the owner wallet; and
* do not expect tranche shares.

### Oracle state and pending epochs

A scheduled close-only period with a live oracle does not activate an LP surcharge. Normal epoch rules continue.

When the onchain `oracleFrozen` state is active:

* pending epochs remain the ordinary route while trader positions are open;
* finalization may continue while the stored mark remains within the extended frozen-market freshness limit; and
* the surcharge used for the batch is the live tranche rate at finalization.

If the mark becomes too old even for frozen-oracle policy, finalization can be blocked until acceptable data or a protocol recovery path becomes available. See [Market states and oracle closures](../how-plether-works/market-states-and-oracle-closures.md#what-closures-mean-for-lps).

### Troubleshoot by lifecycle stage

| What you see | Likely meaning | What to check |
| --- | --- | --- |
| USDC approval confirmed, but no epoch appears | Only the allowance transaction completed | Look for a separate funded request transaction before retrying |
| USDC left the wallet, but active shares are zero | The request is still in escrow or shares remain unclaimed | Check the epoch ID, activation, finalization and claim status |
| `[Cancel request]` is unavailable | The activation epoch has probably begun | Confirm the epoch boundary; ordinary cancellation stops at activation |
| Activation time passed, but the epoch is not finalized | Finalization has not been submitted or a gate is failing | Check oracle freshness, protocol state and Senior impairment |
| Finalization simulation fails during Senior impairment | Deposits cannot be activated into an impaired stack | Use the special impairment cancellation path when offered |
| Finalization is blocked by an over-stale mark | Trader liabilities cannot be reconciled with acceptable oracle data | Wait for eligible data or protocol recovery; do not assume cancellation is available |
| Final shares differ from the preview | Request-time shares were only an estimate | Review the batch price and any frozen-oracle surcharge at finalization |
| Epoch says finalized, but shares are missing | The allocation is still in vault escrow | Submit `[Claim shares]` and verify its receipt |
| A pending amount does not appear in **Current value** | Pending USDC is not an active tranche claim | Track it in the pending-epoch section until claim completes |

### Before you stop monitoring a request

Confirm one—and only one—terminal outcome:

* **Cancelled:** the escrowed USDC returned and no shares were issued.
* **Claimed:** the pending entry cleared and active vault shares appeared.

An approval receipt, an activation timestamp or an epoch-finalization transaction is not by itself the final depositor outcome.

For interpreting the resulting position, continue to [Read your LP position and pool health](read-your-lp-position-and-pool-health.md). For the complete accounting model, see [The HousePool and tranche waterfall](../how-plether-works/the-housepool-and-tranche-waterfall.md).

[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^keeper]: A permissionless actor or bot that submits order-finalization or protocol-maintenance transactions.
[^oracle]: A service that supplies external market data to smart contracts; Plether uses Pyth price feeds.
