# Withdraw liquidity

Withdrawing liquidity exchanges active Senior or Junior Vault shares for USDC[^usdc]. It is a tranche-vault action, not a withdrawal from the Trading Account's Margin Account.

LP[^lp] withdrawals are **synchronous**. There is no LP withdrawal queue: a permitted withdrawal confirms and sends USDC directly to the recipient, while an amount above the current limit cannot be submitted.

> **Synchronous does not mean always available.**
>
> Your share value can be positive while your current withdrawal limit is lower—or zero. Plether allows only physically free HousePool cash to leave after reserving for trader obligations and applying Senior–Junior priority.

> **Current interface status**
>
> The `Vaults` interface is under development and is not yet part of the published testnet application. Immediate deposits, pending-deposit lifecycle actions and synchronous withdrawals are available on the current development branch. The labels on this page—such as `Withdraw USDC` and `Withdrawal preview`—describe that in-progress interface and should be treated as placeholders until it is deployed.
>
> The visible `Withdraw` action on the current Perps page withdraws from the Trading Account's **Margin Account**. It does not redeem Senior or Junior Vault shares.
>
> LP withdrawals are not currently gas-sponsored. Keep enough Arbitrum Sepolia ETH in the connected owner wallet for the transaction, and treat an LP action as sponsored only if the interface explicitly marks that action as **Sponsored**.

### 1. Check what you are withdrawing

Before opening the withdrawal flow, confirm that you hold **active** shares in the intended tranche.

These are different states:

| What you hold | Can it be withdrawn through the LP withdrawal flow? |
| --- | --- |
| Active Senior Vault shares | Yes, subject to the live Senior limit and holder gates |
| Active Junior Vault shares | Yes, subject to the live Junior limit and holder gates |
| A pending deposit request | No; the request must be cancelled while eligible or finalized and claimed |
| Unclaimed shares from a finalized epoch | No; claim the shares first |
| USDC in the Trading Account's Margin Account | No; use the separate trader withdrawal flow |
| MockUSDC in the owner wallet | It is already in the wallet and is not an LP position |

The tranche position should distinguish:

* `Current value` — the current USDC accounting value of your shares;
* `Share price` — the current accounting value per share;
* `Withdrawable now` — the current asset-denominated limit for your position.

`Current value` and `Withdrawable now` answer different questions. A tranche share is a claim on tranche accounting value, not an unconditional claim on an equal portion of the HousePool's raw USDC balance.

### 2. Read the live maximum

The tranche vault exposes two ERC-4626[^erc4626] limits conceptually:

* `maxWithdraw` answers how much USDC the holder may currently request through the asset-denominated withdrawal path.
* `maxRedeem` answers how many shares the holder may currently submit through a share-denominated redemption path.

The in-progress `Vaults` interface accepts a USDC amount and uses the withdrawal path. It does not currently provide a share-count or **Redeem** control. `maxRedeem` is still useful for understanding the underlying vault rule, but LPs should use `Withdrawable now` for the interface's live USDC limit.

Both limits are ceilings, not guarantees that a previously viewed amount will remain available. The vault reconciles HousePool accounting when the withdrawal is submitted, so the live limit can change between opening the preview and confirming the transaction.

During an active holder cooldown, both `maxWithdraw` and `maxRedeem` return zero.

> A withdrawal preview does not reserve liquidity or create a place in a queue. If conditions change before confirmation, request a fresh preview.

### 3. Understand the withdrawal firewall

Plether reserves cash for trader obligations before allowing LP capital to leave.

Conceptually:

```
Free USDC
= physical HousePool assets
− withdrawal reserves
```

Withdrawal reserves include:

* Maximum bounded trader liability
* Existing trader claims
* USDC already set aside for trader claims
* Unassigned assets
* Any additional explicit protocol reserve

Only the remaining physically free USDC can support LP withdrawals. This protection is the **withdrawal firewall**.

The `Vaults` pool metrics should help separate the inputs:

* `Free liquidity` is HousePool cash remaining after protected reserves.
* `Withdrawal reserve` is the cash Plether is retaining for protected obligations.
* `Pool withdrawal cap` is the current tranche-level ceiling before applying your personal share balance and holder gates.
* `Market state` summarizes degraded mode, oracle-frozen state and mark freshness. It does not by itself prove that a withdrawal is permitted; also require a fresh positive `Withdrawable now` value and an enabled action.

Free liquidity is not total HousePool assets, total tranche NAV[^nav] or the amount every LP can withdraw.

For the full accounting model, see [**The HousePool and tranche waterfall**](../how-plether-works/the-housepool-and-tranche-waterfall.md#the-withdrawal-firewall).

### 4. Apply Senior–Junior withdrawal priority

Trader obligations rank ahead of both tranches. Senior–Junior priority applies only after those obligations have been reserved.

At pool level, Senior receives first access to free LP cash:

```
Senior maximum withdrawal
= min(free USDC, Senior principal)
```

Junior can withdraw only from free liquidity above the complete Senior claim:

```
Junior maximum withdrawal
= min(
    Junior principal,
    max(free USDC − Senior principal, 0)
  )
```

Your personal limit is then further constrained by:

* Your share balance
* Your holder cooldown
* Current oracle and protocol state
* The tranche's active frozen-oracle surcharge

This is why:

* Senior `Withdrawable now` can be below Senior `Current value`.
* Junior `Withdrawable now` can be zero even while Junior shares retain positive value.
* An increase in free liquidity can restore some withdrawal capacity without changing your share count.

Senior has relative withdrawal priority, not an unconditional right to immediate cash.

### 5. Check the cooldown and minimum

An immediate deposit starts a fixed **one-hour withdrawal cooldown** for the holder. Depositing more into the same vault refreshes the applicable cooldown.

During the cooldown:

* Shares cannot be withdrawn.
* Shares cannot be transferred to bypass the restriction.
* `maxWithdraw` and `maxRedeem` return zero.

A later share transfer propagates the relevant cooldown timestamp to the receiver.

Every successful withdrawal or redemption resets the holder's one-hour cooldown. After a partial withdrawal, the remaining shares therefore normally need to complete another cooldown before the next withdrawal.

Ordinary deposits and partial withdrawals are subject to the vault's live minimum. The current development frontend enforces at least `1 USDC` for an ordinary withdrawal; the deployed interface and onchain vault rules are authoritative.

A complete residual exit remains possible when the holder's entire remaining claim is below `1 USDC`. This is a dust-exit exception for closing the complete position; it does not permit an arbitrary sub-minimum partial withdrawal.

### 6. Review a frozen-oracle surcharge

A scheduled close-only runway does not, by itself, activate an LP surcharge. The surcharge applies only while the onchain `oracleFrozen` state is active.

During that state, a withdrawal may remain available under the extended frozen-market freshness policy. Each tranche applies its own configured rate.

The current `Vaults` interface uses the asset-denominated `withdraw(assets)` path: the amount entered is the target USDC wallet receipt. While the surcharge is active:

* Delivering that target USDC amount requires burning more shares than the ordinary quote.
* The additional economic value remains inside the same tranche.
* The retained value does not go to the protocol treasury or the other tranche.

A share-denominated `redeem(shares)` instead returns less USDC for the submitted shares, but the current interface does not expose that path.

Senior and Junior can have different surcharge rates. Use the refreshed vault quote and the configured onchain tranche rate rather than assuming a fixed value. The current development preview shows the surcharge state but does not itemize a numeric rate.

The extended frozen-market window is not indefinite. If the accepted oracle data becomes too stale, LP entry and exit can be blocked until valid data or protocol recovery becomes available.

### 7. Submit the withdrawal

When the LP interface is available:

1. Open `Vaults`.
2. Select the Senior or Junior position you intend to exit.
3. Select the `withdraw` toggle.
4. Enter an `Amount to withdraw` no greater than `Withdrawable now`.
5. Select `Review withdraw`.
6. Read the complete `Withdrawal preview`.
7. Select `Withdraw USDC`, then confirm the owner-wallet transaction.
8. Wait for confirmation, then verify the result.

The in-progress `Withdrawal preview` shows:

* Selected tranche
* Requested USDC amount
* Estimated shares burned
* Current `Share price`
* Synchronous settlement when permitted
* A reminder that the live maximum already reflects the holder cooldown
* Frozen-oracle surcharge state
* Network, relative-risk label and quote-refresh time

Before opening the preview, also check `Withdrawable now`, `Pool withdrawal cap`, `Free liquidity`, `Withdrawal reserve` and `Market state` on the vault page. The current preview does not separately itemize a numeric surcharge rate, share-cost decomposition, recipient row or gas estimate. Confirm the connected owner wallet and decoded vault call before signing, and treat the onchain quote as authoritative.

Verify that the transaction targets the selected **Tranche Vault**. Do not send shares to the HousePool, Margin Clearinghouse or an unknown contract.

> **Screenshot placeholder — Withdrawal preview**
>
> Add the deployed `Withdrawal preview` together with the surrounding live-limit fields after the interface is finalized. The capture should show the tranche, requested USDC, estimated shares burned, share price, settlement mode, cooldown treatment, surcharge state, network, quote time and final action without exposing unrelated wallet data.

### 8. Verify the result

After the transaction confirms, check all of the following:

1. The block explorer shows a successful transaction to the verified tranche vault.
2. The recipient wallet received the requested USDC amount.
3. The expected number of vault shares was burned, including any extra shares required by a frozen-oracle surcharge.
4. `Current value` and the share balance reflect any remaining position.
5. The pending request list did not change; LP withdrawals do not use deposit epochs.
6. If shares remain, the holder cooldown has restarted.
7. Any frozen-oracle surcharge reconciles with the configured onchain tranche rate, refreshed quote and additional shares burned; the current preview's generic surcharge state is not enough by itself.

Do not verify the exit from wallet USDC alone. The share burn and tranche-vault transaction establish which LP position changed.

### If the full amount is unavailable

If `Withdrawable now` is lower than your intended exit, you can:

* Withdraw an eligible amount within the live limit, understanding that the holder cooldown restarts; or
* Leave the shares invested and try again after conditions change.

Withdrawal capacity may improve as trader positions close, claims are funded, liabilities are released or valid oracle data becomes available. There is no guaranteed time by which this will happen, and the protocol does not queue the unfilled remainder.

If the limit is unexpectedly zero, the preview changes, or the transaction fails, use [**LP troubleshooting**](lp-troubleshooting.md).

### Exit checklist

Before confirming:

* Confirm that this is a Senior or Junior Vault withdrawal, not a Margin Account withdrawal.
* Confirm the connected owner wallet and supported network.
* Check `Current value`, `Share price` and `Withdrawable now` separately.
* Keep the requested amount within the fresh live maximum.
* Check the one-hour holder cooldown.
* Check the protocol minimum and whether a complete dust exit applies.
* Review the withdrawal firewall and tranche priority.
* Check the oracle state and any tranche-specific surcharge.
* Compare requested USDC, estimated shares burned and the refreshed vault quote.
* Verify the tranche-vault address and recipient.
* Keep enough Arbitrum Sepolia ETH for the transaction.

LP withdrawals can be reduced, delayed or blocked. Review [**LP risks and safeguards**](lp-risks-and-safeguards.md) before treating tranche value as available cash.

[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^erc4626]: The Ethereum tokenized-vault standard used for Plether tranche shares.
[^nav]: Net asset value, the accounting value of a pool or tranche after assets and liabilities.
