# Liquidity provider quickstart

> **Supply the balance sheet behind the market.**

Plether liquidity providers deposit USDC into the **HousePool**, the capital base that stands behind trader profits and protocol liabilities.

LP capital is not idle collateral. It can be used to pay winning traders. In return, LPs participate in realized trader losses, carry and other pool revenue according to the Senior–Junior waterfall.

**Liability is the product. Return is what LPs receive for underwriting it.**

> **Current interface status**
>
> The testnet application currently displays aggregate HousePool liquidity but does not yet provide the LP deposit, pending-epoch, share-claim or withdrawal interface.
>
> Control names in square brackets below are placeholders for the forthcoming LP interface.
>
> The `Deposit` button in the existing welcome window and trader ticket funds a **trader margin account**. It does not provide liquidity to the HousePool.

### What you receive

LPs do not deposit directly into the HousePool. They deposit through one of two ERC-4626 tranche vaults:

* The **Senior Vault**
* The **Junior Vault**

In return, you receive vault shares representing a proportional claim on that tranche.

Shares are not fixed at one USDC. Their value can rise or fall as the pool earns revenue, pays traders and moves losses through the waterfall.

Never send USDC directly to the HousePool. Use the verified Senior or Junior Vault listed on the current deployment page.

### 1. Choose your tranche

The two tranches underwrite the same HousePool but take different positions in the capital structure.

|                         | Senior                                              | Junior                                                              |
| ----------------------- | --------------------------------------------------- | ------------------------------------------------------------------- |
| **Return profile**      | Configurable target coupon funded from Junior NAV   | Residual pool revenue after Senior obligations                      |
| **Loss priority**       | Absorbs losses after Junior is exhausted            | Absorbs losses first                                                |
| **Withdrawal priority** | First claim on free LP liquidity                    | Withdraws only from free liquidity above the Senior claim           |
| **Main risk**           | Coupon can stop and principal can still be impaired | Pays the Senior coupon and can be partially or completely wiped out |
| **Upside**              | Targeted return and restoration priority            | Variable residual upside from underwriting                          |

The basic waterfall is:

```
Losses:       Junior first → Senior second
New revenue:  Restore impaired Senior → Junior receives the residual
Coupon:       Junior NAV → Senior
```

#### Senior is protected, not protected from everything

The Senior tranche receives a target coupon funded directly from Junior NAV.

“Target” matters. The coupon is:

* Not a guaranteed APY
* Not external yield
* Not necessarily funded by trading revenue
* Limited by the amount of Junior capital available
* Not converted into a separate debt claim if Junior cannot fund it

When paid, the coupon increases Senior’s protected high-water mark. If Senior is later impaired, future pool revenue restores it toward that mark before Junior receives additional surplus.

Senior is therefore **last-loss**, not risk-free. If losses exhaust Junior capital, Senior share value can decline.

#### Junior receives the residual

Junior capital pays the Senior target coupon and absorbs pool losses first.

In exchange, Junior receives residual realized trading revenue after Senior restoration and coupon obligations have been accounted for.

Junior has greater upside participation, but it can lose value before Senior is affected. A sufficiently large loss can wipe out the Junior tranche completely.

> **Neither tranche is a savings account.**
>
> Senior changes the order in which risk is absorbed. It does not remove the risk.

### 2. Understand what changes LP value

LP share value can increase through:

* Realized trader losses collected by the pool
* Realized carry paid for LP-backed exposure
* Positive VPI and other trader-to-pool price adjustments
* Oracle-frozen LP surcharges retained inside the affected tranche

LP share value can decrease through:

* Profits paid or owed to traders
* VPI rebates paid by the pool
* Liquidation shortfalls and bad debt
* Oracle, smart-contract, stablecoin and operational failures
* The Senior target coupon, from Junior’s perspective

Protocol execution fees belong to the protocol treasury. They should not be treated as LP revenue.

Plether also applies conservative accounting:

* Unrealized trader profits are treated as pool liabilities.
* Unrealized trader losses are not treated as spendable LP assets until physically realized.

This means your displayed share value and your currently withdrawable USDC can differ.

### 3. Prepare your wallet

To test liquidity provision, you need:

* A compatible self-custody wallet
* Arbitrum Sepolia selected in your wallet
* Arbitrum Sepolia ETH for transaction fees
* MockUSDC to deposit
* Enough ETH for multiple transactions if the deposit uses a pending epoch

You can request MockUSDC through the testnet welcome window or `Get mock USDC` notice.

After receiving MockUSDC, close the welcome window. Do not select its `Deposit` action for LP purposes—it opens the trader margin-deposit flow.

### 4. Review the pool before depositing

Open the future `[Liquidity]` section and review both tranches.

At minimum, the interface should show:

* Total tranche assets
* Current share price
* Senior target coupon
* Recent or projected performance, clearly marked as variable
* Current Senior and Junior capital
* Whether Senior is impaired
* Deposit availability
* Immediate or pending deposit mode
* Active oracle-frozen fee, if any
* Pool free liquidity
* Current withdrawal availability
* Oracle and market state

Do not choose a tranche based only on the highest displayed return. Understand where it sits in the loss and withdrawal waterfall.

> **Screenshot placeholder — LP overview**
>
> Show the future `[Liquidity]` page with Senior and Junior cards. Include total assets, share price, target or historical return, relative risk, current deposit mode, active fee and withdrawal availability.

### 5. Enter the deposit

Select the Senior or Junior tranche, enter the amount of MockUSDC and review the expected vault shares.

The preview should identify:

* Selected tranche
* USDC deposited
* Estimated shares received
* Current share price
* Immediate or pending deposit
* Expected activation time, if pending
* Active oracle-frozen surcharge
* Wallet balance after deposit
* Tranche risk classification

The number of shares determines your proportional ownership. The USDC value shown in the preview is not a guaranteed future redemption value.

If the vault allowance is insufficient, the first step is approving the selected tranche vault to use your MockUSDC. The approval and deposit or request are separate transactions.

Verify that the spender is the selected **Tranche Vault**, not the HousePool, Margin Clearinghouse or an unknown contract.

> **Screenshot placeholder — Deposit preview**
>
> Show the selected tranche, deposit amount, estimated shares, share price, deposit mode, active fee and approval status. The vault address should be visible or linked to the verified deployment page.

### 6. Immediate versus pending deposits

The protocol determines which deposit path is available.

| Market condition                                                     | Deposit path          | When shares are received                 |
| -------------------------------------------------------------------- | --------------------- | ---------------------------------------- |
| No trader positions are open and all safety gates pass               | Immediate deposit     | In the deposit transaction               |
| One or more trader positions are open                                | Pending deposit epoch | After activation, finalization and claim |
| Deposits are paused, Senior is impaired or another safety gate fails | Deposit unavailable   | No shares issued                         |

Immediate deposits are deliberately restricted while trader positions exist. This prevents new shares from being priced against an incomplete view of unrealized trader losses.

In normal live-market conditions, the pending-epoch route should be treated as the standard LP entry path.

### 7. Immediate deposit flow

If `[Immediate deposit]` is available:

1. Select the tranche.
2. Enter the USDC amount.
3. Review the share and fee preview.
4. Approve the selected vault if required.
5. Confirm the deposit transaction.
6. Verify that the vault shares appear in your LP position.

An immediate deposit starts a **one-hour withdrawal cooldown**. During that period, the newly held shares cannot be withdrawn or transferred.

Depositing more into the same vault refreshes the applicable cooldown.

### 8. Pending deposit flow

If trader positions are open, the application should use `[Request deposit]`.

#### Request the deposit

1. Select Senior or Junior.
2. Enter the MockUSDC amount.
3. Review the estimated activation time.
4. Approve the selected vault if required.
5. Confirm the funded deposit request.
6. Record the assigned epoch.

The MockUSDC leaves your wallet immediately and is held in vault escrow. You do not yet own active tranche shares, and the requested funds are not yet participating in the HousePool.

The current vaults use one-hour epochs. Requests are assigned two epoch IDs ahead, producing a wait of roughly one to two hours depending on when the request is submitted.

#### Before activation

A pending request can normally be cancelled before its activation epoch begins.

If you cancel:

* The request is removed.
* The escrowed MockUSDC returns to your wallet.
* No tranche shares are issued.

#### After activation

Once the activation epoch begins, the request normally becomes binding and can no longer be cancelled.

The epoch must then be finalized. Finalization is permissionless: it may be submitted by the application, a keeper or any user.

Finalization:

* Reconciles the HousePool
* Fixes one batch share price for the epoch
* Applies any active oracle-frozen surcharge
* Moves the batch USDC into the HousePool
* Mints the batch shares into vault escrow

The final number of shares can differ from the request-time estimate because tranche economics and fee conditions may change before finalization.

#### Claim your shares

Finalization does not automatically transfer shares to every depositor.

After the epoch is finalized:

1. Select `[Claim shares]`.
2. Confirm the transaction.
3. Verify that the shares appear in your LP position.
4. Confirm that the pending request has cleared.

If Senior impairment prevents the epoch from finalizing, cancellation becomes available again so depositors can recover their escrowed USDC.

> **Screenshot placeholder — Pending deposit**
>
> Show the requested USDC, selected tranche, epoch number, estimated activation time and current state:
>
> `Pending → Active → Finalized → Shares claimed`
>
> Include the appropriate `[Cancel request]`, `[Finalize epoch]` and `[Claim shares]` actions.

### 9. Monitor your LP position

Once shares are active, the LP position should show:

* Tranche
* Vault shares held
* Current USDC value
* Average entry value
* Current share price
* Unrealized change in value
* Current withdrawable USDC
* Active cooldown
* Active oracle-frozen fee
* Pending deposit epochs, if any

LP economics are reflected through the value of your vault shares. There is no separate periodic interest payment that must be harvested.

For Senior, share value reflects coupon transfers, restoration and any losses that reach the tranche.

For Junior, share value reflects residual realized revenue minus Senior coupon transfers and first-loss absorption.

A rising historical share price does not guarantee that the next period will be profitable.

> **Screenshot placeholder — LP position**
>
> Show tranche shares, current USDC value, share price, change in value, current withdrawable amount, cooldown and any pending epochs.

### 10. Understand withdrawal availability

LP withdrawals are synchronous rather than epoch-queued: when a permitted withdrawal confirms, USDC goes directly to the recipient wallet.

However, not all share value is necessarily withdrawable at every moment.

Before allowing capital to leave, Plether reserves USDC for:

* Winning-trader liabilities
* Outstanding trader claims
* Current directional exposure
* Other explicit or unassigned liabilities
* The Senior claim ahead of Junior withdrawals

Conceptually:

```
Free LP liquidity
= Physical pool assets
− Reserved trader liabilities
− Other protected amounts
```

Senior can withdraw up to the lower of its tranche claim and free LP liquidity.

Junior can withdraw only from free liquidity remaining above the complete Senior claim.

As a result:

* Senior withdrawal availability can be below Senior share value.
* Junior withdrawal availability can be zero while Senior remains fully protected.
* A displayed LP balance is not a promise of immediate redemption.
* Withdrawal availability may improve as positions close and liabilities are released.

This mechanism is the **withdrawal firewall**. It prevents LP withdrawals from consuming collateral already standing behind traders.

### 11. Withdraw or redeem shares

When you are ready to exit:

1. Open the tranche position.
2. Select `[Withdraw]`.
3. Choose either a USDC amount or number of shares to redeem.
4. Check the live maximum.
5. Review any active fee.
6. Confirm the transaction.
7. Verify the USDC receipt in your wallet.

The vault reconciles pool accounting during withdrawal. The final allowed amount may therefore be lower than the position’s displayed USDC value.

An immediate deposit must complete its one-hour cooldown before withdrawal. A successful partial withdrawal starts another one-hour cooldown for the remaining shares.

If the maximum is lower than your intended withdrawal, the remaining shares stay invested. You can try again after the cooldown and when more pool liquidity becomes free.

Ordinary partial withdrawals must satisfy the vault’s minimum amount. A complete residual exit can still be permitted when the remaining value is below that minimum.

> **Screenshot placeholder — Withdrawal preview**
>
> Show total position value, share balance, current maximum withdrawal, requested amount, shares burned, cooldown, active fee and expected wallet receipt.

### Oracle-frozen LP actions

When the FX oracle is genuinely frozen, LP entry and exit can remain available under a tranche-specific surcharge.

The surcharge works differently from a protocol fee:

* Depositors receive fewer shares.
* Withdrawers receive less USDC for the tranche claim being redeemed.
* The retained value stays inside the same tranche.
* Existing LPs in that tranche benefit from the retained amount.
* It does not go to the protocol treasury.

Senior and Junior can have different surcharge rates. Always use the live value shown in the interface rather than assuming a fixed number.

The surcharge begins only when the oracle is actually frozen. A market-close runway by itself does not activate it.

### Common problems

| Problem                                             | What to check                                                                          |
| --------------------------------------------------- | -------------------------------------------------------------------------------------- |
| MockUSDC was deposited but no LP shares appeared    | You may have used the trader Margin Account deposit instead of a Tranche Vault         |
| Immediate deposit is unavailable                    | Open trader positions require the pending-epoch route                                  |
| Pending request shows no shares                     | Wait for activation, epoch finalization and then claim                                 |
| Pending request cannot be cancelled                 | The activation epoch has probably begun                                                |
| Epoch cannot finalize                               | Activation time, oracle state, Senior impairment or other deposit gate                 |
| Share amount differs from the original estimate     | Final batch pricing and active fees are applied at finalization                        |
| Withdrawable amount is zero                         | Cooldown, reserved trader liabilities, oracle state, degraded mode or tranche priority |
| Junior cannot withdraw despite positive share value | Free liquidity does not currently exceed the Senior claim                              |
| Senior return is below its target                   | The coupon is not guaranteed and is limited by available Junior NAV                    |
| Share value declined                                | Realized trader profits, rebates, bad debt or waterfall losses reduced tranche assets  |

### First-deposit checklist

Before approving the vault:

* Confirm whether you are choosing Senior or Junior.
* Understand the tranche’s place in the loss waterfall.
* Verify the official vault address.
* Confirm that you are not depositing into the trader margin account.
* Check whether the deposit is immediate or pending.
* Review the expected shares and current share price.
* Check the current oracle-frozen fee.
* Understand when a pending request becomes non-cancellable.
* Keep enough Arbitrum Sepolia ETH for approval, request, finalization and claim transactions.
* Accept that share value and withdrawal availability can both change.

### Continue reading

* **Senior and Junior liquidity**
* **How the tranche waterfall works**
* **Where LP returns come from**
* **Pending deposit epochs**
* **The LP withdrawal firewall**
* **LP fees during market closures**
* **LP risks and loss scenarios**
