# Choose Senior or Junior

> **Both vaults underwrite the same trader liabilities. Senior changes the order of loss and withdrawal priority; it does not remove risk.**

Plether LPs[^lp] supply USDC[^usdc] through one of two ERC-4626[^erc4626] tranche[^tranche] vaults:

* The **Senior Vault**, represented by `psLP` shares
* The **Junior Vault**, represented by `pjLP` shares

The vaults are different claims on the same HousePool. They are not separate strategies or separate pools of market risk.

> **Use the Vaults page for LP actions**
>
> The current `Vaults` interface exposes the **Senior Vault** and **Junior Vault**, their available accounting and performance data, and their queued deposit and withdrawal controls. The existing `Deposit` control on the Perps page instead funds a trader's **Margin Account**; it does not deposit into either LP vault.

### Compare the two vaults

|                              | Senior Vault                                             | Junior Vault                                                       |
| ---------------------------- | -------------------------------------------------------- | ------------------------------------------------------------------ |
| **Vault-card label**        | More protected option                                    | Higher-risk option                                                  |
| **Return model**             | Targeted return funded by Junior                         | Variable return from trading activity                               |
| **Loss order**               | Absorbs losses after Junior is exhausted                 | Absorbs losses first                                               |
| **Revenue after impairment** | Restored toward its high-water mark first                | Receives residual revenue after Senior is restored                 |
| **Withdrawal priority**      | Matured requests are funded before Junior                | Funded after matured Senior demand, then constrained by the protected Senior-share ratio |
| **Primary trade-off**        | Gives up residual upside for relative priority           | Takes subordination in exchange for variable residual upside       |
| **Annual maintenance fee**   | None                                                     | Live rate shown in the interface; paid by issuing new shares        |
| **Can principal decline?**   | Yes                                                      | Yes                                                                |
| **Can the tranche be wiped?** | Yes                                                     | Yes, before Senior is impaired                                     |
| **Is return guaranteed?**    | No                                                       | No                                                                 |

“More protected” compares Senior with Junior inside Plether's LP capital structure. It does not mean low risk in absolute terms.

### What choosing Senior means

Senior is the last-loss LP tranche. Junior principal absorbs HousePool losses first. If a loss is larger than the remaining Junior principal, Senior principal also falls.

Senior receives a configured target coupon. That coupon is:

* transferred from available Junior principal;
* limited by the amount Junior can fund;
* not a guaranteed APY[^apy];
* not external yield or new HousePool revenue;
* not converted into an accumulating debt claim when it goes unpaid.

When paid, coupon becomes part of Senior principal. While Senior is unimpaired, it also raises Senior's protected high-water mark. If Senior later falls below that mark, future reconciled LP-owned value restores Senior toward it before Junior receives more residual value.

Senior therefore prioritizes relative capital protection and a targeted return allocation over residual upside. It can still receive less than the target, become temporarily illiquid, lose principal or be completely wiped out.

### What choosing Junior means

Junior is the first-loss and residual-return tranche.

Junior principal:

* funds the Senior target coupon;
* absorbs reconciled HousePool losses before Senior;
* receives ordinary residual LP-owned value once any Senior impairment has been restored;
* is funded only after matured Senior withdrawal demand is cleared, subject to free cash, Junior principal and the governed Senior-share ratio.

This creates more variable economics. Junior may benefit more from residual trading revenue, but coupon transfers, the annual vault fee and losses can reduce its share value even while Senior remains unimpaired. The interface displays the live annual fee and accrued fee shares; issuing those fee shares dilutes existing Junior holders.

A sufficiently large loss can reduce Junior accounting value to zero while Senior still retains value. Ordinary deposits cannot silently revive a wiped tranche; recovery requires reconciled HousePool/LP-owned value allocated through the waterfall or explicit recapitalization that preserves existing ownership rights. A positive marked receivable can affect NAV but is not withdrawal cash until collected.

### Decide by the outcome you must be able to accept

| If this happens | Senior outcome | Junior outcome |
| --- | --- | --- |
| Junior cannot fund the complete target coupon | Receives only the available amount; the unpaid portion does not accrue as debt | Can be reduced to zero by the coupon transfer |
| Reconciliation applies a loss smaller than Junior principal | Remains ahead of the loss | Absorbs the loss |
| The loss exhausts Junior | Begins absorbing the remaining loss | Is wiped before Senior is impaired |
| Later reconciled LP-owned value is available while Senior is impaired | Is restored toward its high-water mark first | Waits for Senior restoration before receiving residual value |
| Free LP liquidity is below dormant Senior principal, with no matured Senior request | May withdraw within the available Senior cap | May still withdraw if the governed Senior-share ratio remains protected |
| Trader liabilities reserve the available cash | Cannot use the reserved cash | Cannot use the reserved cash |

Senior may be the closer fit only if you accept that its coupon and principal are not guaranteed and that withdrawals can still be constrained.

Junior may be the closer fit only if you accept first-loss exposure, Senior coupon funding, subordinated withdrawals and the possibility of an earlier complete wipeout.

If you require guaranteed principal, a fixed return or access to all of your capital on demand, neither tranche provides that outcome.

### Trader obligations come before both tranches

“Senior” describes priority relative to Junior. It does not place Senior ahead of traders.

Before LP cash can leave, Plether reserves HousePool assets for bounded live-position liability, existing trader claims and other protected amounts. Trader claims rank ahead of both vaults.

The remaining free LP liquidity is then allocated by tranche priority:

1. Matured Senior requests are funded first, up to their demand, Senior principal and available free cash.
2. If no matured Senior backlog remains, Junior funding is capped independently by remaining free cash, Junior principal and the governed maximum Senior share of protected tranche capital. Dormant Senior principal is not fully reserved against Junior withdrawals.

Positive NAV[^nav] is therefore not a promise of immediate redemption. A Junior position can retain positive value while its current withdrawal capacity is zero. Senior can also have less withdrawable USDC than accounting value.

See [**Settlement liquidity and trader claims**](../how-plether-works/settlement-liquidity-and-trader-claims.md) for the trader-priority rules and [**The HousePool and tranche waterfall**](../how-plether-works/the-housepool-and-tranche-waterfall.md) for the complete withdrawal firewall.

### Review the live state before deciding

On the main `Vaults` page, compare **Vault value**, **7d APY** when shown, **Share price**, **Loss order**, **Return**, **Withdrawals** and **Fee** on both cards. Also review the shared **Total pool funds**, **Reserved funds**, **Available liquidity** and **Next processing time in**.

After opening a vault, use the live sections rather than the headline return alone:

* **Vault header:** **Current vault value**, **Share price**, **Estimated withdrawal liquidity** and **How returns work**
* **Overview:** the connected wallet's position summary, **How this vault works**, shared pool status, the Senior protection or Junior loss buffer, delayed settings changes and the five-minute submission deadline
* **Performance:** the **Seven-day share price** chart, **7d realized APY**, **7d return**, **Start share price** and **Current share price**; this section appears only when complete deployment-matched history is available
* **Your position:** **Current value**, **Shares available to withdraw**, **USDC ready for wallet**, and pending deposit or withdrawal claims
* **Activity:** holder distribution and recent vault activity for the selected tranche
* **Action panel:** **Deposit status**, the current share estimate and processing state; the overview also shows the selected tranche's withdrawal-only **Temporary pricing fee**, and a withdrawal's **Expected processing** appears in its preview

Every deposit and withdrawal is queued for hourly processing. The contract uses the request transaction's block-inclusion timestamp; inclusion at or after the five-minute cutoff before an hour targets the following hour, even if the transaction was signed or sent earlier. Treat the confirmed request record as authoritative. The displayed processing time is not a promise: LP-worker availability, pricing, protocol health and withdrawal liquidity can delay completion.

Treat **7d realized APY**, **7d return** and the share-price chart as historical context only. They are calculated from the displayed complete deployment-matched series, can be negative, and do not guarantee the next period's result.

### Avoid these shortcuts

* **“Senior is safe.”** Senior is protected by priority, not by a principal guarantee.
* **“Junior has the higher displayed APY, so it is better.”** Return must be read alongside first-loss and withdrawal subordination.
* **“A lower share price is cheaper.”** Senior and Junior have separate share supplies and accounting principals; their prices are not directly comparable.
* **“Available liquidity is what I can withdraw.”** Pool-level liquidity is only one input to **Estimated withdrawal liquidity** and your holder-specific **Shares available to withdraw**.
* **“Splitting between both vaults removes protocol risk.”** Both claims depend on the same HousePool, contracts, oracle and USDC.

Before depositing, continue to [**Understand LP returns and share value**](understand-lp-returns-and-share-value.md) and [**LP risks and safeguards**](lp-risks-and-safeguards.md). For the full accounting mechanics, use [**The HousePool and tranche waterfall**](../how-plether-works/the-housepool-and-tranche-waterfall.md) as the canonical reference.

[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^erc4626]: The Ethereum tokenized-vault standard used for Plether tranche shares.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^apy]: Annual percentage yield, an annualized return measure that includes compounding.
[^nav]: Net asset value, the accounting value of a pool or tranche after assets and liabilities.
