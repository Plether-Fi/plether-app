# Choose Senior or Junior

> **Both vaults underwrite the same trader liabilities. Senior changes the order of loss and withdrawal priority; it does not remove risk.**

Plether LPs[^lp] supply USDC[^usdc] through one of two ERC-4626[^erc4626] tranche[^tranche] vaults:

* The **Senior Vault**, represented by `psLP` shares
* The **Junior Vault**, represented by `pjLP` shares

The vaults are different claims on the same HousePool. They are not separate strategies or separate pools of market risk.

> **Current interface status**
>
> The `Vaults` interface is under development and is not yet part of the published testnet application. Immediate deposits, pending-deposit lifecycle actions and synchronous withdrawals are available on the current development branch. Names such as `Senior Vault` and `Junior Vault` refer to that in-progress interface.
>
> The existing `Deposit` control on the Perps page funds a trader's **Margin Account**. It does not deposit into either LP vault.

### Compare the two vaults

|                              | Senior Vault                                             | Junior Vault                                                       |
| ---------------------------- | -------------------------------------------------------- | ------------------------------------------------------------------ |
| **Relative-risk label**      | Lower relative risk                                      | Higher relative risk                                               |
| **Return model**             | Configurable target coupon funded from Junior            | Residual HousePool return after Senior priority                     |
| **Loss order**               | Absorbs losses after Junior is exhausted                 | Absorbs losses first                                               |
| **Revenue after impairment** | Restored toward its high-water mark first                | Receives residual revenue after Senior is restored                 |
| **Withdrawal priority**      | First claim on free LP liquidity                         | Only free liquidity remaining above the complete Senior claim      |
| **Primary trade-off**        | Gives up residual upside for relative priority           | Takes subordination in exchange for variable residual upside       |
| **Can principal decline?**   | Yes                                                      | Yes                                                                |
| **Can the tranche be wiped?** | Yes                                                     | Yes, before Senior is impaired                                     |
| **Is return guaranteed?**    | No                                                       | No                                                                 |

“Lower relative risk” compares Senior with Junior inside Plether's LP capital structure. It does not mean low risk in absolute terms.

### What choosing Senior means

Senior is the last-loss LP tranche. Junior principal absorbs HousePool losses first. If a loss is larger than the remaining Junior principal, Senior principal also falls.

Senior receives a configured target coupon. That coupon is:

* transferred from available Junior principal;
* limited by the amount Junior can fund;
* not a guaranteed APY[^apy];
* not external yield or new HousePool revenue;
* not converted into an accumulating debt claim when it goes unpaid.

When paid, coupon becomes part of Senior principal. While Senior is unimpaired, it also raises Senior's protected high-water mark. If Senior later falls below that mark, future HousePool revenue restores Senior toward it before Junior receives more residual revenue.

Senior therefore prioritizes relative capital protection and a targeted return allocation over residual upside. It can still receive less than the target, become temporarily illiquid, lose principal or be completely wiped out.

### What choosing Junior means

Junior is the first-loss and residual-return tranche.

Junior principal:

* funds the Senior target coupon;
* absorbs realized HousePool losses before Senior;
* receives ordinary residual revenue once any Senior impairment has been restored;
* withdraws only when free LP liquidity exceeds the complete Senior claim.

This creates more variable economics. Junior may benefit more from residual trading revenue, but coupon transfers and losses can reduce its share value even while Senior remains unimpaired.

A sufficiently large loss can reduce Junior accounting value to zero while Senior still retains value. Ordinary deposits cannot silently revive a wiped tranche; recovery requires realized HousePool/LP-owned revenue allocated through the waterfall or explicit recapitalization that preserves existing ownership rights.

### Decide by the outcome you must be able to accept

| If this happens | Senior outcome | Junior outcome |
| --- | --- | --- |
| Junior cannot fund the complete target coupon | Receives only the available amount; the unpaid portion does not accrue as debt | Can be reduced to zero by the coupon transfer |
| The HousePool realizes a loss smaller than Junior principal | Remains ahead of the loss | Absorbs the loss |
| The loss exhausts Junior | Begins absorbing the remaining loss | Is wiped before Senior is impaired |
| Later revenue arrives while Senior is impaired | Is restored toward its high-water mark first | Waits for Senior restoration before receiving residual revenue |
| Free LP liquidity is below the Senior claim | May withdraw only within the available Senior cap | Cannot withdraw |
| Trader liabilities reserve the available cash | Cannot use the reserved cash | Cannot use the reserved cash |

Senior may be the closer fit only if you accept that its coupon and principal are not guaranteed and that withdrawals can still be constrained.

Junior may be the closer fit only if you accept first-loss exposure, Senior coupon funding, subordinated withdrawals and the possibility of an earlier complete wipeout.

If you require guaranteed principal, a fixed return or access to all of your capital on demand, neither tranche provides that outcome.

### Trader obligations come before both tranches

“Senior” describes priority relative to Junior. It does not place Senior ahead of traders.

Before LP cash can leave, Plether reserves HousePool assets for bounded live-position liability, existing trader claims and other protected amounts. Trader claims rank ahead of both vaults.

The remaining free LP liquidity is then allocated by tranche priority:

1. Senior can withdraw up to the lower of its accounting claim and free LP liquidity.
2. Junior can withdraw only from free liquidity remaining above the complete Senior claim.

Positive NAV[^nav] is therefore not a promise of immediate redemption. A Junior position can retain positive value while its current withdrawal capacity is zero. Senior can also have less withdrawable USDC than accounting value.

See [**Settlement liquidity and trader claims**](../how-plether-works/settlement-liquidity-and-trader-claims.md) for the trader-priority rules and [**The HousePool and tranche waterfall**](../how-plether-works/the-housepool-and-tranche-waterfall.md) for the complete withdrawal firewall.

### Review the live state before deciding

When the `Vaults` interface becomes available, compare more than the headline return. At minimum, review:

* **Return model:** target coupon for Senior or residual return for Junior
* **Tranche TVL / NAV:** current accounting value, not guaranteed redemption value
* **Share price:** current value per vault share, which can rise or fall
* **Senior impairment:** whether Senior principal is below its high-water mark
* **Pool withdrawal cap:** current pool-level cash constraint, not your personal withdrawal limit
* **Trader claims and liability:** obligations that rank ahead of both tranches
* **Oracle and protocol state:** conditions that can restrict entry or exit
* **Deposit mode:** immediate, pending epoch or unavailable
* **Frozen-oracle surcharge:** the live tranche-specific cost while the onchain `oracleFrozen` state is active

Treat `7d APY`, historical performance and projected return as context only. Historical performance is not yet indexed in the current frontend work, and no displayed rate can guarantee the next period's result.

### Avoid these shortcuts

* **“Senior is safe.”** Senior is protected by priority, not by a principal guarantee.
* **“Junior has the higher displayed APY, so it is better.”** Return must be read alongside first-loss and withdrawal subordination.
* **“A lower share price is cheaper.”** Senior and Junior have separate share supplies and accounting principals; their prices are not directly comparable.
* **“Pool liquidity is what I can withdraw.”** Pool-level free liquidity is only one input to a holder's tranche-specific maximum.
* **“Splitting between both vaults removes protocol risk.”** Both claims depend on the same HousePool, contracts, oracle and USDC.

Before depositing, continue to [**Understand LP returns and share value**](understand-lp-returns-and-share-value.md) and [**LP risks and safeguards**](lp-risks-and-safeguards.md). For the full accounting mechanics, use [**The HousePool and tranche waterfall**](../how-plether-works/the-housepool-and-tranche-waterfall.md) as the canonical reference.

[^lp]: Liquidity provider, a participant that supplies USDC capital to the HousePool.
[^usdc]: A US dollar-denominated stablecoin Plether uses for margin and settlement.
[^erc4626]: The Ethereum tokenized-vault standard used for Plether tranche shares.
[^tranche]: A pool layer with its own loss priority, withdrawal priority and return profile.
[^apy]: Annual percentage yield, an annualized return measure that includes compounding.
[^nav]: Net asset value, the accounting value of a pool or tranche after assets and liabilities.
