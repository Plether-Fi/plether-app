# Key risks at a glance

> **Plether makes counterparty risk measurable. It does not make trading or providing liquidity risk-free.**

Before you open a position or deposit into a vault, know what Plether's safeguards do—and what they do not do.

### Read Plether's claims precisely

| Safeguard | What it does | What it does **not** do |
| --- | --- | --- |
| **Fixed `0.00–2.00` range** | Makes every position's worst-case payout calculable before acceptance | Prevent external FX markets from moving past `2.00` |
| **Pre-trade solvency check** | Blocks new exposure if pool backing cannot cover worst-case payouts plus a buffer | Guarantee LP principal or make bad debt impossible |
| **No counterparty ADL** | Another trader's loss will never shrink your profitable position | Protect your own under-margined position from full liquidation |
| **Delayed oracle execution** | Reduces front-running and keeper price-picking | Eliminate slippage, queue delays, or infrastructure outages |
| **Gas-sponsored Trading Account** | Covers eligible Perps network gas without requiring ETH in your owner wallet | Guarantee sponsor availability, or let Plether act without your signature |
| **Senior tranche priority** | Junior absorbs pool losses and funds the Senior coupon first | Guarantee Senior yield or protect Senior principal if Junior is wiped out |
| **Withdrawal firewall** | Keeps cash needed for trader payouts inside the pool | Guarantee immediate LP withdrawals on demand |

### Five things traders must accept

1. **Orders are delayed, binding and non-cancellable.** Once confirmed onchain, an order enters a global first-in, first-out queue and settles against a post-commit oracle price.
2. **Liquidation is full, not partial.** If position equity falls to or below maintenance—or if carry or VPI reserves go uncovered—the entire position can be liquidated. Maintenance rises to **2.00%** during FX market-close windows.
3. **Carry accrues continuously—even on weekends.** Both LONG USD and SHORT USD can pay carry at the same time. Carry consumes active position margin first, then free USDG, so it can pull a quiet position closer to liquidation.
4. **Profits can temporarily become a trader claim.** If pool free cash cannot cover a full payout immediately, the entire payout is recorded as a senior trader claim until pool cash covers all outstanding claims.
5. **Take profit / stop loss is not a guaranteed fill price.** Triggered exits queue a market-style close order (uncapped slippage in the current web interface) and remain paused while the FX oracle is frozen over weekends.

### Five things liquidity providers must accept

1. **LP capital pays winning traders and absorbs bad debt.** Return is underwriting compensation, not fixed or risk-free yield.
2. **Junior is first-loss; Senior is last-loss, not risk-free.** Junior can be wiped out first, and Senior can still lose principal once Junior reaches zero.
3. **Share value is not immediately withdrawable cash.** Trader liabilities and claims rank ahead of both LP vaults, and Senior withdrawals are funded before Junior.
4. **Deposits and withdrawals are processed hourly.** Requests included in the final five minutes before an hour roll into the following hour. Share prices can move while requests wait, and a one-hour withdrawal cooldown applies after activation.
5. **Vault transactions are not gas-sponsored.** LP actions run directly from your connected owner wallet and require Arbitrum Sepolia ETH for gas.

### Shared platform risks

* **Testnet & pre-audit status:** The active deployment is **Core v2.1.0 on Arbitrum Sepolia** using **MockUSDG** (no real-world value). Plether Perps completed a pre-audit security consultation, not a formal production audit.
* **Smart accounts & infrastructure:** Core perps contracts are non-upgradeable, while the testnet Trading Account uses an upgradeable `SimpleAccount` proxy (testnet-only). Execution relies on Pyth FX feeds, keepers, the self-hosted bundler/paymaster, and Arbitrum Sepolia finality.
* **Governance:** Risk-sensitive parameter changes use a **72-hour timelock**; emergency pauses can take effect immediately.

For the exhaustive risk breakdown—covering basis risk, smart-account custody, oracle outages, degraded mode, and full checklists—read [Full risk disclosures](risks-you-should-understand-first.md) and [Security, audits and governance](../reference/security-and-governance.md).
