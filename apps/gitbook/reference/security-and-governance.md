# Security, audits and governance

> **Immutability prevents silent logic swaps. Timelocks provide notice. Neither guarantees that contracts or parameters are bug-free.**

### Audit and release status

* **Pre-audit consultation:** Plether Perps completed an external pre-audit security consultation on an earlier code snapshot.
* **Formal production audit:** **Not yet completed.** Treat the active **Core v2.1.0** deployment on Arbitrum Sepolia (using **MockUSDG**) strictly as a test environment.
* **What to verify before mainnet:** Every production release must be evaluated against its exact source commit, verified onchain bytecode, oracle configuration, parameter values, admin addresses, and final audit report scope.

### Immutability boundaries

Plether separates immutable core market logic from testnet account-abstraction plumbing:

| Component | Upgradeable? | Practical Implication |
| --- | --- | --- |
| **Core Perps & Vault contracts** (`CfdEngine`, `OrderRouter`, `HousePool`, `TrancheVault`, `MarginClearinghouse`, `PositionProtectionBook`, `PletherOracle`, `PletherVerifyingPaymaster`) | **No** (non-upgradeable) | The protocol owner cannot hot-patch deployed logic; a material fix requires deploying a new stack and migrating. The `0.00–2.00` index settlement cap is an immutable constant. |
| **Testnet Plether Trading Account (`SimpleAccount` v0.8)** | **Yes** (ERC-1967 / UUPS proxy) | Explicitly **testnet-only** on Arbitrum Sepolia. Production requires an immutable or upgrade-locked account implementation, which derives different addresses with no automatic state migration. |

### Governance powers and 72-hour timelocks

Most risk-sensitive configuration changes require a **72-hour timelock** (surfaced under **Delayed settings changes** in the Vaults interface):

* **Pool risk settings:** Senior target coupon rate, Senior exposure & share caps, frozen LP withdrawal surcharges, and mark-staleness limits.
* **Junior fee settings:** Junior annual maintenance fee rate and fee recipient.
* **Engine & trading settings:** Margin rates (initial, maintenance, FAD), base carry rate, execution fee, VPI factor, skew limit, settlement buffer, frozen-close spread, and liquidation charge parameters.
* **Oracle & router settings:** Order execution windows, confidence filters, holiday closure overrides, and migrating to a newly deployed `PletherOracle` contract.

#### Immediate emergency controls

Emergency pauses through `EmergencyPauseCoordinator` take effect **immediately** without a 72-hour delay:

* **Router / risk-commitment pause:** Blocks new opens and increases while keeping closes, liquidations, and margin additions open.
* **Pool deposit pause:** Blocks new LP deposits while keeping protective withdrawal controls separate.
* **Hourly LP processing pause:** Halts new epoch activation/funding while preserving pre-boundary cancellations and already-ready claims/refunds.
* **Degraded mode (automatic latch):** Automatically triggered if a terminal settlement reveals effective pool backing below worst-case live liability; blocks new trader risk, new LP deposits, and new LP withdrawal funding until solvency recovers and the owner explicitly clears the latch.

### Self-custody and sponsorship boundaries

* Your connected owner wallet (or Privy embedded EOA) controls your Trading Account through its signature rules.
* Plether's self-hosted paymaster (`PletherVerifyingPaymaster`) and bundler (Alto) can refuse gas sponsorship or go offline, **but they cannot forge your signature, alter signed calldata, or withdraw funds without your authorization**.
* Never share your seed phrase, private key, or passkey credentials. Official Plether support will never ask for them.

### Reporting issues

If you find a security vulnerability or an unexpected settlement outcome, preserve:

* Owner-wallet and Trading Account addresses
* Network (`Arbitrum Sepolia`) and exact timestamps
* UserOperation hash, transaction hashes, Order ID, or deposit/withdrawal reference
* Observed contract error or screenshot

For full risk disclosures, see [Full risk disclosures](../welcome/risks-you-should-understand-first.md) and [Deployments, contracts and live parameters](deployments-and-parameters.md).
