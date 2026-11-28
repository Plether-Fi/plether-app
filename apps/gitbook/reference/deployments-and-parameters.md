# Deployments, contracts and live parameters

> **Always verify the network, token and contract addresses before signing any transaction.**

The active Plether Perps DEX release is **Core v2.1.0** on **Arbitrum Sepolia**.

> **Mainnet vs. testnet web apps**
>
> * **Plether Perps DEX & LP Vaults (testnet):** [app.sepolia.plether.com](https://app.sepolia.plether.com) on **Arbitrum Sepolia** (`chainId: 421614`), using six-decimal **MockUSDG** (no real-world value).
> * **Legacy Spot app (Ethereum mainnet):** [app.plether.com](https://app.plether.com) currently defaults to the legacy Ethereum `/spot` interface (`plDXY-BULL` / `plDXY-BEAR` in USDC). Perps and Vaults are not live on mainnet yet.

### Active release summary

| Field | Value |
| --- | --- |
| **Release version** | `v2.1.0` (schema 4, order interface 3) |
| **Network** | Arbitrum Sepolia (`chainId: 421614`) |
| **Release date** | `2026-11-27` (`phase: active`) |
| **Deployment block** | `333192048` (`2026-11-27 16:25:29 UTC`) |
| **Core source commit** | `d5ef9725f72f5e131272da27004266d8a3d51e50` |
| **AA manifest version** | `perps-aa-arbitrum-sepolia-usdg-210-v2` |
| **Settlement token** | Mock USDG (`USDG`, 6 decimals, mock test token; no EIP-3009) |

You can also inspect the active contracts and your connected Owner Wallet / Trading Account addresses at any time via the **Build** link in the application footer.

### Core contract addresses (Arbitrum Sepolia)

| Contract | Role | Address |
| --- | --- | --- |
| **MockUSDG (`settlementToken`)** | 6-decimal test margin & settlement token | `0x66705F56f2c88C9474d021D9C6AFE134205a63CE` |
| **MarginClearinghouse** | Holds trader Margin Account balances & reserves | `0x11BeF9BAB445C0E1F5b0280EE7E20498e3FC9D63` |
| **CfdEngine** | Core positions, margin, carry, VPI & claims | `0xa3C43242F4E0a7C9a21Bd7dE48376D2f0779CaA6` |
| **HousePool** | Shared USDG liquidity pool & tranche waterfall | `0x23735f88637cbf329ca1C4f49B96953d25A24b4a` |
| **SeniorVault (`psLP`)** | Senior LP ERC-4626/async tranche vault (9 decimals) | `0xCac0AeB2b8d7874e33b5CD7f6eF7c2f85d98469F` |
| **JuniorVault (`pjLP`)** | Junior LP ERC-4626/async tranche vault (9 decimals) | `0xe69d0d809b8CdaD43074C2B92B0e4C0969364277` |
| **PletherOracle** | 6-feed Pyth FX basket oracle | `0x3BAc571AF4f9F6535696F82460EFE56A348C91B2` |
| **OrderRouter** | Delayed FIFO order commitment & execution | `0xB4c7548ba764e22107964637e31558af724c1c3E` |
| **OrderLifecycleBook** | Order timing, status & receipt storage | `0x194cd4822c70a2851d5B14abC96018BE41a5a5f0` |
| **CfdOrderAdmissionValidator** | Pre-trade open admission & slippage band validator | `0x6F6eBb24C49a449e0Ec14208D883d0e4938b4a58` |
| **PositionProtectionBook** | Onchain TP/SL records & trigger accounting | `0xc7C8ED1cb968F124D4faF01655AdEC222EaEFf02` |
| **CfdClosePreview** | Onchain close-settlement preview helper | `0x2EAD91843fD8e161204B68CF518335BfFD514C58` |
| **EmergencyPauseCoordinator** | Coordinated emergency pause control | `0x59bb4dd58F9dd146F61A4aF8F7311ab2f4793056` |
| **Pyth** | External Pyth Network contract on Arbitrum Sepolia | `0x0B73614636C855Bf23F342F307FB981A3e47f42B` |

### Account Abstraction (gas-sponsorship) infrastructure

| Component | Version / Policy | Address |
| --- | --- | --- |
| **EntryPoint** | ERC-4337 v0.8 | `0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108` |
| **SimpleAccount Factory** | `permissionless-simple-v0.8` (index `0`, testnet-only) | `0x13E9ed32155810FDbd067D4522C492D6f68E5944` |
| **SimpleAccount Implementation** | UUPS proxy implementation (testnet-only) | `0x28426d752372D68d34340bd94390950DcE3C9ec3` |
| **PletherVerifyingPaymaster** | `plether-verifying-v1` (non-upgradeable) | `0x9761091045616A388f5fE1433721B272c78fe31b` |

### Plether Dollar Index Pyth feeds & reference rates

Each deployed `PletherOracle` fixes six Pyth FX feeds, their quote inversions, reference coefficients, and 8-decimal base reference prices:

| Component | Pyth Feed | Inverted | Reference Coefficient | Base Reference Rate (8 dec) | Pyth Feed ID |
| --- | --- | --- | --- | --- | --- |
| **EUR/USD** | `EUR/USD` | No | `57.6%` | `1.17500000` (`117500000`) | `0xa995d00bb36a63cef7fd2c287dc105fc8f3d93779f062f09551b0af3e81ec30b` |
| **JPY/USD** | `USD/JPY` | Yes | `13.6%` | `0.00638000` (`638000`) | `0xef2c98c804ba503c6a707e38be4dfbb16683775f195b091252bf24693042fd52` |
| **GBP/USD** | `GBP/USD` | No | `11.9%` | `1.34480000` (`134480000`) | `0x84c2dde9633d93d1bcad84e7dc41c9d56578b7ec52fabedc1f335d673df0a7c1` |
| **CAD/USD** | `USD/CAD` | Yes | `9.1%` | `0.72880000` (`72880000`) | `0x3112b03a41c910ed446852aacf67118cb1bec67b2cd0b9a214c58cc0eaa2ecca` |
| **SEK/USD** | `USD/SEK` | Yes | `4.2%` | `0.10860000` (`10860000`) | `0x8ccb376aa871517e807358d4e3cf0bc7fe4950474dbe6c9ffc21ef64e43fc676` |
| **CHF/USD** | `USD/CHF` | Yes | `3.6%` | `1.26100000` (`126100000`) | `0x0b1e3297e69f162877b577b0d6a47a0d63b2392bc8499e6540da4187a63e28f8` |

### Accounting decimals & canonical units

| Quantity | Precision | Notes |
| --- | --- | --- |
| **USDG (settlement assets)** | `6` decimals | `1 USDG = 1,000,000` atomic units |
| **Index / mark / execution price** | `8` decimals | Fixed settlement bounds `0.00` to `2.00` (`0` to `200,000,000`) |
| **Position size (`plDXY`)** | `18` decimals | Canonical lot quantum: **`100 plDXY`** (`10^20` raw units) |
| **LP Vault shares (`psLP`, `pjLP`)** | `9` decimals | Virtual shares offset: `1,000` |

### Deployment-time economic & risk parameters (Core v2.1.0)

> Risk parameters (other than the immutable `0.00–2.00` settlement range) can be updated via **72-hour timelocked** governance. Always treat live onchain reads and the in-app preview as authoritative.

#### Trading, margin & liquidation

| Parameter | Deployment Value | Notes |
| --- | --- | --- |
| **Initial margin rate** | `0.30%` (`30 bps`) | Protocol ceiling `333x`; UI caps normal leverage conservatively |
| **Maintenance margin rate (live)** | `0.10%` (`10 bps`) | Liquidatable when `positionEquity ≤ maintenanceMargin` |
| **Market-close (FAD) margin rate** | `2.00%` (`200 bps`) | Active Fri 16:50 NY – Sun 17:05 NY and closure-day overrides |
| **Minimum open notional** | `1,000 USDG` | Enforced on open/increase execution |
| **Minimum open slippage tolerance** | `0.25%` (`25 bps`) | Floor on finite open/increase slippage (`Infinity` / `Market` still allowed) |
| **Max pending orders per account** | `5` | Active queue entries per Trading Account |
| **Order execution window** | `30 seconds` | Order V3 review `submitBy`: `120s` (45s sign guard / 30s submit guard) |
| **Minimum order settlement delay** | `2 seconds` | Post-commit oracle observations must publish at least `2s` after commit |
| **Liquidation charge** | `0.10%` (`10 bps`, min `1.00 USDG`) | Split **50% keeper** (min `0.50 USDG`) / **50% LP pool** / **0% protocol** |

#### Fees, VPI, carry & oracle policy

| Parameter | Deployment Value | Notes |
| --- | --- | --- |
| **Protocol execution fee** | `0.04%` (`4 bps`) | Charged on executed contract notional (treasury-designated when cash-credited) |
| **Open / increase execution reward** | `0.01%` (`1 bp`), clamped `0.01–0.25 USDG` | Paid to order finalizer / cleaner |
| **Reduce / close execution reward** | `0 USDG` | Applies to voluntary closes and TP/SL closes |
| **TP/SL trigger reward** | `0.25 USDG` | Reserved from free USDG when protection is created |
| **Base annualized carry rate** | `5.00%` (`500 bps`) | Flat annual rate on full maximum-profit exposure (independent of margin or utilization) |
| **VPI factor (`k`)** | `0.01` (`1e16` WAD) | Quadratic skew curve |
| **Max directional skew ratio** | `40%` (`4e17` WAD) | Caps net directional exposure relative to pool depth |
| **Settlement solvency buffer** | `0.25%` (`25 bps`) | Applied to post-trade worst-case directional liability |
| **Frozen-close spread** | `0.50%` (`50 bps`) | Voluntary closes during `oracleFrozen` only; 100% LP-owned |
| **Max basket confidence ratio** | `0.25%` (`25 bps`) | Observations with wider uncertainty are rejected |
| **Adverse confidence multiplier** | `0.25×` (`2,500 bps`) | Applied to opens, live/FAD closes, and liquidations; waived on frozen voluntary closes |
| **Max component publish-time divergence** | `5 seconds` | All 6 Pyth FX feeds must align within 5s |
| **Max frozen oracle staleness** | `3 days` (`259,200s`) | Bounds stale pricing during FX closures |

#### Liquidity pool & tranche vaults

| Parameter | Deployment Value | Notes |
| --- | --- | --- |
| **LP epoch duration** | `1 hour` (`3,600s`) | Submission cutoff: **5 minutes** before each hour |
| **LP withdrawal cooldown** | `1 hour` (`3,600s`) | Starts at deposit activation; restarts on withdrawal cancellation or zero-value share return |
| **Max Senior exposure** | `40,000,000 USDG` | Absolute Senior capital cap |
| **Max Senior share** | `80.00%` (`8,000 bps`) | Governs Senior deposit cap and Junior withdrawal ratio |
| **Junior annual maintenance fee** | `0.00%` (`0 bps`) | Configurable via 72h timelock; when nonzero, collected via share issuance |
| **Frozen LP withdrawal surcharge** | Senior `0.25%` / Junior `0.75%` | Retained inside the respective tranche; live onchain value is authoritative |
| **Governance timelock** | `72 hours` | Applies to pool risk, Junior fee, engine, and router parameter updates |
