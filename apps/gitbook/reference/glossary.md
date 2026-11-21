# Glossary

A–Z reference for terms used across Plether Perps DEX and LP Vaults.

---

### A

* **Adverse confidence adjustment:** A protective price shift applied during live opens, live/FAD closes, and liquidations using Pyth's reported confidence interval (currently `0.25×` basket confidence), moving execution against the trader so uncertain quotes do not drain LP capital. Waived on voluntary frozen-oracle closes (where the frozen-close spread applies instead).
* **ADL (Auto-deleveraging):** Forcibly shrinking unrelated profitable positions to cover another trader's shortfall. Plether does **not** use counterparty ADL.
* **Available to Trade:** Free, unreserved USDG inside your Margin Account that can fund new orders, fees, margin additions, or withdrawals.

### B

* **Basis risk:** The risk that Plether's arithmetic, bounded, six-currency basket diverges from raw DXY, FX futures, or a specific single-currency exposure you intend to hedge.
* **Borrow base:** The portion of a position's maximum bounded profit financed by LP capital rather than assigned position margin: `max(maximum bounded profit − assigned position margin, 0)`.
* **bps (Basis points):** One hundredth of a percent (`100 bps = 1%`; `50 bps = 0.50%`).
* **Bundler:** An ERC-4337 service (Plether runs a self-hosted Alto bundler) that packages signed UserOperations and submits them to the onchain EntryPoint.

### C

* **Carry (Cost of carry):** Continuous time-based fee paid to LPs on a position's LP-backed borrow base, scaled by same-side pool utilization. Not trader-to-trader funding—both LONG USD and SHORT USD can pay carry simultaneously, including over weekends. Collected from active position margin first, then free USDG.
* **Close-only:** Market state where new opens and increases are blocked while reductions, closes, liquidations, and margin additions remain available. Covers both the live-oracle FAD shoulders and the weekend `oracleFrozen` window.
* **Contract notional:** `position quantity × raw basket price (B)`. Used onchain for margin requirements, leverage, execution fees, frozen-close spreads, and liquidation charges. Distinct from **Order exposure** (`position quantity × displayed price D`).

### D

* **Degraded mode:** Automatic latched safety state triggered if a terminal settlement leaves effective pool backing below remaining worst-case live trader liability. Blocks new trader risk, new LP deposits, and new LP withdrawal funding until solvency recovers and the owner explicitly clears the latch.
* **Displayed price (`D`):** The dollar-oriented index shown in the UI: `D = 2.00 − B`, bounded to `0.00–2.00`. Rises when USD strengthens (LONG USD profits) and falls when USD weakens (SHORT USD profits).
* **DXY:** The traditional U.S. Dollar Index. Plether uses the same six currencies and starting weights as inspiration, but computes a normalized arithmetic basket onchain rather than tracking raw DXY.

### E

* **Effective backing:** `max(physical pool assets − aggregate trader claims, 0)`. Used to verify pool solvency before admitting new risk.
* **ERC-4626:** Ethereum tokenized-vault standard used by Plether's Senior (`psLP`) and Junior (`pjLP`) LP vaults (with asynchronous hourly request queues).
* **Execution fee:** Protocol fee charged on executed contract notional (`0.04%` in Core v2.0.0), designated for the protocol treasury when cash-credited.
* **Execution limit:** The worst confidence-adjusted oracle price a trader accepts for an order. Bounds execution price only—not fees, VPI, carry, or frozen-close spread.
* **Execution reward:** USDG bounty reserved on **opening and increasing** orders (`1 bp`, clamped `0.01–0.20 USDG`) to compensate the keeper who finalizes or cleans up the order. Reductions and closes have **zero** execution reward.

### F

* **FAD (Friday Afternoon Deleverage):** Scheduled weekly close-only window around the FX weekend closure (Friday 16:30 NY – Sunday 17:15 NY) and configured holiday closures. Blocks new risk and raises the maintenance margin requirement to `3.00%`.
* **FIFO (First in, first out):** Global order execution queue where earlier committed orders must resolve (execute, fail, expire, or be cleared by liquidation) before later orders can execute.
* **Frozen-close spread:** Fixed LP-owned fee (`0.50%` of reduced contract notional) charged on voluntary reductions and closes executed while `oracleFrozen` is active. Does not apply during live/FAD windows or to liquidations.

### H

* **High-water mark (Protected balance):** The Senior tranche's protected capital target. If losses impair Senior below this level, future LP revenue restores Senior first before Junior receives residual upside.

### J

* **Junior tranche (`pjLP`):** First-loss LP vault. Absorbs pool losses first, funds the Senior target coupon, and receives residual LP upside after Senior obligations are satisfied.

### K

* **Keeper:** Any permissionless actor or automated worker that submits valid onchain state transitions (order finalization, expired-order cleanup, liquidations, TP/SL triggers, oracle refreshes, or hourly LP epoch settlement).

### L

* **Latched (Close delayed):** TP/SL state entered when a triggered close attempt fails (e.g., expires). The original exit remains permanently binding even if the price reverses, and cannot be edited or removed.
* **Liquidity pool:** Shared USDG balance sheet (`HousePool`) funded by Senior and Junior LPs that acts as the economic counterparty to all traders.

### M

* **Maintenance margin:** Minimum Position equity required to keep a position open (`contract notional × active maintenance rate`). Reaching or falling below it makes the full position liquidatable.
* **Margin Account:** Internal USDG balance held by Plether's clearinghouse for a Trading Account. Divided into free balance (`Available to Trade`), assigned position margin, pending-order margin, and dedicated reserves.
* **MockUSDG:** Six-decimal test token used on Arbitrum Sepolia (`USDG`). Has no real-world value and is not redeemable for real dollars.

### N

* **NAV (Net Asset Value):** Accounting value of a pool or tranche after assets, claims, and marked trader positions (`Terminal NAV`). Distinct from immediately withdrawable cash.

### O

* **`oracleFrozen`:** Calendar-driven state during FX closures (Friday 17:00 NY – Sunday 17:00 NY and configured holidays) when live FX updates are not expected. Voluntary closes use bounded stale oracle data plus the frozen-close spread; new opens and new TP/SL triggers are blocked.
* **Order exposure:** Displayed dollar-oriented value of an order (`Order quantity × displayed price D`).
* **Order quantity (`plDXY`):** Fixed contract size of a position or order, entered in `100 plDXY` increments.
* **Owner wallet:** Your connected external wallet or Privy embedded EOA that signs authorizations for your Plether Trading Account and directly executes LP Vault transactions.

### P

* **Paymaster:** Smart contract (`PletherVerifyingPaymaster`) that pays Arbitrum Sepolia network gas for allowlisted Trading Account UserOperations.
* **Plether Trading Account:** Deterministic smart-account address (`SimpleAccount` v0.8 on testnet) derived from your owner wallet that owns your Perps positions, orders, Margin Account, TP/SL records, and trader claims.
* **PnL (Profit and loss):** Directional financial gain or loss from index price movement, before fees, VPI, carry, and spreads.
* **Position equity:** `position margin after accrued carry + same-account trader claim + exact unrealized price PnL`. Used to determine price-liquidation eligibility. Excludes free USDG, pending-order funds, and dedicated reserves.
* **Position margin (PnL pledge):** USDG explicitly assigned to back an open position's price losses and reduce its LP-backed borrow base.
* **Pyth:** Decentralized oracle network supplying the six signed FX price feeds used to construct the Plether Dollar Index.

### R

* **Raw basket (`B`):** Weighted arithmetic mean of the six normalized foreign-currency USD exchange rates, clamped to `0.00–2.00`. Moves inversely to dollar strength (`D = 2.00 − B`).

### S

* **Senior tranche (`psLP`):** Last-loss LP vault with withdrawal funding priority over Junior and a targeted coupon funded from available Junior principal. Not principal-protected or guaranteed.
* **Settlement buffer:** Liability-scaled reserve (`0.25%` of worst-case directional liability) required above bounded payouts for new-risk admission and LP withdrawal safety.
* **Skew:** Directional imbalance between aggregate LONG USD and SHORT USD open interest.

### T

* **Terminal NAV:** Exact signed, collateral-capped pool valuation snapshot used for LP deposit and withdrawal accounting. Marked trader gains reduce LP NAV; marked trader losses increase it only up to collectible collateral/claims (and remain accounting value, not withdrawal cash, until collected).
* **Trader claim:** Senior USDG liability recorded by the liquidity pool when free pool cash cannot immediately fund a complete fresh trader payout. Settles all-or-nothing once physical pool assets cover all outstanding claims.
* **Tranche:** One risk-and-return layer of the shared liquidity pool (Senior or Junior).

### U

* **USDG:** Six-decimal stablecoin unit used for Plether Perps margin and settlement (MockUSDG on Arbitrum Sepolia testnet).
* **UserOperation:** ERC-4337 signed instruction submitted by your Trading Account through a bundler for gas-sponsored execution.

### V

* **VPI (Virtual Price Impact):** Separate quadratic USDG charge (when increasing pool skew) or provisional rebate (when reducing skew). Never changes the oracle price; subject to a lifetime clamp (`cumulative charges − rebates ≥ 0`).

### W

* **Withdrawal cooldown:** Fixed one-hour (`3,600s`) waiting period on LP vault shares starting at deposit activation. Claiming ready shares preserves that timestamp; cancelling a withdrawal or returning zero-value shares restarts the one-hour cooldown for the entire tranche position.
* **Withdrawal firewall:** Pool-level accounting rule preventing cash reserved for bounded trader liability, the settlement buffer, and trader claims from funding LP withdrawals.
