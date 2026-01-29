# Plether Frontend Specification

Generate the frontend app that interacts with the Plether protocol routers and contracts.

## Tech Stack
* **Framework**: Vite + React SPA
* **Web3**: wagmi + viem
* **Theme**: Dark only
* **Responsive**: Fully responsive (desktop and mobile equal priority)

## Wallet
* Connect with self-custody wallet
* Support: Injected wallets (MetaMask, Rabby) + WalletConnect
* Exact amount approvals per transaction (no unlimited approvals)
* Read-only mode: App viewable without wallet connection (prices, APYs, stats)

## Networks
* Ethereum Mainnet (production)
* Sepolia testnet (for testing)

## Core Protocol
* Mint plDXY-BEAR + plDXY-BULL pairs from USDC
* Burn plDXY-BEAR + plDXY-BULL pairs to redeem USDC

## Spot Trading
* Buy plDXY-BEAR on Curve (USDC → plDXY-BEAR)
* Buy plDXY-BULL via ZapRouter (single-sided entry) - abstract away flash mint mechanics
* Sell plDXY-BEAR on Curve (plDXY-BEAR → USDC)
* Sell plDXY-BULL via ZapRouter (single-sided exit) - abstract away flash mint mechanics
* Market orders only (no limit orders)
* Slippage settings: Presets (0.5%, 1%) + custom input (capped at 1% protocol max)
* Swap details: Simple by default with expandable "show details" toggle (route, price impact, fees)

## Leverage Trading
* Open leveraged BEAR position (LeverageRouter)
* Open leveraged BULL position (BullLeverageRouter)
* Close leveraged positions (BEAR and BULL)
* **Full position adjustment**: Add/remove collateral, partial close, leverage adjustment in-place
* **Basic preview**: Show final liquidation price and collateral after user inputs amount
* Slippage settings: Presets + custom input (capped at 1%)

## Position Management
* Separate position cards for each position (not aggregated)
* Position health monitoring: liquidation price, health factor
* In-app warnings only when health factor is low (no push/email notifications)

## Staking
* Stake plDXY-BEAR → splDXY-BEAR
* Stake plDXY-BULL → splDXY-BULL
* Unstake from splDXY-BEAR / splDXY-BULL
* Display: Current staked balance and pending rewards only (no historical APY charts)

## Morpho Lending
* Provide liquidity to Morpho lending pool
* Borrow USDC against staked collateral
* Repay debt / withdraw collateral
* Display: Live APY and user's supplied amount only (minimal stats)

## Dashboard & Portfolio
* **Main dashboard**: Portfolio overview showing total value across all positions (spot, staked, leveraged, lending)
* **Price display**:
  - Compact header bar with plDXY price and protocol status (Active/Paused/Settled)
  - Detailed dashboard widget with more info
* **Transaction history**: Full history with filters by type (mint, swap, stake, leverage, etc.)

## UX Details

### Transaction Handling
* Show estimated gas cost before confirming
* Pending transactions: Persistent header badge showing pending TX count
* Failed transactions: Modal with detailed explanation and suggested fix

### Education
* Tooltips for key terms and concepts
* Links to external documentation for deeper learning

### Analytics
* Basic anonymous tracking: Page views, feature usage
* No wallet or identity tracking

## Price & Status Display
* plDXY price from BasketOracle
* Protocol status indicator (Active / Paused / Settled)
* Available in header bar (compact) and dashboard widget (detailed)
