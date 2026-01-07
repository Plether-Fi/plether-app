# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
npm run dev      # Start dev server (http://localhost:5173)
npm run build    # TypeScript check + production build
npm run lint     # ESLint
npm run preview  # Preview production build
```

## Architecture

Plether is a DeFi frontend for trading DXY-BEAR and DXY-BULL tokens on Ethereum.

### Tech Stack
- **Framework**: Vite + React 19 + TypeScript
- **Web3**: wagmi + viem + Web3Modal (WalletConnect)
- **Styling**: Tailwind CSS v4 (CSS-first config in `src/index.css`)
- **State**: Zustand for local state, TanStack Query for server state
- **Routing**: React Router

### Key Directories
- `src/pages/` - Route components (Dashboard, Trade, Mint, Stake, Leverage, Positions, Lend, History)
- `src/components/ui/` - Reusable UI primitives (Button, Card, Modal, etc.)
- `src/components/layout/` - Header, MobileNav, Layout wrapper
- `src/components/wallet/` - ConnectButton, NetworkSwitcher
- `src/config/wagmi.ts` - Wagmi config with chains and WalletConnect
- `src/contracts/` - Contract addresses and ABIs (placeholders until real contracts)
- `src/stores/` - Zustand stores (transactions, settings)
- `src/hooks/` - Custom hooks for contract interactions (to be implemented)

### Patterns
- **Wallet connection**: Uses Web3Modal with injected + WalletConnect connectors
- **Contract hooks**: Create wagmi hooks in `src/hooks/` for each contract interaction
- **Approvals**: Always use exact amount approvals, never unlimited
- **Slippage**: Max 1% (protocol limit), stored in settingsStore
- **Networks**: Mainnet (chainId 1) and Sepolia (chainId 11155111)

### Custom Theme Colors
Defined in `src/index.css` via `@theme`:
- `primary-*` - Green accent (buttons, active states)
- `bear` - Red for DXY-BEAR elements
- `bull` - Green for DXY-BULL elements
- `surface-*` - Dark grays for backgrounds

### Adding Contract Integration
1. Add ABI to `src/contracts/abis/`
2. Add address to `src/contracts/addresses.ts` (both mainnet and sepolia)
3. Create hook in `src/hooks/` using wagmi's `useReadContract`/`useWriteContract`
4. Replace mock data in pages with actual hook calls
