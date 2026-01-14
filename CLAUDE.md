# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
npm run build    # TypeScript check + production build
npm run lint     # ESLint
```

Dev server is always running at http://localhost:5173.

## Architecture

Plether is a DeFi frontend for trading DXY-BEAR and DXY-BULL tokens on Ethereum.

### Tech Stack
- **Framework**: Vite + React 19 + TypeScript
- **Web3**: wagmi + viem + Web3Modal (WalletConnect)
- **Styling**: Tailwind CSS v4 (CSS-first config in `src/index.css`)
- **State**: Zustand for local state, TanStack Query for server state
- **Routing**: React Router
- **Icons**: Material Symbols (loaded via Google Fonts)
- **Font**: Rajdhani (cyberpunk theme)

### Key Directories
- `src/pages/` - Route components (Dashboard with Hedge/Leverage/Yield tabs, Mint, Stake, History)
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

### Cyberpunk Theme Colors
Defined in `src/index.css` via `@theme`:
- `cyber-bg` (#0D0A1C) - Main background
- `cyber-surface-dark` / `cyber-surface-light` - Card backgrounds
- `cyber-neon-green` (#00FF99) - Primary accent, success states
- `cyber-electric-fuchsia` (#FF00CC) - BEAR token, warnings, secondary actions
- `cyber-bright-blue` (#00CCFF) - Links, hover states
- `cyber-warning-text` (#FFD700) - Warning text
- `cyber-border-glow` (#4A00FF) - Subtle borders
- `bear` - Alias for fuchsia (DXY-BEAR elements)
- `bull` - Alias for neon green (DXY-BULL elements)

### Adding Contract Integration
1. Add ABI to `src/contracts/abis/`
2. Add address to `src/contracts/addresses.ts` (both mainnet and sepolia)
3. Create hook in `src/hooks/` using wagmi's `useReadContract`/`useWriteContract`
4. Replace mock data in pages with actual hook calls

### Storybook
- After modifying a component, run `npm run build-storybook` to verify stories still render
- For interactive/animated stories, use Storybook's `play` function with `step()` to create named steps visible in the Interactions panel
- Import from `storybook/test` (Storybook 10+)
- Example pattern:
  ```tsx
  play: async ({ step }) => {
    await step('Step name', async () => {
      // action
      await sleep(500)
    })
  }
  ```

### Testing
- **NEVER reimplement application logic in tests.** Tests must import and test actual utility functions, not recreate them. If logic is inline in a component, extract it to `src/utils/` first, then test the extracted function.
- Run tests: `npm test`
- Tests live in `__tests__/` directories adjacent to the code they test
