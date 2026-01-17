# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
npm run build              # TypeScript check + production build
npm run lint               # ESLint
npm test                   # Unit tests
npm test -- path/to/file   # Single test file
npm run test:integration   # Integration tests (requires Anvil running)
npm run anvil              # Start Anvil fork (requires SEPOLIA_RPC_URL in .env)
npm run storybook          # Component explorer at http://localhost:6006
npm run build-storybook    # Verify stories render after component changes
```

Dev server runs at http://localhost:5173.

## Architecture

Plether is a DeFi frontend for trading DXY-BEAR and DXY-BULL tokens on Ethereum.

### Tech Stack
- **Framework**: Vite + React 19 + TypeScript
- **Web3**: wagmi + viem + Web3Modal (WalletConnect)
- **Styling**: Tailwind CSS v4 (CSS-first config in `src/index.css`)
- **State**: Zustand for local state, TanStack Query for server state
- **Error Handling**: better-result for typed Result-based error handling

### Key Directories
- `src/pages/` - Route components (Dashboard, Mint, Stake, History)
- `src/components/ui/` - Reusable UI primitives
- `src/hooks/` - Contract interaction hooks returning `Result<T, Error>` types
- `src/stores/` - Zustand stores (transactions, settings)
- `src/contracts/` - ABIs and addresses (mainnet + sepolia)
- `src/utils/errors.ts` - TaggedError definitions for transaction errors

### Error Handling Pattern

All async operations (especially contract interactions) return `Result<T, E>` from better-result:

```typescript
import { Result } from 'better-result'
import { parseTransactionError, type TransactionError } from '../utils/errors'

async function doThing(): Promise<Result<Hash, TransactionError>> {
  return Result.tryPromise({
    try: () => someAsyncOp(),
    catch: (err) => parseTransactionError(err)
  })
}

// Checking results - use STATIC methods:
Result.isOk(result)    // ✅ correct
Result.isError(result) // ✅ correct (not isErr)
result.isOk()          // ❌ wrong - these don't exist
```

Error types are defined as TaggedErrors in `src/utils/errors.ts`:
- `UserRejectedError` - User cancelled transaction
- `InsufficientFundsError` - Gas, token, or allowance issues
- `ContractRevertError` - Contract execution failed
- `NetworkError`, `TimeoutError`, `UnknownTransactionError`

### Wallet & Contract Patterns
- **Approvals**: Always use exact amount approvals, never unlimited
- **Networks**: Mainnet (1), Sepolia (11155111), Anvil local fork (31337)
- **Slippage**: Max 1% (protocol limit), stored in settingsStore

### Adding Contract Integration
1. Add ABI to `src/contracts/abis/`
2. Add address to `src/contracts/addresses.ts` (mainnet + sepolia)
3. Create hook in `src/hooks/` using wagmi's `useReadContract`/`useWriteContract`
4. Return `Result<T, TransactionError>` from async operations

### Theme Colors
Defined in `src/index.css` via `@theme`:
- `cyber-neon-green` (#00FF99) - Primary accent, DXY-BULL
- `cyber-electric-fuchsia` (#FF00CC) - DXY-BEAR, secondary actions
- `bear` / `bull` - Aliases for token-specific styling

### Testing
- Tests live in `__tests__/` directories adjacent to code
- Never reimplement application logic in tests - import and test actual functions
- Use `Result.isOk(result)` and `Result.isError(result)` for assertions
- Design for testability using "functional core, imperative shell": keep pure business logic in `src/utils/` separate from IO code (hooks, API calls)

### Storybook
- For interactive stories, use `play` function with `step()` for named steps
- Import from `storybook/test` (Storybook 10+)

### Source Code Reference
Source code for dependencies is available in `opensrc/` - use when you need to understand package internals.
