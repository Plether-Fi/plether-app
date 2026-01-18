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

### Component Structure
One exported component per file. Private helper components used only within the same file are acceptable, but any component intended for reuse must have its own file. Import components via barrel exports (`../components/ui`) rather than direct file paths.

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

### Testing Strategy

**Test Types:**
1. **Unit** (`*.test.ts`) - Pure functions, stores, hooks with mocked wagmi
2. **Component** (Storybook + play functions) - UI interactions
3. **Integration** (`*.integration.test.ts`) - Real contracts via Anvil
4. **E2E** (`e2e/*.spec.ts`) - Critical user journeys

**Commands:**
```bash
npm test                   # Unit tests
npm run test:integration   # Integration tests (requires: npm run anvil)
npm run test:e2e           # E2E tests
npm run test:e2e:ui        # E2E with Playwright UI
```

**When to Write Each Type:**
- Pure function → Unit test
- Zustand store → Unit test
- Hook with wagmi → Unit test (mock wagmi) + Integration test (real contracts)
- Component with interactions → Storybook story + play function
- Multi-page user flow → E2E (only critical paths)

**Required Patterns:**
- Mock wagmi BEFORE importing hooks (hoisting matters)
- Use `Result.isOk(result)` / `Result.isError(result)` (static methods, not instance)
- Use `createTestWrapper()` for hooks needing React context
- Reset Zustand stores in `beforeEach`
- Skip integration tests gracefully if contracts not deployed

**Example - Hook Unit Test:**
```typescript
const mockWriteContract = vi.fn()
vi.mock('wagmi', () => ({
  useWriteContract: () => ({ writeContract: mockWriteContract, ... }),
}))

import { useMyHook } from '../useMyHook'  // Import AFTER mock

beforeEach(() => {
  vi.resetAllMocks()
  useTransactionStore.setState({ pendingTransactions: [] })
})
```

**Test File Locations:**
- Unit: `src/**/__tests__/*.test.{ts,tsx}`
- Integration: `src/**/__tests__/*.integration.test.{ts,tsx}`
- E2E: `e2e/tests/*.spec.ts`
- Stories: `src/stories/*.stories.tsx`

**General Rules:**
- Tests live in `__tests__/` directories adjacent to code
- Never reimplement application logic in tests - import and test actual functions
- Design for testability using "functional core, imperative shell": keep pure business logic in `src/utils/` separate from IO code (hooks, API calls)

### Storybook
- For interactive stories, use `play` function with `step()` for named steps
- Import from `storybook/test` (Storybook 10+)

### Source Code Reference

Source code for dependencies is available in `opensrc/` for deeper understanding of implementation details. Use this when you need to understand how a package works internally, not just its types/interface.

See `opensrc/sources.json` for the list of available packages and their versions.

**Fetching Additional Source Code:**
```bash
npx opensrc <package>           # npm package (e.g., npx opensrc zod)
npx opensrc pypi:<package>      # Python package (e.g., npx opensrc pypi:requests)
npx opensrc crates:<package>    # Rust crate (e.g., npx opensrc crates:serde)
npx opensrc <owner>/<repo>      # GitHub repo (e.g., npx opensrc vercel/ai)
```
