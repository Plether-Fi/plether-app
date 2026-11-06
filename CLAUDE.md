# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Structure

This is a monorepo with the following structure:
- `apps/frontend/` - React frontend application
- `apps/api/` - Backend API (non-JavaScript, coming soon)

## Issue Tracking

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

## Git Policy

**Do not commit unless explicitly asked.** Wait for the user to request a commit before staging or committing changes.

**Never amend commits.** Always create new commits. Only use `--amend` if the user explicitly asks for it.

## Session Completion

**When ending a work session**, complete ALL steps below. Work is NOT complete until `git push` succeeds.

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Push to remote**:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Hand off** - Provide context for next session

**Rules:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- If push fails, resolve and retry until it succeeds

## Commands

All commands run from `apps/frontend/`:

```bash
cd apps/frontend
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

Plether is a DeFi frontend for trading plDXY-BEAR and plDXY-BULL tokens on Ethereum.

### Tech Stack
- **Framework**: Vite + React 19 + TypeScript
- **Web3**: Privy + wagmi + viem (external and embedded EVM wallets)
- **Styling**: Tailwind CSS v4 (CSS-first config in `apps/frontend/src/index.css`)
- **State**: Zustand for local state, TanStack Query for server state
- **Error Handling**: better-result for typed Result-based error handling

### Key Directories
- `apps/frontend/src/pages/` - Route components (Dashboard, Mint, Stake, History)
- `apps/frontend/src/components/ui/` - Reusable UI primitives
- `apps/frontend/src/hooks/` - Contract interaction hooks returning `Result<T, Error>` types
- `apps/frontend/src/stores/` - Zustand stores (transactions, settings)
- `apps/frontend/src/contracts/` - ABIs and addresses (mainnet + sepolia)
- `apps/frontend/src/utils/errors.ts` - TaggedError definitions for transaction errors

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

Error types are defined as TaggedErrors in `apps/frontend/src/utils/errors.ts`:
- `UserRejectedError` - User cancelled transaction
- `InsufficientFundsError` - Gas, token, or allowance issues
- `ContractRevertError` - Contract execution failed
- `NetworkError`, `TimeoutError`, `UnknownTransactionError`

### Wallet & Contract Patterns
- **Approvals**: Always use exact amount approvals, never unlimited
- **Networks**: Mainnet (1), Sepolia (11155111), Anvil local fork (31337)
- **Slippage**: Max 1% (protocol limit), stored in settingsStore

### DeFi Math — Decimal/Scale Rules

**Never trust code comments about scales. Always verify against actual on-chain values.**

When writing formulas with token amounts, oracle prices, or protocol values:

1. **Query the actual value** via RPC (`eth_call`) to determine its magnitude empirically
2. **Label each operand's decimals**: e.g., `collateral(18) * oraclePrice(36) * lltv(18)`
3. **Compute the divisor**: `divisor = sum_of_input_decimals - desired_output_decimals`
   - Example: `18 + 36 - 6 = 48` → divide by `10^48` to get USDC (6 dec)
4. **Sanity check**: plug in real values and verify the output is plausible

Reference scales for this project:
| Value | Scale | How to verify |
|-------|-------|---------------|
| ERC20 amount | `decimals()` (USDC=6, tokens=18) | `balanceOf()` |
| Morpho oracle price | 10^36 | `price()` on the oracle |
| Morpho LLTV | 10^18 | `marketParams.lltv` |

### Contract Reference

See `API.md` for the complete protocol API reference including all contract functions, parameters, and error types.

### Error Decoding

See `APIERRORS.md` for all contract error selectors. Use this to:
- **Decode revert errors**: When a contract call fails with a custom error (e.g., `0x50285b92`), look up the selector to find the error name and meaning
- **Ensure comprehensive error handling**: When implementing new features, review relevant contract errors to handle all possible failure cases with appropriate user messages

### Adding Contract Integration
1. Add ABI to `apps/frontend/src/contracts/abis/`
2. Add address to `apps/frontend/src/contracts/addresses.ts` (mainnet + sepolia)
3. Create hook in `apps/frontend/src/hooks/` using wagmi's `useReadContract`/`useWriteContract`
4. Return `Result<T, TransactionError>` from async operations

### Backend API Client

The `apps/frontend/src/api/` directory contains the typed client for the Plether backend API. This layer provides:
- **Aggregated data fetching** - Single API call returns all dashboard data
- **Server-side caching** - Reduces RPC costs and improves load times
- **Real-time updates** - WebSocket connection for price streaming
- **Transaction history** - Event-indexed historical data

See `specs/backend-api.md` for the full API specification.

**Usage:**
```typescript
// React Query hooks (preferred for components)
import { useUserDashboard, useProtocolStatus, useMintQuote } from '../api';

function Dashboard() {
  const { data: protocol } = useProtocolStatus();
  const { data: user } = useUserDashboard(address);
  const { data: quote } = useMintQuote(amount);
}

// Direct client (for non-React code)
import { plethApi } from '../api';

const result = await plethApi.getUserDashboard(address);
if (Result.isOk(result)) {
  console.log(result.value.data);
}
```

**Migration Strategy:**
- Read operations: Migrate to API hooks (when backend is deployed)
- Write operations: Keep using wagmi hooks (transactions stay client-side)
- Approvals: Keep using wagmi hooks (user wallet interaction required)

### Theme Colors
Defined in `apps/frontend/src/index.css` via `@theme`:
- `positive` (#00FF99) - plDXY-BULL and positive state
- `brand-orange` (#FF572D) - plDXY-BEAR, negative state, and warning actions
- `bear` / `bull` - Aliases for token-specific styling

### Currency Display
- **Never use dollar sign ($)** to represent USDC values
- Use `formatUsd()` from `apps/frontend/src/utils/formatters.ts` which formats numbers without $
- Append "USDC" suffix where appropriate (e.g., "100.00 USDC" not "$100.00")
- Always use 2 decimal places for USDC values
- Values less than 0.01 but greater than 0 display as "<0.01 USDC"

### Testing Strategy

**Test Types:**
1. **Unit** (`*.test.ts`) - Pure functions, stores, hooks with mocked wagmi
2. **Component** (Storybook + play functions) - UI interactions
3. **Integration** (`*.integration.test.ts`) - Real contracts via Anvil
4. **E2E** (`apps/frontend/e2e/*.spec.ts`) - Critical user journeys

**Commands:**
```bash
cd apps/frontend
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
- Unit: `apps/frontend/src/**/__tests__/*.test.{ts,tsx}`
- Integration: `apps/frontend/src/**/__tests__/*.integration.test.{ts,tsx}`
- E2E: `apps/frontend/e2e/tests/*.spec.ts`
- Stories: `apps/frontend/src/stories/*.stories.tsx`

**General Rules:**
- Tests live in `__tests__/` directories adjacent to code
- Never reimplement application logic in tests - import and test actual functions
- Design for testability using "functional core, imperative shell": keep pure business logic in `apps/frontend/src/utils/` separate from IO code (hooks, API calls)

### Storybook
- For interactive stories, use `play` function with `step()` for named steps
- Import from `storybook/test` (Storybook 10+)

## Visual Development & Testing

### Quick Visual Check

**IMMEDIATELY after implementing any front-end change:**

1. **Identify what changed** - Review the modified components/pages
2. **Navigate to affected pages** - Use `mcp__playwright__browser_navigate` to visit each changed view
3. **Verify design compliance** - Compare against `/context/design-principles.md`
4. **Validate feature implementation** - Ensure the change fulfills the user's specific request
5. **Check acceptance criteria** - Review any provided context files or requirements
6. **Capture evidence** - Take full page screenshot at desktop viewport (1440px) of each changed view
7. **Check for errors** - Run `mcp__playwright__browser_console_messages` ⚠️

This verification ensures changes meet design standards and user requirements.

### Playwright MCP Integration

#### Essential Commands for UI Testing

```javascript
// Navigation & Screenshots
mcp__playwright__browser_navigate(url); // Navigate to page
mcp__playwright__browser_take_screenshot(); // Capture visual evidence
mcp__playwright__browser_resize(
  width,
  height
); // Test responsiveness

// Interaction Testing
mcp__playwright__browser_click(element); // Test clicks
mcp__playwright__browser_type(
  element,
  text
); // Test input
mcp__playwright__browser_hover(element); // Test hover states

// Validation
mcp__playwright__browser_console_messages(); // Check for errors
mcp__playwright__browser_snapshot(); // Accessibility check
mcp__playwright__browser_wait_for(
  text / element
); // Ensure loading
```

### Design Compliance Checklist

When implementing UI features, verify:

- [ ] **Visual Hierarchy**: Clear focus flow, appropriate spacing
- [ ] **Consistency**: Uses design tokens, follows patterns
- [ ] **Responsiveness**: Works on mobile (375px), tablet (768px), desktop (1440px)
- [ ] **Accessibility**: Keyboard navigable, proper contrast, semantic HTML
- [ ] **Performance**: Fast load times, smooth animations (150-300ms)
- [ ] **Error Handling**: Clear error states, helpful messages
- [ ] **Polish**: Micro-interactions, loading states, empty states

### Wallet Tests and Screenshots

The canonical Playwright wallet fixture is
`apps/frontend/e2e/fixtures/wallet.ts`. Import its `test` and `expect`, request
`mockConnectedWallet`, and await it before the first navigation:

```typescript
import { test, expect } from '../fixtures/wallet'

test('wallet network changes', async ({ page, mockConnectedWallet }) => {
  const wallet = await mockConnectedWallet()
  await page.goto('/')
  await wallet.setChain(421614)
  await expect.poll(() => page.evaluate(async () => {
    const provider = (window as unknown as {
      ethereum: { request(input: { method: string }): Promise<string> }
    }).ethereum
    return provider.request({ method: 'eth_chainId' })
  })).toBe('0x66eee')
})
```

For a standalone Playwright setup, call `installTestWallet(page)` before
navigation. The injected provider is discoverable as **Plether Test Wallet**
through EIP-6963; injection does not complete Privy login or automatically select
the wallet in the application. UI connection tests need a configured development
Privy app and must select the detected wallet through the connection flow.

The fixture produces real `personal_sign` and `eth_signTypedData_v4` signatures
from public Anvil test accounts. Its returned controller supports
`setAccount(0 | 1)`, `setChain(chainId)`, `disconnect()`, and
`rejectNextRequest(method)`. Account and network changes emit the corresponding
provider events, and rejected requests return wallet error codes. It does not
submit transactions or contact a live RPC; unsupported methods reject with code
4200. Use the local Perps/Anvil integration tooling for on-chain operations and
real Privy development accounts for social login and embedded-wallet checks.

Documentation screenshots live in `screenshots/`. Set a 1024 × 1000 viewport,
wait for the intended connection and loading states, hide development controls,
and capture with `page.screenshot({ path: 'screenshots/example.png' })`. Verify
the saved image dimensions and inspect it before adding it to documentation.
Do not change contract-address files or production RPC configuration just to
stage a screenshot.

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
