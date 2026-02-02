# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Issue Tracking

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

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

Plether is a DeFi frontend for trading plDXY-BEAR and plDXY-BULL tokens on Ethereum.

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

### Contract Reference

See `API.md` for the complete protocol API reference including all contract functions, parameters, and error types.

### Error Decoding

See `APIERRORS.md` for all contract error selectors. Use this to:
- **Decode revert errors**: When a contract call fails with a custom error (e.g., `0x50285b92`), look up the selector to find the error name and meaning
- **Ensure comprehensive error handling**: When implementing new features, review relevant contract errors to handle all possible failure cases with appropriate user messages

### Adding Contract Integration
1. Add ABI to `src/contracts/abis/`
2. Add address to `src/contracts/addresses.ts` (mainnet + sepolia)
3. Create hook in `src/hooks/` using wagmi's `useReadContract`/`useWriteContract`
4. Return `Result<T, TransactionError>` from async operations

### Theme Colors
Defined in `src/index.css` via `@theme`:
- `cyber-neon-green` (#00FF99) - Primary accent, plDXY-BULL
- `cyber-electric-fuchsia` (#FF00CC) - plDXY-BEAR, secondary actions
- `bear` / `bull` - Aliases for token-specific styling

### Currency Display
- **Never use dollar sign ($)** to represent USDC values
- Use `formatUsd()` from `src/utils/formatters.ts` which formats numbers without $
- Append "USDC" suffix where appropriate (e.g., "100.00 USDC" not "$100.00")

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

### Mock Wallet for MCP Testing

To test with a connected wallet state, inject the mock wallet before navigating. The mock uses EIP-6963 for auto-connection.

```javascript
// Step 1: Inject mock wallet with EIP-6963 (run ONCE per browser session)
mcp__playwright__browser_run_code({
  code: `async (page) => {
    await page.addInitScript(() => {
      const MOCK_ADDRESS = '0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266';
      const CHAIN_ID = '0xaa36a7';
      const mockProvider = {
        isMetaMask: true,
        selectedAddress: MOCK_ADDRESS,
        chainId: CHAIN_ID,
        networkVersion: '11155111',
        _metamask: { isUnlocked: () => Promise.resolve(true) },
        request: async ({ method }) => {
          if (method === 'eth_requestAccounts' || method === 'eth_accounts') return [MOCK_ADDRESS];
          if (method === 'eth_chainId') return CHAIN_ID;
          if (method === 'eth_getBalance') return '0x8ac7230489e80000';
          if (method === 'eth_call') return '0x' + '0'.repeat(64);
          return null;
        },
        on: (event, cb) => {
          if (event === 'accountsChanged') setTimeout(() => cb([MOCK_ADDRESS]), 100);
          if (event === 'chainChanged') setTimeout(() => cb(CHAIN_ID), 100);
        },
        removeListener: () => {},
      };
      Object.defineProperty(window, 'ethereum', { value: mockProvider, writable: false });
      // EIP-6963: Auto-announce as MetaMask
      const announce = () => window.dispatchEvent(new CustomEvent('eip6963:announceProvider', {
        detail: Object.freeze({
          info: { uuid: 'mock', name: 'MetaMask', icon: '', rdns: 'io.metamask' },
          provider: mockProvider
        })
      }));
      window.addEventListener('eip6963:requestProvider', announce);
      announce();
      setTimeout(announce, 100);
    });
    return 'Mock wallet ready';
  }`
});

// Step 2: Navigate to page (wallet will auto-connect via EIP-6963)
mcp__playwright__browser_navigate({ url: "http://localhost:5173" });
```

Full implementation: `e2e/fixtures/mockWallet.ts`

### Documentation Screenshots

Screenshots for documentation live in `screenshots/`. Use consistent settings:

**Resolution:** 1024x1000 pixels (set with `mcp__playwright__browser_resize`)

**Setup for Real Transactions:**
1. Move `addresses.local.json` to `.bak` so app uses Sepolia addresses
2. Change wagmi config to use port 8546 (`http://127.0.0.1:8546`)
3. Use `MOCK_WALLET_ANVIL_SCRIPT` from `e2e/fixtures/mockWallet.ts` which proxies to Anvil
4. Start Anvil fork: `npm run anvil` (uses port 8546 with Sepolia fork)

**Key Lessons:**
- `addInitScript` only runs on NEW page loads - close browser and reopen to apply changes
- Hide TanStack devtools before screenshots: `document.querySelector('.tsqd-open-btn-container').style.display = 'none'`
- Mock wallet needs `eth_sendTransaction` handler to proxy transactions to Anvil
- Anvil test account `0xf39F...2266` is pre-funded and unlocked for direct tx sending
- Screenshots save to `.playwright-mcp/` - copy to `screenshots/` directory
- Always verify dimensions with `file screenshots/*.png` for consistency

**Workflow:**
```bash
# 1. Setup
mv src/contracts/addresses.local.json src/contracts/addresses.local.json.bak
# Edit wagmi.ts: change anvil port to 8546

# 2. Take screenshots with Playwright MCP
mcp__playwright__browser_resize(1024, 1000)
mcp__playwright__browser_run_code(MOCK_WALLET_ANVIL_SCRIPT)
mcp__playwright__browser_navigate("http://localhost:5173/mint")
# ... interact and screenshot ...

# 3. Cleanup
cp .playwright-mcp/*.png screenshots/
mv src/contracts/addresses.local.json.bak src/contracts/addresses.local.json
# Revert wagmi.ts port change
```

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
