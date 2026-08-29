# Plether Frontend

React web application for trading plDXY-BEAR and plDXY-BULL tokens.

## Tech Stack

- React 19 + TypeScript + Vite
- wagmi + viem + permissionless.js + Web3Modal
- Tailwind CSS v4
- Zustand + TanStack Query

## Development

```bash
npm install
npm run dev        # Dev server at http://localhost:5173
npm run build      # TypeScript check + production build
npm run lint       # ESLint
npm test           # Unit tests
npm run anvil      # Start Anvil fork (port 8546, requires SEPOLIA_RPC_URL)
npm run test:integration  # Integration tests (requires Anvil)
npm run storybook  # Component explorer at http://localhost:6006
npm run perps:oracle-worker -- --once  # Update the perps mark from backend-cached Pyth data
npm run perps:oracle-worker -- --loop  # Keep updating the perps mark from backend cache
```

## TradingView Advanced Charts

The plDXY Perpetual panel uses TradingView Advanced Charts exclusively. If its
licensed runtime assets are unavailable, the panel shows an explicit unavailable
state instead of switching chart engines.

Clone the private TradingView repository outside this public repository, then
install the runtime assets locally:

```bash
git clone --depth 1 --branch v32.0.0 git@github.com:tradingview/charting_library.git /tmp/tradingview-charting_library
npm run tradingview:install -- /tmp/tradingview-charting_library
npm run dev
```

`public/charting_library/` is intentionally ignored by Git. Never commit,
publish as source, or attach those files to a public CI artifact. Production
deployments fetch version `v32.0.0` (pinned to commit
`f2a61ba473ec254b69f9c1377c67e4b81eff853b`) with the read-only
`TRADINGVIEW_GITHUB_TOKEN` GitHub Actions secret and copy only the runtime
assets into the deployed site.

Before enabling Advanced Charts in production:

- have counsel review the public Terms of Service and the rights to redistribute
  the market data displayed by the chart;
- have counsel confirm that the proprietary runtime can be loaded alongside
  this AGPL-licensed frontend under the intended deployment model, or document
  an approved licensing exception or isolation strategy;
- publish the required announcement about the licensed TradingView integration, or
  obtain TradingView's written approval for an alternative promotional
  placement that includes a contextual backlink;
- keep both the chart's built-in attribution and the site-wide "Charts by
  TradingView" link visible; and
- ensure TradingView can access the public implementation for compliance review.

## Managed gas sponsorship

The Arbitrum Sepolia integration currently uses a deterministic
permissionless.js SimpleAccount v0.8 as the Trading Account. The connected
wallet is its owner and is never used as a fallback transaction sender.

This account choice is for managed testnet testing only. The official
SimpleAccount is upgradeable and does not satisfy the core handoff's
production requirement for immutable execution semantics. Production remains
disabled until a reviewed immutable account deployment replaces it. Treat
these testnet Trading Accounts as disposable because a replacement factory
will derive different addresses.

Local and hosted testnet deployments use the bundled
`/perps-aa-manifest.json` by default. Set `VITE_PERPS_AA_MANIFEST_URL` only to
override that path. Its `pimlicoRpcUrl` is a same-origin
`/api/perps/v1/aa/...` path. The backend proxy owns the Pimlico API key,
injects the approved sponsorship policy, validates Plether call targets, and
relays only the required bundler/paymaster/status methods.

Configure `AA_PROXY_ORIGIN_TOKEN` as a Cloudflare Pages secret and require its
`X-Plether-AA-Proxy-Token` value on the backend `/api/aa/pimlico` route. This
keeps direct callers from spoofing the client IP used for rate limits. The
testnet deploy workflow provisions the Pages value from the GitHub Actions
secret with the same name; the backend must receive the matching secret.

Before `eth_sendUserOperation`, the frontend atomically persists the locally
computed hash with the signed UserOperation preimage. Recovery parses that
preimage, recomputes and matches the exact hash, and derives the nonce and
nonzero pinned-Pimlico `validUntil` from it. Pimlico statuses are diagnostic
only after an exact receipt miss. A receipt becomes terminal evidence only
after its transaction and EntryPoint event are verified against the canonical
RPC at or below the safe head. Pimlico outages remain inconclusive but do not
prevent independent safe-chain nonce and expiry recovery.

The trade ticket may progress earlier from an exact canonical-latest receipt
after its matching EntryPoint event and expected protocol event are validated.
That interactive inclusion signal does not mark the sponsored operation
confirmed, persist a verified transaction hash, clear authorization state, or
release its lane; all retry and recovery decisions continue to require the
safe-chain evidence above.

At one safe block, recovery reads the timestamp and EntryPoint nonce for the
key encoded in the nonce's upper 192 bits. An unchanged nonce after the
verified deadline is retry-safe expired. An advanced nonce without a verified
event releases the lane as outcome unknown and non-retryable because the old
nonce cannot land.

Blockscout is only a positive event locator: empty or missing results never
prove absence, and every hit is verified by an exact one-block canonical RPC
`UserOperationEvent` query. Third-party, RPC, and corrupt-data failures remain
locked unless the verified protocol nonce/expiry proof resolves future
execution. Legacy hash-only records and old diagnostic terminal labels are
re-locked by storage migration, backfilled into their directly addressed lane
head while holding that lane's browser lock, and never auto-expire. Each one
instead exposes a force-release escape hatch warning that the old action may
already have executed or may still execute later. It requires reviewing the
Trading Account and operation hash and explicitly accepting the risk before
repeating the action. Before retrying, close or reload every other Plether tab
so an already-open legacy client cannot restore the obsolete shared-store lock.

Submission, stale-record recovery, and manual release share a browser-wide
lane lock. The app refuses to send unless its exact hash and signed preimage
were durably written to a dedicated per-operation recovery journal and a
directly addressable chain/account/lane head. Under the lane lock, submission
and recovery read that head directly rather than depending on storage-key
enumeration. The conservative head is written before its journals, making a
crash between durable writes fail closed. Cross-tab hydration merges journals
with the old shared snapshot, but version 1 treats that whole-store key as a
read-only legacy inbox. Mutable current persistence uses per-operation
journals; only the locked migration and submission paths mutate lane heads.
Manual and canonical resolutions are written first to append-only
ID/hash/status tombstones and overlaid on every recovery read. This prevents
any current tab—not just the submitting one—from erasing legacy evidence or
rewriting a resolved operation into a blocker through a stale write.
Hash-verified records can continue recovery across a metadata-only manifest
version bump; a changed EntryPoint still fails hash verification closed.
Operations are never automatically rebuilt or sent through the owner EOA.

## Networks

- Mainnet (chainId 1)
- Sepolia (chainId 11155111)
- Arbitrum Sepolia (chainId 421614)
- Anvil local fork (chainId 31337) via `npm run anvil`

## License

AGPL-3.0-only
