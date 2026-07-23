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

The frontend persists the locally computed UserOperation hash before calling
`eth_sendUserOperation`. Ambiguous submissions remain locked for status and
receipt reconciliation; they are not automatically rebuilt or sent through
the owner EOA.

## Networks

- Mainnet (chainId 1)
- Sepolia (chainId 11155111)
- Arbitrum Sepolia (chainId 421614)
- Anvil local fork (chainId 31337) via `npm run anvil`

## License

AGPL-3.0-only
