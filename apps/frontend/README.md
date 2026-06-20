# Plether Frontend

React web application for trading plDXY-BEAR and plDXY-BULL tokens.

## Tech Stack

- React 19 + TypeScript + Vite
- wagmi + viem + Web3Modal
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

## Networks

- Mainnet (chainId 1)
- Sepolia (chainId 11155111)
- Anvil local fork (chainId 31337) via `npm run anvil`

## License

AGPL-3.0-only
