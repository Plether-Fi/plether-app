# Plether

[![Tests](https://github.com/plether-fi/plether-frontend/actions/workflows/test.yml/badge.svg)](https://github.com/plether-fi/plether-frontend/actions/workflows/test.yml)
[![Storybook](https://img.shields.io/badge/storybook-deployed-ff4785)](https://plether-fi.github.io/plether-frontend/)
[![License: AGPL-3.0](https://img.shields.io/badge/license-AGPL--3.0-blue)](LICENSE)

DeFi frontend for trading DXY-BEAR and DXY-BULL tokens on Ethereum.

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
npm run storybook  # Component explorer at http://localhost:6006
```

## Storybook

https://plether-fi.github.io/plether-frontend/

## Networks

- Mainnet (chainId 1)
- Sepolia (chainId 11155111)
- Anvil local fork (chainId 31337) via `npm run anvil`

## License

AGPL-3.0-only
