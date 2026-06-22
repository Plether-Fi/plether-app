# Plether

[![Tests](https://github.com/Plether-Fi/plether-app/actions/workflows/test.yml/badge.svg)](https://github.com/Plether-Fi/plether-app/actions/workflows/test.yml)
[![License: AGPL-3.0](https://img.shields.io/badge/license-AGPL--3.0-blue)](LICENSE)

DeFi protocol for trading plDXY-BEAR and plDXY-BULL synthetic tokens on Ethereum.

## Repository Structure

```
apps/
├── frontend/    # React web application
└── backend/     # Haskell/Scotty API server
```

## Quick Start

### Frontend

```bash
cd apps/frontend
npm install
npm run dev          # http://localhost:5173
```

### Backend

```bash
cd apps/backend
cp .env.example .env
# Edit .env with your RPC_URL
cabal build
cabal run plether-api   # http://localhost:3001
```

## Documentation

- [Frontend README](apps/frontend/README.md) - React app setup, testing, Storybook
- [Backend README](apps/backend/README.md) - API endpoints, caching, configuration
- [Plether Core ABI/API Reference](https://plether-fi.github.io/plether-core/) - Smart contract interfaces and generated API docs

## Networks

| Network | Chain ID | Description |
|---------|----------|-------------|
| Mainnet | 1 | Production |
| Sepolia | 11155111 | Testnet |
| Anvil | 31337 | Local development |

## License

AGPL-3.0-only
