import { http, createConfig } from 'wagmi'
import { defineChain } from 'viem'
import { mainnet, sepolia } from 'wagmi/chains'
import { injected, walletConnect } from 'wagmi/connectors'
import { transactionManager } from '../services/transactionManager'

// WalletConnect project ID - replace with your own from https://cloud.walletconnect.com
export const WALLETCONNECT_PROJECT_ID = '1ac6ecffb101d037c113363688a6ef8e'

export const anvil = defineChain({
  id: 31337,
  name: 'Anvil',
  nativeCurrency: { name: 'Ether', symbol: 'ETH', decimals: 18 },
  rpcUrls: {
    default: { http: ['http://127.0.0.1:8545'] },
  },
})

export const config = createConfig({
  chains: [mainnet, sepolia, anvil],
  connectors: [
    injected(),
    walletConnect({
      projectId: WALLETCONNECT_PROJECT_ID,
      metadata: {
        name: 'Plether',
        description: 'plDXY-BEAR and plDXY-BULL trading protocol',
        url: 'https://app.plether.com',
        icons: ['https://app.plether.com/logo.png'],
      },
    }),
  ],
  transports: {
    [mainnet.id]: http(),
    [sepolia.id]: http(),
    [anvil.id]: http('http://127.0.0.1:8545'),
  },
})

declare module 'wagmi' {
  interface Register {
    config: typeof config
  }
}

// Initialize global transaction manager
transactionManager.setConfig(config)
