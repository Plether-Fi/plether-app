import { http, type Config } from 'wagmi'
import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'
import { defineChain } from 'viem'
import { WagmiAdapter } from '@reown/appkit-adapter-wagmi'
import { createAppKit } from '@reown/appkit/react'
import { mainnet as appKitMainnet, sepolia as appKitSepolia } from '@reown/appkit/networks'
import type { AppKitNetwork } from '@reown/appkit/networks'
import { transactionManager } from '../services/transactionManager'

const WALLETCONNECT_PROJECT_ID = '1ac6ecffb101d037c113363688a6ef8e'

export const anvil = defineChain({
  id: 31337,
  name: 'Anvil',
  nativeCurrency: { name: 'Ether', symbol: 'ETH', decimals: 18 },
  rpcUrls: {
    default: { http: ['http://127.0.0.1:8545'] },
  },
})

const appKitAnvil = {
  id: 31337,
  name: 'Anvil',
  chainNamespace: 'eip155' as const,
  caipNetworkId: 'eip155:31337' as const,
  nativeCurrency: { name: 'Ether', symbol: 'ETH', decimals: 18 },
  rpcUrls: {
    default: { http: ['http://127.0.0.1:8545'] },
  },
} satisfies AppKitNetwork

const appKitArbitrumSepolia = {
  id: arbitrumSepolia.id,
  name: 'Arbitrum Sepolia',
  chainNamespace: 'eip155' as const,
  caipNetworkId: 'eip155:421614' as const,
  nativeCurrency: arbitrumSepolia.nativeCurrency,
  rpcUrls: {
    default: { http: ['https://sepolia-rollup.arbitrum.io/rpc'] },
  },
  blockExplorers: {
    default: { name: 'Arbiscan', url: 'https://sepolia.arbiscan.io' },
  },
} satisfies AppKitNetwork

const networks: [AppKitNetwork, ...AppKitNetwork[]] = [
  appKitMainnet,
  appKitSepolia,
  appKitArbitrumSepolia,
  appKitAnvil,
]

const metadata = {
  name: 'Plether',
  description: 'Synthetic Dollar Strength and Weakness Protocol by Plether Labs Limited',
  url: window.location.origin,
  icons: [`${window.location.origin}/logo.png`],
}

const wagmiAdapter = new WagmiAdapter({
  projectId: WALLETCONNECT_PROJECT_ID,
  networks,
  transports: {
    [mainnet.id]: http('https://eth-mainnet.g.alchemy.com/v2/7RXotrWbfzbfZZvA4ARaZ'),
    [sepolia.id]: http('https://eth-sepolia.g.alchemy.com/v2/7RXotrWbfzbfZZvA4ARaZ'),
    [arbitrumSepolia.id]: http('https://sepolia-rollup.arbitrum.io/rpc'),
    [anvil.id]: http('http://127.0.0.1:8545'),
  },
})

createAppKit({
  adapters: [wagmiAdapter],
  projectId: WALLETCONNECT_PROJECT_ID,
  networks,
  metadata,
  themeMode: 'dark',
  themeVariables: {
    '--w3m-accent': '#FF00CC',
    '--w3m-color-mix': '#00FF99',
    '--w3m-color-mix-strength': 10,
    '--w3m-border-radius-master': '0px',
  },
})

type Chains = readonly [typeof mainnet, typeof sepolia, typeof arbitrumSepolia, typeof anvil]
export const config = wagmiAdapter.wagmiConfig as Config<Chains>

declare module 'wagmi' {
  interface Register {
    config: typeof config
  }
}

transactionManager.setConfig(config)
