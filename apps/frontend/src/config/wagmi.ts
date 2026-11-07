import { createConfig } from '@privy-io/wagmi'
import { http, type Config } from 'wagmi'
import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'
import { defineChain } from 'viem'
import { transactionManager } from '../services/transactionManager'

function optionalRpcUrl(value: unknown): string | undefined {
  return typeof value === 'string' && value.trim().length > 0 ? value.trim() : undefined
}

const mainnetRpcUrl = optionalRpcUrl(import.meta.env.VITE_MAINNET_RPC_URL)
const sepoliaRpcUrl = optionalRpcUrl(import.meta.env.VITE_SEPOLIA_RPC_URL)
const arbitrumSepoliaRpcUrl = optionalRpcUrl(import.meta.env.VITE_ARBITRUM_SEPOLIA_RPC_URL)
  ?? (import.meta.env.VITE_DEPLOYMENT_ENV === 'sepolia'
    ? new URL('/api/perps/v1/rpc', typeof window === 'undefined'
      ? 'https://plether-testnet.pages.dev' : window.location.origin).href
    : 'https://sepolia-rollup.arbitrum.io/rpc')

export const anvil = defineChain({
  id: 31337,
  name: 'Anvil',
  nativeCurrency: { name: 'Ether', symbol: 'ETH', decimals: 18 },
  rpcUrls: { default: { http: ['http://127.0.0.1:8545'] } },
})

// Privy and wagmi must use the same chains and public RPC URLs. Supplying the
// URLs on the chain objects also configures the embedded wallet's provider.
export const walletChains = [
  {
    ...mainnet,
    rpcUrls: { default: { http: [mainnetRpcUrl ?? mainnet.rpcUrls.default.http[0]] } },
  },
  {
    ...sepolia,
    rpcUrls: { default: { http: [sepoliaRpcUrl ?? sepolia.rpcUrls.default.http[0]] } },
  },
  {
    ...arbitrumSepolia,
    rpcUrls: { default: { http: [arbitrumSepoliaRpcUrl] } },
  },
  anvil,
] as const

export const config: Config<typeof walletChains> = createConfig({
  chains: walletChains,
  transports: {
    [mainnet.id]: http(mainnetRpcUrl),
    [sepolia.id]: http(sepoliaRpcUrl),
    [arbitrumSepolia.id]: http(arbitrumSepoliaRpcUrl),
    [anvil.id]: http('http://127.0.0.1:8545'),
  },
})

declare module 'wagmi' {
  interface Register {
    config: typeof config
  }
}

transactionManager.setConfig(config)
