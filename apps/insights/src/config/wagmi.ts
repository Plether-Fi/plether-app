import { createAppKit } from '@reown/appkit/react'
import { WagmiAdapter } from '@reown/appkit-adapter-wagmi'
import type { AppKitNetwork } from '@reown/appkit/networks'
import { http, type Config } from 'wagmi'
import { arbitrumSepolia } from 'wagmi/chains'

const configuredProjectId: unknown = import.meta.env.VITE_WALLETCONNECT_PROJECT_ID
// WalletConnect project IDs are public browser identifiers. Reuse the same
// project as the trading frontend for local development; production builds
// still provide and validate VITE_WALLETCONNECT_PROJECT_ID explicitly.
const projectId = typeof configuredProjectId === 'string' && configuredProjectId.trim().length > 0
  ? configuredProjectId.trim()
  : 'ac255192981643094de1bdfd0f501d55'
const configuredRpcUrl: unknown = import.meta.env.VITE_ARBITRUM_SEPOLIA_RPC_URL
const rpcUrl = typeof configuredRpcUrl === 'string' && configuredRpcUrl.trim().length > 0
  ? configuredRpcUrl.trim()
  : 'https://sepolia-rollup.arbitrum.io/rpc'

export const appKitArbitrumSepolia = {
  id: arbitrumSepolia.id,
  name: 'Arbitrum Sepolia',
  chainNamespace: 'eip155' as const,
  caipNetworkId: `eip155:${String(arbitrumSepolia.id)}` as const,
  nativeCurrency: arbitrumSepolia.nativeCurrency,
  rpcUrls: { default: { http: [rpcUrl] } },
  blockExplorers: {
    default: { name: 'Arbiscan', url: 'https://sepolia.arbiscan.io' },
  },
} satisfies AppKitNetwork

const networks: [AppKitNetwork, ...AppKitNetwork[]] = [appKitArbitrumSepolia]
const wagmiAdapter = new WagmiAdapter({
  projectId,
  networks,
  transports: {
    [arbitrumSepolia.id]: http(rpcUrl),
  },
})

export const walletConnectionConfigured = projectId.length > 0

if (walletConnectionConfigured) {
  createAppKit({
    adapters: [wagmiAdapter],
    projectId,
    networks,
    defaultNetwork: appKitArbitrumSepolia,
    // Do not interrupt registration on page load when a previously connected
    // wallet is on another chain. The wallet step performs the explicit switch
    // to Arbitrum Sepolia immediately before issuing a signing challenge.
    allowUnsupportedChain: true,
    metadata: {
      name: 'Plether Insights',
      description: 'Plether testnet competition registration',
      url: window.location.origin,
      icons: [`${window.location.origin}/logomark.svg`],
    },
    themeMode: 'dark',
    themeVariables: {
      '--w3m-font-family': "'Uncut Sans', ui-sans-serif, system-ui, sans-serif",
      '--w3m-accent': '#FF572D',
      '--w3m-color-mix': '#250917',
      '--w3m-color-mix-strength': 18,
      '--w3m-border-radius-master': '0px',
      '--w3m-z-index': 80,
    },
    features: {
      analytics: false,
      email: false,
      socials: false,
      swaps: false,
      onramp: false,
      receive: false,
      send: false,
      history: false,
      smartSessions: false,
      pay: false,
      reownAuthentication: false,
    },
  })
}

export const wagmiConfig = wagmiAdapter.wagmiConfig as Config<readonly [typeof arbitrumSepolia]>

declare module 'wagmi' {
  interface Register {
    config: typeof wagmiConfig
  }
}
