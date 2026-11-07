import type { PrivyClientConfig } from '@privy-io/react-auth'
import { createConfig } from '@privy-io/wagmi'
import { http } from 'wagmi'
import { arbitrumSepolia } from 'wagmi/chains'

function optionalValue(value: unknown): string | undefined {
  return typeof value === 'string' && value.trim().length > 0 ? value.trim() : undefined
}

export const privyAppId = optionalValue(import.meta.env.VITE_PRIVY_APP_ID)
export const privyClientId = optionalValue(import.meta.env.VITE_PRIVY_CLIENT_ID)
export const walletConnectionConfigured = Boolean(privyAppId)

const rpcUrl = optionalValue(import.meta.env.VITE_ARBITRUM_SEPOLIA_RPC_URL)
  ?? 'https://sepolia-rollup.arbitrum.io/rpc'

export const insightsChain = {
  ...arbitrumSepolia,
  rpcUrls: { default: { http: [rpcUrl] } },
}

export const privyConfig = {
  loginMethods: ['email', 'google', 'passkey'],
  appearance: {
    theme: '#250917',
    accentColor: '#FF572D',
    logo: '/logomark.svg',
    landingHeader: 'Connect',
    showWalletLoginFirst: false,
    walletChainType: 'ethereum-only',
    walletList: ['metamask', 'coinbase_wallet', 'detected_ethereum_wallets', 'wallet_connect'],
  },
  embeddedWallets: { ethereum: { createOnLogin: 'users-without-wallets' } },
  // With no defaultChain, external wallets may connect on their current chain.
  // Registration explicitly switches before issuing an ownership challenge.
  supportedChains: [insightsChain],
} satisfies PrivyClientConfig

export const wagmiConfig = createConfig({
  chains: [insightsChain],
  transports: { [arbitrumSepolia.id]: http(rpcUrl) },
})

// The trading frontend declares wagmi's Register type; avoid a conflicting declaration here.
