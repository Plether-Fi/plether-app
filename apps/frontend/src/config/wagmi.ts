import { http, type Config } from 'wagmi'
import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'
import { defineChain } from 'viem'
import { WagmiAdapter } from '@reown/appkit-adapter-wagmi'
import { mainnet as appKitMainnet, sepolia as appKitSepolia } from '@reown/appkit/networks'
import type { AppKitNetwork } from '@reown/appkit/networks'
import { transactionManager } from '../services/transactionManager'

type AppKitInstance = ReturnType<(typeof import('@reown/appkit/react'))['createAppKit']>
type AppKitOpenOptions = Parameters<AppKitInstance['open']>[0]

const envWalletConnectProjectId: unknown = import.meta.env.VITE_WALLETCONNECT_PROJECT_ID
const WALLETCONNECT_PROJECT_ID =
  typeof envWalletConnectProjectId === 'string' && envWalletConnectProjectId.length > 0
    ? envWalletConnectProjectId
    : 'ac255192981643094de1bdfd0f501d55'
function optionalRpcUrl(value: unknown): string | undefined {
  return typeof value === 'string' && value.trim().length > 0 ? value.trim() : undefined
}

const MAINNET_RPC_URL = optionalRpcUrl(import.meta.env.VITE_MAINNET_RPC_URL)
const SEPOLIA_RPC_URL = optionalRpcUrl(import.meta.env.VITE_SEPOLIA_RPC_URL)
const envArbitrumSepoliaRpcUrl: unknown = import.meta.env.VITE_ARBITRUM_SEPOLIA_RPC_URL
const ARBITRUM_SEPOLIA_RPC_URL =
  typeof envArbitrumSepoliaRpcUrl === 'string' && envArbitrumSepoliaRpcUrl.length > 0
    ? envArbitrumSepoliaRpcUrl
    : 'https://sepolia-rollup.arbitrum.io/rpc'
const APPKIT_THEME_OVERRIDE_ID = 'plether-appkit-theme-overrides'

let appKitThemeObserver: MutationObserver | undefined
let appKitPromise: Promise<AppKitInstance> | undefined

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

export const appKitArbitrumSepolia = {
  id: arbitrumSepolia.id,
  name: 'Arbitrum Sepolia',
  chainNamespace: 'eip155' as const,
  caipNetworkId: 'eip155:421614' as const,
  nativeCurrency: arbitrumSepolia.nativeCurrency,
  rpcUrls: {
    default: { http: [ARBITRUM_SEPOLIA_RPC_URL] },
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
    [mainnet.id]: http(MAINNET_RPC_URL),
    [sepolia.id]: http(SEPOLIA_RPC_URL),
    [arbitrumSepolia.id]: http(ARBITRUM_SEPOLIA_RPC_URL),
    [anvil.id]: http('http://127.0.0.1:8545'),
  },
})

function installAppKitThemeOverrides() {
  const existingStyle = document.getElementById(APPKIT_THEME_OVERRIDE_ID)
  const styleText = `
    :root {
      --apkt-tokens-theme-overlay: rgba(37, 9, 23, 0.85);
      --apkt-tokens-theme-backgroundPrimary: #3B212D;
      --apkt-tokens-theme-backgroundInvert: #FFF5F9;
      --apkt-tokens-theme-foregroundPrimary: #250917;
      --apkt-tokens-theme-foregroundSecondary: #3B212D;
      --apkt-tokens-theme-foregroundTertiary: #4B2A39;
      --apkt-tokens-theme-borderPrimary: rgba(255, 171, 150, 0.18);
      --apkt-tokens-theme-borderPrimaryDark: rgba(255, 171, 150, 0.18);
      --apkt-tokens-theme-borderSecondary: rgba(255, 171, 150, 0.28);
      --apkt-tokens-theme-textPrimary: #FFF5F9;
      --apkt-tokens-theme-textSecondary: #D8CBD0;
      --apkt-tokens-theme-textTertiary: #D8CBD0;
      --apkt-tokens-theme-textInvert: #FFF5F9;
      --apkt-tokens-theme-iconDefault: #FFF5F9;
      --apkt-tokens-theme-iconInverse: #FFF5F9;

      --apkt-tokens-core-backgroundAccentPrimary: #FF572D;
      --apkt-tokens-core-borderAccentPrimary: #FF572D;
      --apkt-tokens-core-foregroundAccent010: rgba(255, 87, 45, 0.12);
      --apkt-tokens-core-foregroundAccent020: rgba(255, 87, 45, 0.2);
      --apkt-tokens-core-foregroundAccent040: rgba(255, 87, 45, 0.4);
      --apkt-tokens-core-foregroundAccent060: rgba(255, 87, 45, 0.6);
      --apkt-tokens-core-iconAccentPrimary: #FF572D;
      --apkt-tokens-core-textAccentPrimary: #FFAB96;

      --apkt-tokens-core-backgroundSuccess: rgba(255, 171, 150, 0.16);
      --apkt-tokens-core-borderSuccess: #FFAB96;
      --apkt-tokens-core-iconSuccess: #FFAB96;
      --apkt-tokens-core-textSuccess: #FFAB96;
      --apkt-tokens-core-backgroundWarning: rgba(255, 87, 45, 0.18);
      --apkt-tokens-core-borderWarning: #FF572D;
      --apkt-tokens-core-iconWarning: #FF572D;
      --apkt-tokens-core-textWarning: #FFAB96;
      --apkt-tokens-core-backgroundError: rgba(255, 87, 45, 0.18);
      --apkt-tokens-core-borderError: #FF572D;
      --apkt-tokens-core-iconError: #FF572D;
      --apkt-tokens-core-textError: #FF572D;
    }

    w3m-modal {
      background-color: rgba(37, 9, 23, 0.85) !important;
      backdrop-filter: blur(4px) !important;
      -webkit-backdrop-filter: blur(4px) !important;
      transition: none !important;
    }
  `

  if (existingStyle) {
    if (existingStyle.textContent !== styleText) {
      existingStyle.textContent = styleText
    }
    if (document.head.lastElementChild !== existingStyle) {
      document.head.appendChild(existingStyle)
    }
    return
  }

  const style = document.createElement('style')
  style.id = APPKIT_THEME_OVERRIDE_ID
  style.textContent = styleText
  document.head.appendChild(style)

  if (!appKitThemeObserver) {
    appKitThemeObserver = new MutationObserver(() => {
      const overrideStyle = document.getElementById(APPKIT_THEME_OVERRIDE_ID)

      if (overrideStyle && document.head.lastElementChild !== overrideStyle) {
        document.head.appendChild(overrideStyle)
      }
    })

    appKitThemeObserver.observe(document.head, { childList: true })
  }
}

export function syncAppKitModalStyleOverrides() {
  installAppKitThemeOverrides()
  window.setTimeout(installAppKitThemeOverrides, 0)
  window.setTimeout(installAppKitThemeOverrides, 100)
  window.setTimeout(installAppKitThemeOverrides, 400)
}

export function ensureAppKit(): Promise<AppKitInstance> {
  if (appKitPromise) return appKitPromise

  appKitPromise = import('@reown/appkit/react').then(({ createAppKit }) => {
    const appKit = createAppKit({
      adapters: [wagmiAdapter],
      projectId: WALLETCONNECT_PROJECT_ID,
      networks,
      metadata,
      themeMode: 'dark',
      themeVariables: {
        '--w3m-font-family': "'Uncut Sans', ui-sans-serif, system-ui, sans-serif",
        '--w3m-accent': '#FF572D',
        '--w3m-color-mix': '#250917',
        '--w3m-color-mix-strength': 18,
        '--w3m-border-radius-master': '0px',
        '--w3m-z-index': 80,
      },
    })
    installAppKitThemeOverrides()
    syncAppKitModalStyleOverrides()
    return appKit
  }).catch((error: unknown) => {
    appKitPromise = undefined
    throw error
  })

  return appKitPromise
}

export async function openAppKit(options?: AppKitOpenOptions): Promise<void> {
  const appKit = await ensureAppKit()
  syncAppKitModalStyleOverrides()
  await appKit.open(options)
  syncAppKitModalStyleOverrides()
}

export async function switchAppKitToArbitrumSepolia(): Promise<void> {
  const appKit = await ensureAppKit()
  await appKit.switchNetwork(appKitArbitrumSepolia, { throwOnFailure: true })
}

export function scheduleAppKitInitialization(): void {
  const scheduleImport = () => {
    if (typeof window.requestIdleCallback === 'function') {
      window.requestIdleCallback(() => { void ensureAppKit().catch(() => undefined) }, { timeout: 1_500 })
    } else {
      window.setTimeout(() => { void ensureAppKit().catch(() => undefined) }, 1_000)
    }
  }

  if (document.readyState === 'complete') {
    scheduleImport()
  } else {
    window.addEventListener('load', scheduleImport, { once: true })
  }
}

type Chains = readonly [typeof mainnet, typeof sepolia, typeof arbitrumSepolia, typeof anvil]
export const config = wagmiAdapter.wagmiConfig as Config<Chains>

declare module 'wagmi' {
  interface Register {
    config: typeof config
  }
}

transactionManager.setConfig(config)
