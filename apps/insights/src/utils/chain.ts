export interface PerpsChainConfig {
  chainId: number
  name: string
  shortName: string
  isTestnet: boolean
  explorerName: string
  explorerBaseUrl: string
  tradeAppUrl: string
  defaultAssetSymbol: string
}

export const ARBITRUM_ONE_CHAIN_ID = 42161
export const ARBITRUM_SEPOLIA_CHAIN_ID = 421614

const CHAINS: Record<number, PerpsChainConfig> = {
  [ARBITRUM_ONE_CHAIN_ID]: {
    chainId: ARBITRUM_ONE_CHAIN_ID,
    name: 'Arbitrum One',
    shortName: 'Arbitrum',
    isTestnet: false,
    explorerName: 'Arbiscan',
    explorerBaseUrl: 'https://arbiscan.io',
    tradeAppUrl: 'https://app.plether.com',
    defaultAssetSymbol: 'USDG',
  },
  [ARBITRUM_SEPOLIA_CHAIN_ID]: {
    chainId: ARBITRUM_SEPOLIA_CHAIN_ID,
    name: 'Arbitrum Sepolia',
    shortName: 'Arb Sepolia',
    isTestnet: true,
    explorerName: 'Arbiscan',
    explorerBaseUrl: 'https://sepolia.arbiscan.io',
    tradeAppUrl: 'https://app.sepolia.plether.com',
    defaultAssetSymbol: 'USDG',
  },
}

export function getDefaultChainId(): number {
  const configured = Number(import.meta.env.VITE_DEFAULT_CHAIN_ID)
  if (Number.isSafeInteger(configured) && configured in CHAINS) {
    return configured
  }
  return ARBITRUM_SEPOLIA_CHAIN_ID
}

export function resolvePerpsChain(chainId?: number | null): PerpsChainConfig {
  if (chainId != null && chainId in CHAINS) {
    return CHAINS[chainId]
  }
  return CHAINS[getDefaultChainId()]
}

export function explorerAddressUrl(address: string, chainId?: number | null): string {
  const chain = resolvePerpsChain(chainId)
  return `${chain.explorerBaseUrl}/address/${address}`
}

export function explorerTxUrl(txHash: string, chainId?: number | null): string {
  const chain = resolvePerpsChain(chainId)
  return `${chain.explorerBaseUrl}/tx/${txHash}`
}

export function arbiscanLinkKey(
  kind: 'tx' | 'address',
  value: string,
  chainId?: number | null,
): string {
  const href =
    kind === 'tx' ? explorerTxUrl(value, chainId) : explorerAddressUrl(value, chainId)
  return href.toLowerCase()
}

export function vaultsAppUrl(chainId?: number | null): string {
  const chain = resolvePerpsChain(chainId)
  return `${chain.tradeAppUrl}/vaults`
}
