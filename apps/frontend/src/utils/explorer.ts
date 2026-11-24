import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'

function explorerBaseUrl(chainId: number | undefined): string {
  return chainId === mainnet.id
    ? 'https://etherscan.io'
    : chainId === sepolia.id
      ? 'https://sepolia.etherscan.io'
      : chainId === arbitrumSepolia.id
        ? 'https://arbitrum-sepolia.blockscout.com'
        : 'https://sepolia.etherscan.io'
}

export function getExplorerTxUrl(chainId: number | undefined, hash: string): string {
  return `${explorerBaseUrl(chainId)}/tx/${hash}`
}

export function getExplorerUserOperationUrl(chainId: number | undefined, hash: string): string {
  return `${explorerBaseUrl(chainId)}/op/${hash}`
}
