import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'

export function getExplorerTxUrl(chainId: number | undefined, hash: string): string {
  const baseUrl = chainId === mainnet.id
    ? 'https://etherscan.io'
    : chainId === sepolia.id
      ? 'https://sepolia.etherscan.io'
      : chainId === arbitrumSepolia.id
        ? 'https://arbitrum-sepolia.blockscout.com'
        : 'https://sepolia.etherscan.io'

  return `${baseUrl}/tx/${hash}`
}
