import { mainnet } from 'wagmi/chains'

export function getExplorerTxUrl(chainId: number | undefined, hash: string): string {
  const baseUrl = chainId === mainnet.id
    ? 'https://etherscan.io'
    : 'https://sepolia.etherscan.io'

  return `${baseUrl}/tx/${hash}`
}
