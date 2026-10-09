import type { Address } from 'viem'
import type { FundingSource } from './types'

// Initial reviewed route: Ethereum USDC/USDT -> native USDC on Arbitrum One.
// No Sepolia or arbitrary-token fallback is permitted.
export const ETHEREUM_USDC: Address = '0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48'
export const ETHEREUM_USDT: Address = '0xdac17f958d2ee523a2206206994597c13d831ec7'
export const ARBITRUM_USDC: Address = '0xaf88d065e77c8cc2239327c5edb3a432268e5831'
export const ACROSS_ETHEREUM_SPOKE_POOL: Address = '0x5c7bcd6e7de5423a257d81b442095a1a6ced35c5'
export const ACROSS_PERIPHERY: Address = '0x97ccdbea4632140639ad5ea9b944aa034eb15fd4'
export const ACROSS_ARBITRUM_HANDLER: Address = '0x0f7ae28de1c8532170ad4ee566b5801485c13a0e'
export const ACROSS_ARBITRUM_LOGGER: Address = '0xbf75133b48b0a42ab9374027902e83c5e2949034'
export const ACROSS_ARBITRUM_LOGGER_CODE_HASH = '0x833b49ceddf001f197603e23297ddc21147b7d9e61adbe812e1167b3a7fe2fe5' as const
const ZERO_EXCHANGE: Address = '0x0000000000001ff3684f28c67538d4d072c22734'
export const FUNDING_SOURCES: FundingSource[] = [ETHEREUM_USDC, ETHEREUM_USDT].map((token, i) => ({
  chainId: 1, name: 'Ethereum', token, symbol: i === 0 ? 'USDC' : 'USDT', decimals: 6,
  transactionTargets: [ACROSS_ETHEREUM_SPOKE_POOL, ACROSS_PERIPHERY],
  approvalSpenders: [ACROSS_ETHEREUM_SPOKE_POOL, ACROSS_PERIPHERY],
  swapExchanges: [ZERO_EXCHANGE, '0x66a9893cc07d91d95644aedd05d03f95e1dba8af'], spokePools: [ACROSS_ETHEREUM_SPOKE_POOL],
}))
