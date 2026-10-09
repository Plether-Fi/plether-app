import { getAddress, type Address, type Hex } from 'viem'
import { ACROSS_ARBITRUM_HANDLER } from './sources'
import type { FundingDestination, FundingIntent, FundingQuote, SavedFunding } from './types'

export const OWNER = '0x1111111111111111111111111111111111111111' as Address
export const BENEFICIARY = '0x2222222222222222222222222222222222222222' as Address
export const CLEARINGHOUSE = '0x3333333333333333333333333333333333333333' as Address
// Synthetic release identities for isolated tests, not production deployment pins.
export const DESTINATION_SPOKE_POOL = '0x4444444444444444444444444444444444444444' as Address
export const DESTINATION_SPOKE_POOL_IMPLEMENTATION = '0x5555555555555555555555555555555555555555' as Address
export const MULTICALL_HANDLER = getAddress(ACROSS_ARBITRUM_HANDLER)
export const OTHER_ADDRESS = '0x6666666666666666666666666666666666666666' as Address
export const DESTINATION_USDC = '0xaf88d065e77c8cc2239327c5edb3a432268e5831' as Address
export const SOURCE_USDC = '0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48' as Address
export const QUOTE_ID: Hex = `0x${'77'.repeat(32)}`
export const FALLBACK_HASH: Hex = `0x${'88'.repeat(32)}`
export const SOURCE_HASH: Hex = `0x${'11'.repeat(32)}`
export const DEPOSIT_HASH: Hex = `0x${'22'.repeat(32)}`
export const BLOCK_HASH: Hex = `0x${'33'.repeat(32)}`

export function releaseFixture() {
  return {
    destinationChainId: 42161,
    releaseId: 'perps-arbitrum-funding-reviewed-test-fixture',
    clearinghouse: CLEARINGHOUSE,
    token: DESTINATION_USDC,
    multicallHandler: MULTICALL_HANDLER,
    multicallHandlerCodeHash: `0x${'44'.repeat(32)}`,
    destinationSpokePool: DESTINATION_SPOKE_POOL,
    destinationSpokePoolCodeHash: `0x${'88'.repeat(32)}`,
    destinationSpokePoolImplementation: DESTINATION_SPOKE_POOL_IMPLEMENTATION,
    destinationSpokePoolImplementationCodeHash: `0x${'99'.repeat(32)}`,
    clearinghouseCodeHash: `0x${'66'.repeat(32)}`,
    confirmations: 12,
    startBlock: 123456,
  }
}

export function destinationFixture(): FundingDestination {
  const { destinationChainId, releaseId, clearinghouse, token, multicallHandler, destinationSpokePool } = releaseFixture()
  return { owner: OWNER, beneficiary: BENEFICIARY, destinationChainId, releaseId, clearinghouse, token, multicallHandler, destinationSpokePool }
}

export function quoteFixture(): FundingQuote {
  const { destinationChainId, releaseId, clearinghouse, token, multicallHandler, destinationSpokePool } = releaseFixture()
  return {
    destinationChainId, releaseId, clearinghouse, token, multicallHandler, destinationSpokePool,
    quoteId: QUOTE_ID,
    // State/API fixtures have no signable provider calldata or destination actions.
    destinationMessage: '0x',
    ownerAddress: OWNER,
    expiresAt: 2_000_000_000,
    provider: 'across',
    sourceChainId: 1,
    sourceToken: SOURCE_USDC,
    sourceAmount: '100000000',
    beneficiary: BENEFICIARY,
    estimatedAmount: '99000000',
    minimumAmount: '98000000',
    // State and API fixtures intentionally contain no signable provider payload.
    sourceTransactions: [],
  }
}

export function intentFixture(overrides: Partial<FundingIntent> = {}): FundingIntent {
  return { ...quoteFixture(), intentId: 'intent-test-1', status: 'awaiting-source', ...overrides }
}

export function confirmedIntentFixture(): FundingIntent {
  return intentFixture({ status: 'confirmed', sourceTxHash: SOURCE_HASH, depositTxHash: DEPOSIT_HASH, depositBlockNumber: '123500', depositBlockHash: BLOCK_HASH, creditedAmount: '99000000' })
}

export function needsDepositIntentFixture(): FundingIntent {
  return intentFixture({ status: 'needs-deposit', sourceTxHash: SOURCE_HASH, fallbackTxHash: FALLBACK_HASH, fallbackBlockNumber: '123500', fallbackBlockHash: BLOCK_HASH, fallbackAmount: '99000000' })
}

export function terminalSourceIntentFixture(): FundingIntent {
  return intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH, sourceStatus: 'reverted', sourceTerminal: true, sourceBlockNumber: '23000000', sourceBlockHash: BLOCK_HASH, sourceConfirmations: 2 })
}

export function savedFixture(intent = intentFixture()): SavedFunding {
  return { version: 1, destination: destinationFixture(), intent }
}

export function memoryStorage(): Storage {
  const data = new Map<string, string>()
  return {
    get length() { return data.size },
    clear: () => { data.clear() },
    getItem: (key) => data.get(key) ?? null,
    key: (index) => [...data.keys()][index] ?? null,
    removeItem: (key) => { data.delete(key) },
    setItem: (key, value) => { data.set(key, value) },
  }
}
