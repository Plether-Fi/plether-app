import type { Address, Hex } from 'viem'
import type { FundingDestination, FundingIntent, FundingQuote, SavedFunding } from './types'

export const OWNER = '0x1111111111111111111111111111111111111111' as Address
export const BENEFICIARY = '0x2222222222222222222222222222222222222222' as Address
export const CLEARINGHOUSE = '0x3333333333333333333333333333333333333333' as Address
export const FACTORY = '0x4444444444444444444444444444444444444444' as Address
export const RECEIVER = '0x5555555555555555555555555555555555555555' as Address
export const OTHER_ADDRESS = '0x6666666666666666666666666666666666666666' as Address
export const DESTINATION_USDC = '0xaf88d065e77c8cc2239327c5edb3a432268e5831' as Address
export const SOURCE_USDC = '0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48' as Address
export const SOURCE_HASH: Hex = `0x${'11'.repeat(32)}`
export const DEPOSIT_HASH: Hex = `0x${'22'.repeat(32)}`
export const BLOCK_HASH: Hex = `0x${'33'.repeat(32)}`

export function releaseFixture() {
  return {
    destinationChainId: 42161,
    releaseId: 'perps-arbitrum-funding-reviewed-test-fixture',
    clearinghouse: CLEARINGHOUSE,
    token: DESTINATION_USDC,
    receiverFactory: FACTORY,
    factoryCodeHash: `0x${'44'.repeat(32)}`,
    clearinghouseCodeHash: `0x${'66'.repeat(32)}`,
    confirmations: 12,
    startBlock: 123456,
  }
}

export function destinationFixture(): FundingDestination {
  const { destinationChainId, releaseId, clearinghouse, token, receiverFactory } = releaseFixture()
  return { owner: OWNER, beneficiary: BENEFICIARY, destinationChainId, releaseId, clearinghouse, token, receiverFactory }
}

export function quoteFixture(): FundingQuote {
  return {
    ...releaseFixture(),
    quoteId: 'quote-test-1',
    intentSalt: `0x${'77'.repeat(32)}`,
    ownerAddress: OWNER,
    expiresAt: 2_000_000_000,
    provider: 'across',
    sourceChainId: 1,
    sourceToken: SOURCE_USDC,
    sourceAmount: '100000000',
    beneficiary: BENEFICIARY,
    receiver: RECEIVER,
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
