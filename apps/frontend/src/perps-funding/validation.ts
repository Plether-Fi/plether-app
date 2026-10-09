import { getAddress, isAddress, zeroAddress, type Address, type Hex } from 'viem'
import { ARBITRUM_USDC, FUNDING_SOURCES } from './sources'
import type { FundingConfig, FundingDestination, FundingIntent, FundingManifest, FundingQuote, SourceTransaction } from './types'

export function record(value: unknown): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) throw new Error('Invalid funding response.')
  return value as Record<string, unknown>
}
export function text(value: unknown): string {
  if (typeof value !== 'string' || !value.trim()) throw new Error('Missing funding response field.')
  return value
}
export function amount(value: unknown): string {
  if (typeof value !== 'string' || !/^(0|[1-9][0-9]*)$/.test(value) || BigInt(value) >= 2n ** 256n) throw new Error('Invalid funding amount.')
  return value
}
export function address(value: unknown): Address {
  if (typeof value !== 'string' || !isAddress(value) || value.toLowerCase() === zeroAddress) throw new Error('Invalid funding address.')
  return getAddress(value)
}
export function chainId(value: unknown): number {
  if (typeof value !== 'number' || !Number.isSafeInteger(value) || value <= 0) throw new Error('Invalid funding chain.')
  return value
}
export function hash(value: unknown): Hex {
  if (typeof value !== 'string' || !/^0x[0-9a-fA-F]{64}$/.test(value)) throw new Error('Invalid funding transaction evidence.')
  return value as Hex
}
export function sameAddress(a: string, b: string): boolean { return a.toLowerCase() === b.toLowerCase() }

export function parseFundingManifest(value: unknown): FundingManifest {
  const v = record(value)
  if (v.destinationChainId !== 42161 || !sameAddress(address(v.token), ARBITRUM_USDC)) throw new Error('This funding release does not support the reviewed route.')
  const confirmations = chainId(v.confirmations)
  if (confirmations > 1000 || typeof v.startBlock !== 'number' || !Number.isSafeInteger(v.startBlock) || v.startBlock < 0) throw new Error('Invalid funding confirmation policy.')
  return { clearinghouseCodeHash: hash(v.clearinghouseCodeHash), factoryCodeHash: hash(v.factoryCodeHash), confirmations, startBlock: v.startBlock, releaseId: text(v.releaseId), provider: 'across', destinationChainId: chainId(v.destinationChainId), token: address(v.token), clearinghouse: address(v.clearinghouse), receiverFactory: address(v.receiverFactory), sources: FUNDING_SOURCES }

}

export function parseFundingConfig(value: unknown): FundingConfig {
  const v = record(value)
  if (v.enabled !== true) return { enabled: false, reason: typeof v.reason === 'string' ? v.reason : 'Funding is not enabled for this release.' }
  return { enabled: true, clearinghouseCodeHash: hash(v.clearinghouseCodeHash), factoryCodeHash: hash(v.factoryCodeHash), confirmations: chainId(v.confirmations), startBlock: typeof v.startBlock === 'number' && Number.isSafeInteger(v.startBlock) && v.startBlock >= 0 ? v.startBlock : undefined, provider: text(v.provider), destinationChainId: chainId(v.destinationChainId), releaseId: text(v.releaseId), clearinghouse: address(v.clearinghouse), token: address(v.token), receiverFactory: address(v.receiverFactory) }
}

export function assertFundingConfig(manifest: FundingManifest, config: FundingConfig): void {
  if (!config.enabled || config.clearinghouseCodeHash?.toLowerCase() !== manifest.clearinghouseCodeHash.toLowerCase() || config.factoryCodeHash?.toLowerCase() !== manifest.factoryCodeHash.toLowerCase() || config.confirmations !== manifest.confirmations || config.startBlock !== manifest.startBlock || config.provider !== manifest.provider || config.releaseId !== manifest.releaseId || config.destinationChainId !== manifest.destinationChainId || !config.token || !sameAddress(config.token, manifest.token) || !config.clearinghouse || !sameAddress(config.clearinghouse, manifest.clearinghouse) || !config.receiverFactory || !sameAddress(config.receiverFactory, manifest.receiverFactory)) throw new Error(config.reason ?? 'Funding release configuration does not match.')
}

export function parseSourceTransaction(value: unknown): SourceTransaction {
  const v = record(value)
  if (v.kind !== 'approval' && v.kind !== 'bridge') throw new Error('Invalid funding transaction kind.')
  if (typeof v.data !== 'string' || !/^0x([0-9a-fA-F]{2})*$/.test(v.data)) throw new Error('Invalid funding transaction data.')
  return { kind: v.kind, chainId: chainId(v.chainId), to: address(v.to), data: v.data as Hex, value: amount(v.value) }
}

export function parseFundingQuote(value: unknown): FundingQuote {
  const v = record(value)
  const expiresAt = chainId(v.expiresAt)
  if (expiresAt > 253402300799) throw new Error('Invalid funding quote expiry.')
  if (!Array.isArray(v.sourceTransactions)) throw new Error('Missing funding source transactions.')
  const quote: FundingQuote = { intentSalt: hash(v.intentSalt), ownerAddress: address(v.ownerAddress), receiverFactory: address(v.receiverFactory), quoteId: text(v.quoteId), expiresAt, provider: text(v.provider), sourceChainId: chainId(v.sourceChainId), sourceToken: address(v.sourceToken), sourceAmount: amount(v.sourceAmount), destinationChainId: chainId(v.destinationChainId), token: address(v.token), beneficiary: address(v.beneficiary), clearinghouse: address(v.clearinghouse), receiver: address(v.receiver), releaseId: text(v.releaseId), estimatedAmount: amount(v.estimatedAmount), minimumAmount: amount(v.minimumAmount), sourceTransactions: Array.isArray(v.sourceTransactions) ? v.sourceTransactions.map(parseSourceTransaction) : [] }
  return quote
}

const statuses = new Set(['awaiting-source', 'bridging', 'received', 'depositing', 'confirmed', 'retryable', 'failed'])
export function parseFundingIntent(value: unknown): FundingIntent {
  const v = record(value)
  const status = text(v.status)
  if (!statuses.has(status)) throw new Error('Unknown funding state.')
  if (v.sourceConfirmations != null && (typeof v.sourceConfirmations !== 'number' || !Number.isSafeInteger(v.sourceConfirmations) || v.sourceConfirmations < 0)) throw new Error('Invalid source confirmation count.')
  return { ...parseFundingQuote(v), intentId: text(v.intentId), status: status as FundingIntent['status'], ...(v.sourceTxHash != null ? { sourceTxHash: hash(v.sourceTxHash) } : {}), ...(v.depositTxHash != null ? { depositTxHash: hash(v.depositTxHash) } : {}), ...(v.depositBlockHash != null ? { depositBlockHash: hash(v.depositBlockHash) } : {}), ...(v.depositBlockNumber != null ? { depositBlockNumber: amount(v.depositBlockNumber) } : {}), ...(v.creditedAmount != null ? { creditedAmount: amount(v.creditedAmount) } : {}), ...(v.sourceBlockNumber != null ? { sourceBlockNumber: amount(v.sourceBlockNumber) } : {}), ...(v.sourceBlockHash != null ? { sourceBlockHash: hash(v.sourceBlockHash) } : {}), ...(v.sourceConfirmations != null ? { sourceConfirmations: v.sourceConfirmations } : {}), ...(typeof v.sourceTerminal === 'boolean' ? { sourceTerminal: v.sourceTerminal } : {}), ...(['pending', 'confirmed', 'reverted'].includes(String(v.sourceStatus)) ? { sourceStatus: v.sourceStatus as FundingIntent['sourceStatus'] } : {}), ...(['pending', 'filled', 'expired', 'refunded'].includes(String(v.bridgeStatus)) ? { bridgeStatus: v.bridgeStatus as FundingIntent['bridgeStatus'] } : {}), ...(typeof v.lastError === 'string' ? { reason: v.lastError } : typeof v.reason === 'string' ? { reason: v.reason } : {}) }
}

export function assertDestination(quote: FundingQuote, destination: FundingDestination): void {
  if (!sameAddress(quote.ownerAddress, destination.owner) || !sameAddress(quote.receiverFactory, destination.receiverFactory) || quote.releaseId !== destination.releaseId || quote.destinationChainId !== destination.destinationChainId || !sameAddress(quote.beneficiary, destination.beneficiary) || !sameAddress(quote.token, destination.token) || !sameAddress(quote.clearinghouse, destination.clearinghouse)) throw new Error('Funding destination changed. No transaction was requested.')
}
