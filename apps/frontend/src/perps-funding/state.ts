import { address, amount, assertDestination, chainId, hash, parseFundingIntent, record, text } from './validation'
import type { FundingDestination, FundingIntent, FundingQuote, SavedFunding } from './types'

export function fundingStorageKey(owner: string, releaseId: string): string {
  return `plether.perps.funding.v1:${owner.toLowerCase()}:${releaseId}`
}

export function fundingReady(intent: FundingIntent): boolean {
  return intent.status === 'confirmed' && Boolean(intent.depositTxHash && intent.depositBlockHash) &&
    intent.depositBlockNumber !== undefined && intent.creditedAmount !== undefined &&
    BigInt(intent.creditedAmount) > 0n && BigInt(intent.creditedAmount) >= BigInt(intent.minimumAmount)
}

/** A reverted source observation alone is not enough to abandon an intent. */
export function fundingSourceFailed(intent: FundingIntent): boolean {
  return intent.status !== 'confirmed' && intent.sourceStatus === 'reverted' && intent.sourceTerminal === true && Boolean(intent.sourceTxHash && intent.sourceBlockHash) && intent.sourceBlockNumber !== undefined && (intent.sourceConfirmations ?? 0) >= 2
}

/** Provider delivery and receiver balance are deliberately insufficient evidence. */
export function fundingStatusLabel(intent: FundingIntent, refreshed: boolean): string {
  if (!refreshed) return 'Checking saved transfer'
  if (fundingReady(intent)) return 'Ready to trade'
  if (intent.status === 'bridging' && intent.sourceStatus === 'reverted') return 'Source transaction reverted'
  if (intent.status === 'bridging' && intent.bridgeStatus === 'expired') return 'Bridge deadline passed'
  if (intent.status === 'bridging' && intent.bridgeStatus === 'refunded') return 'Refund reported by provider'
  switch (intent.status) {
    case 'awaiting-source': return 'Awaiting source transaction'
    case 'bridging': return 'Transfer in progress'
    case 'received': return 'Funds received — deposit pending'
    case 'depositing': return 'Confirming margin deposit'
    case 'confirmed': return 'Verifying clearinghouse credit'
    case 'retryable': return 'Deposit needs another attempt'
    case 'failed': return 'Transfer needs attention'
  }
}

export function reconcileFunding(saved: SavedFunding, next: FundingIntent, allowSourceReplacement = false): SavedFunding {
  assertDestination(next, saved.destination)
  const old = saved.intent
  if (old.intentId !== next.intentId) throw new Error('Funding intent identity changed. Transfer paused for review.')
  assertSameQuote(old, next)
  const sourceTxHash = allowSourceReplacement ? next.sourceTxHash : saved.sourceTxHash ?? old.sourceTxHash
  if (sourceTxHash && next.sourceTxHash && sourceTxHash.toLowerCase() !== next.sourceTxHash.toLowerCase()) throw new Error('Funding source transaction changed. Transfer paused for review.')
  return { ...saved, intent: next, sourceTxHash: sourceTxHash ?? next.sourceTxHash }
}

export function saveFunding(storage: Storage, saved: SavedFunding): void {
  storage.setItem(fundingStorageKey(saved.destination.owner, saved.destination.releaseId), JSON.stringify(saved))
}

export function restoreFunding(storage: Storage, owner: string, releaseId: string): SavedFunding | null {
  const raw = storage.getItem(fundingStorageKey(owner, releaseId))
  if (raw === null) return null
  const v = record(JSON.parse(raw))
  const d = record(v.destination)
  const destination: FundingDestination = { owner: address(d.owner), beneficiary: address(d.beneficiary), destinationChainId: chainId(d.destinationChainId), token: address(d.token), clearinghouse: address(d.clearinghouse), receiverFactory: address(d.receiverFactory), releaseId: text(d.releaseId) }
  if (v.version !== 1 || destination.owner.toLowerCase() !== owner.toLowerCase() || destination.releaseId !== releaseId) throw new Error('Saved funding identity is invalid.')
  const intent = parseFundingIntent(v.intent)
  assertDestination(intent, destination)
  if (intent.creditedAmount != null) amount(intent.creditedAmount)
  return { version: 1, destination, intent, ...(v.sourceTxHash == null ? {} : { sourceTxHash: hash(v.sourceTxHash) }), sourceSubmissionPending: v.sourceSubmissionPending === true }
}

/** Creating or refreshing an intent cannot replace the quote the user reviewed. */
export function assertSameQuote(old: FundingQuote, next: FundingQuote): void {
  if (old.intentSalt.toLowerCase() !== next.intentSalt.toLowerCase() || old.quoteId !== next.quoteId || old.receiver.toLowerCase() !== next.receiver.toLowerCase() || old.sourceChainId !== next.sourceChainId || old.sourceToken.toLowerCase() !== next.sourceToken.toLowerCase() || old.sourceAmount !== next.sourceAmount || old.minimumAmount !== next.minimumAmount || old.estimatedAmount !== next.estimatedAmount || old.provider !== next.provider || old.expiresAt !== next.expiresAt || JSON.stringify(old.sourceTransactions) !== JSON.stringify(next.sourceTransactions)) throw new Error('Funding intent identity changed. Transfer paused for review.')
}

export function archiveCompletedFunding(storage: Storage, saved: SavedFunding): void {
  if (!fundingReady(saved.intent) && !fundingSourceFailed(saved.intent)) throw new Error('This funding transfer is not yet confirmed.')
  // Keep each completed reference independently so starting another transfer never
  // discards the source/receiver evidence needed for later support or reconciliation.
  storage.setItem(`${fundingStorageKey(saved.destination.owner, saved.destination.releaseId)}:history:${saved.intent.intentId}`, JSON.stringify(saved))
  storage.removeItem(fundingStorageKey(saved.destination.owner, saved.destination.releaseId))
}
