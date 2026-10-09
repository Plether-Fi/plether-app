import { describe, expect, it } from 'vitest'
import { archiveCompletedFunding, assertSameQuote, fundingReady, fundingSourceFailed, fundingStatusLabel, fundingStorageKey, reconcileFunding, restoreFunding, saveFunding } from './state'
import { BLOCK_HASH, OTHER_ADDRESS, OWNER, SOURCE_HASH, confirmedIntentFixture, intentFixture, memoryStorage, savedFixture, terminalSourceIntentFixture } from './testFixtures'
import type { FundingIntent } from './types'

describe('canonical funding readiness', () => {
  it('requires a confirmed clearinghouse credit and all canonical receipt evidence', () => {
    const confirmed = confirmedIntentFixture()
    expect(fundingReady(confirmed)).toBe(true)
    for (const field of ['depositTxHash', 'depositBlockHash', 'depositBlockNumber', 'creditedAmount'] as const) {
      expect(fundingReady({ ...confirmed, [field]: undefined }), field).toBe(false)
    }
    expect(fundingReady({ ...confirmed, creditedAmount: '0' })).toBe(false)
    expect(fundingReady({ ...confirmed, creditedAmount: '97999999' })).toBe(false)
    expect(fundingReady({ ...confirmed, creditedAmount: confirmed.minimumAmount })).toBe(true)
  })

  it.each(['awaiting-source', 'bridging', 'received', 'depositing', 'retryable', 'failed'] as const)(
    'does not report readiness from %s, even with old deposit evidence', (status) => {
      const intent = { ...confirmedIntentFixture(), status }
      expect(fundingReady(intent)).toBe(false)
      expect(fundingStatusLabel(intent, true)).not.toBe('Ready to trade')
    }
  )

  it('keeps a restored confirmed transfer untrusted until the API is refreshed', () => {
    const storage = memoryStorage()
    const saved = savedFixture(confirmedIntentFixture())
    saveFunding(storage, saved)
    const restored = restoreFunding(storage, OWNER, saved.destination.releaseId)!
    expect(fundingStatusLabel(restored.intent, false)).toBe('Checking saved transfer')
    expect(fundingStatusLabel(restored.intent, true)).toBe('Ready to trade')
    expect(fundingStatusLabel(intentFixture({ status: 'confirmed' }), true)).toBe('Verifying clearinghouse credit')
  })

  it('accepts a canonical reorg regression and removes readiness', () => {
    const old = savedFixture(confirmedIntentFixture())
    const reverted = intentFixture({ status: 'depositing', sourceTxHash: SOURCE_HASH })
    const next = reconcileFunding(old, reverted)
    expect(next.intent.status).toBe('depositing')
    expect(next.intent.depositBlockHash).toBeUndefined()
    expect(fundingReady(next.intent)).toBe(false)
    expect(next.sourceTxHash).toBe(SOURCE_HASH)
  })
})

describe('funding intent identity and persistence', () => {
  it('preserves the destination identity and an interrupted source broadcast across reloads', () => {
    const storage = memoryStorage()
    const saved = { ...savedFixture(), sourceSubmissionPending: true, sourceTxHash: SOURCE_HASH }
    saveFunding(storage, saved)
    const restored = restoreFunding(storage, OWNER, saved.destination.releaseId)!
    expect(restored).toMatchObject({
      version: 1,
      destination: { owner: OWNER, beneficiary: saved.destination.beneficiary, destinationChainId: 42161, receiverFactory: saved.destination.receiverFactory },
      sourceSubmissionPending: true,
      sourceTxHash: SOURCE_HASH,
    })
    expect(restored.intent.sourceChainId).toBe(1)
    expect(restoreFunding(storage, OTHER_ADDRESS, saved.destination.releaseId)).toBeNull()
    expect(restoreFunding(storage, OWNER, 'a-different-release')).toBeNull()
  })

  it('reconciles repeated polling idempotently and retains locally saved source evidence', () => {
    const saved = { ...savedFixture(), sourceTxHash: SOURCE_HASH, sourceSubmissionPending: true }
    const remote = intentFixture({ status: 'bridging' })
    const once = reconcileFunding(saved, remote)
    expect(reconcileFunding(once, remote)).toEqual(once)
    expect(once.sourceTxHash).toBe(SOURCE_HASH)
    expect(once.sourceSubmissionPending).toBe(true)
  })

  it.each([
    ['intentId', 'different-intent'], ['quoteId', 'different-quote'], ['intentSalt', BLOCK_HASH],
    ['ownerAddress', OTHER_ADDRESS], ['beneficiary', OTHER_ADDRESS],
    ['receiver', OTHER_ADDRESS], ['receiverFactory', OTHER_ADDRESS],
    ['clearinghouse', OTHER_ADDRESS], ['token', OTHER_ADDRESS],
    ['sourceToken', OTHER_ADDRESS], ['sourceChainId', 10],
    ['destinationChainId', 421614], ['sourceAmount', '100000001'],
    ['minimumAmount', '97000000'], ['estimatedAmount', '99500000'],
    ['expiresAt', 2_000_000_001], ['provider', 'unreviewed-provider'],
    ['releaseId', 'another-release'],
  ] as const)('rejects a changed %s instead of following a different transfer', (field, value) => {
    const saved = savedFixture()
    expect(() => reconcileFunding(saved, { ...saved.intent, [field]: value } as FundingIntent)).toThrow()
  })

  it('rejects conflicting source hashes but permits the API to first learn a local hash', () => {
    const saved = { ...savedFixture(), sourceTxHash: SOURCE_HASH }
    expect(() => reconcileFunding(saved, intentFixture({ sourceTxHash: BLOCK_HASH }))).toThrow(/source transaction changed/)
    expect(reconcileFunding(saved, intentFixture({ sourceTxHash: SOURCE_HASH })).sourceTxHash).toBe(SOURCE_HASH)
    expect(reconcileFunding(savedFixture(), intentFixture({ sourceTxHash: SOURCE_HASH })).sourceTxHash).toBe(SOURCE_HASH)
  })

  it('allows a verified replacement only through the explicit reconciliation path', () => {
    const saved = { ...savedFixture(intentFixture({ sourceTxHash: SOURCE_HASH, sourceStatus: 'pending' })), sourceTxHash: SOURCE_HASH }
    const replacement = intentFixture({ sourceTxHash: BLOCK_HASH, sourceStatus: 'pending' })
    expect(() => reconcileFunding(saved, replacement)).toThrow(/source transaction changed/)
    expect(reconcileFunding(saved, replacement, true)).toMatchObject({ sourceTxHash: BLOCK_HASH, intent: { sourceTxHash: BLOCK_HASH } })
    expect(() => reconcileFunding(saved, { ...replacement, beneficiary: OTHER_ADDRESS }, true)).toThrow()
    expect(() => reconcileFunding(saved, { ...replacement, sourceAmount: '1' }, true)).toThrow()
  })

  it('does not let intent creation or refresh replace the reviewed wallet transaction', () => {
    const quote = intentFixture()
    const replacement = {
      ...quote,
      sourceTransactions: [{ kind: 'bridge' as const, chainId: 1, to: OTHER_ADDRESS, data: '0x1234' as const, value: '0' }],
    }
    expect(() => assertSameQuote(quote, replacement)).toThrow()
    expect(() => reconcileFunding(savedFixture(quote), replacement)).toThrow()
  })

  it.each([
    { version: 2 },
    { destination: { ...savedFixture().destination, owner: OTHER_ADDRESS } },
    { destination: { ...savedFixture().destination, releaseId: 'different-release' } },
    { destination: { ...savedFixture().destination, beneficiary: OTHER_ADDRESS } },
    { intent: { ...intentFixture(), status: 'ready-from-provider' } },
    { sourceTxHash: '0x1234' },
  ])('fails closed on an invalid saved identity or transaction', (corruption) => {
    const storage = memoryStorage()
    const saved = savedFixture()
    storage.setItem(fundingStorageKey(OWNER, saved.destination.releaseId), JSON.stringify({ ...saved, ...corruption }))
    expect(() => restoreFunding(storage, OWNER, saved.destination.releaseId)).toThrow()
  })

  it('does not silently discard malformed JSON or a failed persistence write', () => {
    const storage = memoryStorage()
    const saved = savedFixture()
    storage.setItem(fundingStorageKey(OWNER, saved.destination.releaseId), '{')
    expect(() => restoreFunding(storage, OWNER, saved.destination.releaseId)).toThrow()
    const fullStorage = { ...storage, setItem: () => { throw new Error('quota exceeded') } }
    expect(() => saveFunding(fullStorage, saved)).toThrow('quota exceeded')
  })

  it('keeps completed transfer evidence when making room for the next deposit', () => {
    const storage = memoryStorage()
    const saved = savedFixture(confirmedIntentFixture())
    saveFunding(storage, saved)
    archiveCompletedFunding(storage, saved)
    expect(restoreFunding(storage, OWNER, saved.destination.releaseId)).toBeNull()
    const key = `${fundingStorageKey(OWNER, saved.destination.releaseId)}:history:${saved.intent.intentId}`
    expect(JSON.parse(storage.getItem(key)!)).toEqual(saved)
  })

  it('never removes an active transfer when the archive cannot be written', () => {
    const storage = memoryStorage()
    const saved = savedFixture(confirmedIntentFixture())
    saveFunding(storage, saved)
    const unavailable = { ...storage, setItem: () => { throw new Error('quota exceeded') } }
    expect(() => archiveCompletedFunding(unavailable, saved)).toThrow('quota exceeded')
    expect(restoreFunding(storage, OWNER, saved.destination.releaseId)?.intent.intentId).toBe(saved.intent.intentId)
  })

  it('refuses to archive an unconfirmed delivery or incomplete canonical receipt', () => {
    const storage = memoryStorage()
    expect(() => archiveCompletedFunding(storage, savedFixture(intentFixture({ status: 'received' })))).toThrow()
    expect(() => archiveCompletedFunding(storage, savedFixture(intentFixture({ status: 'confirmed' })))).toThrow()
    expect(storage.length).toBe(0)
  })
})

describe('definitive source failure evidence', () => {
  it('permits archival only after a terminal reverted receipt has at least two confirmations', () => {
    const intent = terminalSourceIntentFixture()
    expect(fundingSourceFailed(intent)).toBe(true)
    expect(fundingReady(intent)).toBe(false)
    const storage = memoryStorage()
    const saved = savedFixture(intent)
    saveFunding(storage, saved)
    archiveCompletedFunding(storage, saved)
    expect(restoreFunding(storage, OWNER, saved.destination.releaseId)).toBeNull()
    const archived = JSON.parse(storage.getItem(`${fundingStorageKey(OWNER, saved.destination.releaseId)}:history:${intent.intentId}`)!)
    expect(archived.intent).toMatchObject({ sourceStatus: 'reverted', sourceTerminal: true, sourceConfirmations: 2, sourceTxHash: SOURCE_HASH, sourceBlockHash: BLOCK_HASH })
  })

  it.each(['sourceStatus', 'sourceTerminal', 'sourceTxHash', 'sourceBlockNumber', 'sourceBlockHash', 'sourceConfirmations'] as const)('does not permit archival without %s', (field) => {
    const intent = { ...terminalSourceIntentFixture(), [field]: undefined }
    expect(fundingSourceFailed(intent)).toBe(false)
    expect(() => archiveCompletedFunding(memoryStorage(), savedFixture(intent))).toThrow()
  })

  it.each([
    { sourceStatus: 'pending' as const }, { sourceStatus: 'confirmed' as const },
    { sourceTerminal: false }, { sourceConfirmations: 0 }, { sourceConfirmations: 1 },
    { status: 'confirmed' as const },
  ])('does not treat nonterminal or contradictory source evidence as final', (observation) => {
    expect(fundingSourceFailed({ ...terminalSourceIntentFixture(), ...observation })).toBe(false)
  })

  it.each(['expired', 'refunded'] as const)('does not allow abandoning a transfer on the provider’s %s advisory alone', (bridgeStatus) => {
    const intent = intentFixture({ status: 'bridging', sourceTxHash: SOURCE_HASH, bridgeStatus })
    expect(fundingSourceFailed(intent)).toBe(false)
    expect(fundingReady(intent)).toBe(false)
    expect(() => archiveCompletedFunding(memoryStorage(), savedFixture(intent))).toThrow()
  })
})
