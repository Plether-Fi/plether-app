import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import {
  cancelSponsoredOperationRequest,
  canForceUnlockLegacySponsoredOperation,
  createSponsoredOperationSignal,
  forceUnlockLegacySponsoredOperation,
  LEGACY_AMBIGUOUS_OPERATION_MANIFEST_VERSION,
  mergeSponsoredOperationState,
  migrateSponsoredOperationState,
  releaseSponsoredOperationSignal,
  restoreSponsoredOperationLane,
  sponsoredOperationAutomaticRecoveryDelayMs,
  sponsoredOperationAutomaticRecoveryIsDue,
  sponsoredOperationAutomaticRecoveryIsExhausted,
  SPONSORED_OPERATION_AUTOMATIC_RECOVERY_MAX_DELAY_MS,
  SPONSORED_OPERATION_AUTOMATIC_RECOVERY_WINDOW_MS,
  SPONSORED_OPERATION_JOURNAL_PREFIX,
  SPONSORED_OPERATION_LANE_HEAD_PREFIX,
  SPONSORED_OPERATION_LANE_RELEASE_PREFIX,
  SPONSORED_OPERATION_RESOLUTION_PREFIX,
  SPONSORED_OPERATION_STORAGE_NAME,
  SponsoredOperationLockedError,
  useSponsoredOperationStore,
} from '../operationStore'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address

function begin(id: string) {
  useSponsoredOperationStore.getState().beginOperation({
    id,
    ownerAddress: OWNER,
    accountAddress: ACCOUNT,
    chainId: 421614,
    accountMode: 'simple',
    manifestVersion: 'v1',
    action: 'place-order',
  })
}

describe('sponsored operation store', () => {
  beforeEach(() => {
    globalThis.localStorage.clear()
    vi.stubGlobal('navigator', {
      locks: {
        request: vi.fn(async (
          name: string,
          _options: LockOptions,
          callback: (lock: Lock | null) => Promise<unknown> | unknown
        ) => await callback({ name, mode: 'exclusive' } as Lock)),
      } as unknown as LockManager,
    })
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
  })

  afterEach(() => {
    vi.useRealTimers()
    vi.restoreAllMocks()
    vi.unstubAllGlobals()
  })

  it('accepts sponsorship only after managed preparation succeeds', () => {
    begin('operation-1')

    useSponsoredOperationStore.getState().transition(
      'operation-1',
      'requesting-sponsorship'
    )
    expect(
      useSponsoredOperationStore.getState().operations[0]?.sponsorshipAccepted
    ).toBe(false)

    useSponsoredOperationStore.getState().transition(
      'operation-1',
      'awaiting-signature'
    )
    expect(
      useSponsoredOperationStore.getState().operations[0]?.sponsorshipAccepted
    ).toBe(true)
  })

  it('allows only one mutable flow per Trading Account lane', () => {
    begin('operation-1')

    expect(() => begin('operation-2')).toThrow(SponsoredOperationLockedError)

    useSponsoredOperationStore.getState().transition('operation-1', 'confirmed')
    expect(() => begin('operation-2')).not.toThrow()
  })

  it('persists UserOperation and transaction hashes separately', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      '0x1234'
    )
    useSponsoredOperationStore.getState().recordTransactionHash(
      'operation-1',
      '0xabcd'
    )

    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'submitting',
      userOperationHash: '0x1234',
      transactionHash: '0xabcd',
    })
  })

  it('durably releases the lane after exact successful latest-chain inclusion', async () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-07-31T08:00:00.000Z'))
    const userOperationHash = `0x${'12'.repeat(32)}` as Hex
    const includedTransactionHash = `0x${'34'.repeat(32)}` as Hex
    begin('operation-1')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      userOperationHash
    )).toBe(true)
    useSponsoredOperationStore.getState().failOperation({
      id: 'operation-1',
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: false,
    })

    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'operation-1',
      {
        transactionHash: includedTransactionHash,
        blockNumber: '123',
        blockHash: `0x${'78'.repeat(32)}`,
      }
    )).toBe(true)
    expect(useSponsoredOperationStore.getState().operations[0])
      .not.toHaveProperty('laneReleasedAfterSuccessfulInclusion')
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'operation-1',
    })
    expect(useSponsoredOperationStore.getState()
      .releaseLaneAfterSuccessfulInclusion(
        'operation-1',
        {
          transactionHash: includedTransactionHash,
          blockNumber: '123',
          blockHash: `0x${'78'.repeat(32)}`,
          success: true,
        }
      )).toBe(true)
    const includedOperation =
      useSponsoredOperationStore.getState().operations[0]!
    expect(includedOperation).toMatchObject({
      status: 'confirming',
      userOperationHash,
      includedTransactionHash,
      includedBlockNumber: '123',
      includedBlockHash: `0x${'78'.repeat(32)}`,
      inclusionObservedAt: expect.any(Number),
      inclusionEvidenceRevision: 1,
      laneReleasedAfterSuccessfulInclusion: true,
      sponsorshipAccepted: true,
    })
    expect(includedOperation.reason).toBeUndefined()
    expect(includedOperation.retryable).toBeUndefined()
    expect(includedOperation.transactionHash).toBeUndefined()
    expect(includedOperation.transactionHashVerified).toBeUndefined()
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})

    const journalKey =
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}operation-1`
    const durableJournal = globalThis.localStorage.getItem(journalKey)
    vi.advanceTimersByTime(5_000)
    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'operation-1',
      {
        transactionHash: includedTransactionHash,
        blockNumber: '123',
        blockHash: `0x${'78'.repeat(32)}`,
      }
    )).toBe(true)
    expect(useSponsoredOperationStore.getState().operations[0]?.updatedAt)
      .toBe(includedOperation.updatedAt)
    expect(globalThis.localStorage.getItem(journalKey)).toBe(durableJournal)

    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    await useSponsoredOperationStore.persist.rehydrate()

    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        id: 'operation-1',
        status: 'confirming',
        includedTransactionHash,
        laneReleasedAfterSuccessfulInclusion: true,
      })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
    expect(() => begin('operation-2')).not.toThrow()
  })

  it('does not release the lane when the durable release marker cannot persist', () => {
    const userOperationHash = `0x${'12'.repeat(32)}` as Hex
    const includedTransactionHash = `0x${'34'.repeat(32)}` as Hex
    begin('operation-1')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      userOperationHash
    )).toBe(true)
    const journalKey =
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}operation-1`
    const releaseKey =
      `${SPONSORED_OPERATION_LANE_RELEASE_PREFIX}` +
      `operation-1:${userOperationHash.toLowerCase()}`
    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'operation-1',
      { transactionHash: includedTransactionHash }
    )).toBe(true)
    const preReleaseJournal = globalThis.localStorage.getItem(journalKey)
    expect(preReleaseJournal).not.toContain(
      'laneReleasedAfterSuccessfulInclusion'
    )
    const originalSetItem =
      globalThis.localStorage.setItem.bind(globalThis.localStorage)
    vi.spyOn(globalThis.localStorage, 'setItem').mockImplementation(
      (key, value) => {
        if (key === releaseKey) {
          throw new Error('simulated lane-release tombstone failure')
        }
        originalSetItem(key, value)
      }
    )

    expect(useSponsoredOperationStore.getState()
      .releaseLaneAfterSuccessfulInclusion(
        'operation-1',
        { transactionHash: includedTransactionHash, success: true }
      )).toBe(false)
    expect(useSponsoredOperationStore.getState().operations[0])
      .not.toHaveProperty('laneReleasedAfterSuccessfulInclusion')
    expect(globalThis.localStorage.getItem(releaseKey)).toBeNull()
    expect(globalThis.localStorage.getItem(journalKey))
      .toBe(preReleaseJournal)
    expect(globalThis.localStorage.getItem(journalKey)).not.toContain(
      'laneReleasedAfterSuccessfulInclusion'
    )
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'operation-1',
    })
    expect(() => begin('operation-2')).toThrow(SponsoredOperationLockedError)
  })

  it('releases after the tombstone barrier even if the mutable journal write fails', () => {
    const userOperationHash = `0x${'12'.repeat(32)}` as Hex
    const includedTransactionHash = `0x${'34'.repeat(32)}` as Hex
    begin('operation-1')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      userOperationHash
    )).toBe(true)
    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'operation-1',
      { transactionHash: includedTransactionHash }
    )).toBe(true)
    const journalKey =
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}operation-1`
    const releaseKey =
      `${SPONSORED_OPERATION_LANE_RELEASE_PREFIX}` +
      `operation-1:${userOperationHash.toLowerCase()}`
    const preReleaseJournal = globalThis.localStorage.getItem(journalKey)
    const originalSetItem =
      globalThis.localStorage.setItem.bind(globalThis.localStorage)
    let releaseBarrierPersisted = false
    vi.spyOn(globalThis.localStorage, 'setItem').mockImplementation(
      (key, value) => {
        if (key === releaseKey) {
          originalSetItem(key, value)
          releaseBarrierPersisted = true
          return
        }
        if (releaseBarrierPersisted && key === journalKey) {
          throw new Error('simulated post-barrier journal failure')
        }
        originalSetItem(key, value)
      }
    )

    expect(useSponsoredOperationStore.getState()
      .releaseLaneAfterSuccessfulInclusion(
        'operation-1',
        { transactionHash: includedTransactionHash, success: true }
      )).toBe(true)
    expect(releaseBarrierPersisted).toBe(true)
    expect(globalThis.localStorage.getItem(releaseKey))
      .toContain(includedTransactionHash)
    expect(globalThis.localStorage.getItem(journalKey))
      .toBe(preReleaseJournal)
    expect(globalThis.localStorage.getItem(journalKey)).not.toContain(
      'laneReleasedAfterSuccessfulInclusion'
    )
    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({ laneReleasedAfterSuccessfulInclusion: true })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
  })

  it('restores release from its tombstone after an old v1 journal overwrite', async () => {
    const userOperationHash = `0x${'12'.repeat(32)}` as Hex
    const includedTransactionHash = `0x${'34'.repeat(32)}` as Hex
    begin('operation-1')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      userOperationHash
    )).toBe(true)
    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'operation-1',
      { transactionHash: includedTransactionHash }
    )).toBe(true)
    const journalKey =
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}operation-1`
    const releaseKey =
      `${SPONSORED_OPERATION_LANE_RELEASE_PREFIX}` +
      `operation-1:${userOperationHash.toLowerCase()}`
    const oldV1Journal = globalThis.localStorage.getItem(journalKey)
    expect(oldV1Journal).not.toContain(
      'laneReleasedAfterSuccessfulInclusion'
    )

    expect(useSponsoredOperationStore.getState()
      .releaseLaneAfterSuccessfulInclusion(
        'operation-1',
        { transactionHash: includedTransactionHash, success: true }
      )).toBe(true)
    expect(globalThis.localStorage.getItem(releaseKey)).not.toBeNull()

    // A still-open v1 tab can finish a stale mutable-journal write after the
    // new append-only release barrier. The tombstone must win on hydration.
    globalThis.localStorage.setItem(journalKey, oldV1Journal!)
    expect(globalThis.localStorage.getItem(journalKey)).not.toContain(
      'laneReleasedAfterSuccessfulInclusion'
    )
    useSponsoredOperationStore.setState({ operations: [], activeLanes: {} })
    await useSponsoredOperationStore.persist.rehydrate()

    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        id: 'operation-1',
        status: 'confirming',
        includedTransactionHash,
        laneReleasedAfterSuccessfulInclusion: true,
      })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
  })

  it('merges newer durable inclusion evidence into a signal-held stale record', async () => {
    const userOperationHash = `0x${'12'.repeat(32)}` as Hex
    const firstTransactionHash = `0x${'34'.repeat(32)}` as Hex
    const replacementTransactionHash = `0x${'56'.repeat(32)}` as Hex
    begin('operation-1')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      userOperationHash
    )).toBe(true)
    const staleBeforeInclusion = {
      ...useSponsoredOperationStore.getState().operations[0]!,
    }
    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'operation-1',
      { transactionHash: firstTransactionHash }
    )).toBe(true)
    expect(useSponsoredOperationStore.getState()
      .releaseLaneAfterSuccessfulInclusion(
        'operation-1',
        { transactionHash: firstTransactionHash, success: true }
      )).toBe(true)
    expect(useSponsoredOperationStore.getState().clearObservedInclusion(
      'operation-1'
    )).toBe(true)

    createSponsoredOperationSignal('operation-1')
    try {
      useSponsoredOperationStore.setState({
        operations: [staleBeforeInclusion],
        activeLanes: {
          [`${ACCOUNT.toLowerCase()}:default`]: 'operation-1',
        },
      })
      await useSponsoredOperationStore.persist.rehydrate()

      const durableRetraction =
        useSponsoredOperationStore.getState().operations[0]!
      expect(durableRetraction).toMatchObject({
        inclusionEvidenceRevision: 2,
        laneReleasedAfterSuccessfulInclusion: true,
      })
      expect(durableRetraction.includedTransactionHash).toBeUndefined()
      expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})

      expect(useSponsoredOperationStore.getState().recordObservedInclusion(
        'operation-1',
        { transactionHash: replacementTransactionHash }
      )).toBe(true)
      const durableReplacement =
        useSponsoredOperationStore.getState().operations[0]!
      expect(durableReplacement).toMatchObject({
        includedTransactionHash: replacementTransactionHash,
        inclusionEvidenceRevision: 3,
        laneReleasedAfterSuccessfulInclusion: true,
      })

      useSponsoredOperationStore.setState({
        operations: [durableRetraction],
        activeLanes: {},
      })
      await useSponsoredOperationStore.persist.rehydrate()

      expect(useSponsoredOperationStore.getState().operations[0])
        .toMatchObject({
          includedTransactionHash: replacementTransactionHash,
          inclusionEvidenceRevision: 3,
          laneReleasedAfterSuccessfulInclusion: true,
        })
      expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
    } finally {
      releaseSponsoredOperationSignal('operation-1')
    }
  })

  it('rejects a duplicate hash after a released inclusion is rehydrated', async () => {
    const userOperationHash = `0x${'12'.repeat(32)}` as Hex
    begin('operation-1')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      userOperationHash
    )).toBe(true)
    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'operation-1',
      { transactionHash: `0x${'34'.repeat(32)}` }
    )).toBe(true)
    expect(useSponsoredOperationStore.getState()
      .releaseLaneAfterSuccessfulInclusion(
        'operation-1',
        {
          transactionHash: `0x${'34'.repeat(32)}`,
          success: true,
        }
      )).toBe(true)

    useSponsoredOperationStore.setState({ operations: [], activeLanes: {} })
    await useSponsoredOperationStore.persist.rehydrate()
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})

    begin('operation-2')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-2',
      userOperationHash
    )).toBe(false)
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-2',
      `0x${'56'.repeat(32)}`
    )).toBe(true)
  })

  it('retracts reorged inclusion and accepts a replacement before safe confirmation', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-07-31T08:00:00.000Z'))
    const userOperationHash = `0x${'12'.repeat(32)}` as Hex
    const firstTransactionHash = `0x${'34'.repeat(32)}` as Hex
    const replacementTransactionHash = `0x${'56'.repeat(32)}` as Hex
    begin('operation-1')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      userOperationHash
    )).toBe(true)
    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'operation-1',
      { transactionHash: firstTransactionHash }
    )).toBe(true)
    expect(useSponsoredOperationStore.getState()
      .releaseLaneAfterSuccessfulInclusion(
        'operation-1',
        { transactionHash: firstTransactionHash, success: true }
      )).toBe(true)
    const staleIncludedOperation =
      useSponsoredOperationStore.getState().operations[0]!
    expect(staleIncludedOperation.laneReleasedAfterSuccessfulInclusion)
      .toBe(true)

    begin('operation-2')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-2',
      `0x${'ab'.repeat(32)}`
    )).toBe(true)

    // Evidence ordering must survive a user or OS clock adjustment.
    vi.setSystemTime(new Date('2026-07-31T07:00:00.000Z'))
    expect(
      useSponsoredOperationStore.getState().clearObservedInclusion(
        'operation-1'
      )
    ).toBe(true)
    const retractedOperation =
      useSponsoredOperationStore.getState().operations[0]!
    expect(retractedOperation.includedTransactionHash).toBeUndefined()
    expect(retractedOperation.inclusionObservedAt).toBeUndefined()
    expect(retractedOperation.inclusionEvidenceRevision).toBe(2)
    expect(retractedOperation.laneReleasedAfterSuccessfulInclusion).toBe(true)
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'operation-2',
    })

    const merged = mergeSponsoredOperationState({
      operations: [staleIncludedOperation],
      activeLanes: {},
    }, useSponsoredOperationStore.getState())
    expect(merged.operations[0]?.includedTransactionHash).toBeUndefined()
    expect(merged.operations[0]?.laneReleasedAfterSuccessfulInclusion)
      .toBe(true)

    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'operation-1',
      { transactionHash: replacementTransactionHash }
    )).toBe(true)
    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        status: 'confirming',
        includedTransactionHash: replacementTransactionHash,
      })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'operation-2',
    })

    useSponsoredOperationStore.getState().recordTransactionHash(
      'operation-1',
      replacementTransactionHash
    )
    useSponsoredOperationStore.getState().transition(
      'operation-1',
      'confirmed'
    )
    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'operation-1',
      { transactionHash: firstTransactionHash }
    )).toBe(false)
    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        status: 'confirmed',
        transactionHash: replacementTransactionHash,
        transactionHashVerified: true,
      })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'operation-2',
    })
  })

  it('never overwrites the first persisted UserOperation hash', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      `0x${'12'.repeat(32)}`
    )
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      `0x${'34'.repeat(32)}`
    )

    expect(
      useSponsoredOperationStore.getState().operations[0]?.userOperationHash
    ).toBe(`0x${'12'.repeat(32)}`)
  })

  it('acknowledges attention without changing operation recency', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-07-17T08:00:00.000Z'))
    begin('operation-1')

    vi.advanceTimersByTime(1_000)
    useSponsoredOperationStore.getState().failOperation({
      id: 'operation-1',
      reason: 'POLICY_DENIED',
      retryable: false,
    })
    const failedAt = useSponsoredOperationStore.getState().operations[0]
      ?.updatedAt
    const failedRevision = useSponsoredOperationStore.getState().operations[0]
      ?.attentionRevision

    vi.advanceTimersByTime(1_000)
    useSponsoredOperationStore.getState().acknowledgeOperations([{
      id: 'operation-1',
      attentionRevision: failedRevision ?? 0,
    }])
    const acknowledgedOperation = useSponsoredOperationStore.getState()
      .operations[0]

    expect(acknowledgedOperation?.acknowledgedAttentionRevision).toBe(1)
    expect(acknowledgedOperation?.updatedAt).toBe(failedAt)

    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      '0x1234'
    )
    useSponsoredOperationStore.getState().recordTransactionHash(
      'operation-1',
      '0xabcd'
    )
    useSponsoredOperationStore.getState().incrementRetry('operation-1')
    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        attentionRevision: 1,
        acknowledgedAttentionRevision: 1,
      })

    useSponsoredOperationStore.getState().failOperation({
      id: 'operation-1',
      reason: 'POLICY_DENIED',
      retryable: false,
    })
    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        attentionRevision: 1,
        acknowledgedAttentionRevision: 1,
        updatedAt: Date.now(),
      })

    useSponsoredOperationStore.getState().acknowledgeOperations([{
      id: 'operation-1',
      attentionRevision: 1,
    }])
    expect(
      useSponsoredOperationStore.getState().operations[0]
        ?.acknowledgedAttentionRevision
    ).toBe(1)
  })

  it('surfaces a new attention outcome after an acknowledged timeout', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().failOperation({
      id: 'operation-1',
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: true,
    })
    useSponsoredOperationStore.getState().acknowledgeOperations([{
      id: 'operation-1',
      attentionRevision: 1,
    }])

    useSponsoredOperationStore.getState().transition('operation-1', 'dropped')

    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        status: 'dropped',
        attentionRevision: 2,
        acknowledgedAttentionRevision: 1,
      })
  })

  it('does not resurrect acknowledged attention for the same timeout', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().failOperation({
      id: 'operation-1',
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: false,
    })
    useSponsoredOperationStore.getState().acknowledgeOperations([{
      id: 'operation-1',
      attentionRevision: 1,
    }])
    const acknowledged = useSponsoredOperationStore.getState().operations[0]!

    useSponsoredOperationStore.getState().failOperation({
      id: 'operation-1',
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: false,
    })

    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      attentionRevision: acknowledged.attentionRevision,
      acknowledgedAttentionRevision:
        acknowledged.acknowledgedAttentionRevision,
      updatedAt: acknowledged.updatedAt,
    })
  })

  it('does not locally cancel an operation once submission has started', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().transition(
      'operation-1',
      'submitting'
    )

    cancelSponsoredOperationRequest('operation-1')

    expect(useSponsoredOperationStore.getState().operations[0]?.status)
      .toBe('submitting')
    expect(() => begin('operation-2')).toThrow(SponsoredOperationLockedError)
  })

  it('removes stale and terminal operations from persisted active lanes', () => {
    begin('operation-1')
    useSponsoredOperationStore.setState((state) => ({
      operations: state.operations.map((operation) => ({
        ...operation,
        status: 'confirmed',
        updatedAt: Date.now(),
      })),
      activeLanes: state.activeLanes,
    }))

    useSponsoredOperationStore.getState().cleanupOperations()

    expect(() => begin('operation-2')).not.toThrow()
  })

  it('backs automatic recovery off exponentially with a bounded delay', () => {
    const now = Date.now()
    begin('operation-1')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      `0x${'12'.repeat(32)}`
    )
    const operation = useSponsoredOperationStore.getState().operations[0]!

    expect(sponsoredOperationAutomaticRecoveryIsDue(operation, now)).toBe(true)
    expect(sponsoredOperationAutomaticRecoveryDelayMs(1)).toBe(5_000)
    expect(sponsoredOperationAutomaticRecoveryDelayMs(2)).toBe(10_000)
    expect(sponsoredOperationAutomaticRecoveryDelayMs(3)).toBe(20_000)
    expect(sponsoredOperationAutomaticRecoveryDelayMs(100)).toBe(
      SPONSORED_OPERATION_AUTOMATIC_RECOVERY_MAX_DELAY_MS
    )
    expect(sponsoredOperationAutomaticRecoveryIsExhausted(
      operation,
      now + SPONSORED_OPERATION_AUTOMATIC_RECOVERY_WINDOW_MS
    )).toBe(true)
    expect(sponsoredOperationAutomaticRecoveryIsDue(
      operation,
      now + SPONSORED_OPERATION_AUTOMATIC_RECOVERY_WINDOW_MS
    )).toBe(false)
  })

  it('migrates day-old hash recovery records to a durable unknown outcome', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-08-01T08:00:00.000Z'))
    const userOperationHash = `0x${'12'.repeat(32)}` as Hex
    begin('stale-timeout')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'stale-timeout',
      userOperationHash
    )
    useSponsoredOperationStore.getState().failOperation({
      id: 'stale-timeout',
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: false,
    })

    vi.advanceTimersByTime(24 * 60 * 60 * 1000 + 1)
    useSponsoredOperationStore.getState().cleanupOperations()

    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'outcome-unknown',
      automaticRecoveryExpired: true,
      retryable: false,
    })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_RESOLUTION_PREFIX}` +
      `stale-timeout:${userOperationHash}:outcome-unknown`
    )).toContain('"automaticRecoveryExpired":true')
  })

  it('keeps the account lane locked while a submitted operation receipt is uncertain', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      `0x${'12'.repeat(32)}`
    )
    useSponsoredOperationStore.getState().failOperation({
      id: 'operation-1',
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: true,
    })

    expect(() => begin('operation-2')).toThrow(SponsoredOperationLockedError)

    useSponsoredOperationStore.getState().transition(
      'operation-1',
      'dropped'
    )
    expect(() => begin('operation-2')).not.toThrow()
  })

  it('does not resurrect a terminal operation during late recovery', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().transition(
      'operation-1',
      'confirmed'
    )

    useSponsoredOperationStore.getState().transition(
      'operation-1',
      'confirming'
    )
    useSponsoredOperationStore.getState().failOperation({
      id: 'operation-1',
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: false,
    })

    expect(
      useSponsoredOperationStore.getState().operations[0]?.status
    ).toBe('confirmed')
    expect(() => begin('operation-2')).not.toThrow()
  })

  it('manually releases only the known hash-only legacy lock as outcome unknown', async () => {
    useSponsoredOperationStore.getState().beginOperation({
      id: 'legacy-operation',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: LEGACY_AMBIGUOUS_OPERATION_MANIFEST_VERSION,
      action: 'place-order',
    })
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'legacy-operation',
      `0x${'12'.repeat(32)}`
    )
    useSponsoredOperationStore.getState().failOperation({
      id: 'legacy-operation',
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: false,
    })

    await forceUnlockLegacySponsoredOperation('legacy-operation')

    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        status: 'outcome-unknown',
        forcedLegacyUnlock: true,
        retryable: false,
      })
    expect(
      useSponsoredOperationStore.getState().operations[0]?.reason
    ).toBeUndefined()
    expect(() => begin('operation-2')).not.toThrow()
  })

  it('does not let a later legacy snapshot relock a force-released hash', async () => {
    begin('seed-operation')
    const legacyOperation = {
      ...useSponsoredOperationStore.getState().operations[0]!,
      id: 'legacy-operation',
      manifestVersion: LEGACY_AMBIGUOUS_OPERATION_MANIFEST_VERSION,
      status: 'dropped' as const,
      userOperationHash: `0x${'12'.repeat(32)}` as `0x${string}`,
      updatedAt: Date.now() + 60_000,
    }
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    globalThis.localStorage.clear()
    const legacySnapshot = JSON.stringify({
      state: {
        operations: [legacyOperation],
        activeLanes: {},
      },
      version: 0,
    })
    globalThis.localStorage.setItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      legacySnapshot
    )
    restoreSponsoredOperationLane({
      chainId: 421614,
      accountAddress: ACCOUNT,
      lane: 'default',
    })

    expect(await forceUnlockLegacySponsoredOperation(
      'legacy-operation'
    )).toBe(true)

    // A still-open version-0 tab can republish a more recently timestamped
    // diagnostic snapshot. The v1 journal's explicit outcome-unknown decision
    // is stronger evidence and must remain lane-releasing.
    globalThis.localStorage.setItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      legacySnapshot
    )
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    restoreSponsoredOperationLane({
      chainId: 421614,
      accountAddress: ACCOUNT,
      lane: 'default',
    })

    expect(useSponsoredOperationStore.getState().operations
      .find((operation) => operation.id === 'legacy-operation'))
      .toMatchObject({
        status: 'outcome-unknown',
        forcedLegacyUnlock: true,
      })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})

    begin('operation-2')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-2',
      `0x${'34'.repeat(32)}`
    )).toBe(true)
  })

  it('keeps legacy resolution durable across cleanup and stale journal writes', async () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-07-29T08:00:00.000Z'))
    begin('seed-operation')
    const legacyHash = `0x${'12'.repeat(32)}` as `0x${string}`
    const legacyOperation = {
      ...useSponsoredOperationStore.getState().operations[0]!,
      id: 'legacy-operation',
      status: 'dropped' as const,
      userOperationHash: legacyHash,
    }
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    globalThis.localStorage.clear()
    globalThis.localStorage.setItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      JSON.stringify({
        state: {
          operations: [legacyOperation],
          activeLanes: {},
        },
        version: 0,
      })
    )
    restoreSponsoredOperationLane({
      chainId: 421614,
      accountAddress: ACCOUNT,
      lane: 'default',
    })
    const journalKey =
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}legacy-operation`
    const staleJournal = globalThis.localStorage.getItem(journalKey)
    expect(staleJournal).toContain('receipt-timeout')

    expect(await forceUnlockLegacySponsoredOperation(
      'legacy-operation'
    )).toBe(true)
    const resolutionKey =
      `${SPONSORED_OPERATION_RESOLUTION_PREFIX}` +
      `legacy-operation:${legacyHash}:outcome-unknown`
    expect(globalThis.localStorage.getItem(resolutionKey))
      .toContain('outcome-unknown')

    vi.advanceTimersByTime(25 * 60 * 60 * 1000)
    useSponsoredOperationStore.getState().cleanupOperations()
    expect(globalThis.localStorage.getItem(resolutionKey))
      .toContain('outcome-unknown')
    expect(globalThis.localStorage.getItem(journalKey))
      .toContain('outcome-unknown')

    // Model an unrelated tab completing a stale journal write after the
    // resolution commit, then make key enumeration omit that journal.
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    globalThis.localStorage.setItem(journalKey, staleJournal!)
    vi.spyOn(globalThis.localStorage, 'key').mockReturnValue(null)

    restoreSponsoredOperationLane({
      chainId: 421614,
      accountAddress: ACCOUNT,
      lane: 'default',
    })

    expect(useSponsoredOperationStore.getState().operations
      .find((operation) => operation.id === 'legacy-operation'))
      .toMatchObject({
        status: 'outcome-unknown',
        forcedLegacyUnlock: true,
      })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
    expect(globalThis.localStorage.getItem(resolutionKey))
      .toContain('outcome-unknown')
  })

  it('does not force-unlock a hash from an unknown manifest', async () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      `0x${'12'.repeat(32)}`
    )
    useSponsoredOperationStore.getState().failOperation({
      id: 'operation-1',
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: false,
    })

    await forceUnlockLegacySponsoredOperation('operation-1')

    expect(
      useSponsoredOperationStore.getState().operations[0]?.status
    ).toBe('receipt-timeout')
    expect(() => begin('operation-2')).toThrow(SponsoredOperationLockedError)
  })

  it('relocks v0 diagnostic terminal records for explicit manual recovery', () => {
    begin('operation-1')
    const legacyOperation = {
      ...useSponsoredOperationStore.getState().operations[0]!,
      manifestVersion: 'older-deployment',
      status: 'dropped' as const,
      userOperationHash: `0x${'12'.repeat(32)}` as `0x${string}`,
      transactionHash: `0x${'34'.repeat(32)}` as `0x${string}`,
    }

    const migrated = migrateSponsoredOperationState({
      operations: [legacyOperation],
      activeLanes: {},
    }, 0)

    expect(migrated.operations[0]).toMatchObject({
      status: 'receipt-timeout',
      legacyManualUnlockEligible: true,
      retryable: false,
    })
    expect(migrated.operations[0]?.transactionHash).toBeUndefined()
    expect(migrated.activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'operation-1',
    })
    expect(
      canForceUnlockLegacySponsoredOperation(migrated.operations[0]!)
    ).toBe(true)
  })

  it('relocks a v0 confirmed record instead of trusting its transaction hash', () => {
    begin('operation-1')
    const legacyOperation = {
      ...useSponsoredOperationStore.getState().operations[0]!,
      manifestVersion: 'older-deployment',
      status: 'confirmed' as const,
      userOperationHash: `0x${'12'.repeat(32)}` as `0x${string}`,
      transactionHash: `0x${'34'.repeat(32)}` as `0x${string}`,
    }

    const migrated = migrateSponsoredOperationState({
      operations: [legacyOperation],
      activeLanes: {},
    }, 0)

    expect(migrated.operations[0]).toMatchObject({
      status: 'receipt-timeout',
      legacyManualUnlockEligible: true,
      retryable: false,
    })
    expect(migrated.operations[0]?.transactionHash).toBeUndefined()
    expect(
      migrated.operations[0]?.transactionHashVerified
    ).toBeUndefined()
    expect(migrated.activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'operation-1',
    })
  })

  it('publishes the legacy lane guard before writing operation journals', () => {
    begin('operation-1')
    const legacyOperation = {
      ...useSponsoredOperationStore.getState().operations[0]!,
      status: 'dropped' as const,
      userOperationHash: `0x${'12'.repeat(32)}` as `0x${string}`,
    }
    globalThis.localStorage.clear()
    const originalSetItem =
      globalThis.localStorage.setItem.bind(globalThis.localStorage)
    originalSetItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      JSON.stringify({
        state: {
          operations: [legacyOperation],
          activeLanes: {},
        },
        version: 0,
      })
    )
    vi.spyOn(globalThis.localStorage, 'setItem').mockImplementation(
      (key, value) => {
        if (key.startsWith(SPONSORED_OPERATION_JOURNAL_PREFIX)) {
          throw new Error('simulated crash before journal write')
        }
        originalSetItem(key, value)
      }
    )

    try {
      restoreSponsoredOperationLane({
        chainId: 421614,
        accountAddress: ACCOUNT,
        lane: 'default',
      })
    } catch (error) {
      expect(error).toBeInstanceOf(Error)
    }

    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_LANE_HEAD_PREFIX}` +
      `421614:${ACCOUNT.toLowerCase()}:default`
    )).toContain('operation-1')
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}operation-1`
    )).toBeNull()
    expect(globalThis.localStorage.getItem(
      SPONSORED_OPERATION_STORAGE_NAME
    )).not.toBeNull()
    expect(JSON.parse(globalThis.localStorage.getItem(
      SPONSORED_OPERATION_STORAGE_NAME
    )!).version).toBe(0)
  })

  it('does not publish a headless hash during unlocked v0 hydration', async () => {
    begin('legacy-operation')
    const legacyOperation = {
      ...useSponsoredOperationStore.getState().operations[0]!,
      status: 'dropped' as const,
      userOperationHash: `0x${'12'.repeat(32)}` as `0x${string}`,
    }
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    globalThis.localStorage.clear()
    globalThis.localStorage.setItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      JSON.stringify({
        state: {
          operations: [legacyOperation],
          activeLanes: {},
        },
        version: 0,
      })
    )

    await useSponsoredOperationStore.persist.rehydrate()

    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        id: 'legacy-operation',
        status: 'receipt-timeout',
      })
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_LANE_HEAD_PREFIX}` +
      `421614:${ACCOUNT.toLowerCase()}:default`
    )).toBeNull()
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}legacy-operation`
    )).toBeNull()
    expect(JSON.parse(globalThis.localStorage.getItem(
      SPONSORED_OPERATION_STORAGE_NAME
    )!).version).toBe(0)
  })

  it('rejects a signed submission when a different durable lane head survives', async () => {
    begin('legacy-operation')
    const legacyOperation = {
      ...useSponsoredOperationStore.getState().operations[0]!,
      status: 'dropped' as const,
      userOperationHash: `0x${'12'.repeat(32)}` as `0x${string}`,
    }
    globalThis.localStorage.clear()
    globalThis.localStorage.setItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      JSON.stringify({
        state: {
          operations: [legacyOperation],
          activeLanes: {},
        },
        version: 0,
      })
    )
    restoreSponsoredOperationLane({
      chainId: 421614,
      accountAddress: ACCOUNT,
      lane: 'default',
    })
    await useSponsoredOperationStore.persist.rehydrate()
    const laneHeadKey =
      `${SPONSORED_OPERATION_LANE_HEAD_PREFIX}` +
      `421614:${ACCOUNT.toLowerCase()}:default`
    const legacyLaneHead = globalThis.localStorage.getItem(laneHeadKey)
    expect(legacyLaneHead).toContain('legacy-operation')

    // Model an older tab erasing the shared snapshot while the directly
    // addressable legacy head and journal survive.
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    globalThis.localStorage.setItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      JSON.stringify({
        state: { operations: [], activeLanes: {} },
        version: 1,
      })
    )
    begin('new-operation')

    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'new-operation',
      `0x${'34'.repeat(32)}`
    )).toBe(false)
    expect(globalThis.localStorage.getItem(laneHeadKey))
      .toBe(legacyLaneHead)
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}new-operation`
    )).toBeNull()
    expect(
      useSponsoredOperationStore.getState().operations
        .find((operation) => operation.id === 'new-operation')
        ?.userOperationHash
    ).toBeUndefined()
  })

  it('restores a submitted operation from its per-operation journal', async () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      `0x${'12'.repeat(32)}`
    )
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}operation-1`
    )).toContain(`0x${'12'.repeat(32)}`)

    // Model a last-writer race that loses the operation from the shared
    // snapshot. The per-operation journal remains authoritative.
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    globalThis.localStorage.setItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      JSON.stringify({
        state: { operations: [], activeLanes: {} },
        version: 1,
      })
    )

    await useSponsoredOperationStore.persist.rehydrate()

    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        id: 'operation-1',
        status: 'submitting',
        userOperationHash: `0x${'12'.repeat(32)}`,
      })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'operation-1',
    })
  })

  it('merges peer storage without erasing a newer persisted submission hash', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      `0x${'12'.repeat(32)}`
    )
    const currentState = useSponsoredOperationStore.getState()
    const currentOperation = currentState.operations[0]!
    const staleOperation = {
      ...currentOperation,
      status: 'awaiting-signature' as const,
      userOperationHash: undefined,
      updatedAt: currentOperation.updatedAt - 1,
    }

    const merged = mergeSponsoredOperationState({
      operations: [staleOperation],
      activeLanes: {},
    }, currentState)

    expect(merged.operations[0]).toMatchObject({
      status: 'submitting',
      userOperationHash: `0x${'12'.repeat(32)}`,
    })
    expect(merged.activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'operation-1',
    })
  })

  it('keeps a local operation when a peer snapshot omits its id', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      `0x${'12'.repeat(32)}`
    )

    const merged = mergeSponsoredOperationState({
      operations: [],
      activeLanes: {},
    }, useSponsoredOperationStore.getState())

    expect(merged.operations).toHaveLength(1)
    expect(merged.operations[0]?.userOperationHash)
      .toBe(`0x${'12'.repeat(32)}`)
    expect(merged.activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'operation-1',
    })
  })

  it('never creates or rewrites the read-only legacy snapshot', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      `0x${'12'.repeat(32)}`
    )
    expect(globalThis.localStorage.getItem(
      SPONSORED_OPERATION_STORAGE_NAME
    )).toBeNull()
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}operation-1`
    )).toContain(`0x${'12'.repeat(32)}`)

    // Generic Zustand persistence may run from any tab. It can update
    // directly addressed journals, but the whole-store key remains a
    // read-only legacy inbox.
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })

    expect(globalThis.localStorage.getItem(
      SPONSORED_OPERATION_STORAGE_NAME
    )).toBeNull()
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}operation-1`
    )).toContain(`0x${'12'.repeat(32)}`)
  })

  it('does not erase legacy evidence that races an unrelated tab write', () => {
    begin('seed-operation')
    const legacyOperation = {
      ...useSponsoredOperationStore.getState().operations[0]!,
      id: 'legacy-operation',
      status: 'dropped' as const,
      userOperationHash: `0x${'34'.repeat(32)}` as `0x${string}`,
    }
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    globalThis.localStorage.clear()

    const originalGetItem =
      globalThis.localStorage.getItem.bind(globalThis.localStorage)
    const originalSetItem =
      globalThis.localStorage.setItem.bind(globalThis.localStorage)
    let injected = false
    vi.spyOn(globalThis.localStorage, 'getItem').mockImplementation((key) => {
      const staleValue = originalGetItem(key)
      if (!injected && key === SPONSORED_OPERATION_STORAGE_NAME) {
        injected = true
        originalSetItem(
          SPONSORED_OPERATION_STORAGE_NAME,
          JSON.stringify({
            state: {
              operations: [legacyOperation],
              activeLanes: {},
            },
            version: 0,
          })
        )
      }
      return staleValue
    })

    // This models generic persistence in a different current-code tab. It
    // read the old value before a version-0 tab published its operation.
    begin('current-operation')

    expect(injected).toBe(true)
    expect(originalGetItem(SPONSORED_OPERATION_STORAGE_NAME))
      .toContain('legacy-operation')
  })

  it('promotes the next ambiguous legacy hash after one is force-released', async () => {
    begin('operation-1')
    const base = useSponsoredOperationStore.getState().operations[0]!
    const migrated = migrateSponsoredOperationState({
      operations: [
        {
          ...base,
          id: 'older-operation',
          status: 'dropped',
          userOperationHash: `0x${'12'.repeat(32)}`,
          updatedAt: 1,
        },
        {
          ...base,
          id: 'newer-operation',
          status: 'execution-reverted',
          userOperationHash: `0x${'34'.repeat(32)}`,
          updatedAt: 2,
        },
      ],
      activeLanes: {},
    }, 0)
    globalThis.localStorage.clear()
    useSponsoredOperationStore.setState(migrated)

    expect(migrated.activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'newer-operation',
    })
    await forceUnlockLegacySponsoredOperation('newer-operation')

    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'older-operation',
    })
    expect(
      useSponsoredOperationStore.getState().operations
        .find((operation) => operation.id === 'newer-operation')?.status
    ).toBe('outcome-unknown')

    // The rollout guard must promote/preserve the remaining ambiguous hash
    // without relying on shared-state or journal-key enumeration.
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    globalThis.localStorage.setItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      JSON.stringify({
        state: { operations: [], activeLanes: {} },
        version: 1,
      })
    )
    vi.spyOn(globalThis.localStorage, 'key').mockReturnValue(null)
    restoreSponsoredOperationLane({
      chainId: 421614,
      accountAddress: ACCOUNT,
      lane: 'default',
    })

    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'older-operation',
    })
  })

  it('promotes the next ambiguous legacy hash after canonical confirmation', () => {
    begin('operation-1')
    const base = useSponsoredOperationStore.getState().operations[0]!
    const migrated = migrateSponsoredOperationState({
      operations: [
        {
          ...base,
          id: 'older-operation',
          status: 'dropped',
          userOperationHash: `0x${'12'.repeat(32)}`,
          updatedAt: 1,
        },
        {
          ...base,
          id: 'newer-operation',
          status: 'confirmed',
          userOperationHash: `0x${'34'.repeat(32)}`,
          updatedAt: 2,
        },
      ],
      activeLanes: {},
    }, 0)
    globalThis.localStorage.clear()
    useSponsoredOperationStore.setState(migrated)

    useSponsoredOperationStore.getState().transition(
      'newer-operation',
      'confirmed'
    )

    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'older-operation',
    })
  })
})
