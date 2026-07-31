import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import {
  SponsorRequestError,
  SponsoredPreflightError,
} from '../errors'
import { SponsoredOperationCoordinationError } from '../laneLock'
import {
  SponsoredOperationLockedError,
  useSponsoredOperationStore,
} from '../operationStore'
import {
  beginSponsoredOperationTracking,
  trackSponsoredOperationPreflightFailure,
} from '../operationTracker'

const analyticsMocks = vi.hoisted(() => ({
  trackPerpsSponsoredOperation: vi.fn(),
}))

vi.mock('../../analytics/perps', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../../analytics/perps')>()
  return {
    ...actual,
    trackPerpsSponsoredOperation: analyticsMocks.trackPerpsSponsoredOperation,
  }
})

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const USER_OPERATION_HASH = `0x${'12'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'34'.repeat(32)}` as Hex

describe('sponsored operation tracker', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    globalThis.localStorage.clear()
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
  })

  it('marks sponsorship accepted after managed preparation succeeds', () => {
    const tracker = beginSponsoredOperationTracking({
      id: 'operation-1',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: 'v1',
      action: 'deposit',
    })

    tracker.onStatus('requesting-sponsorship')

    expect(analyticsMocks.trackPerpsSponsoredOperation).toHaveBeenNthCalledWith(
      2,
      'requesting-sponsorship',
      expect.objectContaining({ sponsorship_accepted: false })
    )

    tracker.onStatus('awaiting-signature')

    expect(analyticsMocks.trackPerpsSponsoredOperation).toHaveBeenNthCalledWith(
      3,
      'awaiting-signature',
      expect.objectContaining({ sponsorship_accepted: true })
    )
  })

  it('preserves stable sponsor failures for retry/support UX', () => {
    const tracker = beginSponsoredOperationTracking({
      id: 'operation-1',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: 'v1',
      action: 'place-order',
    })

    tracker.fail(new SponsorRequestError({
      reason: 'SPONSOR_BUDGET_EXCEEDED',
      message: 'daily budget reached',
      retryable: true,
    }))

    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'failed',
      reason: 'SPONSOR_BUDGET_EXCEEDED',
      retryable: true,
    })
  })

  it('does not downgrade an operation after it is terminal', () => {
    const tracker = beginSponsoredOperationTracking({
      id: 'operation-1',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: 'v1',
      action: 'deposit',
    })

    tracker.onStatus('confirmed')
    tracker.fail(new Error('post-confirmation local cleanup failed'))

    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'confirmed',
    })
  })

  it('does not turn observed inclusion into a safe-head timeout', () => {
    const tracker = beginSponsoredOperationTracking({
      id: 'operation-included',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: 'v1',
      action: 'place-order',
    })
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      tracker.id,
      USER_OPERATION_HASH
    )).toBe(true)
    expect(tracker.onObservedInclusion({
      transactionHash: TRANSACTION_HASH,
    })).toBe(true)

    tracker.fail(new Error('safe head did not catch up before timeout'))

    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        status: 'confirming',
        includedTransactionHash: TRANSACTION_HASH,
      })
    expect(useSponsoredOperationStore.getState().operations[0]?.reason)
      .toBeUndefined()
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'operation-included',
    })
  })

  it('emits an explicit terminal reason for typed preflight failures', () => {
    const reason = trackSponsoredOperationPreflightFailure({
      accountMode: 'simple',
      manifestVersion: 'v1',
      action: 'deposit',
    }, new SponsoredPreflightError({
      reason: 'IDENTITY_NOT_READY',
      message: 'Trading Account identity is not ready',
    }))

    expect(reason).toBe('IDENTITY_NOT_READY')
    expect(analyticsMocks.trackPerpsSponsoredOperation).toHaveBeenCalledWith(
      'preflight_failed',
      {
        manifest_version: 'v1',
        account_mode: 'simple',
        action_kind: 'deposit',
        wallet_family: undefined,
        wallet_version: undefined,
        sponsorship_accepted: false,
        retry_count: 0,
        reason_code: 'IDENTITY_NOT_READY',
        terminal_outcome: 'preflight_failed',
      }
    )
  })

  it.each([
    [
      new SponsoredOperationLockedError('operation-1'),
      'LANE_BUSY',
    ],
    [
      new SponsoredOperationCoordinationError('Web Locks unavailable'),
      'BROWSER_COORDINATION_UNAVAILABLE',
    ],
  ])('maps coordination errors to stable preflight reasons', (error, reason) => {
    trackSponsoredOperationPreflightFailure({
      action: 'deposit',
    }, error)

    expect(analyticsMocks.trackPerpsSponsoredOperation).toHaveBeenCalledWith(
      'preflight_failed',
      expect.objectContaining({ reason_code: reason })
    )
  })
})
