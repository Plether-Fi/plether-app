import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address } from 'viem'
import { SponsorRequestError } from '../errors'
import { useSponsoredOperationStore } from '../operationStore'
import { beginSponsoredOperationTracking } from '../operationTracker'

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

describe('sponsored operation tracker', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
  })

  it('keeps sponsorship hidden until the stub has been accepted', () => {
    const tracker = beginSponsoredOperationTracking({
      id: 'operation-1',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'separate-immutable',
      manifestVersion: 'v1',
      action: 'deposit',
    })

    tracker.onStatus('requesting-stub')
    tracker.onStatus('estimating')

    expect(analyticsMocks.trackPerpsSponsoredOperation).toHaveBeenNthCalledWith(
      2,
      'requesting-stub',
      expect.objectContaining({ sponsorship_accepted: false })
    )
    expect(analyticsMocks.trackPerpsSponsoredOperation).toHaveBeenNthCalledWith(
      3,
      'estimating',
      expect.objectContaining({ sponsorship_accepted: true })
    )
  })

  it('preserves stable sponsor failures for retry/support UX', () => {
    const tracker = beginSponsoredOperationTracking({
      id: 'operation-1',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'separate-immutable',
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
})
