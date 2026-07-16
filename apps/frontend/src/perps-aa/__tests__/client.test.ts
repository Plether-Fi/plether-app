import { beforeEach, describe, expect, it, vi } from 'vitest'
import { SponsorRequestError } from '../errors'

const mocks = vi.hoisted(() => ({
  sendSponsoredAction: vi.fn(),
}))

vi.mock('@plether/perps-aa-client', async (importOriginal) => {
  const actual = await importOriginal<typeof import('@plether/perps-aa-client')>()
  return {
    ...actual,
    sendSponsoredAction: mocks.sendSponsoredAction,
  }
})

import { sendSponsoredActionWithRestart } from '../client'

describe('sendSponsoredActionWithRestart', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('restarts the complete orchestration after RESTART_ESTIMATION', async () => {
    const restart = vi.fn()
    mocks.sendSponsoredAction
      .mockRejectedValueOnce(new SponsorRequestError({
        reason: 'RESTART_ESTIMATION',
        message: 'stale estimate',
        retryable: true,
      }))
      .mockResolvedValueOnce({ userOperationHash: '0x1234' })

    await expect(sendSponsoredActionWithRestart({
      chainId: 421614,
      action: {} as never,
      account: {} as never,
      sponsor: {} as never,
      bundler: {} as never,
      onEstimationRestart: restart,
    })).resolves.toEqual({ userOperationHash: '0x1234' })

    expect(mocks.sendSponsoredAction).toHaveBeenCalledTimes(2)
    expect(restart).toHaveBeenCalledWith(1)
  })

  it('does not retry a deterministic sponsor denial', async () => {
    mocks.sendSponsoredAction.mockRejectedValue(new SponsorRequestError({
      reason: 'POLICY_DENIED',
      message: 'not eligible',
      retryable: false,
    }))

    await expect(sendSponsoredActionWithRestart({
      chainId: 421614,
      action: {} as never,
      account: {} as never,
      sponsor: {} as never,
      bundler: {} as never,
    })).rejects.toMatchObject({ reason: 'POLICY_DENIED' })

    expect(mocks.sendSponsoredAction).toHaveBeenCalledTimes(1)
  })
})
