import { asSponsorRequestError, isDefinitiveSponsorshipRefusal } from './errors'

describe('asSponsorRequestError', () => {
  it('distinguishes explicit simulation refusals from an ambiguous provider outage', () => {
    for (const reason of ['INSUFFICIENT_FREE_EQUITY','INVALID_ORDER_DEADLINE','SIMULATION_FAILED']) expect(isDefinitiveSponsorshipRefusal(reason)).toBe(true)
    expect(isDefinitiveSponsorshipRefusal('BUNDLER_UNAVAILABLE')).toBe(false)
  })
  it('collects JSON-RPC policy metadata through viem error wrappers', () => {
    const rpcCause = Object.assign(new Error('rate limited'), {
      data: {
        reason: 'RATE_LIMITED',
        retryable: true,
        callIndex: 2,
      },
    })
    const viemWrapper = Object.assign(new Error('Limit exceeded'), {
      code: -32005,
      data: undefined,
      cause: rpcCause,
    })

    expect(asSponsorRequestError(viemWrapper)).toMatchObject({
      reason: 'RATE_LIMITED',
      retryable: true,
      callIndex: 2,
      rpcCode: -32005,
    })
  })
})
