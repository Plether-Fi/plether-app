import { asSponsorRequestError } from './errors'

describe('asSponsorRequestError', () => {
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
