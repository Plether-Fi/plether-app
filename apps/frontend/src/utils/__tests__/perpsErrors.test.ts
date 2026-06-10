import { describe, expect, it } from 'vitest'
import { encodeErrorResult, parseAbi } from 'viem'
import { getPerpsErrorMessage } from '../perpsErrors'

const ORACLE_ERROR_ABI = parseAbi([
  'error OrderRouter__MevDetected()',
  'error PletherOracle__StalePrice(uint8 mode, bytes32 feedId, uint256 publishTime, uint256 maxStaleness, uint256 currentTimestamp)',
])

const ZERO_FEED_ID = '0x0000000000000000000000000000000000000000000000000000000000000000'

describe('getPerpsErrorMessage', () => {
  it('preserves instrumented commit receipt diagnostics', () => {
    const message = 'Commit reverted after wallet confirmation, but the receipt did not include decodable revert data. Failed tx: 0x123.'

    expect(getPerpsErrorMessage(new Error(message), 'commit')).toBe(message)
  })

  it('describes commit-block timing guard as retryable reveal-not-ready state', () => {
    const data = encodeErrorResult({
      abi: ORACLE_ERROR_ABI,
      errorName: 'OrderRouter__MevDetected',
      args: [],
    })

    const message = getPerpsErrorMessage({ cause: { raw: data } }, 'execute')

    expect(message).toBe('Reveal is not ready yet. Wait a few seconds and retry self-execute.')
    expect(message).not.toContain('failed')
  })

  it('describes historical Pyth parse rejection separately from expired price data', () => {
    const data = encodeErrorResult({
      abi: ORACLE_ERROR_ABI,
      errorName: 'PletherOracle__StalePrice',
      args: [0, ZERO_FEED_ID, 1_781_096_515n, 60n, 1_781_096_515n],
    })

    const message = getPerpsErrorMessage({ cause: { raw: data } }, 'execute')

    expect(message).toContain('Historical Pyth update was rejected')
    expect(message).toContain('not expired')
    expect(message).toContain('unique historical tick after commit')
    expect(message).not.toContain('expired before the transaction landed')
  })

  it('still describes genuinely stale Pyth data as expired', () => {
    const data = encodeErrorResult({
      abi: ORACLE_ERROR_ABI,
      errorName: 'PletherOracle__StalePrice',
      args: [0, '0x1111111111111111111111111111111111111111111111111111111111111111', 1_781_096_400n, 60n, 1_781_096_515n],
    })

    const message = getPerpsErrorMessage({ cause: { raw: data } }, 'execute')

    expect(message).toContain('Pyth price data expired before the transaction landed')
    expect(message).toContain('age: 115s')
    expect(message).toContain('limit: 60s')
  })
})
