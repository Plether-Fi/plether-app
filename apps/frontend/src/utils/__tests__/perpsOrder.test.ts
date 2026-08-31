import { describe, expect, it } from 'vitest'
import { parseUnits } from 'viem'
import { notionalUsdcToQuantizedSizeDelta, notionalUsdcToSizeDelta } from '../perps'
import { PERPS_POSITION_SIZE_QUANTUM } from '../../contracts/perpsConstants'
import { resolvePerpsSizeDelta } from '../perpsOrder'

const PRICE = 98_413_251n
const CURRENT_POSITION_SIZE = 4_500n * 10n ** 18n
const CURRENT_POSITION_NOTIONAL = 4_428_596_295n
const DISPLAY_MAX_NOTIONAL = parseUnits('4450.97', 6)

describe('resolvePerpsSizeDelta', () => {
  it('uses exact onchain position size for max reduce/full close', () => {
    const sizeDelta = resolvePerpsSizeDelta({
      isReducingCurrentPosition: true,
      currentPositionSize: CURRENT_POSITION_SIZE,
      notionalUsdc: DISPLAY_MAX_NOTIONAL,
      maxNotionalUsdc: DISPLAY_MAX_NOTIONAL,
      oraclePrice: PRICE,
    })

    expect(sizeDelta).toBe(CURRENT_POSITION_SIZE)
    expect(sizeDelta).not.toBe(notionalUsdcToSizeDelta(DISPLAY_MAX_NOTIONAL, PRICE))
  })

  it('rounds a partial reduce down to the protocol quantum', () => {
    const partialNotional = parseUnits('2000', 6)
    const sizeDelta = resolvePerpsSizeDelta({
      isReducingCurrentPosition: true,
      currentPositionSize: CURRENT_POSITION_SIZE,
      notionalUsdc: partialNotional,
      maxNotionalUsdc: DISPLAY_MAX_NOTIONAL,
      oraclePrice: PRICE,
    })

    expect(sizeDelta).toBe(notionalUsdcToQuantizedSizeDelta(partialNotional, PRICE))
    expect(sizeDelta % PERPS_POSITION_SIZE_QUANTUM).toBe(0n)
    expect(sizeDelta).toBeLessThan(CURRENT_POSITION_SIZE)
  })

  it('rounds open sizes down to the protocol quantum', () => {
    const openNotional = parseUnits('500', 6)
    const sizeDelta = resolvePerpsSizeDelta({
      isReducingCurrentPosition: false,
      currentPositionSize: CURRENT_POSITION_SIZE,
      notionalUsdc: openNotional,
      maxNotionalUsdc: CURRENT_POSITION_NOTIONAL,
      oraclePrice: PRICE,
    })

    expect(sizeDelta).toBe(notionalUsdcToQuantizedSizeDelta(openNotional, PRICE))
    expect(sizeDelta % PERPS_POSITION_SIZE_QUANTUM).toBe(0n)
    expect(sizeDelta).toBeLessThanOrEqual(notionalUsdcToSizeDelta(openNotional, PRICE))
  })
})
