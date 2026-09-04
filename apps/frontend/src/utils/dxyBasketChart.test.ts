import { describe, expect, it } from 'vitest'
import type { PerpsBasketCandle } from '../api'
import { perpsBasketCandleToChartCandle } from './dxyBasketChart'

function candle(overrides: Partial<PerpsBasketCandle> = {}): PerpsBasketCandle {
  return {
    timestamp: 1_700_000_000,
    rawOpenPrice: '98000000',
    rawHighPrice: '101000000',
    rawLowPrice: '97000000',
    rawClosePrice: '99000000',
    volumeUsdc: '1250000',
    longFlowVolumeUsdc: '750000',
    shortFlowVolumeUsdc: '250000',
    tradeCount: 2,
    sampleCount: 3,
    quality: 'observed',
    revision: 1,
    priceComplete: true,
    volumeComplete: true,
    complete: true,
    ...overrides,
  }
}

describe('perpsBasketCandleToChartCandle', () => {
  it('inverts raw OHLC and swaps high and low for displayed plDXY', () => {
    expect(perpsBasketCandleToChartCandle(candle(), '200000000')).toEqual({
      timestamp: 1_700_000_000,
      open: 1.02,
      high: 1.03,
      low: 0.99,
      close: 1.01,
    })
  })

  it('uses the response display-price cap instead of assuming a cap of two', () => {
    expect(perpsBasketCandleToChartCandle(candle(), '250000000')).toEqual({
      timestamp: 1_700_000_000,
      open: 1.52,
      high: 1.53,
      low: 1.49,
      close: 1.51,
    })
  })

  it('rejects malformed raw prices and invalid OHLC ordering', () => {
    expect(perpsBasketCandleToChartCandle(
      candle({ rawOpenPrice: 'not-a-price' }),
      '200000000'
    )).toBeUndefined()
    expect(perpsBasketCandleToChartCandle(
      candle({ rawLowPrice: '102000000' }),
      '200000000'
    )).toBeUndefined()
    expect(perpsBasketCandleToChartCandle(
      candle({ rawHighPrice: '200000000' }),
      '200000000'
    )).toBeUndefined()
  })

  it('rejects non-positive, unsafe, and out-of-domain display caps', () => {
    expect(perpsBasketCandleToChartCandle(candle(), '0')).toBeUndefined()
    expect(perpsBasketCandleToChartCandle(candle(), '9007199254740992')).toBeUndefined()
    expect(perpsBasketCandleToChartCandle(candle(), '99000000')).toBeUndefined()
  })
})
