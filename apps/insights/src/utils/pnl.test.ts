import { describe, expect, it } from 'vitest'
import type { WalletDetails } from '../api'
import { calculatePnlBreakdown } from './pnl'

function wallet(overrides: Partial<WalletDetails> = {}): WalletDetails {
  return {
    rank: 1,
    address: '0x1111111111111111111111111111111111111111',
    displayName: 'Trader',
    pnl: '-1311133712',
    realizedPnl: '1408495009',
    roiBps: -131,
    volume: '6621779485325',
    trades: 4,
    activeDays: 1,
    liquidations: 0,
    prizePlace: null,
    prizePlaces: [],
    prizeAmountUsdc: null,
    eligible: false,
    eligibilityStatus: 'pending',
    eligibilityReasons: [],
    equity: '198688866288',
    position: {
      market: 'plDXY Perp',
      side: 'short',
      size: '20400000000',
      sizeDelta: '1',
      margin: '606972630',
      entryPrice: '0.97410638',
      markPrice: null,
      unrealizedPnl: '-13840777',
      liquidatable: false,
    },
    ...overrides,
  }
}

describe('calculatePnlBreakdown', () => {
  it('reconciles directional price P&L with net competition P&L', () => {
    expect(calculatePnlBreakdown(wallet())).toEqual({
      realized: '1408495009',
      unrealized: '-13840777',
      directional: '1394654232',
      costsAndAdjustments: '-2705787944',
      net: '-1311133712',
    })
  })

  it('uses zero unrealized P&L for a flat account', () => {
    expect(calculatePnlBreakdown(wallet({ position: null, pnl: '1000000', realizedPnl: '1500000' })))
      .toMatchObject({ unrealized: '0', costsAndAdjustments: '-500000', net: '1000000' })
  })

  it('returns null when the API provides an invalid numeric value', () => {
    expect(calculatePnlBreakdown(wallet({ realizedPnl: 'unknown' }))).toBeNull()
  })
})
