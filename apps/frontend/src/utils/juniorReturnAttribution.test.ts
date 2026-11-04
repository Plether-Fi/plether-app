import { describe, expect, it } from 'vitest'
import { buildJuniorReturnBreakdown, juniorReturnPeriod } from './juniorReturnAttribution'
import { juniorReturnExample as attribution, juniorReturnExamplePeriod as period } from '../stories/fixtures/juniorReturnAttribution'
import { createVaultAssetHistory } from '../stories/fixtures/vaultAssetHistory'

describe('junior diluted share-price attribution', () => {
  it('rounds the full-precision 14-day example to +0.523% and 14.56% APY', () => {
    const result = buildJuniorReturnBreakdown(period, attribution)!
    expect(result.returnPp.toFixed(3)).toBe('0.523')
    expect(result.apyPercent?.toFixed(2)).toBe('14.56')
    expect(result.residualPp).toBeNull()
    expect(result.bars).toHaveLength(9)
    const bars = result.bars!
    expect(bars[0].start).toBe(0)
    for (let index = 1; index < 8; index++) expect(bars[index].start).toBe(bars[index - 1].end)
    expect(bars[8].start).toBe(0)
    expect(bars[8].end).toBe(result.returnPp)
    expect(bars[7].end).toBeCloseTo(result.returnPp, 12)
  })

  it('rejects malformed accounting explanations', () => {
    for (const notes of ['hidden', [null], ['x'.repeat(2001)], Array(17).fill('note')]) {
      const result = buildJuniorReturnBreakdown(period, { ...attribution, notes })!
      expect(result.bars).toBeNull()
    }
  })

  it('does not convert USDG totals into contribution percentages', () => {
    const changed = structuredClone(attribution)
    changed.components.netVpi.amountUsdg = '2000000'
    const result = buildJuniorReturnBreakdown(period, changed)!
    expect(result.bars?.[0].contributionPp).toBe(0.292)
    expect(result.returnPp).toBe(buildJuniorReturnBreakdown(period, attribution)?.returnPp)
    expect(result.bars?.find((bar) => bar.id === 'unexplained')?.amountUsdg).toBe('3809998000000')
  })

  it('keeps deposits and withdrawals neutral when shares and assets change', () => {
    const history = createVaultAssetHistory()
    history.junior.points = history.junior.points.map((point, index) => ({
      ...point, totalAssets: String(index % 2 ? 900000000000n : 1000000000n),
      totalSupply: String(index % 2 ? 900000000000000n : 1000000000000n),
    }))
    const result = buildJuniorReturnBreakdown(juniorReturnPeriod(history))!
    expect(result.returnPp).toBe(0)
    expect(result.apyPercent).toBe(0)
    expect(result.bars).toBeNull()
  })

  it('identifies residuals without changing either PnL component', () => {
    const changed = structuredClone(attribution)
    changed.components.netVpi.contributionPp -= 0.05
    const result = buildJuniorReturnBreakdown(period, changed)!
    expect(result.residualPp).toBeCloseTo(0.05)
    expect(result.bars?.find((bar) => bar.id === 'settledTraderPnl')?.contributionPp).toBe(0.087)
    expect(result.bars?.find((bar) => bar.id === 'openPositionValuation')?.contributionPp).toBe(-0.097)
    expect(result.bars?.at(-2)?.id).toBe('unexplained')
    expect(result.bars?.at(-2)?.end).toBe(result.bars?.at(-1)?.end)
  })

  it('uses actual elapsed seconds and handles losses, total loss and zero start', () => {
    const shorter = buildJuniorReturnBreakdown({ ...period, endTimestamp: period.startTimestamp + 7.25 * 86400 })!
    expect(shorter.apyPercent).toBeCloseTo(Math.expm1(Math.log1p(shorter.returnPp / 100) * 365 / 7.25) * 100)
    expect(buildJuniorReturnBreakdown({ ...period, endDilutedSharePrice: '0' })).toMatchObject({ returnPp: -100, apyPercent: -100 })
    expect(buildJuniorReturnBreakdown({ ...period, endDilutedSharePrice: '990000000000000000' })).toMatchObject({ returnPp: -1 })
    expect(buildJuniorReturnBreakdown({ ...period, startDilutedSharePrice: '0' })).toBeNull()
    expect(buildJuniorReturnBreakdown({ ...period, endTimestamp: period.startTimestamp })).toBeNull()
  })

  it('does not claim that a rounded 0.523% produces exactly 14.56% APY', () => {
    expect(buildJuniorReturnBreakdown({ ...period, endDilutedSharePrice: '1005230000000000000' })?.apyPercent?.toFixed(2)).toBe('14.57')
  })

  it.each(['missing', 'nan', 'wrong period', 'positive deduction', 'missing USDG'] as const)('rejects %s attribution', (kind) => {
    const changed = structuredClone(attribution)
    if (kind === 'missing') Reflect.deleteProperty(changed.components, 'realizedCarry')
    if (kind === 'nan') changed.components.netVpi.contributionPp = NaN
    if (kind === 'wrong period') changed.startTimestamp++
    if (kind === 'positive deduction') changed.components.maintenanceDilution.contributionPp = 0.01
    if (kind === 'missing USDG') changed.components.netVpi.amountUsdg = ''
    expect(buildJuniorReturnBreakdown(period, changed)?.bars).toBeNull()
  })

  it('withholds stale, incomplete, empty-supply and discontinuous history', () => {
    const history = createVaultAssetHistory()
    expect(juniorReturnPeriod(history)).toBeDefined()
    history.coverage.complete = false
    expect(juniorReturnPeriod(history)).toBeUndefined()
    history.coverage.complete = true
    history.junior.points[2].markFresh = false
    expect(juniorReturnPeriod(history)).toBeUndefined()
    history.junior.points[2].markFresh = true
    history.junior.points[2].totalSupply = '0'
    expect(juniorReturnPeriod(history)).toBeUndefined()
    history.junior.points.splice(2, 2)
    expect(juniorReturnPeriod(history)).toBeUndefined()
  })
})
