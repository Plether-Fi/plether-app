import { describe, expect, it } from 'vitest'
import { buildOgCardSvg, selectLiquidityEfficiencyOgCardData } from '../og/ogCardSvg.js'
import { parseOgImageRoute, resolveRouteOgMeta } from '../og/ogMeta.js'
import {
  SHARE_CARD_HEIGHT,
  SHARE_CARD_WIDTH,
  buildShareCardSvg,
  buildShareTweetUrl,
  escapeXml,
  shareCardFilename,
} from './shareCard'

const account = `0x${'a'.repeat(40)}`

describe('shareCard SVG & social helpers', () => {
  it('renders a 1200x630 closed-position PnL card with USDG formatting and no dollar sign', () => {
    const svg = buildShareCardSvg({
      kind: 'trade',
      traderLabel: '@alice_fx',
      account,
      networkName: 'Arbitrum Sepolia',
      releaseVersion: 'v2.0.0',
      market: 'plDXY-PERP',
      side: 'long',
      realizedPnlAssets: '420500000',
      price: '104500000000000000000',
      sizeDelta: '500000000000000000000',
      notionalAssets: '5000000000',
      occurredAt: '2026-11-26T12:00:00Z',
      txHash: `0x${'1'.repeat(64)}`,
    })
    expect(svg).toContain(`width="${String(SHARE_CARD_WIDTH)}"`)
    expect(svg).toContain(`height="${String(SHARE_CARD_HEIGHT)}"`)
    expect(svg).toContain('+420.50 USDG')
    expect(svg).not.toContain('$')
    expect(svg).toContain('CLOSED LONG')
    expect(svg).toContain('@alice_fx')
    expect(svg).toContain('Arbitrum Sepolia · v2.0.0')
    expect(svg).toContain('26 Nov 2026, 12:00')
    expect(svg).not.toContain('26 Nov 2026, 12:00 UTC')
    expect(svg).toContain('font-size="19" font-weight="700">26 Nov 2026, 12:00</text>')
  })

  it('renders an opened-position transaction receipt card with notional, entry price, and X share link', () => {
    const openData = {
      kind: 'open_trade' as const,
      traderLabel: '@alice_fx',
      account,
      networkName: 'Arbitrum Sepolia',
      releaseVersion: 'v2.0.0',
      market: 'plDXY-PERP',
      side: 'short' as const,
      price: '1054110000000000000',
      sizeDelta: '47433000000000000000000',
      notionalAssets: '50000000000',
      occurredAt: '2026-11-23T11:53:00Z',
      txHash: `0x${'f'.repeat(64)}`,
    }
    const svg = buildShareCardSvg(openData)
    expect(svg).toContain('OPENED POSITION RECEIPT')
    expect(svg).toContain('OPENED SHORT')
    expect(svg).toContain('50,000.00 USDG')
    expect(svg).toContain('23 Nov 2026, 11:53')
    expect(svg).not.toContain('$')
    expect(shareCardFilename(openData)).toBe('plether-open_trade-alice_fx.png')
    expect(buildShareTweetUrl(openData, `https://insights.plether.com/wallets/${account}`)).toContain(
      'Opened+SHORT+plDXY-PERP',
    )
  })

  it('renders a Divisions badge card with tier crest, rank, and weekly return', () => {
    const svg = buildShareCardSvg({
      kind: 'division',
      traderLabel: '@pavilion6018',
      account,
      networkName: 'Arbitrum Sepolia',
      tier: 'Gold',
      rank: 1,
      weeklyReturnLabel: '+29.67%',
      weeklyPnlAssets: '1250000000',
      activeDays: 4,
      movement: 'promoted',
      weekLabel: 'Week of 23 Nov 2026',
      provisional: false,
    })
    expect(svg).toContain('RANK #1')
    expect(svg).toContain('GOLD DIVISION')
    expect(svg).toContain('M82.9025 32.979L95.932 51.4464') // official gold.svg sunburst path
    expect(svg).toContain('+29.67% weekly return')
    expect(svg.match(/\+29\.67%/g)).toHaveLength(1) // never duplicate weekly return
    expect(svg).toContain('PROMOTED')
    expect(svg).toContain('ACTIVE FX SESSIONS')
    expect(svg).toContain('4 / 5')
    expect(svg).not.toContain('TIER STANDING')

    const unrankedSvg = buildShareCardSvg({
      kind: 'division',
      traderLabel: '@pavilion6018',
      account,
      networkName: 'Arbitrum Sepolia',
      tier: null,
      rank: 1,
      weeklyReturnLabel: '+28.35%',
      activeDays: 2,
      weekLabel: 'Week of 22 Nov 2026',
      provisional: true,
    })
    expect(unrankedSvg).not.toContain('BADGE PENDING')
    expect(unrankedSvg).not.toContain('LIVE DIVISION')
    expect(unrankedSvg).not.toContain('stroke-dasharray')
    expect(unrankedSvg).toContain('SCORING CUTOFF')
    expect(unrankedSvg).toContain('Fri 21:00 UTC')
    expect(unrankedSvg).toContain('Pending Monday')
  })

  it('renders an Open Position flex card with ROE and escapes XML characters in trader labels', () => {
    const svg = buildShareCardSvg({
      kind: 'position',
      traderLabel: '<alpha & beta>',
      account,
      networkName: 'Arbitrum One',
      releaseVersion: 'v2.0.0',
      market: 'plDXY-PERP',
      side: 'short',
      size: '250000000000000000000',
      entryPrice: '103250000000000000000',
      marginAssets: '200000000',
      unrealizedPnlAssets: '50000000',
      liquidatable: false,
    })
    expect(svg).toContain('OPEN SHORT')
    expect(svg).toContain('+50.00 USDG')
    expect(svg).toContain('+25.00% ROE')
    expect(svg).toContain('&lt;alpha &amp; beta&gt;')
    expect(escapeXml('"test"')).toBe('&quot;test&quot;')
  })

  it('builds valid filenames and X share intent URLs', () => {
    const data = {
      kind: 'division' as const,
      traderLabel: '@alice_fx',
      account,
      networkName: 'Arbitrum Sepolia',
      tier: 'Diamond' as const,
      rank: 2,
      weeklyReturnLabel: '+18.40%',
      activeDays: 5,
      weekLabel: 'Live Week',
      provisional: true,
    }
    expect(shareCardFilename(data)).toBe('plether-division-alice_fx.png')
    const url = buildShareTweetUrl(data, `https://insights.plether.com/wallets/${account}`)
    expect(url).toContain('https://x.com/intent/tweet?')
    expect(url).toContain('Rank+%232')
    expect(url).toContain('Diamond+Division')
  })

  it('renders a Trading Account overview card with life-long balance chart distinct from Divisions cards', () => {
    const svg = buildShareCardSvg({
      kind: 'account',
      traderLabel: '@mnitarik',
      account,
      networkName: 'Arbitrum Sepolia',
      releaseVersion: 'v2.0.0',
      accountBalanceAssets: '146935680000',
      realizedPnlAssets: '71572430000',
      volumeAssets: '731020000000',
      totalTrades: 17,
      activeDays: 3,
      firstActivityAt: '2026-11-18T10:00:00Z',
      tier: 'Silver',
      balancePoints: [
        { timestamp: 1794996000, balanceAssets: '100000000000' },
        { timestamp: 1795200000, balanceAssets: '122000000000' },
        { timestamp: 1795690000, balanceAssets: '146935680000' },
      ],
    })
    expect(svg).toContain('ALL-TIME REALIZED PNL')
    expect(svg).toContain('LIFE-LONG BALANCE')
    expect(svg).toContain('+71,572.43 USDG')
    expect(svg).toContain('+46.94%')
    expect(svg).toContain('100.00K → 146.94K USDG')
    expect(svg).toContain('146.94K USDG')
    expect(svg).not.toContain('SILVER DIVISION')
    expect(svg).not.toContain('$')
  })

  it('renders a Position Lifecycle Audit share card with MFE/MAE, max leverage, and X intent URL', () => {
    const lifecycleData = {
      kind: 'position_lifecycle' as const,
      traderLabel: '@alice_fx',
      account,
      networkName: 'Arbitrum Sepolia',
      releaseVersion: 'v2.0.0',
      positionId: 'pos-0x1111:0',
      market: 'plDXY-PERP',
      side: 'long' as const,
      status: 'closed' as const,
      netPnlAssets: '1250400000',
      roiBps: 1840,
      entryPrice: '104200000000000000000',
      exitOrMarkPrice: '106500000000000000000',
      peakNotionalAssets: '50000000000',
      maxLeverageLabel: '7.4x',
      mfePnlAssets: '1500000000',
      maePnlAssets: '-210000000',
      liqBufferLabel: '14.2%',
      durationLabel: '3h 42m',
    }
    const svg = buildShareCardSvg(lifecycleData)
    expect(svg).toContain(`width="${String(SHARE_CARD_WIDTH)}"`)
    expect(svg).toContain(`height="${String(SHARE_CARD_HEIGHT)}"`)
    expect(svg).toContain('CLOSED LONG')
    expect(svg).toContain('POSITION LIFECYCLE NET PNL')
    expect(svg).toContain('+1,250.40 USDG')
    expect(svg).toContain('+18.40% ROI')
    expect(svg).toContain('7.4x')
    expect(svg).toContain('Held 3h 42m')
    expect(svg).toContain('pos-0x1111:0 · insights.plether.com')
    expect(svg).not.toContain('$')
    expect(shareCardFilename(lifecycleData)).toBe('plether-position_lifecycle-alice_fx.png')
    const url = buildShareTweetUrl(
      lifecycleData,
      `https://insights.plether.com/wallets/${account}/positions/pos-0x1111:0`,
    )
    expect(url).toContain('https://x.com/intent/tweet?')
    expect(url).toContain('Closed+LONG+plDXY-PERP+lifecycle')
    expect(url).toContain('7.4x+max+lev')
  })

  it('renders LP Pool Capital Efficiency share card, server OG card, and /og/liquidity.png route meta', () => {
    const effData = {
      kind: 'liquidity_efficiency' as const,
      networkName: 'Arbitrum Sepolia',
      releaseVersion: 'v2.0.0',
      poolTvlAssets: '266324039691594',
      seniorTvlAssets: '40098400866815',
      juniorTvlAssets: '226225638824779',
      volume24hAssets: '176322889629990',
      volume7dAssets: '1968245457410846',
      turnover24hX100: 66,
      turnover7dAvgDailyX100: 117,
      turnover30dAvgDailyX100: 104,
      effectiveTakeRate7dBpsX100: 1252,
      annualizedFeeYield7dBps: 5344,
      currentLongOiAssets: '119554364913940',
      currentShortOiAssets: '105460606838158',
      currentGrossOiAssets: '225014971752098',
      currentNetSkewAssets: '14093758075782',
      grossOiToPoolBps: 8449,
      netSkewToPoolBps: 529,
      turnoverSeriesX100: [72, 118, 165, 262, 109, 66],
      observedAt: '2026-11-28T14:21:14Z',
    }
    const svg = buildShareCardSvg(effData)
    expect(svg).toContain('LP POOL EFFICIENCY')
    expect(svg).toContain('GROSS OI 84.5% OF POOL')
    expect(svg).toContain('0.66×')
    expect(svg).toContain('1.17× / d')
    expect(svg).toContain('53.44% APR')
    expect(svg).toContain('12.52 bps')
    expect(svg).toContain('Plether LP House Pool')
    expect(svg).toContain('insights.plether.com/liquidity')
    expect(svg).not.toContain('$')
    expect(shareCardFilename(effData)).toBe('plether-liquidity-efficiency.png')
    const tweetUrl = buildShareTweetUrl(effData, 'https://insights.plether.com/liquidity')
    expect(tweetUrl).toContain('0.66x+24h+Vol%2FPool')
    expect(tweetUrl).toContain('84.5%25+OI%2FPool')

    const ogData = selectLiquidityEfficiencyOgCardData({
      data: {
        network: { chainId: 421614, releaseVersion: 'v2.0.0' },
        observedAt: effData.observedAt,
        summary: {
          poolTvlAssets: effData.poolTvlAssets,
          seniorTvlAssets: effData.seniorTvlAssets,
          juniorTvlAssets: effData.juniorTvlAssets,
          volume24hAssets: effData.volume24hAssets,
          volume7dAssets: effData.volume7dAssets,
          turnover24hX100: effData.turnover24hX100,
          turnover7dAvgDailyX100: effData.turnover7dAvgDailyX100,
          turnover30dAvgDailyX100: effData.turnover30dAvgDailyX100,
          effectiveTakeRate7dBpsX100: effData.effectiveTakeRate7dBpsX100,
          annualizedFeeYield7dBps: effData.annualizedFeeYield7dBps,
          currentLongOiAssets: effData.currentLongOiAssets,
          currentShortOiAssets: effData.currentShortOiAssets,
          currentGrossOiAssets: effData.currentGrossOiAssets,
          currentNetSkewAssets: effData.currentNetSkewAssets,
          grossOiToPoolBps: effData.grossOiToPoolBps,
          netSkewToPoolBps: effData.netSkewToPoolBps,
        },
        series7d: [{ annualizedTurnoverDailyX100: 165 }, { annualizedTurnoverDailyX100: 66 }],
      },
    })
    expect(ogData).not.toBeNull()
    const ogSvg = buildOgCardSvg(ogData)
    expect(ogSvg).toContain('LP POOL EFFICIENCY')
    expect(ogSvg).toContain('0.66×')
    expect(ogSvg).toContain('53.44% APR')
    expect(ogSvg).not.toContain('$')

    expect(parseOgImageRoute('/og/liquidity.png')).toEqual({ kind: 'liquidity' })
    const meta = resolveRouteOgMeta(new URL('https://insights.plether.com/liquidity'))
    expect(meta.imageUrl).toBe('https://insights.plether.com/og/liquidity.png')
  })
})
