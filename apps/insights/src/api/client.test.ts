import { afterEach, describe, expect, it, vi } from 'vitest'
import { getCurrentCompetition, getLeaderboard, getStatus, getWallet, InsightsApiError } from './client'
import type { Competition } from './types'

const competition: Competition = {
  id: 'competition-1',
  slug: 'testnet-trading-2026',
  name: 'Testnet Trading Competition',
  status: 'live',
  startsAt: '2026-07-20T16:00:00Z',
  tradingCutoffAt: '2026-08-03T16:00:00Z',
  resultsAt: '2026-08-05T12:00:00Z',
  startingBalance: '100000000000',
  pnlEligibilityThreshold: '1000000000',
  minActiveDays: 5,
  prizes: [],
  latestIndexedBlock: 123,
  latestIndexedAt: '2026-07-20T12:00:00Z',
}

afterEach(() => vi.unstubAllGlobals())

describe('Insights API client', () => {
  it('normalizes the current competition envelope', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response(JSON.stringify({ competition }), { status: 200 })))
    await expect(getCurrentCompetition()).resolves.toEqual(competition)
  })

  it('does not fetch status when competition metrics are absent', async () => {
    const competitionWithoutMetrics = {
      id: competition.id,
      slug: competition.slug,
      name: competition.name,
      status: competition.status,
      startsAt: competition.startsAt,
      tradingCutoffAt: competition.tradingCutoffAt,
      resultsAt: competition.resultsAt,
      startingBalance: competition.startingBalance,
      pnlEligibilityThreshold: competition.pnlEligibilityThreshold,
      minActiveDays: competition.minActiveDays,
      prizes: competition.prizes,
    }
    const fetchMock = vi.fn().mockResolvedValue(
      new Response(JSON.stringify({ competition: competitionWithoutMetrics }), { status: 200 }),
    )
    vi.stubGlobal('fetch', fetchMock)

    await expect(getCurrentCompetition()).resolves.toEqual({
      ...competitionWithoutMetrics,
      latestIndexedBlock: null,
      latestIndexedAt: null,
    })
    expect(fetchMock).toHaveBeenCalledTimes(1)
  })

  it('normalizes participant metrics from status', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response(JSON.stringify({
      chainId: 421614,
      status: {
        healthy: true,
        participantCount: 358,
        eligibleCount: 42,
        indexedThroughBlock: '123',
        indexerUpdatedAt: '2026-07-20T12:00:00Z',
      },
    }), { status: 200 })))

    await expect(getStatus()).resolves.toEqual({
      healthy: true,
      latestIndexedBlock: 123,
      latestIndexedAt: '2026-07-20T12:00:00Z',
      chainId: 421614,
      participantCount: 358,
      eligibleCount: 42,
    })
  })

  it('encodes leaderboard pagination and search', async () => {
    const fetchMock = vi.fn().mockResolvedValue(new Response(JSON.stringify({ competition, standings: [], nextCursor: null, provisional: true }), { status: 200 }))
    vi.stubGlobal('fetch', fetchMock)
    await getLeaderboard(competition.slug, { limit: 25, cursor: 'next page', search: 'alice' })
    expect(fetchMock).toHaveBeenCalledWith(
      '/api/insights/v1/competitions/testnet-trading-2026/leaderboard?limit=25&cursor=next+page&search=alice',
      expect.objectContaining({ headers: { Accept: 'application/json' } }),
    )
  })

  it('exposes typed API errors', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response(JSON.stringify({ error: { code: 'not_found', message: 'Missing' } }), { status: 404 })))
    await expect(getCurrentCompetition()).rejects.toMatchObject<Partial<InsightsApiError>>({ status: 404, code: 'not_found', message: 'Missing' })
  })

  it('keeps raw position and activity size deltas while deriving exact USDC notionals', async () => {
    const sizeDelta = '123456789012345678901234'
    const price = '101234567'
    const expectedNotional = ((BigInt(sizeDelta) * BigInt(price)) / 100_000_000_000_000_000_000n).toString()
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response(JSON.stringify({
      competition,
      wallet: {
        rank: 1,
        wallet: '0x1111111111111111111111111111111111111111',
        alias: 'Alice',
        finalPnlUsdc: '1500000000',
        roiBps: 150,
        volumeUsdc: '1000000000',
        executedTrades: 5,
        activeDays: 5,
        liquidations: 0,
        prizePlace: 1,
        prizePlaces: [1],
        prizeAmountUsdc: '600000000',
        eligibilityStatus: 'eligible',
        prizeEligible: true,
        currentAccountValueUsdc: '101500000000',
        realizedPnlUsdc: '375000000',
        position: {
          market: 'plDXY Perp',
          side: 'long',
          sizeDelta,
          marginUsdc: '1000000000',
          entryPrice: price,
          unrealizedPnlUsdc: '250000000',
          liquidatable: false,
        },
      },
      activity: [{
        activityType: 'Open',
        occurredAt: '2026-07-20T12:00:00Z',
        side: 0,
        sizeDelta,
        price,
        txHash: '0xabc',
        logIndex: 7,
      }],
    }), { status: 200 })))

    const response = await getWallet(competition.slug, '0x1111111111111111111111111111111111111111')
    expect(response.wallet).toMatchObject({
      prizePlace: 1,
      prizePlaces: [1],
      prizeAmountUsdc: '600000000',
      realizedPnl: '375000000',
      position: {
        side: 'long',
        size: expectedNotional,
        sizeDelta,
        margin: '1000000000',
        entryPrice: '1.01234567',
        unrealizedPnl: '250000000',
      },
    })
    expect(response.activity?.[0]).toMatchObject({
      side: 'long',
      size: expectedNotional,
      sizeDelta,
      price: '1.01234567',
    })
  })

  it('reconstructs realized P&L from activity during a rolling backend deployment', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response(JSON.stringify({
      competition,
      wallet: {
        wallet: '0x1111111111111111111111111111111111111111',
        finalPnlUsdc: '-5000000',
        activeDays: 1,
        liquidations: 0,
      },
      activity: [
        { activityType: 'Close', occurredAt: '2026-07-20T12:00:00Z', pnlUsdc: '12000000' },
        { activityType: 'Liquidated', occurredAt: '2026-07-20T13:00:00Z', pnlUsdc: '-3000000' },
      ],
    }), { status: 200 })))

    const response = await getWallet(competition.slug, '0x1111111111111111111111111111111111111111')
    expect(response.wallet.realizedPnl).toBe('9000000')
  })
})
