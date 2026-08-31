import { afterEach, describe, expect, it, vi } from 'vitest'
import {
  completeRegistration,
  createRegistrationSession,
  createXAuthorization,
  getCurrentCompetition,
  getLeaderboard,
  getRegistrationSession,
  getStatus,
  getWallet,
  InsightsApiError,
} from './client'
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

  it('uses the code-defined September profit threshold over stale API metadata', async () => {
    const septemberCompetition = {
      ...competition,
      slug: 'testnet-trading-2026-09',
      pnlEligibilityThreshold: '1000000000',
    }
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response(JSON.stringify({ competition: septemberCompetition }), { status: 200 })))

    await expect(getCurrentCompetition()).resolves.toMatchObject({
      slug: septemberCompetition.slug,
      pnlEligibilityThreshold: '1000000',
    })
  })

  it('normalizes registration metadata for the current competition', async () => {
    const wireCompetition = {
      ...competition,
      fxSessionBoundaryUtc: '21:00',
      registration: {
        status: 'open' as const,
        opensAt: '2026-08-28T10:00:00Z',
        closesAt: '2026-09-20T21:00:00Z',
        minimumXAccountAgeDays: 90,
        targetXHandle: 'plether_fi',
        rulesVersion: '2026-09-01',
        privacyVersion: '2026-09-01',
      },
    }
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response(JSON.stringify({ competition: wireCompetition }), { status: 200 })))

    await expect(getCurrentCompetition()).resolves.toMatchObject({
      fxSessionBoundaryUtc: '21:00',
      registration: wireCompetition.registration,
    })
  })

  it('uses credentialed registration requests and the CSRF header', async () => {
    const registration = {
      status: 'in_progress' as const,
      csrfToken: 'csrf-token',
      expiresAt: '2026-08-28T12:00:00Z',
      steps: { xIdentity: 'pending' as const, xFollow: 'pending' as const, wallet: 'pending' as const, completed: false },
      requiredConsents: { rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1' },
    }
    const fetchMock = vi.fn()
      .mockResolvedValueOnce(new Response(JSON.stringify({ registration }), { status: 200 }))
      .mockResolvedValueOnce(new Response(JSON.stringify({ authorizationUrl: 'https://x.com/i/oauth2/authorize?state=test' }), { status: 200 }))
      .mockResolvedValueOnce(new Response(JSON.stringify({ registration }), { status: 200 }))
    vi.stubGlobal('fetch', fetchMock)

    await expect(getRegistrationSession(competition.slug)).resolves.toEqual(registration)
    await expect(createXAuthorization(competition.slug, registration.csrfToken)).resolves.toContain('https://x.com')
    await expect(completeRegistration(competition.slug, registration.csrfToken, 'rules-v1', 'privacy-v1')).resolves.toEqual(registration)

    expect(fetchMock).toHaveBeenNthCalledWith(1,
      `/api/insights/v1/competitions/${competition.slug}/registrations/session`,
      expect.objectContaining({ credentials: 'include', method: 'GET' }),
    )
    const authorizeInit = fetchMock.mock.calls[1]?.[1] as RequestInit
    expect(new Headers(authorizeInit.headers).get('X-Registration-CSRF')).toBe('csrf-token')
    expect(authorizeInit).toMatchObject({ credentials: 'include', method: 'POST' })
    const completeInit = fetchMock.mock.calls[2]?.[1] as RequestInit
    expect(JSON.parse(String(completeInit.body))).toEqual({
      acceptRules: true,
      acceptPrivacy: true,
      rulesVersion: 'rules-v1',
      privacyVersion: 'privacy-v1',
    })
  })

  it('creates registration sessions with a Turnstile token but no unavailable CSRF value', async () => {
    const registration = {
      status: 'in_progress' as const,
      csrfToken: 'issued-csrf',
      expiresAt: '2026-08-28T12:00:00Z',
      steps: { xIdentity: 'pending' as const, xFollow: 'pending' as const, wallet: 'pending' as const, completed: false },
      requiredConsents: { rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1' },
    }
    const fetchMock = vi.fn().mockResolvedValue(new Response(JSON.stringify({ registration }), { status: 200 }))
    vi.stubGlobal('fetch', fetchMock)

    await createRegistrationSession(competition.slug, 'turnstile-token')

    const requestInit = fetchMock.mock.calls[0]?.[1] as RequestInit
    expect(new Headers(requestInit.headers).has('X-Registration-CSRF')).toBe(false)
    expect(JSON.parse(String(requestInit.body))).toEqual({ turnstileToken: 'turnstile-token' })
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

  it('keeps raw basket accounting for notionals while displaying the plDXY price', async () => {
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
        executionFeeUsdc: '1765060537',
        vpiUsdc: '4854090357',
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
        entryPrice: '0.98765433',
        unrealizedPnl: '250000000',
      },
    })
    expect(response.activity?.[0]).toMatchObject({
      side: 'long',
      size: expectedNotional,
      sizeDelta,
      price: '0.98765433',
      executionFee: '1765060537',
      vpi: '4854090357',
    })
  })

  it('normalizes protocol-fee aliases and signed close VPI', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(new Response(JSON.stringify({
      competition,
      wallet: {
        wallet: '0x1111111111111111111111111111111111111111',
        finalPnlUsdc: '0',
        activeDays: 1,
        liquidations: 0,
      },
      activity: [{
        activityType: 'Close',
        occurredAt: '2026-07-20T12:00:00Z',
        protocolFeeUsdc: '11280147',
        vpiDeltaUsdc: '-30992947',
      }],
    }), { status: 200 })))

    const response = await getWallet(competition.slug, '0x1111111111111111111111111111111111111111')
    expect(response.activity?.[0]).toMatchObject({
      executionFee: '11280147',
      vpi: '-30992947',
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
