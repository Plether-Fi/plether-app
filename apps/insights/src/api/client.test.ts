import { afterEach, describe, expect, it, vi } from 'vitest'
import {
  getCurrentCompetition,
  getCurrentProtocolRelease,
  getHousePool,
  getKeeper,
  getKeepers,
  getLeaderboard,
  getParameterChanges,
  getParameters,
  getProtocolOrder,
  getProtocolOverview,
  getProtocolTransaction,
  getProtocolTransactions,
  getProtocolWallet,
  getProtocolWallets,
  getStatus,
  getTranche,
  getTrancheHistory,
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

  it('uses every release-scoped protocol route and encodes path segments', async () => {
    const fetchMock = vi.fn().mockImplementation(
      () => Promise.resolve(new Response(JSON.stringify({}), { status: 200 })),
    )
    vi.stubGlobal('fetch', fetchMock)

    await getCurrentProtocolRelease()
    await getProtocolOverview('release / 1')
    await getProtocolTransaction('release / 1', '0xhash/value')
    await getProtocolOrder('release / 1', 'order / 9')
    await getHousePool('release / 1')
    await getTranche('release / 1', 'senior / one')
    await getTrancheHistory('release / 1', 'junior / one')
    await getKeepers('release / 1', '24h')
    await getKeeper('release / 1', '0xkeeper/value', '30d')
    await getProtocolWallets('release / 1', { window: '24h' })
    await getProtocolWallet('release / 1', '0xwallet/value', { window: '30d' })
    await getParameters('release / 1')
    await getParameterChanges('release / 1', 75)

    expect(fetchMock.mock.calls.map((call) => call[0])).toEqual([
      '/api/insights/v1/protocol/releases/current',
      '/api/insights/v1/protocol/releases/release%20%2F%201/overview',
      '/api/insights/v1/protocol/releases/release%20%2F%201/transactions/0xhash%2Fvalue',
      '/api/insights/v1/protocol/releases/release%20%2F%201/orders/order%20%2F%209',
      '/api/insights/v1/protocol/releases/release%20%2F%201/house-pool',
      '/api/insights/v1/protocol/releases/release%20%2F%201/tranches/senior%20%2F%20one',
      '/api/insights/v1/protocol/releases/release%20%2F%201/tranches/junior%20%2F%20one/history?limit=500',
      '/api/insights/v1/protocol/releases/release%20%2F%201/keepers?window=24h&limit=100',
      '/api/insights/v1/protocol/releases/release%20%2F%201/keepers/0xkeeper%2Fvalue?window=30d&limit=100',
      '/api/insights/v1/protocol/releases/release%20%2F%201/wallets?window=24h&limit=100',
      '/api/insights/v1/protocol/releases/release%20%2F%201/wallets/0xwallet%2Fvalue?window=30d&limit=100',
      '/api/insights/v1/protocol/releases/release%20%2F%201/parameters',
      '/api/insights/v1/protocol/releases/release%20%2F%201/parameter-changes?limit=75',
    ])
  })

  it('forwards opaque cursors for tranche, keeper, and parameter-change pages', async () => {
    const fetchMock = vi.fn().mockImplementation(
      () => Promise.resolve(new Response(JSON.stringify({}), { status: 200 })),
    )
    vi.stubGlobal('fetch', fetchMock)

    await getTrancheHistory('release-1', 'senior', {
      limit: 25,
      cursor: 'pc2 tranche/cursor',
    })
    await getKeeper('release-1', '0xkeeper', {
      window: '24h',
      limit: 40,
      cursor: 'pc2 keeper/cursor',
    })
    await getKeepers('release-1', {
      window: '30d',
      limit: 20,
      cursor: 'pc2 keepers/cursor',
    })
    await getProtocolWallets('release-1', {
      window: '24h',
      limit: 20,
      cursor: 'pc2 wallets/cursor',
    })
    await getProtocolWallet('release-1', '0xwallet', {
      window: '30d',
      limit: 40,
      cursor: 'pc2 wallet/cursor',
    })
    await getParameterChanges('release-1', {
      limit: 75,
      cursor: 'pc2 governance/cursor',
    })

    expect(fetchMock.mock.calls.map((call) => call[0])).toEqual([
      '/api/insights/v1/protocol/releases/release-1/tranches/senior/history?limit=25&cursor=pc2+tranche%2Fcursor',
      '/api/insights/v1/protocol/releases/release-1/keepers/0xkeeper?window=24h&limit=40&cursor=pc2+keeper%2Fcursor',
      '/api/insights/v1/protocol/releases/release-1/keepers?window=30d&limit=20&cursor=pc2+keepers%2Fcursor',
      '/api/insights/v1/protocol/releases/release-1/wallets?window=24h&limit=20&cursor=pc2+wallets%2Fcursor',
      '/api/insights/v1/protocol/releases/release-1/wallets/0xwallet?window=30d&limit=40&cursor=pc2+wallet%2Fcursor',
      '/api/insights/v1/protocol/releases/release-1/parameter-changes?limit=75&cursor=pc2+governance%2Fcursor',
    ])
  })

  it('normalizes operational wallet liveness telemetry without inventing unavailable identities', async () => {
    const address = '0x1111111111111111111111111111111111111111'
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({
      releaseId: 'release-1',
      chainId: '421614',
      confirmedBlock: {
        number: '123',
        hash: `0x${'1'.repeat(64)}`,
        timestamp: 1_785_000_000,
      },
      indexerTimestamp: 1_785_000_010,
      calculationVersion: 'protocol-transparency-v1',
      evidence: { wallets: { level: 'mixed' } },
      availability: [{
        field: 'wallets.oracleUpdater',
        reason: 'oracle_updater_identity_not_published_by_current_release',
      }],
      wallets: {
        window: '24h',
        windowStart: 1_784_913_600,
        windowEnd: 1_785_000_000,
        definition: 'Public release-scoped operational wallets.',
        wallets: [{
          address,
          roles: ['governance_executor'],
          roleSources: [{ role: 'governance_executor', source: 'release_manifest' }],
          status: 'critical',
          nativeBalanceWei: '9000000000000000',
          observedGasCostWei: '3000000000000000',
          observedTransactionNativeValueWei: '1000000000000000',
          observedActionCount: '3',
          observedTransactionCount: '2',
          medianObservedSuccessfulActionNativeOutlayWei: '1000000000000000',
          estimatedActionsRemaining: '9',
          runwayFormula: {
            formulaIdentifier: 'native_balance_div_median_outlay_v1',
            calculationVersion: 'protocol-transparency-v1',
            expression: 'balance / median outlay',
            sampleCount: '2',
          },
          lastActivityTimestamp: 1_784_999_900,
          evidence: { level: 'derived' },
          availability: [],
        }],
        nextCursor: null,
        units: { nativeBalanceWei: 'wei' },
      },
    })))

    const response = await getProtocolWallets('release-1', { window: '24h' })
    expect(response.wallets).toMatchObject({
      window: '24h',
      windowStart: 1_784_913_600,
      oracleUpdaterIdentityAvailable: null,
      items: [{
        address,
        roles: ['governance_executor'],
        status: 'critical',
        nativeBalanceWei: '9000000000000000',
        observedGasCostWei: '3000000000000000',
        observedTransactionNativeValueWei: '1000000000000000',
        observedTransactionCount: '2',
        estimatedTransactionsAtObservedGrossSpend: '9',
        medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei: '1000000000000000',
        runwayFormula: {
          formulaIdentifier: 'native_balance_div_median_outlay_v1',
          sampleCount: '2',
        },
      }],
    })
    expect(response.availability).toContainEqual({
      field: 'wallets.oracleUpdater',
      reason: 'oracle_updater_identity_not_published_by_current_release',
    })
  })

  it('flattens operational wallet detail while preserving raw action and receipt evidence', async () => {
    const address = '0x1111111111111111111111111111111111111111'
    const transactionHash = `0x${'a'.repeat(64)}`
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(Response.json({
      releaseId: 'release-1',
      chainId: '421614',
      confirmedBlock: {
        number: '123',
        hash: `0x${'1'.repeat(64)}`,
        timestamp: 1_785_000_000,
      },
      indexerTimestamp: 1_785_000_010,
      calculationVersion: 'protocol-transparency-v1',
      evidence: { wallet: 'mixed_exact_and_derived' },
      availability: [],
      wallet: {
        address,
        roles: ['oracle_updater'],
        roleSources: [{ role: 'oracle_updater', source: 'release_manifest_public_registry' }],
        status: 'warning',
        balances: { nativeBalanceWei: '90000000000000000' },
        activitySummary: {
          observedActionCount: '4',
          observedTransactionCount: '3',
          observedGasCostWei: '3000000000000000',
          observedTransactionNativeValueWei: '1000000000000000',
          lastActivityTimestamp: 1_784_999_900,
        },
        runway: {
          estimatedTransactionsAtObservedGrossSpend: '90',
          medianObservedSuccessfulOperationalTransactionGrossNativeSpendWei: '1000000000000000',
          formulaIdentifier: 'operational_wallet.available_native_gross_spend.v1',
          calculationVersion: 'operational-wallet-gross-spend-v1',
          estimateKind: 'conservative_observed_gross_spend_diagnostic',
          expression: 'floor(balance / median gross spend)',
          sampleCount: '3',
        },
        actions: [{
          actionId: 'action-1',
          transactionHash,
          timestamp: 1_784_999_900,
          actionType: 'mark_update',
          outcome: 'success',
          gasCostWei: '900000000000000',
          transactionNativeValueWei: null,
          evidence: { level: 'partial' },
          transactionEvidence: { level: 'exact_receipt' },
          availability: [],
          transactionAvailability: [{
            field: 'transaction.nativeValueWei',
            reason: 'transaction_native_value_unavailable',
          }],
        }],
        nextCursor: null,
        evidence: { runway: { level: 'derived' } },
        availability: [],
      },
    })))

    const response = await getProtocolWallet('release-1', address, { window: '7d' })
    expect(response.wallet).toMatchObject({
      address,
      roles: ['oracle_updater'],
      nativeBalanceWei: '90000000000000000',
      observedActionCount: '4',
      observedTransactionCount: '3',
      estimatedTransactionsAtObservedGrossSpend: '90',
      lastActivityTimestamp: 1_784_999_900,
      lastActivityTransactionHash: transactionHash,
      activity: [{
        activityId: 'action-1',
        transactionHash,
        gasCostWei: '900000000000000',
        nativeValueWei: null,
        evidence: {
          action: { level: 'partial' },
          transaction: { level: 'exact_receipt' },
        },
        availability: [{
          field: 'transaction.nativeValueWei',
          reason: 'transaction_native_value_unavailable',
        }],
      }],
    })
  })

  it('encodes transaction filters and forwards pagination', async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      new Response(JSON.stringify({}), { status: 200 }),
    )
    vi.stubGlobal('fetch', fetchMock)

    await getProtocolTransactions('release-1', {
      actionType: 'order execution',
      outcome: 'success',
      address: '0xparticipant',
      account: '0xaccount',
      keeper: '0xkeeper',
      contract: '0xcontract',
      transactionHash: '0xhash',
      from: '2026-07-01',
      to: '2026-07-31',
      limit: 25,
      cursor: 'v1.block/log',
    })

    expect(fetchMock).toHaveBeenCalledWith(
      '/api/insights/v1/protocol/releases/release-1/transactions?limit=25&actionType=order+execution&outcome=success&address=0xparticipant&account=0xaccount&keeper=0xkeeper&contract=0xcontract&transactionHash=0xhash&from=2026-07-01&to=2026-07-31&cursor=v1.block%2Flog',
      expect.objectContaining({ headers: { Accept: 'application/json' } }),
    )
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
