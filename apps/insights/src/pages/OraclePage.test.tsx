import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { afterEach, describe, expect, it, vi } from 'vitest'
import type { OracleHealthResponse } from '../api'
import { OraclePage } from './OraclePage'

const MOCK_ORACLE: OracleHealthResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-27T09:00:00Z',
  overallStatus: 'healthy',
  regime: 'Live',
  live: {
    basketPrice: '94756243',
    onChainMarkPrice: '94756243',
    onChainMarkTime: '2026-11-27T08:59:58Z',
    latestPublishTime: '2026-11-27T08:59:58Z',
    minPublishTime: '2026-11-27T08:59:57Z',
    fetchedAt: '2026-11-27T08:59:59Z',
    source: 'backend_hermes_latest_v2',
    ageSeconds: 2,
    spreadSeconds: 1,
    maxComponentConfidenceBps: 6,
    oracleFrozen: false,
    fadWindow: false,
    tradingActive: true,
    openPolicy: {
      closeOnly: false,
      oracleFrozen: false,
      isFadWindow: false,
      maxStalenessSeconds: 15,
      requireStoredMark: false,
      allowAnyStoredMark: false,
    },
    closePolicy: {
      closeOnly: false,
      oracleFrozen: false,
      isFadWindow: false,
      maxStalenessSeconds: 15,
      requireStoredMark: false,
      allowAnyStoredMark: false,
    },
  },
  thresholds: {
    maxComponentDivergenceSeconds: 5,
    orderStalenessLimitSeconds: 15,
    liquidationStalenessLimitSeconds: 30,
    maxConfidenceRatioBps: 50,
    adverseConfidenceMultiplierBps: 10000,
    displayPriceCap: '200000000',
  },
  summary24h: {
    admittedUpdates: 14280,
    latestAdmittedUpdates: 12100,
    revealAdmittedUpdates: 2180,
    rejectedUpdates: 3,
    staleMarketClosedPolls: 0,
    admissionRateBps: 9998,
    maxSpreadSeconds: 4,
    maxLagSeconds: 5,
  },
  windows: {
    window1h: {
      windowSeconds: 3600,
      bucketSeconds: 120,
      timeline: [
        {
          bucketStart: '2026-11-27T08:56:00Z',
          bucketEnd: '2026-11-27T08:58:00Z',
          status: 'healthy',
          admittedCount: 24,
          latestCount: 20,
          revealCount: 4,
          rejectedCount: 0,
          staleCount: 0,
          highSpreadCount: 0,
          maxSpreadSeconds: 1,
          maxLagSeconds: 2,
        },
        {
          bucketStart: '2026-11-27T08:58:00Z',
          bucketEnd: '2026-11-27T09:00:00Z',
          status: 'rejected',
          admittedCount: 22,
          latestCount: 20,
          revealCount: 2,
          rejectedCount: 1,
          staleCount: 0,
          highSpreadCount: 1,
          maxSpreadSeconds: 4,
          maxLagSeconds: 5,
        },
      ],
    },
    window24h: {
      windowSeconds: 86400,
      bucketSeconds: 1800,
      timeline: [
        {
          bucketStart: '2026-11-27T08:00:00Z',
          bucketEnd: '2026-11-27T08:30:00Z',
          status: 'healthy',
          admittedCount: 310,
          latestCount: 280,
          revealCount: 30,
          rejectedCount: 0,
          staleCount: 0,
          highSpreadCount: 0,
          maxSpreadSeconds: 2,
          maxLagSeconds: 3,
        },
        {
          bucketStart: '2026-11-27T08:30:00Z',
          bucketEnd: '2026-11-27T09:00:00Z',
          status: 'rejected',
          admittedCount: 295,
          latestCount: 270,
          revealCount: 25,
          rejectedCount: 3,
          staleCount: 0,
          highSpreadCount: 2,
          maxSpreadSeconds: 4,
          maxLagSeconds: 5,
        },
      ],
    },
  },
  admissionChecks: [
    {
      key: 'six_feed_signature',
      label: '6-feed Pyth signature & completeness',
      status: 'passed',
      threshold: '6/6 FX feeds verified',
      currentValue: '6/6 feeds',
      rejections24h: 0,
      description: 'All 6 DXY FX feeds cryptographically verified via parsePythUpdateData',
    },
    {
      key: 'publish_time_divergence',
      label: 'Component publish-time divergence',
      status: 'passed',
      threshold: '≤ 5s spread',
      currentValue: '1s',
      rejections24h: 3,
      description: 'Rejects updates where max(publishTime) - min(publishTime) exceeds limit',
    },
  ],
  components: [
    {
      symbol: 'EUR/USD',
      feedSymbol: 'EUR/USD',
      feedId: '0xa995d00bb36a63cef7fd2c287dc105fc8f3d93779f062f09551b0af3e81ec30b',
      price: '117500000',
      rawPrice: '117500000',
      confidence: '70500',
      confidenceBps: 6,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T08:59:58Z',
      driftFromMinSeconds: 1,
      weightBps: 5760,
      inverted: false,
    },
  ],
  rejectionBreakdown: [
    {
      reasonCode: 'publish_time_divergence',
      outcome: 'rejected',
      count24h: 3,
      lastOccurredAt: '2026-11-27T08:59:12Z',
    },
  ],
  recentRejections: [
    {
      id: '91',
      occurredAt: '2026-11-27T08:59:12Z',
      route: 'latest',
      outcome: 'rejected',
      reasonCode: 'publish_time_divergence',
      minPublishTime: '2026-11-27T08:59:04Z',
      maxPublishTime: '2026-11-27T08:59:11Z',
      spreadSeconds: 7,
      ageSeconds: 8,
      detail: 'Component publish times exceeded 5s divergence policy',
    },
  ],
}

afterEach(() => {
  vi.restoreAllMocks()
})

describe('OraclePage', () => {
  it('renders chronological oracle health blocks, value-rejection checklist, FX components, and rejections table', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn((input: RequestInfo | URL) => {
        const url = typeof input === 'string' ? input : input instanceof URL ? input.toString() : input.url
        if (url.includes('/oracle/health')) {
          return Promise.resolve(
            new Response(JSON.stringify({ data: MOCK_ORACLE }), {
              status: 200,
              headers: { 'Content-Type': 'application/json' },
            }),
          )
        }
        if (url.includes('/market/fad-risk')) {
          return Promise.resolve(
            new Response(JSON.stringify({ error: { message: 'Not found' } }), {
              status: 404,
              headers: { 'Content-Type': 'application/json' },
            }),
          )
        }
        return Promise.resolve(
          new Response(JSON.stringify({ data: {} }), {
            status: 200,
            headers: { 'Content-Type': 'application/json' },
          }),
        )
      }),
    )

    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } })
    render(
      <QueryClientProvider client={client}>
        <MemoryRouter initialEntries={['/oracle']}>
          <OraclePage />
        </MemoryRouter>
      </QueryClientProvider>,
    )

    expect(await screen.findByText('Oracle Update Health Timeline')).toBeInTheDocument()
    expect(screen.getByText('Value-Rejection & Admission Guard Checklist')).toBeInTheDocument()
    expect(screen.getByText('6-feed Pyth signature & completeness')).toBeInTheDocument()
    expect(screen.getByText('Component publish-time divergence')).toBeInTheDocument()
    expect(screen.getAllByText('EUR/USD').length).toBeGreaterThanOrEqual(1)
    expect(screen.getByText('Component publish times exceeded 5s divergence policy')).toBeInTheDocument()

    // Toggle 1h window and verify slice options update
    fireEvent.click(screen.getByRole('button', { name: '1h' }))
    expect(screen.getByRole('listbox', { name: 'Oracle health 1h slices' })).toBeInTheDocument()
  })
})
