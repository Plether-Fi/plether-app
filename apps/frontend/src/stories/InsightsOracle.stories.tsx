import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter } from 'react-router-dom'
import type { OracleHealthBucket, OracleHealthResponse } from '../../../insights/src/api/oracle'
import { OraclePage } from '../../../insights/src/pages/OraclePage'
import './InsightsPositionBreakdown.css'

function buildSampleTimeline(
  count: number,
  stepSeconds: number,
  rejectedIndices: number[],
  warnIndices: number[],
): OracleHealthBucket[] {
  const endMs = Date.parse('2026-11-27T09:00:00Z')
  const startMs = endMs - count * stepSeconds * 1000
  return Array.from({ length: count }, (_, i) => {
    const s = new Date(startMs + i * stepSeconds * 1000).toISOString()
    const e = new Date(startMs + (i + 1) * stepSeconds * 1000).toISOString()
    const rejected = rejectedIndices.includes(i) ? 2 : 0
    const warn = warnIndices.includes(i)
    return {
      bucketStart: s,
      bucketEnd: e,
      status: rejected > 0 ? 'rejected' : warn ? 'degraded' : 'healthy',
      admittedCount: stepSeconds === 120 ? 24 : 310,
      latestCount: stepSeconds === 120 ? 20 : 280,
      revealCount: stepSeconds === 120 ? 4 : 30,
      rejectedCount: rejected,
      staleCount: 0,
      highSpreadCount: warn ? 2 : 0,
      maxSpreadSeconds: rejected > 0 ? 5 : warn ? 4 : 1,
      maxLagSeconds: rejected > 0 ? 6 : 2,
    }
  })
}

const SAMPLE_ORACLE_HEALTH: OracleHealthResponse = {
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
    maxComponentConfidenceBps: 7,
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
    admittedUpdates: 14880,
    latestAdmittedUpdates: 13440,
    revealAdmittedUpdates: 1440,
    rejectedUpdates: 6,
    staleMarketClosedPolls: 0,
    admissionRateBps: 9996,
    maxSpreadSeconds: 5,
    maxLagSeconds: 6,
  },
  windows: {
    window1h: {
      windowSeconds: 3600,
      bucketSeconds: 120,
      timeline: buildSampleTimeline(30, 120, [21], [12, 22]),
    },
    window24h: {
      windowSeconds: 86400,
      bucketSeconds: 1800,
      timeline: buildSampleTimeline(48, 1800, [14, 31, 44], [15, 28, 45]),
    },
  },
  admissionChecks: [
    {
      key: 'six_feed_signature',
      label: '6-feed Pyth signature & completeness',
      status: 'passed',
      threshold: '6/6 FX feeds verified',
      currentValue: '6/6 feeds',
      rejections24h: 1,
      description: 'All 6 DXY FX feeds cryptographically verified via parsePythUpdateData',
    },
    {
      key: 'publish_time_divergence',
      label: 'Component publish-time divergence',
      status: 'passed',
      threshold: '≤ 5s spread',
      currentValue: '1s',
      rejections24h: 4,
      description: 'Rejects updates where max(publishTime) - min(publishTime) exceeds PletherOracle__PublishTimeDivergence',
    },
    {
      key: 'staleness_bound',
      label: 'Freshness / staleness guard',
      status: 'passed',
      threshold: '≤ 15s age',
      currentValue: '2s',
      rejections24h: 1,
      description: 'Rejects updates older than orderExecutionStalenessLimit (PletherOracle__StalePrice)',
    },
    {
      key: 'confidence_ratio',
      label: 'Basket confidence ratio',
      status: 'passed',
      threshold: '≤ 50 bps',
      currentValue: '7 bps',
      rejections24h: 0,
      description: 'Rejects wide Pyth FX uncertainty intervals (PletherOracle__BasketConfidenceTooWide)',
    },
    {
      key: 'display_domain',
      label: 'Display-domain & positivity bounds',
      status: 'passed',
      threshold: '0 < raw < 2.00',
      currentValue: 'Valid',
      rejections24h: 0,
      description: 'Rejects non-positive component prices or baskets outside the v1 display cap',
    },
    {
      key: 'oracle_regime',
      label: 'On-chain oracle execution regime',
      status: 'passed',
      threshold: 'Live (unfrozen)',
      currentValue: 'Live',
      rejections24h: 0,
      description: 'PletherOracle.getOrderExecutionPolicy(open/close) and PerpsPublicLens status',
    },
  ],
  components: [
    {
      symbol: 'EUR/USD',
      feedSymbol: 'EUR/USD',
      feedId: '0xa995d00bb36a63cef7fd2c287dc105fc8f3d93779f062f09551b0af3e81ec30b',
      price: '117500000',
      rawPrice: '117500000',
      confidence: '58750',
      confidenceBps: 5,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T08:59:58Z',
      driftFromMinSeconds: 1,
      weightBps: 5760,
      inverted: false,
    },
    {
      symbol: 'JPY/USD',
      feedSymbol: 'USD/JPY',
      feedId: '0xef2c98c804ba503c6a707e38be4dfbb16683775f195b091252bf24693042fd52',
      price: '638000',
      rawPrice: '15674000000',
      confidence: '10971800',
      confidenceBps: 7,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T08:59:58Z',
      driftFromMinSeconds: 1,
      weightBps: 1360,
      inverted: true,
    },
    {
      symbol: 'GBP/USD',
      feedSymbol: 'GBP/USD',
      feedId: '0x84c2dde9633d93d1bcad84e7dc41c9d56578b7ec52fabedc1f335d673df0a7c1',
      price: '134480000',
      rawPrice: '134480000',
      confidence: '53792',
      confidenceBps: 4,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T08:59:57Z',
      driftFromMinSeconds: 0,
      weightBps: 1190,
      inverted: false,
    },
    {
      symbol: 'CAD/USD',
      feedSymbol: 'USD/CAD',
      feedId: '0x3112b03a41c910ed446852aacf67118cb1bec67b2cd0b9a214c58cc0eaa2ecca',
      price: '72880000',
      rawPrice: '137211000',
      confidence: '68605',
      confidenceBps: 5,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T08:59:58Z',
      driftFromMinSeconds: 1,
      weightBps: 910,
      inverted: true,
    },
    {
      symbol: 'SEK/USD',
      feedSymbol: 'USD/SEK',
      feedId: '0x8ccb376aa871517e807358d4e3cf0bc7fe4950474dbe6c9ffc21ef64e43fc676',
      price: '10860000',
      rawPrice: '920810000',
      confidence: '552486',
      confidenceBps: 6,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T08:59:57Z',
      driftFromMinSeconds: 0,
      weightBps: 420,
      inverted: true,
    },
    {
      symbol: 'CHF/USD',
      feedSymbol: 'USD/CHF',
      feedId: '0x0b1e3297e69f162877b577b0d6a47a0d63b2392bc8499e6540da4187a63e28f8',
      price: '126100000',
      rawPrice: '79302000',
      confidence: '31720',
      confidenceBps: 4,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T08:59:58Z',
      driftFromMinSeconds: 1,
      weightBps: 360,
      inverted: true,
    },
  ],
  rejectionBreakdown: [
    {
      reasonCode: 'publish_time_divergence',
      outcome: 'rejected',
      count24h: 4,
      lastOccurredAt: '2026-11-27T08:42:15Z',
    },
    {
      reasonCode: 'stale_price',
      outcome: 'rejected',
      count24h: 1,
      lastOccurredAt: '2026-11-27T02:11:04Z',
    },
    {
      reasonCode: 'onchain_pyth_rejected',
      outcome: 'rejected',
      count24h: 1,
      lastOccurredAt: '2026-11-26T22:08:49Z',
    },
  ],
  recentRejections: [
    {
      id: '104',
      occurredAt: '2026-11-27T08:42:15Z',
      route: 'latest',
      outcome: 'rejected',
      reasonCode: 'publish_time_divergence',
      minPublishTime: '2026-11-27T08:42:07Z',
      maxPublishTime: '2026-11-27T08:42:14Z',
      spreadSeconds: 7,
      ageSeconds: 8,
      detail: 'Component publish times exceeded 5s divergence policy',
    },
    {
      id: '103',
      occurredAt: '2026-11-27T02:11:04Z',
      route: 'reveal',
      outcome: 'rejected',
      reasonCode: 'stale_price',
      minPublishTime: '2026-11-27T02:10:42Z',
      maxPublishTime: '2026-11-27T02:10:45Z',
      spreadSeconds: 3,
      ageSeconds: 22,
      detail: 'Pyth payload exceeded freshness staleness bound',
    },
  ],
}

function OracleStoryCanvas({ data }: { data: OracleHealthResponse }) {
  const client = useMemo(() => {
    const qc = new QueryClient({
      defaultOptions: {
        queries: {
          retry: false,
          staleTime: Number.POSITIVE_INFINITY,
          refetchInterval: false,
          refetchOnMount: false,
          refetchOnWindowFocus: false,
        },
      },
    })
    qc.setQueryData(['insights', 'oracle', 'health'], data)
    return qc
  }, [data])

  return (
    <QueryClientProvider client={client}>
      <MemoryRouter initialEntries={['/oracle']}>
        <div className="min-h-screen bg-app-bg p-6 text-content-primary">
          <div className="mx-auto max-w-7xl">
            <OraclePage />
          </div>
        </div>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta<typeof OracleStoryCanvas> = {
  title: 'Insights/Oracle — Admission & Value-Guard Health',
  component: OracleStoryCanvas,
  parameters: { layout: 'fullscreen' },
}

export default meta
type Story = StoryObj<typeof meta>

export const LiveWithRejectionBlocks: Story = {
  args: { data: SAMPLE_ORACLE_HEALTH },
}
