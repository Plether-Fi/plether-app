import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import type { OracleUpdateDetailResponse } from '../../../insights/src/api/oracle'
import { OracleUpdateDetailPage } from '../../../insights/src/pages/OracleUpdateDetailPage'
import './InsightsPositionBreakdown.css'

const ADMITTED_LIVE_UPDATE: OracleUpdateDetailResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-27T21:14:10Z',
  update: {
    id: '48219',
    source: 'backend_hermes_latest_v2',
    status: 'admitted',
    minPublishTime: '2026-11-27T21:14:02Z',
    maxPublishTime: '2026-11-27T21:14:03Z',
    fetchedAt: '2026-11-27T21:14:04Z',
    spreadSeconds: 1,
    lagSeconds: 2,
    vaaCount: 6,
    basketPrice: '94589000',
    displayPriceCap: '200000000',
    reconstructedBasketPrice: '94589000',
    reconstructionDiffRaw: 0,
    previousBasketPrice: '94631000',
    displayDeltaBps: 4,
    maxComponentConfidenceBps: 5,
    totalTargetWeightBps: 10000,
    totalEffectiveShareBps: 10000,
  },
  navigation: {
    previousId: '48218',
    nextId: '48220',
    previousMaxPublishTime: '2026-11-27T21:13:58Z',
    nextMaxPublishTime: '2026-11-27T21:14:07Z',
  },
  thresholds: {
    maxComponentDivergenceSeconds: 5,
    orderStalenessLimitSeconds: 15,
    liquidationStalenessLimitSeconds: 30,
    maxConfidenceRatioBps: 50,
    adverseConfidenceMultiplierBps: 15000,
    displayPriceCap: '200000000',
  },
  admissionChecks: [
    {
      key: 'six_feed_signature',
      label: '6-feed Pyth signature & completeness',
      status: 'passed',
      threshold: '6/6 FX feeds signed',
      currentValue: '6/6 feeds',
      rejections24h: 0,
      description: 'All 6 DXY FX feeds verified and persisted together as an atomic basket payload',
    },
    {
      key: 'publish_time_divergence',
      label: 'Component publish-time divergence',
      status: 'passed',
      threshold: '≤ 5s spread',
      currentValue: '1s',
      rejections24h: 0,
      description: 'max(publishTime) - min(publishTime) across the 6 signed FX feeds in this update',
    },
    {
      key: 'ingestion_lag',
      label: 'Hermes ingestion lag at admission',
      status: 'passed',
      threshold: '≤ 15s order bound',
      currentValue: '2s',
      rejections24h: 0,
      description: 'Elapsed seconds between earliest component publishTime and backend ingestion',
    },
    {
      key: 'confidence_ratio',
      label: 'Peak component confidence ratio',
      status: 'passed',
      threshold: '≤ 50 bps',
      currentValue: '5 bps',
      rejections24h: 0,
      description: 'Largest Pyth confidence-to-price ratio among the 6 FX components',
    },
    {
      key: 'display_domain',
      label: 'Display-domain & positivity bounds',
      status: 'passed',
      threshold: '0 < raw < 2.00',
      currentValue: 'Valid',
      rejections24h: 0,
      description: 'Verifies raw basket index is strictly within the immutable v1 display transform domain',
    },
    {
      key: 'basket_reconstruction',
      label: 'Weighted sum reconstruction',
      status: 'passed',
      threshold: 'Σ contributions = raw basket',
      currentValue: 'Exact match',
      rejections24h: 0,
      description: 'Recomputes Σ (price × weight / basePrice) across all 6 components against stored basketPrice',
    },
  ],
  components: [
    {
      symbol: 'EUR/USD',
      feedSymbol: 'EUR/USD',
      feedId: '0xa995d00bb36a63cef7fd2c287dc105fc8f3d93779f062f09551b0af3e81ec30b',
      price: '108620000',
      rawPrice: '108620000',
      confidence: '42000',
      confidenceBps: 4,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T21:14:02Z',
      driftFromMinSeconds: 0,
      weightBps: 5760,
      basePrice: '117500000',
      contribution: '53247080',
      effectiveShareBps: 5629,
      changeFromBaseBps: -756,
      inverted: false,
    },
    {
      symbol: 'JPY/USD',
      feedSymbol: 'USD/JPY',
      feedId: '0xef2c98c804ba503c6a707e38be4dfbb16683775f195b091252bf24693042fd52',
      price: '653594',
      rawPrice: '15300000000',
      confidence: '3800000',
      confidenceBps: 2,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T21:14:03Z',
      driftFromMinSeconds: 1,
      weightBps: 1360,
      basePrice: '638000',
      contribution: '13932411',
      effectiveShareBps: 1473,
      changeFromBaseBps: 244,
      inverted: true,
    },
    {
      symbol: 'GBP/USD',
      feedSymbol: 'GBP/USD',
      feedId: '0x84c2dde9633d93d1bcad84e7dc41c9d56578b7ec52fabedc1f335d673df0a7c1',
      price: '126950000',
      rawPrice: '126950000',
      confidence: '38000',
      confidenceBps: 3,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T21:14:02Z',
      driftFromMinSeconds: 0,
      weightBps: 1190,
      basePrice: '134480000',
      contribution: '11233677',
      effectiveShareBps: 1188,
      changeFromBaseBps: -560,
      inverted: false,
    },
    {
      symbol: 'CAD/USD',
      feedSymbol: 'USD/CAD',
      feedId: '0x3112b03a41c910ed446852aacf67118cb1bec67b2cd0b9a214c58cc0eaa2ecca',
      price: '71326676',
      rawPrice: '140200000',
      confidence: '42000',
      confidenceBps: 3,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T21:14:03Z',
      driftFromMinSeconds: 1,
      weightBps: 910,
      basePrice: '72880000',
      contribution: '8906047',
      effectiveShareBps: 942,
      changeFromBaseBps: -213,
      inverted: true,
    },
    {
      symbol: 'SEK/USD',
      feedSymbol: 'USD/SEK',
      feedId: '0x8ccb376aa871517e807358d4e3cf0bc7fe4950474dbe6c9ffc21ef64e43fc676',
      price: '9756097',
      rawPrice: '1025000000',
      confidence: '510000',
      confidenceBps: 5,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T21:14:03Z',
      driftFromMinSeconds: 1,
      weightBps: 420,
      basePrice: '10860000',
      contribution: '3773076',
      effectiveShareBps: 399,
      changeFromBaseBps: -1016,
      inverted: true,
    },
    {
      symbol: 'CHF/USD',
      feedSymbol: 'USD/CHF',
      feedId: '0x0b1e3297e69f162877b577b0d6a47a0d63b2392bc8499e6540da4187a63e28f8',
      price: '122477464',
      rawPrice: '81647673',
      confidence: '32000',
      confidenceBps: 4,
      confidenceExceeded: false,
      exponent: -8,
      publishTime: '2026-11-27T21:14:02Z',
      driftFromMinSeconds: 0,
      weightBps: 360,
      basePrice: '126100000',
      contribution: '3496709',
      effectiveShareBps: 370,
      changeFromBaseBps: -287,
      inverted: true,
    },
  ],
  settledOrders: [
    {
      orderId: '9042',
      account: '0x1111111111111111111111111111111111111111',
      side: 'bull',
      status: 'Executed',
      txHash: '0x590a41c8e2d9f7b3a1c5e7d9f2b4a6c8e0d2f4b6a8c0e2d4f6b8a0c2e4d64d4b',
      executionPrice: '94585000',
      executionOraclePrice: '94589000',
      commitTimestamp: '2026-11-27T21:14:01Z',
      terminalTimestamp: '2026-11-27T21:14:05Z',
    },
    {
      orderId: '9043',
      account: '0x2222222222222222222222222222222222222222',
      side: 'bear',
      status: 'Executed',
      txHash: '0x590a41c8e2d9f7b3a1c5e7d9f2b4a6c8e0d2f4b6a8c0e2d4f6b8a0c2e4d64d4b',
      executionPrice: '94593000',
      executionOraclePrice: '94589000',
      commitTimestamp: '2026-11-27T21:14:02Z',
      terminalTimestamp: '2026-11-27T21:14:05Z',
    },
    {
      orderId: '9044',
      account: '0x3333333333333333333333333333333333333333',
      side: 'bull',
      status: 'Expired',
      txHash: '0x770a41c8e2d9f7b3a1c5e7d9f2b4a6c8e0d2f4b6a8c0e2d4f6b8a0c2e4d69910',
      executionPrice: null,
      executionOraclePrice: '94589000',
      commitTimestamp: '2026-11-27T21:13:55Z',
      terminalTimestamp: '2026-11-27T21:14:05Z',
    },
  ],
}

const HIGH_SPREAD_WARNING_UPDATE: OracleUpdateDetailResponse = {
  ...ADMITTED_LIVE_UPDATE,
  update: {
    ...ADMITTED_LIVE_UPDATE.update,
    id: '48104',
    source: 'backend_hermes_reveal_v2',
    status: 'warning',
    minPublishTime: '2026-11-27T19:45:00Z',
    maxPublishTime: '2026-11-27T19:45:04Z',
    fetchedAt: '2026-11-27T19:45:07Z',
    spreadSeconds: 4,
    lagSeconds: 7,
    basketPrice: '95124000',
    reconstructedBasketPrice: '95124002',
    reconstructionDiffRaw: 2,
    previousBasketPrice: '94980000',
    displayDeltaBps: -14,
    maxComponentConfidenceBps: 42,
  },
  navigation: {
    previousId: '48103',
    nextId: null,
    previousMaxPublishTime: '2026-11-27T19:44:56Z',
    nextMaxPublishTime: null,
  },
  admissionChecks: ADMITTED_LIVE_UPDATE.admissionChecks.map((check) => {
    if (check.key === 'publish_time_divergence') {
      return { ...check, status: 'warning', currentValue: '4s' }
    }
    if (check.key === 'confidence_ratio') {
      return { ...check, status: 'warning', currentValue: '42 bps' }
    }
    if (check.key === 'basket_reconstruction') {
      return { ...check, currentValue: '±2 raw units' }
    }
    return check
  }),
  components: ADMITTED_LIVE_UPDATE.components.map((comp) =>
    comp.symbol === 'SEK/USD'
      ? {
          ...comp,
          confidenceBps: 42,
          driftFromMinSeconds: 4,
          publishTime: '2026-11-27T19:45:04Z',
        }
      : comp,
  ),
  settledOrders: [],
}

function OracleUpdateHarness({ data }: { data: OracleUpdateDetailResponse }) {
  const queryClient = useMemo(() => {
    const client = new QueryClient({
      defaultOptions: {
        queries: {
          enabled: false,
          retry: false,
          staleTime: Infinity,
          gcTime: Infinity,
        },
      },
    })
    client.setQueryData(['insights', 'status'], {
      network: data.network,
      observedAt: data.observedAt,
    })
    client.setQueryData(['insights', 'divisions', 'live'], null)
    client.setQueryData(['insights', 'oracle', 'update', data.update.id], data)
    return client
  }, [data])

  return (
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={[`/oracle/updates/${data.update.id}`]}>
        <div className="min-h-screen bg-[#0d0a0c] p-4 text-[#f5efe9] sm:p-8">
          <div className="mx-auto max-w-6xl">
            <Routes>
              <Route path="/oracle/updates/:id" element={<OracleUpdateDetailPage />} />
            </Routes>
          </div>
        </div>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta<typeof OracleUpdateHarness> = {
  title: 'Insights/Oracle — Price Update Detail',
  component: OracleUpdateHarness,
  parameters: {
    layout: 'fullscreen',
    backgrounds: { default: 'dark' },
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const AdmittedLiveUpdate: Story = {
  args: {
    data: ADMITTED_LIVE_UPDATE,
  },
}

export const HighSpreadWarningUpdate: Story = {
  args: {
    data: HIGH_SPREAD_WARNING_UPDATE,
  },
}
