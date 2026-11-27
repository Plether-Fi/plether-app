import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { render, screen } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { describe, expect, it } from 'vitest'
import type { OracleUpdateDetailResponse } from '../api'
import { OracleUpdateDetailPage } from './OracleUpdateDetailPage'

const MOCK_DETAIL: OracleUpdateDetailResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v1.4.2',
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
      description: 'All 6 DXY FX feeds verified together',
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
  ],
  settledOrders: [
    {
      orderId: '9042',
      account: '0x1111111111111111111111111111111111111111',
      side: 'bull',
      status: 'Executed',
      txHash: '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
      executionPrice: '94587000',
      executionOraclePrice: '94589000',
      commitTimestamp: '2026-11-27T21:14:01Z',
      terminalTimestamp: '2026-11-27T21:14:05Z',
    },
  ],
}

describe('OracleUpdateDetailPage', () => {
  it('renders basket pie chart, component breakdown, and settled orders', () => {
    const queryClient = new QueryClient({
      defaultOptions: { queries: { enabled: false, retry: false } },
    })
    queryClient.setQueryData(['insights', 'oracle', 'update', '48219'], MOCK_DETAIL)

    render(
      <QueryClientProvider client={queryClient}>
        <MemoryRouter initialEntries={['/oracle/updates/48219']}>
          <Routes>
            <Route path="/oracle/updates/:id" element={<OracleUpdateDetailPage />} />
          </Routes>
        </MemoryRouter>
      </QueryClientProvider>,
    )

    expect(screen.getByRole('heading', { name: '1.05411 plDXY' })).toBeInTheDocument()
    expect(screen.getByText('DXY Currency Basket Composition')).toBeInTheDocument()
    expect(screen.getByText('Exact sum match')).toBeInTheDocument()
    expect(screen.getByText('#9042')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: '← Prev #48218' })).toHaveAttribute(
      'href',
      '/oracle/updates/48218',
    )
    expect(screen.getByRole('link', { name: 'Next #48220 →' })).toHaveAttribute(
      'href',
      '/oracle/updates/48220',
    )
  })
})
