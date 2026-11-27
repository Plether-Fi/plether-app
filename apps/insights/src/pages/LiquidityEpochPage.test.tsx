import { render, screen } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { InsightsApiError, type LiquidityEpochResponse } from '../api'
import { LiquidityEpochPage } from './LiquidityEpochPage'

const epochQuery = vi.hoisted(() => vi.fn())
const protocolContext = vi.hoisted(() => vi.fn())

vi.mock('../api', async () => {
  const actual = await vi.importActual<typeof import('../api')>('../api')
  return {
    ...actual,
    useLiquidityEpoch: epochQuery,
  }
})

vi.mock('../hooks/useProtocolContext', () => ({
  useProtocolContext: protocolContext,
}))

const SAMPLE_EPOCH: LiquidityEpochResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  monitorAddress: '0x1111111111111111111111111111111111111111',
  epoch: 498776,
  epochTimestamp: 1795593600,
  epochUtc: '2026-11-26T00:00:00Z',
  previousEpoch: 498634,
  nextEpoch: 498777,
  latestObservation: {
    epoch: 498776,
    observedBlock: '332945100',
    observedAt: '2026-11-26T00:00:08Z',
    readiness: 'ready',
    executionPath: 'cached_mark',
    healthState: 'healthy',
    observationComplete: true,
    hasMaturedWork: true,
    paused: false,
    masks: {
      operationalBlocker: { raw: '0', activeBits: [] },
      warning: { raw: '0', activeBits: [] },
      dependencyFailure: { raw: '0', activeBits: [] },
      criticalFault: { raw: '0', activeBits: [] },
    },
  },
  observations: [],
  settlements: [
    {
      epoch: 498776,
      cutoffEpoch: '498775',
      status: 'confirmed_success',
      executionPath: 'cached_mark',
      txHash: `0x${'a'.repeat(64)}`,
      receiptBlock: '332945110',
      createdAt: '2026-11-26T00:00:09Z',
      confirmedAt: '2026-11-26T00:00:13Z',
      confirmationLatencySeconds: 4,
      replacementCount: 0,
      seniorDepositAssets: '25000000000',
      seniorRedeemAssets: '5000000000',
      juniorDepositAssets: '10000000000',
      juniorRedeemAssets: '0',
      seniorBacklog: false,
      juniorBacklog: false,
      entriesDeferred: false,
    },
  ],
  vaultSnapshot: {
    epochTimestamp: 1795593600,
    blockNumber: '332945095',
    blockTimestamp: 1795593598,
    blockUtc: '2026-11-25T23:59:58Z',
    markFresh: true,
    senior: {
      totalAssets: '450000000000',
      lockedAssets: '420000000000',
      totalSupply: '440000000000',
      sharePriceWad: '1022700000000000000',
      sharePriceDeltaBps: 12,
    },
    junior: {
      totalAssets: '180000000000',
      lockedAssets: '165000000000',
      totalSupply: '170000000000',
      sharePriceWad: '1058800000000000000',
      sharePriceDeltaBps: 45,
    },
  },
  coveringReport: {
    startBlockNumber: '332900000',
    endBlockNumber: '332950000',
    sourceCommit: 'abc1234',
    schemaVersion: 3,
    updatedAt: '2026-11-26T00:10:00Z',
    sourceBlockCount: 500,
    reportedReturnPp: 1.25,
    unexplainedPp: 0,
    netReturnUsdg: '1250000000',
    unexplainedUsdg: '0',
    liquidationFeesUsdg: '120000000',
    notesCount: 0,
  },
}

beforeEach(() => {
  protocolContext.mockReturnValue({
    chain: {
      chainId: 421614,
      name: 'Arbitrum Sepolia',
      shortName: 'Arb Sepolia',
      isTestnet: true,
      explorerBaseUrl: 'https://sepolia.arbiscan.io',
      tradeAppUrl: 'https://sepolia.plether.com',
      defaultAssetSymbol: 'USDG',
    },
    releaseVersion: 'v2.0.0',
    blockNumber: 332945110,
    observedAt: '2026-11-26T00:00:13Z',
    assetSymbol: 'USDG',
  })
  epochQuery.mockReturnValue({
    data: SAMPLE_EPOCH,
    isLoading: false,
    isError: false,
    error: null,
    refetch: vi.fn(),
  })
})

function renderAt(path: string) {
  return render(
    <MemoryRouter initialEntries={[path]}>
      <Routes>
        <Route path="/liquidity/epochs/:epoch" element={<LiquidityEpochPage />} />
      </Routes>
    </MemoryRouter>,
  )
}

describe('LiquidityEpochPage', () => {
  it('renders epoch header, prev/next navigation, Senior & Junior vault snapshots, settlement tx, bitmasks, and attribution', () => {
    renderAt('/liquidity/epochs/498776')

    expect(screen.getByRole('heading', { name: 'Epoch #498,776' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: /#498,634/ })).toHaveAttribute(
      'href',
      '/liquidity/epochs/498634',
    )
    expect(screen.getByRole('link', { name: /#498,777/ })).toHaveAttribute(
      'href',
      '/liquidity/epochs/498777',
    )

    expect(screen.getByText('1.0227 USDG')).toBeInTheDocument()
    expect(screen.getByText('1.0588 USDG')).toBeInTheDocument()
    expect(screen.getByText('+0.12% (1h)')).toBeInTheDocument()
    expect(screen.getByText('+0.45% (1h)')).toBeInTheDocument()

    expect(screen.getByRole('link', { name: '0xaaaa…aaaa' })).toHaveAttribute(
      'href',
      `/tx/0x${'a'.repeat(64)}`,
    )
    expect(screen.getByText('SettlementMonitorLens diagnostics')).toBeInTheDocument()
    expect(screen.getByText('Covering Junior return attribution audit')).toBeInTheDocument()
  })

  it('fails closed on 404 and invalid epoch identifiers', () => {
    epochQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: true,
      error: new InsightsApiError('Not found', 404),
      refetch: vi.fn(),
    })
    renderAt('/liquidity/epochs/999999')
    expect(screen.getByText('No telemetry found for Epoch #999999')).toBeInTheDocument()
  })

  it('rejects non-numeric epoch identifiers cleanly', () => {
    renderAt('/liquidity/epochs/not-a-number')
    expect(screen.getByText('Invalid LP epoch number')).toBeInTheDocument()
  })
})
