import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import {
  InsightsApiError,
  type SponsoredOperationsResponse,
  type SponsoredStatusResponse,
} from '../api'
import { resolvePerpsChain } from '../utils/chain'
import { SponsoredPage } from './SponsoredPage'

const protocolContext = vi.hoisted(() => vi.fn())
const statusQuery = vi.hoisted(() => vi.fn())
const opsQuery = vi.hoisted(() => vi.fn())

vi.mock('../hooks/useProtocolContext', () => ({ useProtocolContext: protocolContext }))
vi.mock('../api', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../api')>()),
  useSponsoredStatus: statusQuery,
  useSponsoredOperations: opsQuery,
}))

const sampleStatus: SponsoredStatusResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  observedAt: '2026-11-26T18:30:00Z',
  configured: true,
  stack: 'native',
  entryPointVersion: '0.8',
  entryPointAddress: '0x4337084d9e255ff0702461cf8895ce9e3b5ff108',
  paymasterAddress: '0x1111111111111111111111111111111111111111',
  gate: {
    status: 'issuing',
    sponsorshipEnabled: true,
    submissionEnabled: true,
    rolloutMode: 'global',
    issuancePaused: false,
    pausedReason: null,
    paymasterOnchainPaused: false,
    updatedAt: '2026-11-26T18:00:00Z',
  },
  paymaster: {
    entryPointDepositWei: '250000000000000000',
    minDepositWei: '50000000000000000',
    freeHeadroomWei: '248000000000000000',
    depositHealthy: true,
    burn24hWei: '12000000000000000',
    estimatedRunwayHours: 500,
    altoFundingState: 'ready',
    altoFundingReason: 'READY',
  },
  budgets: {
    maxCostPerOpWei: '5000000000000000',
    accountHourlyCapWei: '25000000000000000',
    globalOutstanding: { usedWei: '2000000000000000', capWei: '50000000000000000', utilizationBps: 400 },
    globalHourly: { usedWei: '3500000000000000', capWei: '100000000000000000', utilizationBps: 350 },
    globalDaily: { usedWei: '12000000000000000', capWei: '500000000000000000', utilizationBps: 240 },
  },
  reconciler: {
    safeBlock: '332900400',
    headBlock: '332900420',
    blocksBehind: 20,
    lastSuccessAt: '2026-11-26T18:29:55Z',
    lagSeconds: 5,
    maxSafeLagSeconds: 600,
    fresh: true,
  },
  bundler: {
    reachable: true,
    entryPointSupported: true,
    status: 'ready',
  },
  execution: {
    window1h: { totalOps: 14, succeededOps: 14, revertedOps: 0, successRateBps: 10000, gasSpentWei: '1400000000000000', gasEfficiencyBps: 3800 },
    window24h: { totalOps: 120, succeededOps: 118, revertedOps: 2, successRateBps: 9833, gasSpentWei: '12000000000000000', gasEfficiencyBps: 4100 },
    window7d: { totalOps: 810, succeededOps: 798, revertedOps: 12, successRateBps: 9851, gasSpentWei: '81000000000000000', gasEfficiencyBps: 4050 },
    latencyP50Seconds: 2,
    latencyP95Seconds: 6,
    funnel24h: { reserved: 1, signed: 0, submitted: 1, settled: 118, expired: 3, cancelled: 1 },
    activeUnfinalizedCount: 2,
    oldestUnfinalizedAgeSeconds: 18,
  },
  closeAssistance: {
    available: true,
    verifiedCount24h: 4,
    verifiedCount7d: 19,
    totalAssisted7dAssets: '3800000',
    pendingUnverifiedCount: 0,
    verificationRateBps: 10000,
  },
}

const sampleOps: SponsoredOperationsResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  observedAt: '2026-11-26T18:30:00Z',
  configured: true,
  stack: 'native',
  operations: [
    {
      userOpHash: `0x${'a'.repeat(64)}`,
      txHash: `0x${'b'.repeat(64)}`,
      sender: `0x${'2'.repeat(40)}`,
      blockNumber: '332900412',
      success: true,
      actualGasCostWei: '95000000000000',
      maxCostWei: '250000000000000',
      efficiencyBps: 3800,
      latencySeconds: 2,
      occurredAt: '2026-11-26T18:29:40Z',
      finalizedAt: '2026-11-26T18:29:45Z',
    },
    {
      userOpHash: `0x${'c'.repeat(64)}`,
      txHash: `0x${'d'.repeat(64)}`,
      sender: `0x${'3'.repeat(40)}`,
      blockNumber: '332900399',
      success: false,
      actualGasCostWei: '180000000000000',
      maxCostWei: '250000000000000',
      efficiencyBps: 7200,
      latencySeconds: 5,
      occurredAt: '2026-11-26T18:25:10Z',
      finalizedAt: '2026-11-26T18:25:15Z',
    },
  ],
  controlEvents: [
    {
      id: 12,
      action: 'resume',
      reason: 'Sponsorship issuance resumed',
      createdAt: '2026-11-26T10:00:00Z',
    },
  ],
}

beforeEach(() => {
  protocolContext.mockReturnValue({
    chainId: 421614,
    chain: resolvePerpsChain(421614),
    releaseVersion: 'v2.0.0',
    blockNumber: '332900420',
    observedAt: '2026-11-26T18:30:00Z',
  })
  statusQuery.mockReturnValue({
    data: sampleStatus,
    isLoading: false,
    isError: false,
    refetch: vi.fn(),
  })
  opsQuery.mockReturnValue({
    data: sampleOps,
    isLoading: false,
    isError: false,
  })
})

function show() {
  render(
    <MemoryRouter>
      <SponsoredPage />
    </MemoryRouter>,
  )
}

describe('SponsoredPage (Stage 5 Sponsored Transactions & Paymaster Health)', () => {
  it('renders live gate status, EntryPoint deposit runway, budget gauges, reconciler, close assistance, ops tape, and audit trail', () => {
    show()

    expect(screen.getAllByText('0.2500 ETH').length).toBeGreaterThanOrEqual(1)
    expect(screen.getByText('Fresh · Alto OK')).toBeInTheDocument()
    expect(screen.getByText('EntryPoint v0.8 Ready')).toBeInTheDocument()
    expect(screen.getByText('3.80 USDG')).toBeInTheDocument()
    expect(screen.getByText('Sponsorship issuance resumed')).toBeInTheDocument()

    const opsTable = screen.getByRole('table', { name: /Finalized sponsored UserOperations/i })
    expect(within(opsTable).getByText('Succeeded')).toBeInTheDocument()
    expect(within(opsTable).getByText('Reverted')).toBeInTheDocument()
    expect(within(opsTable).getByText('0.000095 ETH')).toBeInTheDocument()
    expect(opsTable.textContent).not.toContain('$')

    // Switching status filter updates hook parameter
    fireEvent.click(screen.getByRole('tab', { name: 'Reverted' }))
    expect(opsQuery).toHaveBeenLastCalledWith('reverted', 50)
  })

  it('fails closed cleanly when Stage 5 backend endpoints return 404', () => {
    const notFound = new InsightsApiError('Not found', 404)
    statusQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: true,
      error: notFound,
      refetch: vi.fn(),
    })
    opsQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: true,
      error: notFound,
    })

    show()

    expect(screen.getByText('Awaiting Stage 5 backend deploy')).toBeInTheDocument()
    expect(screen.getByText('Stage 5 backend deployment pending')).toBeInTheDocument()
    expect(screen.getAllByText('Unavailable').length).toBeGreaterThanOrEqual(4)
  })

  it('renders unconfigured state honestly without synthetic placeholders', () => {
    statusQuery.mockReturnValue({
      data: { ...sampleStatus, configured: false, stack: 'none', budgets: null },
      isLoading: false,
      isError: false,
      refetch: vi.fn(),
    })

    show()

    expect(screen.getByText('Sponsorship not configured')).toBeInTheDocument()
    expect(screen.getByText('Sponsorship backend not configured')).toBeInTheDocument()
  })
})
