import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { describe, expect, it, vi } from 'vitest'
import type { TradingAccountDetailResponse } from '../api'
import { TradingAccountDetailPage } from './TradingAccountDetailPage'

const mockUseTradingAccountDetail = vi.fn()

vi.mock('../api', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../api')>()
  return {
    ...actual,
    useTradingAccountDetail: (...args: unknown[]) => mockUseTradingAccountDetail(...args),
    useInsightsStatus: () => ({
      data: {
        healthy: true,
        latestIndexedBlock: 333171209,
        latestIndexedAt: '2026-11-27T15:15:00Z',
        chainId: 421614,
      },
    }),
  }
})

vi.mock('../divisions/query', () => ({
  useLiveDivisionsSummary: () => ({ data: undefined }),
}))

const SAMPLE_DETAIL: TradingAccountDetailResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-27T15:15:00Z',
  configured: true,
  sender: '0xfe2c91e9d193a5de484f6b4a7cca3dc05a3533c2',
  found: true,
  ownerAddress: '0x9186a420c7e18b2d5f49a01c3e8b7d6f2a14b444',
  discordUsername: 'mnitarik',
  firstSeenAt: '2026-11-21T09:14:00Z',
  lastActivityAt: '2026-11-27T15:14:40Z',
  profile: {
    deployment: 'sepolia-kernel-v4-canary',
    accountVersion: 'kernel-v4-uups',
    entryPointVersion: '0.9',
    entryPoint: '0x433709009b8330fda32311df1c2afa402ed8d009',
    accountCodeHash: '0x1a9f4c82d6e0b35791c4e8a2f6d0b4c8e2a6f0d4b8c2e6a0f4d8b2c6e0a4f8d2',
    paymaster: '0x9b4f218c6e0a73d5b1f9c2e4a8d6b0c3e5f7a129',
  },
  onchain: {
    rpcAvailable: true,
    deployed: true,
    codeHash: '0x1a9f4c82d6e0b35791c4e8a2f6d0b4c8e2a6f0d4b8c2e6a0f4d8b2c6e0a4f8d2',
    codeHashMatchesPinned: true,
    ethBalanceWei: '12450000000000000',
    entryPointNonce: '36',
    dbHighestNonce: '36',
    nonceSynced: true,
  },
  sponsorship: {
    enabled: true,
    hourlyUsedWei: '3200000000000000',
    hourlyCapWei: '25000000000000000',
    hourlyUtilizationBps: 1280,
    outstandingUsedWei: '1800000000000000',
    outstandingCapWei: '10000000000000000',
    outstandingUtilizationBps: 1800,
    maxCostPerOpWei: '5000000000000000',
    activeReservedCount: 1,
  },
  summary: {
    totalPreparations: 41,
    unsubmittedPreparations: 5,
    totalOperations: 36,
    operations24h: 11,
    sponsoredCount: 25,
    ethCount: 11,
    cancellationOpsCount: 1,
    includedSucceeded: 33,
    includedReverted: 1,
    pendingCount: 1,
    nonceConsumed: 1,
    sponsorshipExpired: 0,
    cancellationsPrepared: 1,
    cancellationsAccepted: 1,
    successRateBps: 9706,
    totalGasCostWei: '3420000000000000',
    sponsoredGasCostWei: '2310000000000000',
    ethGasCostWei: '1110000000000000',
    oldestPendingAgeSeconds: 18,
  },
  pendingOperations: [
    {
      userOpHash: '0x9a4b7c12d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e80819',
      preparationId: 'prep-0036',
      mode: 'sponsored',
      cancellationOnly: false,
      nonce: '36',
      ageSeconds: 18,
      preparedSafeBlock: '333171190',
      nonceScanThroughBlock: '333171204',
      sponsorshipValidUntil: '2026-11-27T15:19:40Z',
      reservedMaxCostWei: '1800000000000000',
      cancellationPrepared: false,
      cancellationHash: null,
      createdAt: '2026-11-27T15:14:42Z',
    },
  ],
  operations: [
    {
      userOpHash: '0x9a4b7c12d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e80819',
      preparationId: 'prep-0036',
      mode: 'sponsored',
      cancellationOnly: false,
      nonce: '36',
      status: 'pending',
      executionSuccess: null,
      reservedMaxCostWei: '1800000000000000',
      actualGasCostWei: null,
      txHash: null,
      blockNumber: null,
      cancellationHash: null,
      createdAt: '2026-11-27T15:14:42Z',
    },
    {
      userOpHash: '0x2c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5bb4c5',
      preparationId: 'prep-0035',
      mode: 'eth',
      cancellationOnly: false,
      nonce: '35',
      status: 'included',
      executionSuccess: true,
      reservedMaxCostWei: null,
      actualGasCostWei: '120000000000000',
      txHash: '0x81e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0b1c2',
      blockNumber: '333170980',
      cancellationHash: null,
      createdAt: '2026-11-27T14:52:10Z',
    },
    {
      userOpHash: '0x7e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1de6f7',
      preparationId: 'prep-0034',
      mode: 'eth',
      cancellationOnly: true,
      nonce: '34',
      status: 'nonce_consumed',
      executionSuccess: null,
      reservedMaxCostWei: null,
      actualGasCostWei: '60000000000000',
      txHash: '0x5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b',
      blockNumber: '333169500',
      cancellationHash: '0x6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c',
      createdAt: '2026-11-27T13:40:00Z',
    },
  ],
  cancellations: [
    {
      cancellationPreparationId: 'cancel-prep-0034',
      originalHash: '0x7e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1de6f7',
      cancellationHash: '0x6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c',
      nonce: '34',
      accepted: true,
      expiresAt: '2026-11-27T13:45:00Z',
      createdAt: '2026-11-27T13:40:00Z',
    },
  ],
  unsubmittedPreparations: [
    {
      preparationId: 'prep-0031-expired',
      mode: 'sponsored',
      nonce: '31',
      preparedSafeBlock: '333164100',
      nonceScanThroughBlock: '333164120',
      expired: true,
      expiresAt: '2026-11-27T11:10:00Z',
      createdAt: '2026-11-27T11:05:00Z',
    },
  ],
}

function renderPage(path = `/trading-accounts/${SAMPLE_DETAIL.sender}`) {
  const qc = new QueryClient({ defaultOptions: { queries: { retry: false } } })
  return render(
    <QueryClientProvider client={qc}>
      <MemoryRouter initialEntries={[path]}>
        <Routes>
          <Route path="/trading-accounts/:address" element={<TradingAccountDetailPage />} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  )
}

describe('TradingAccountDetailPage (ZeroDev Kernel v4 Smart Wallet Inspector)', () => {
  it('renders on-chain verification, sponsorship quota, pending lane, filterable ledger, and audit tables', () => {
    mockUseTradingAccountDetail.mockReturnValue({
      data: SAMPLE_DETAIL,
      isLoading: false,
      isError: false,
      isFetching: false,
      refetch: vi.fn(),
    })

    renderPage()

    expect(screen.getByText('@mnitarik')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: /Open Wallet Page/i })).toHaveAttribute(
      'href',
      `/wallets/${SAMPLE_DETAIL.sender}`,
    )
    expect(screen.getByText('Deployed · Pinned Kernel v4')).toBeInTheDocument()
    expect(screen.getByText(/Nonce Synced \(EP #36\)/i)).toBeInTheDocument()
    expect(screen.getByText('Rolling 1h Sponsorship Cap')).toBeInTheDocument()
    expect(screen.getByText('Active Pending Nonce Lanes')).toBeInTheDocument()
    expect(screen.getByText('ETH Nonce Cancellations')).toBeInTheDocument()
    expect(screen.getByText('Unsubmitted Preparations')).toBeInTheDocument()

    // Filter operation ledger to ETH-Paid
    fireEvent.click(screen.getByRole('button', { name: 'ETH-Paid' }))
    expect(screen.getByText('0x2c7d…b4c5')).toBeInTheDocument()
    expect(screen.queryByText('0x9a4b…0819')).toBeInTheDocument() // still in Pending Lanes table
    // Filter to Cancellations
    fireEvent.click(screen.getByRole('button', { name: 'Cancellations' }))
    expect(screen.getByText('0x6b7c…6b7c')).toBeInTheDocument()
  })

  it('rejects malformed addresses without crashing', () => {
    mockUseTradingAccountDetail.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: false,
      isFetching: false,
      refetch: vi.fn(),
    })

    renderPage('/trading-accounts/not-an-address')
    expect(screen.getByText('Invalid smart account address')).toBeInTheDocument()
  })
})
