import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import type { TradingAccountDetailResponse } from '../../../insights/src/api/tradingAccounts'
import { TradingAccountDetailPage } from '../../../insights/src/pages/TradingAccountDetailPage'
import './InsightsPositionBreakdown.css'

const SAMPLE_ACCOUNT_DETAIL: TradingAccountDetailResponse = {
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
  lastActivityAt: '2026-11-27T15:14:42Z',
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
    hourlyUsedWei: '6400000000000000',
    hourlyCapWei: '25000000000000000',
    hourlyUtilizationBps: 2560,
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
      userOpHash: '0x4f8e1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8dd6e7',
      preparationId: 'prep-0035',
      mode: 'sponsored',
      cancellationOnly: false,
      nonce: '35',
      status: 'included',
      executionSuccess: true,
      reservedMaxCostWei: '1400000000000000',
      actualGasCostWei: '90000000000000',
      txHash: '0xb72e9f1a0c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7dbe61',
      blockNumber: '333171104',
      cancellationHash: null,
      createdAt: '2026-11-27T15:11:20Z',
    },
    {
      userOpHash: '0x2c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5bb4c5',
      preparationId: 'prep-0034',
      mode: 'eth',
      cancellationOnly: false,
      nonce: '34',
      status: 'included',
      executionSuccess: true,
      reservedMaxCostWei: null,
      actualGasCostWei: '120000000000000',
      txHash: '0x81e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0b1c2',
      blockNumber: '333170812',
      cancellationHash: null,
      createdAt: '2026-11-27T14:52:00Z',
    },
    {
      userOpHash: '0x7e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1de6f7',
      preparationId: 'prep-0033',
      mode: 'eth',
      cancellationOnly: true,
      nonce: '33',
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
      cancellationPreparationId: 'cancel-prep-0033',
      originalHash: '0x7e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1de6f7',
      cancellationHash: '0x6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c',
      nonce: '33',
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
    {
      preparationId: 'prep-0029-expired',
      mode: 'eth',
      nonce: '29',
      preparedSafeBlock: '333161020',
      nonceScanThroughBlock: '333161055',
      expired: true,
      expiresAt: '2026-11-27T10:25:00Z',
      createdAt: '2026-11-27T10:20:00Z',
    },
  ],
}

function TradingAccountDetailCanvas({ data }: { data: TradingAccountDetailResponse }) {
  const queryClient = useMemo(() => {
    const qc = new QueryClient({
      defaultOptions: {
        queries: {
          enabled: false,
          retry: false,
          staleTime: Infinity,
          gcTime: Infinity,
          refetchInterval: false,
          refetchOnMount: false,
          refetchOnWindowFocus: false,
          refetchOnReconnect: false,
        },
      },
    })
    qc.setQueryData(
      ['insights', 'trading-accounts', 'detail', data.sender.toLowerCase()],
      data,
    )
    qc.setQueryData(['insights', 'status'], {
      chainId: 421614,
      releaseVersion: 'v2.0.0',
      latestIndexedBlock: 333171209,
      latestIndexedAt: '2026-11-27T15:15:00Z',
      healthy: true,
    })
    qc.setQueryData(['insights', 'divisions', 'live'], {
      status: 'ready',
      snapshotBlock: '333171209',
      snapshotAt: '2026-11-27T15:15:00Z',
      scoringRelease: { version: 'v2.0.0', assetSymbol: 'USDG' },
      walletCount: 19,
      standings: [],
    })
    return qc
  }, [data])

  return (
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={[`/trading-accounts/${data.sender}`]}>
        <div className="min-h-screen bg-app-bg p-6 text-content-primary sm:p-8">
          <div className="mx-auto max-w-7xl">
            <Routes>
              <Route path="/trading-accounts/:address" element={<TradingAccountDetailPage />} />
            </Routes>
          </div>
        </div>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta<typeof TradingAccountDetailCanvas> = {
  title: 'Insights/Trading Account Detail (Kernel v4)',
  component: TradingAccountDetailCanvas,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof TradingAccountDetailCanvas>

export const ActiveDeployedWithPendingLane: Story = {
  args: {
    data: SAMPLE_ACCOUNT_DETAIL,
  },
}

export const HourlyCapExhaustedLowEthGas: Story = {
  args: {
    data: {
      ...SAMPLE_ACCOUNT_DETAIL,
      sender: '0xfd685c86261f389c6872dbf2beec81416c203079',
      discordUsername: 'gkn190761',
      onchain: {
        ...SAMPLE_ACCOUNT_DETAIL.onchain,
        ethBalanceWei: '210000000000000', // 0.00021 ETH -> triggers Low ETH Gas pill
      },
      sponsorship: {
        enabled: true,
        hourlyUsedWei: '24850000000000000',
        hourlyCapWei: '25000000000000000',
        hourlyUtilizationBps: 9940,
        outstandingUsedWei: '9200000000000000',
        outstandingCapWei: '10000000000000000',
        outstandingUtilizationBps: 9200,
        maxCostPerOpWei: '5000000000000000',
        activeReservedCount: 2,
      },
    },
  },
}

export const CounterfactualNotYetDeployed: Story = {
  args: {
    data: {
      ...SAMPLE_ACCOUNT_DETAIL,
      sender: '0x080d22f977b12c3890fa12e4b7c89a0123455601',
      discordUsername: null,
      onchain: {
        rpcAvailable: true,
        deployed: false,
        codeHash: null,
        codeHashMatchesPinned: null,
        ethBalanceWei: '0',
        entryPointNonce: '0',
        dbHighestNonce: '0',
        nonceSynced: true,
      },
      sponsorship: {
        enabled: true,
        hourlyUsedWei: '0',
        hourlyCapWei: '25000000000000000',
        hourlyUtilizationBps: 0,
        outstandingUsedWei: '0',
        outstandingCapWei: '10000000000000000',
        outstandingUtilizationBps: 0,
        maxCostPerOpWei: '5000000000000000',
        activeReservedCount: 0,
      },
      summary: {
        totalPreparations: 1,
        unsubmittedPreparations: 1,
        totalOperations: 0,
        operations24h: 0,
        sponsoredCount: 0,
        ethCount: 0,
        cancellationOpsCount: 0,
        includedSucceeded: 0,
        includedReverted: 0,
        pendingCount: 0,
        nonceConsumed: 0,
        sponsorshipExpired: 0,
        cancellationsPrepared: 0,
        cancellationsAccepted: 0,
        successRateBps: null,
        totalGasCostWei: '0',
        sponsoredGasCostWei: '0',
        ethGasCostWei: '0',
        oldestPendingAgeSeconds: null,
      },
      pendingOperations: [],
      operations: [],
      cancellations: [],
      unsubmittedPreparations: [
        {
          preparationId: 'prep-0000-open',
          mode: 'sponsored',
          nonce: '0',
          preparedSafeBlock: '333171201',
          nonceScanThroughBlock: '333171209',
          expired: false,
          expiresAt: '2026-11-27T15:20:00Z',
          createdAt: '2026-11-27T15:14:55Z',
        },
      ],
    },
  },
}
