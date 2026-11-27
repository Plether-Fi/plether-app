import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter } from 'react-router-dom'
import type {
  SponsoredOperationsResponse,
  SponsoredStatusResponse,
} from '../../../insights/src/api/sponsored'
import { SponsoredPage } from '../../../insights/src/pages/SponsoredPage'
import './InsightsPositionBreakdown.css'

const BASE_STATUS: SponsoredStatusResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-26T21:15:00Z',
  configured: true,
  stack: 'native',
  entryPointVersion: '0.8',
  entryPointAddress: '0x4337084d9e255ff0702461cf8895ce9e3b5ff108',
  paymasterAddress: '0x7a4f9d31c8b2e5a9014c22f68e910b3a4f8c1d02',
  gate: {
    status: 'issuing',
    sponsorshipEnabled: true,
    submissionEnabled: true,
    rolloutMode: 'global',
    issuancePaused: false,
    pausedReason: null,
    paymasterOnchainPaused: false,
    updatedAt: '2026-11-26T10:00:00Z',
  },
  paymaster: {
    entryPointDepositWei: '250000000000000000',
    minDepositWei: '50000000000000000',
    freeHeadroomWei: '247500000000000000',
    depositHealthy: true,
    burn24hWei: '12400000000000000',
    estimatedRunwayHours: 483,
    altoFundingState: 'ready',
    altoFundingReason: 'READY',
  },
  budgets: {
    maxCostPerOpWei: '5000000000000000',
    accountHourlyCapWei: '25000000000000000',
    globalOutstanding: {
      usedWei: '2500000000000000',
      capWei: '50000000000000000',
      utilizationBps: 500,
    },
    globalHourly: {
      usedWei: '1800000000000000',
      capWei: '100000000000000000',
      utilizationBps: 180,
    },
    globalDaily: {
      usedWei: '12400000000000000',
      capWei: '500000000000000000',
      utilizationBps: 248,
    },
  },
  reconciler: {
    safeBlock: '332945102',
    headBlock: '332945120',
    blocksBehind: 18,
    lastSuccessAt: '2026-11-26T21:14:54Z',
    lagSeconds: 6,
    maxSafeLagSeconds: 600,
    fresh: true,
  },
  bundler: {
    reachable: true,
    entryPointSupported: true,
    status: 'ready',
  },
  execution: {
    window1h: {
      totalOps: 18,
      succeededOps: 18,
      revertedOps: 0,
      successRateBps: 10000,
      gasSpentWei: '1800000000000000',
      gasEfficiencyBps: 3920,
    },
    window24h: {
      totalOps: 142,
      succeededOps: 140,
      revertedOps: 2,
      successRateBps: 9859,
      gasSpentWei: '12400000000000000',
      gasEfficiencyBps: 4110,
    },
    window7d: {
      totalOps: 924,
      succeededOps: 912,
      revertedOps: 12,
      successRateBps: 9870,
      gasSpentWei: '84200000000000000',
      gasEfficiencyBps: 4050,
    },
    latencyP50Seconds: 2,
    latencyP95Seconds: 6,
    funnel24h: {
      reserved: 1,
      signed: 0,
      submitted: 1,
      settled: 140,
      expired: 3,
      cancelled: 1,
    },
    activeUnfinalizedCount: 2,
    oldestUnfinalizedAgeSeconds: 14,
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

const BASE_OPS: SponsoredOperationsResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-26T21:15:00Z',
  configured: true,
  stack: 'native',
  operations: [
    {
      userOpHash: '0x9a8f12c4e6b701d9f4a2e8c13b7d504a6c2e9f1b8d3401c7e5a9f2b4d6018e3a',
      txHash: '0x4c1d89e2f0a5b7312d4e8f9a1c3b5d7e9f0a2b4c6d8e0f1a2b3c4d5e6f7a8b9c',
      sender: '0x94d2f81b3e7c4a0128f56a9c3e12d4b8019a7c24',
      blockNumber: '332945112',
      success: true,
      actualGasCostWei: '89000000000000',
      maxCostWei: '240000000000000',
      efficiencyBps: 3708,
      latencySeconds: 2,
      occurredAt: '2026-11-26T21:14:22Z',
      finalizedAt: '2026-11-26T21:14:27Z',
    },
    {
      userOpHash: '0x3f2e1d0c9b8a76543210fedcba9876543210fedcba9876543210abcdef123456',
      txHash: '0x7e6d5c4b3a291807f6e5d4c3b2a1908f7e6d5c4b3a291807f6e5d4c3b2a1908f',
      sender: '0x2b8c4e19a7d3056f12c89e4a7b5d3f018e6a2c49',
      blockNumber: '332945088',
      success: true,
      actualGasCostWei: '104000000000000',
      maxCostWei: '250000000000000',
      efficiencyBps: 4160,
      latencySeconds: 3,
      occurredAt: '2026-11-26T21:11:05Z',
      finalizedAt: '2026-11-26T21:11:10Z',
    },
    {
      userOpHash: '0x55aa11bb22cc33dd44ee55ff6600771188229933aa44bb55cc66dd77ee88ff99',
      txHash: '0x88ff77ee66dd55cc44bb33aa229911887766554433221100ffeeddccbbaa9988',
      sender: '0x61a9c4d2e8f03b751c2e4a8d9f0b3c5e7a1d4f82',
      blockNumber: '332944970',
      success: false,
      actualGasCostWei: '185000000000000',
      maxCostWei: '250000000000000',
      efficiencyBps: 7400,
      latencySeconds: 5,
      occurredAt: '2026-11-26T21:02:18Z',
      finalizedAt: '2026-11-26T21:02:24Z',
    },
    {
      userOpHash: '0x11223344556677889900aabbccddeeff11223344556677889900aabbccddeeff',
      txHash: '0x99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa',
      sender: '0xd4e2a8c1f7b39054e2a1c6d8f0b4a9c2e5d7f103',
      blockNumber: '332944890',
      success: true,
      actualGasCostWei: '92000000000000',
      maxCostWei: '230000000000000',
      efficiencyBps: 4000,
      latencySeconds: 2,
      occurredAt: '2026-11-26T20:54:40Z',
      finalizedAt: '2026-11-26T20:54:44Z',
    },
  ],
  controlEvents: [
    {
      id: 14,
      action: 'resume',
      reason: 'Sponsorship issuance resumed',
      createdAt: '2026-11-26T10:00:00Z',
    },
    {
      id: 13,
      action: 'pause',
      reason: 'Paymaster EntryPoint deposit below minimum floor',
      createdAt: '2026-11-26T09:42:10Z',
    },
    {
      id: 12,
      action: 'resume',
      reason: 'Operator safety control update',
      createdAt: '2026-11-24T15:18:00Z',
    },
  ],
}

function SponsoredStoryFrame({
  status,
  ops,
}: {
  status: SponsoredStatusResponse
  ops: SponsoredOperationsResponse
}) {
  const queryClient = useMemo(() => {
    const client = new QueryClient({
      defaultOptions: {
        queries: {
          retry: false,
          staleTime: Infinity,
          gcTime: Infinity,
          refetchOnMount: false,
          refetchOnWindowFocus: false,
          refetchInterval: false,
        },
      },
    })
    client.setQueryData(['insights', 'status'], {
      healthy: true,
      latestIndexedBlock: status.reconciler.headBlock ? Number(status.reconciler.headBlock) : 332945120,
      latestIndexedAt: status.observedAt,
      chainId: status.network.chainId,
    })
    client.setQueryData(['insights', 'divisions', 'live'], {
      status: 'ready',
      snapshotBlock: status.reconciler.headBlock ?? '332945120',
      snapshotAt: status.observedAt,
      scoringRelease: { version: status.network.releaseVersion, assetSymbol: 'USDG' },
      walletCount: 42,
      standings: [],
    })
    client.setQueryData(['insights', 'sponsored', 'status'], status)
    client.setQueryData(['insights', 'sponsored', 'operations', 'all', 50], ops)
    client.setQueryData(['insights', 'sponsored', 'operations', 'succeeded', 50], {
      ...ops,
      operations: ops.operations.filter((o) => o.success),
    })
    client.setQueryData(['insights', 'sponsored', 'operations', 'reverted', 50], {
      ...ops,
      operations: ops.operations.filter((o) => !o.success),
    })
    return client
  }, [status, ops])

  return (
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/sponsored']}>
        <main className="min-h-screen bg-app-bg px-4 py-8 text-content-primary sm:px-8">
          <div className="mx-auto max-w-7xl">
            <SponsoredPage />
          </div>
        </main>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta = {
  title: 'Insights/Stage 5 — Sponsored Transactions',
  component: SponsoredStoryFrame,
  parameters: {
    layout: 'fullscreen',
  },
} satisfies Meta<typeof SponsoredStoryFrame>

export default meta
type Story = StoryObj<typeof meta>

export const IssuingLive: Story = {
  args: {
    status: BASE_STATUS,
    ops: BASE_OPS,
  },
}

export const KernelAaCanaryHighUtilization: Story = {
  args: {
    status: {
      ...BASE_STATUS,
      stack: 'kernel',
      entryPointVersion: '0.9',
      entryPointAddress: '0x433709009b8330fda32311df1c2afa402ed8d009',
      gate: {
        ...BASE_STATUS.gate,
        rolloutMode: 'canary',
      },
      paymaster: {
        ...BASE_STATUS.paymaster,
        altoFundingState: 'ready',
        altoFundingReason: 'FUNDING_LOW',
      },
      budgets: {
        maxCostPerOpWei: '5000000000000000',
        accountHourlyCapWei: '25000000000000000',
        globalOutstanding: {
          usedWei: '36000000000000000',
          capWei: '50000000000000000',
          utilizationBps: 7200,
        },
        globalHourly: {
          usedWei: '68000000000000000',
          capWei: '100000000000000000',
          utilizationBps: 6800,
        },
        globalDaily: {
          usedWei: '210000000000000000',
          capWei: '500000000000000000',
          utilizationBps: 4200,
        },
      },
      execution: {
        ...BASE_STATUS.execution,
        activeUnfinalizedCount: 7,
        oldestUnfinalizedAgeSeconds: 420,
      },
      closeAssistance: {
        ...BASE_STATUS.closeAssistance,
        pendingUnverifiedCount: 2,
        verificationRateBps: 9048,
      },
    },
    ops: {
      ...BASE_OPS,
      stack: 'kernel',
      controlEvents: [],
    },
  },
}

export const PausedLowDepositAndStaleReconciler: Story = {
  args: {
    status: {
      ...BASE_STATUS,
      gate: {
        ...BASE_STATUS.gate,
        status: 'paused',
        issuancePaused: true,
        pausedReason: 'Paymaster EntryPoint deposit below minimum floor',
      },
      paymaster: {
        entryPointDepositWei: '32000000000000000',
        minDepositWei: '50000000000000000',
        freeHeadroomWei: '0',
        depositHealthy: false,
        burn24hWei: '28000000000000000',
        estimatedRunwayHours: 27,
        altoFundingState: 'blocked',
        altoFundingReason: 'WORKER_INSUFFICIENT_FUNDS',
      },
      budgets: {
        maxCostPerOpWei: '5000000000000000',
        accountHourlyCapWei: '25000000000000000',
        globalOutstanding: {
          usedWei: '46000000000000000',
          capWei: '50000000000000000',
          utilizationBps: 9200,
        },
        globalHourly: {
          usedWei: '89000000000000000',
          capWei: '100000000000000000',
          utilizationBps: 8900,
        },
        globalDaily: {
          usedWei: '440000000000000000',
          capWei: '500000000000000000',
          utilizationBps: 8800,
        },
      },
      reconciler: {
        safeBlock: '332942100',
        headBlock: '332945120',
        blocksBehind: 3020,
        lastSuccessAt: '2026-11-26T20:58:00Z',
        lagSeconds: 1020,
        maxSafeLagSeconds: 600,
        fresh: false,
      },
      bundler: {
        reachable: false,
        entryPointSupported: false,
        status: 'unavailable',
      },
    },
    ops: BASE_OPS,
  },
}

export const NotConfigured: Story = {
  args: {
    status: {
      ...BASE_STATUS,
      configured: false,
      stack: 'none',
      entryPointVersion: null,
      entryPointAddress: null,
      paymasterAddress: null,
      gate: {
        status: 'disabled',
        sponsorshipEnabled: false,
        submissionEnabled: false,
        rolloutMode: 'disabled',
        issuancePaused: null,
        pausedReason: null,
        paymasterOnchainPaused: null,
        updatedAt: null,
      },
      budgets: null,
    },
    ops: {
      ...BASE_OPS,
      configured: false,
      stack: 'none',
      operations: [],
      controlEvents: [],
    },
  },
}
