import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import type { TradingAccountsOverviewResponse } from '../../../insights/src/api/tradingAccounts'
import { TradingAccountsPage } from '../../../insights/src/pages/TradingAccountsPage'
import './InsightsPositionBreakdown.css'

const SAMPLE_KERNEL_OVERVIEW: TradingAccountsOverviewResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-27T15:15:00Z',
  configured: true,
  status: 'healthy',
  profile: {
    deployment: 'sepolia-kernel-v4-canary',
    accountVersion: 'kernel-v4-uups',
    entryPointVersion: '0.9',
    entryPoint: '0x433709009b8330fda32311df1c2afa402ed8d009',
    entryPointCodeHash: '0x8d4a712c9e8b34f56102d9a4c8e7f1b23a45c67d8e9f0123456789abcdef0192',
    factory: '0x6723b44abeec4e71ebbe3232bd5efb65c1ff4720',
    factoryCodeHash: '0x3b91e04f2a7c85d19462f0b3e8a1c7d45e6f708192a3b4c5d6e7f8091a2b3c4d',
    implementation: '0xd6ceddf84be40293d583e92aae60beaf7ea4e010',
    implementationCodeHash: '0x5c20a81e7f93d4b60218c9e4a7f2b5d83c6e9104a2b5c8d1e4f70a3b6c9d2e5f',
    validator: '0x845adb2c711129d4f3966735ed98a9f09fc4ce57',
    validatorCodeHash: '0x7e41b93c5d82a6f01493e7c2b8a5d1f46e90c3b2a5d8e1f40729a4b6c8d0e2f4',
    accountCodeHash: '0x1a9f4c82d6e0b35791c4e8a2f6d0b4c8e2a6f0d4b8c2e6a0f4d8b2c6e0a4f8d2',
    paymaster: '0x9b4f218c6e0a73d5b1f9c2e4a8d6b0c3e5f7a129',
    policyId: 'kernel-sepolia-v4-canary',
  },
  controls: {
    ethModeEnabled: true,
    sponsorshipEnabled: true,
    protectionCommitsEnabled: true,
    ownerAdmissionMode: 'canary',
    admittedOwnerCount: 24,
  },
  pipeline: {
    bundlerReachable: true,
    bundlerEntryPointSupported: true,
    bundlerStatus: 'ok',
    reconcilerSafeBlock: '333171200',
    reconcilerHeadBlock: '333171209',
    reconcilerBlocksBehind: 9,
    reconcilerLagSeconds: 3,
    reconcilerMaxSafeLagSeconds: 120,
    reconcilerFresh: true,
    reconcilerPausedReason: null,
  },
  adoption: {
    uniqueAccounts: 19,
    uniqueOwners: 19,
    activeAccounts24h: 12,
    totalPreparations: 218,
    totalOperations: 184,
    operations24h: 64,
    sponsoredOpsTotal: 127,
    ethOpsTotal: 57,
    sponsoredOps24h: 45,
    ethOps24h: 19,
    cancellationOpsTotal: 4,
    pendingOpsCount: 1,
    oldestPendingAgeSeconds: 14,
  },
  lifecycle: {
    includedSucceeded: 174,
    includedReverted: 2,
    pending: 1,
    nonceConsumed: 5,
    sponsorshipExpired: 2,
    cancellationsPrepared: 5,
    cancellationsAccepted: 4,
    successRateBps: 9886,
    totalGasCostWei: '18420000000000000',
    sponsoredGasCostWei: '12680000000000000',
    ethGasCostWei: '5740000000000000',
  },
  accounts: [
    {
      sender: '0xfe2c91e9d193a5de484f6b4a7cca3dc05a3533c2',
      ownerAddress: '0x9186a420c7e18b2d5f49a01c3e8b7d6f2a14b444',
      discordUsername: 'mnitarik',
      sponsoredCount: 24,
      ethCount: 11,
      includedCount: 34,
      pendingCount: 0,
      retiredCount: 1,
      highestNonce: '35',
      lastMode: 'sponsored',
      firstSeenAt: '2026-11-21T09:14:00Z',
      lastActivityAt: '2026-11-27T15:11:20Z',
    },
    {
      sender: '0xfd685c86261f389c6872dbf2beec81416c203079',
      ownerAddress: '0x327d9c08e5a41b7f2c90d4e1f8a3b6c5d2e1645c',
      discordUsername: 'gkn190761',
      sponsoredCount: 19,
      ethCount: 14,
      includedCount: 31,
      pendingCount: 1,
      retiredCount: 1,
      highestNonce: '33',
      lastMode: 'eth',
      firstSeenAt: '2026-11-21T14:40:00Z',
      lastActivityAt: '2026-11-27T15:14:46Z',
    },
    {
      sender: '0x0790e29dd22247cc6f3c211f2bac8f31b4a26952',
      ownerAddress: '0x4b12e89a0c73d6f51290a8c4e7b1d3f5a9c0e812',
      discordUsername: 'macro_fox',
      sponsoredCount: 31,
      ethCount: 8,
      includedCount: 38,
      pendingCount: 0,
      retiredCount: 1,
      highestNonce: '39',
      lastMode: 'sponsored',
      firstSeenAt: '2026-11-20T18:05:00Z',
      lastActivityAt: '2026-11-27T14:52:10Z',
    },
    {
      sender: '0x080dd7fb7bf78ad4740e4dc9a3b65542a2375601',
      ownerAddress: '0x7c84f019a2d5e3b68901c4f7a2e5d8b1c4f7a093',
      discordUsername: null,
      sponsoredCount: 6,
      ethCount: 9,
      includedCount: 14,
      pendingCount: 0,
      retiredCount: 1,
      highestNonce: '15',
      lastMode: 'eth',
      firstSeenAt: '2026-11-23T11:20:00Z',
      lastActivityAt: '2026-11-27T13:40:05Z',
    },
  ],
  operations: [
    {
      userOpHash: '0x9a4b7c12d8e5f034a6b9c2d1e4f708192a3b4c5d6e7f8091a2b3c4d5e6f70819',
      sender: '0xfd685c86261f389c6872dbf2beec81416c203079',
      ownerAddress: '0x327d9c08e5a41b7f2c90d4e1f8a3b6c5d2e1645c',
      mode: 'eth',
      cancellationOnly: false,
      nonce: '33',
      status: 'pending',
      executionSuccess: null,
      txHash: null,
      blockNumber: null,
      actualGasCostWei: null,
      cancellationHash: null,
      createdAt: '2026-11-27T15:14:46Z',
    },
    {
      userOpHash: '0x4f8e1a2b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7',
      sender: '0xfe2c91e9d193a5de484f6b4a7cca3dc05a3533c2',
      ownerAddress: '0x9186a420c7e18b2d5f49a01c3e8b7d6f2a14b444',
      mode: 'sponsored',
      cancellationOnly: false,
      nonce: '35',
      status: 'included',
      executionSuccess: true,
      txHash: '0xb72ee4b3aad01ea328f33182208022ab5806c4ed9b799fdd214875eb05edbe61',
      blockNumber: '333171182',
      actualGasCostWei: '92400000000000',
      cancellationHash: null,
      createdAt: '2026-11-27T15:11:20Z',
    },
    {
      userOpHash: '0x2c7d9e0f1a2b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5',
      sender: '0x0790e29dd22247cc6f3c211f2bac8f31b4a26952',
      ownerAddress: '0x4b12e89a0c73d6f51290a8c4e7b1d3f5a9c0e812',
      mode: 'eth',
      cancellationOnly: false,
      nonce: '39',
      status: 'included',
      executionSuccess: true,
      txHash: '0x81e2c3d4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2',
      blockNumber: '333170940',
      actualGasCostWei: '118200000000000',
      cancellationHash: null,
      createdAt: '2026-11-27T14:52:10Z',
    },
    {
      userOpHash: '0x7e3f1a9c5b2d4e6f8091a2b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f7',
      sender: '0x080dd7fb7bf78ad4740e4dc9a3b65542a2375601',
      ownerAddress: '0x7c84f019a2d5e3b68901c4f7a2e5d8b1c4f7a093',
      mode: 'eth',
      cancellationOnly: true,
      nonce: '15',
      status: 'nonce_consumed',
      executionSuccess: true,
      txHash: '0x5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b',
      blockNumber: '333169820',
      actualGasCostWei: '64100000000000',
      cancellationHash: '0x1f2e3d4c5b6a798807162534435261708f9e0d1c2b3a495867768594a3b2c1d0',
      createdAt: '2026-11-27T13:40:05Z',
    },
    {
      userOpHash: '0x6d1c2b3a4f5e6d7c8b9a0f1e2d3c4b5a6f7e8d9c0b1a2f3e4d5c6b7a8f9e0d1c',
      sender: '0xfe2c91e9d193a5de484f6b4a7cca3dc05a3533c2',
      ownerAddress: '0x9186a420c7e18b2d5f49a01c3e8b7d6f2a14b444',
      mode: 'sponsored',
      cancellationOnly: false,
      nonce: '34',
      status: 'sponsorship_expired',
      executionSuccess: null,
      txHash: null,
      blockNumber: null,
      actualGasCostWei: null,
      cancellationHash: null,
      createdAt: '2026-11-27T12:18:00Z',
    },
  ],
}

function TradingAccountsStoryCanvas({ data }: { data: TradingAccountsOverviewResponse }) {
  const queryClient = useMemo(() => {
    const qc = new QueryClient({
      defaultOptions: {
        queries: {
          retry: false,
          staleTime: Infinity,
          refetchInterval: false,
          refetchOnMount: false,
          refetchOnWindowFocus: false,
          refetchOnReconnect: false,
        },
      },
    })
    qc.setQueryData(['insights', 'trading-accounts', 'overview'], data)
    qc.setQueryData(['insights', 'status'], {
      chainId: 421614,
      releaseVersion: 'v2.0.0',
      latestIndexedBlock: '333171209',
      latestIndexedAt: '2026-11-27T15:15:00Z',
      healthy: true,
    })
    return qc
  }, [data])

  return (
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/trading-accounts']}>
        <div className="min-h-screen bg-app-bg p-6 text-content-primary sm:p-8">
          <div className="mx-auto max-w-7xl">
            <Routes>
              <Route path="/trading-accounts" element={<TradingAccountsPage />} />
            </Routes>
          </div>
        </div>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta<typeof TradingAccountsStoryCanvas> = {
  title: 'Insights/Trading Accounts (Kernel v4)',
  component: TradingAccountsStoryCanvas,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof TradingAccountsStoryCanvas>

export const LiveKernelCanary: Story = {
  args: {
    data: SAMPLE_KERNEL_OVERVIEW,
  },
}

export const PreCutoverUnconfigured: Story = {
  args: {
    data: {
      ...SAMPLE_KERNEL_OVERVIEW,
      configured: false,
      status: 'unconfigured',
      profile: null,
      controls: null,
      pipeline: null,
      accounts: [],
      operations: [],
    },
  },
}
