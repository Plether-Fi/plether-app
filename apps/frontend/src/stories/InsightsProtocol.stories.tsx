import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter } from 'react-router-dom'
import type { ProtocolManifestResponse } from '../../../insights/src/api/protocol'
import { ProtocolPage } from '../../../insights/src/pages/ProtocolPage'
import './InsightsPositionBreakdown.css'

const BASE_MANIFEST: ProtocolManifestResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-26T21:15:00Z',
  rpcAvailable: true,
  verifiedAtBlock: '332945120',
  release: {
    version: 'v2.0.0',
    date: '2026-10-28',
    phase: 'active',
    deploymentBlock: '323658447',
    sourceCommit: '1209ca8a5b94a4fb6323c7c4585846ed93db3932',
    forgeVersion: '1.5.1-stable',
    manifestSha256: '584cdacada35f8225d2a88811edfbd9622764fe0e6da4b275ba8f33d6f8c0c74',
    bundleSha256: '3ea6e372776abb4fe6b7f3c6b2914b381f4f3da3e71b28872a293d6d32cc8ba7',
    pythAddress: '0x0b73614636c855bf23f342f307fb981a3e47f42b',
    schemaVersion: 4,
    orderInterfaceVersion: 3,
    aaManifestVersion: 'v2',
  },
  settlementToken: {
    name: 'Mock USDG',
    symbol: 'USDG',
    decimals: 6,
    isMock: true,
    address: '0x3d051dbbf5ff427a4ae8ad09746902666af8fb9d',
  },
  safety: {
    available: true,
    phase: 'active',
    lastMarkPrice: '94743619',
    lastMarkTime: '2026-11-26T21:14:48Z',
    tradingActive: true,
    withdrawalLive: true,
    lpEpochSettlementPaused: false,
    oracleFrozen: false,
    fadWindow: false,
    housePoolPaused: false,
    degradedMode: false,
  },
  tvl: {
    available: true,
    isComplete: true,
    verifiedAtBlock: '332945120',
    settlementAssetSymbol: 'USDG',
    totalTvlAssets: '1250000000000',
    protocolContractsTvlAssets: '1095000000000',
    lpVaultsTvlAssets: '820000000000',
    housePoolAssets: '760000000000',
    seniorVaultAssets: '42000000000',
    juniorVaultAssets: '18000000000',
    marginClearinghouseAssets: '275000000000',
    otherContractsAssets: '0',
    kernelTradingAccountsAssets: '155000000000',
    totalTraderMarginAssets: '430000000000',
    contractsTracked: 12,
    contractsWithBalance: 4,
    contractsFailedReads: 0,
    kernelAccountsAvailable: true,
    kernelAccountsTracked: 84,
    kernelAccountsFunded: 31,
    kernelAccountsFailedReads: 0,
    buckets: [
      {
        key: 'lp_vaults',
        label: 'LP Liquidity Pools',
        detail: 'Settled HousePool liquidity + Senior/Junior vault escrow',
        assets: '820000000000',
        shareBps: 6560,
        addressCount: 3,
      },
      {
        key: 'margin_clearinghouse',
        label: 'Margin Clearinghouse',
        detail: 'Deposited trader collateral, locked position margin & claims',
        assets: '275000000000',
        shareBps: 2200,
        addressCount: 1,
      },
      {
        key: 'kernel_v4_accounts',
        label: 'Kernel v4 Trading Accounts',
        detail: 'User margin held directly in ZeroDev Kernel v4 smart wallets',
        assets: '155000000000',
        shareBps: 1240,
        addressCount: 31,
      },
      {
        key: 'other_contracts',
        label: 'Other Protocol Contracts',
        detail: 'Remaining pinned protocol release contracts',
        assets: '0',
        shareBps: 0,
        addressCount: 0,
      },
    ],
    contractHoldings: [
      {
        key: 'housePool',
        name: 'HousePool',
        category: 'liquidity',
        address: '0xeb5604c393843b2f3c347ec24fa9ddad6ab2131e',
        assets: '760000000000',
        shareBps: 6080,
      },
      {
        key: 'marginClearinghouse',
        name: 'MarginClearinghouse',
        category: 'core',
        address: '0xb5b851f627d6df3cf3f096e23e20a58884d7b89a',
        assets: '275000000000',
        shareBps: 2200,
      },
      {
        key: 'seniorVault',
        name: 'Senior Vault',
        category: 'liquidity',
        address: '0x5dc42ba5a1e8dd3e1b6868d10f66a9bf8fb5b83d',
        assets: '42000000000',
        shareBps: 336,
      },
      {
        key: 'juniorVault',
        name: 'Junior Vault',
        category: 'liquidity',
        address: '0x4a8b67184a1e3224c11a191cc69f22c50eef734e',
        assets: '18000000000',
        shareBps: 144,
      },
    ],
    topKernelAccounts: [
      {
        address: '0x71c94f82e3b21094f62d50a8411f22e95489c401',
        assets: '42500000000',
        shareBps: 340,
      },
      {
        address: '0x3a8d1b5c9f440e27c80112a6b5f7e3d901c2a918',
        assets: '28400000000',
        shareBps: 227,
      },
      {
        address: '0x9e02c44a1d78f5602b3c41e8899f10b2204a6d33',
        assets: '19250000000',
        shareBps: 154,
      },
      {
        address: '0x5f41a2b8c9d0e1f234567890abcdef1234567890',
        assets: '12850000000',
        shareBps: 103,
      },
    ],
  },
  verification: {
    totalContracts: 12,
    verifiedCount: 12,
    mismatchCount: 0,
    uncheckedCount: 0,
    matchedParameters: 11,
    driftedParameters: 0,
    unavailableParameters: 0,
  },
  parameters: [
    {
      key: 'initialMarginBps',
      category: 'trader',
      label: 'Initial margin',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Minimum collateral ratio required to open or increase a perpetual position.',
      manifestDefault: '20',
      liveValue: '20',
      status: 'match',
    },
    {
      key: 'maintenanceMarginBps',
      category: 'trader',
      label: 'Maintenance margin',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Equity floor below which an open position becomes eligible for permissionless liquidation.',
      manifestDefault: '10',
      liveValue: '10',
      status: 'match',
    },
    {
      key: 'fadMarginBps',
      category: 'trader',
      label: 'Weekend / FAD margin',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Elevated margin requirement enforced during Forex market closure (FAD) windows.',
      manifestDefault: '300',
      liveValue: '300',
      status: 'match',
    },
    {
      key: 'executionFeeBps',
      category: 'trader',
      label: 'Protocol execution fee',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Fee assessed on executed trade notional for opens, increases, and closes.',
      manifestDefault: '4',
      liveValue: '4',
      status: 'match',
    },
    {
      key: 'vpiFactorWad',
      category: 'trader',
      label: 'VPI impact factor',
      unit: 'wad',
      contract: 'CfdEngine',
      description: 'Scaling constant governing Virtual Price Impact charges for skew-increasing trades and rebates for skew-reducing trades.',
      manifestDefault: '10000000000000000',
      liveValue: '10000000000000000',
      status: 'match',
    },
    {
      key: 'maxSkewRatioWad',
      category: 'trader',
      label: 'Max net skew ratio',
      unit: 'wad',
      contract: 'CfdEngine',
      description: 'Maximum allowed net directional open-interest skew relative to backing HousePool liquidity.',
      manifestDefault: '400000000000000000',
      liveValue: '400000000000000000',
      status: 'match',
    },
    {
      key: 'minimumOpenNotionalAssets',
      category: 'trader',
      label: 'Minimum open notional',
      unit: 'assets',
      contract: 'OrderRouter',
      description: 'Smallest notional trade size permitted when committing an opening order.',
      manifestDefault: '1000000000',
      liveValue: '1000000000',
      status: 'match',
    },
    {
      key: 'liquidationChargeBps',
      category: 'liquidation',
      label: 'Liquidation penalty',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Percentage charge assessed on liquidated notional to incentivize fast liquidations and protect LPs.',
      manifestDefault: '10',
      liveValue: '10',
      status: 'match',
    },
    {
      key: 'keeperShareBps',
      category: 'liquidation',
      label: 'Keeper bounty share',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Portion of liquidation penalties paid directly to the permissionless executing keeper.',
      manifestDefault: '5000',
      liveValue: '5000',
      status: 'match',
    },
    {
      key: 'positionProtectionTriggerBountyAssets',
      category: 'liquidation',
      label: 'TP/SL trigger bounty',
      unit: 'assets',
      contract: 'OrderRouter',
      description: 'Fixed USDG bounty paid to permissionless keepers triggering armed take-profit or stop-loss protections.',
      manifestDefault: '200000',
      liveValue: '200000',
      status: 'match',
    },
    {
      key: 'maxSeniorExposureAssets',
      category: 'lp',
      label: 'Max Senior tranche cap',
      unit: 'assets',
      contract: 'HousePool',
      description: 'Hard absolute ceiling on Senior vault USDG deposits accepted by HousePool.',
      manifestDefault: '40000000000000',
      liveValue: '40000000000000',
      status: 'match',
    },
    {
      key: 'lpEpochDurationSeconds',
      category: 'lp',
      label: 'LP settlement epoch duration',
      unit: 'seconds',
      contract: 'HousePool',
      description: 'Cadence at which queued LP deposit and withdrawal requests mature for permissionless epoch settlement.',
      manifestDefault: null,
      liveValue: '3600',
      status: 'live_only',
    },
    {
      key: 'timelockDelaySeconds',
      category: 'lp',
      label: 'Pool config timelock',
      unit: 'seconds',
      contract: 'HousePool',
      description: 'Minimum delay between proposing and activating HousePool parameter updates.',
      manifestDefault: null,
      liveValue: '172800',
      status: 'live_only',
    },
    {
      key: 'orderExecutionStalenessLimitSeconds',
      category: 'oracle',
      label: 'Order oracle staleness limit',
      unit: 'seconds',
      contract: 'PletherOracle',
      description: 'Maximum age of a Pyth basket update accepted when executing trader orders.',
      manifestDefault: null,
      liveValue: '15',
      status: 'live_only',
    },
  ],
  contracts: [
    {
      key: 'cfdEngine',
      name: 'CfdEngine',
      category: 'core',
      role: 'Core perpetuals accounting, margin, VPI, carry, and PnL settlement engine.',
      address: '0xca18a6783d7542f9d2f28f4f259772b74c4d6baa',
      expectedCodeHash: '0xa0f8e5be6e622a5a83e46e88f6b012757ef924cf613a50cba52a2b5c66b76a6b',
      actualCodeHash: '0xa0f8e5be6e622a5a83e46e88f6b012757ef924cf613a50cba52a2b5c66b76a6b',
      verified: true,
    },
    {
      key: 'marginClearinghouse',
      name: 'MarginClearinghouse',
      category: 'core',
      role: 'Custodies trader USDG collateral, locked position margins, and settlement claims.',
      address: '0xb5b851f627d6df3cf3f096e23e20a58884d7b89a',
      expectedCodeHash: '0xc8e4965893dbd7073303303e9d5ab9c775aabcc1259635e4993fc7fdb0681398',
      actualCodeHash: '0xc8e4965893dbd7073303303e9d5ab9c775aabcc1259635e4993fc7fdb0681398',
      verified: true,
      usdgBalanceAssets: '275000000000',
    },
    {
      key: 'settlementToken',
      name: 'Settlement Token (USDG)',
      category: 'core',
      role: 'Canonical 6-decimal settlement asset across trading accounts and LP vaults.',
      address: '0x3d051dbbf5ff427a4ae8ad09746902666af8fb9d',
      expectedCodeHash: '0x12233a5f6b0abf6a0f8447e56f9507afdd142a78cc42ebb0d041a3997bd299f6',
      actualCodeHash: '0x12233a5f6b0abf6a0f8447e56f9507afdd142a78cc42ebb0d041a3997bd299f6',
      verified: true,
    },
    {
      key: 'orderRouter',
      name: 'OrderRouter',
      category: 'orders',
      role: 'Public entry point for committing, executing, and liquidating bounded V3 orders.',
      address: '0x918663d6da8cb33b325a04c4a5e8c51d42f3b444',
      expectedCodeHash: '0x6c65de67ef2824572537e3641a71100100c49d909b196c43e5d271312534d3cf',
      actualCodeHash: '0x6c65de67ef2824572537e3641a71100100c49d909b196c43e5d271312534d3cf',
      verified: true,
    },
    {
      key: 'orderLifecycleBook',
      name: 'OrderLifecycleBook',
      category: 'orders',
      role: 'Stores signed V3 order intents, terminal outcomes, and execution config hash.',
      address: '0x327d5fd04e17d96dd5c4f845a0b4256a9e19645c',
      expectedCodeHash: '0xc3f34d5c32716e1818e1ae1cf5e14892a639bc35f15894610e619afe648ba554',
      actualCodeHash: '0xc3f34d5c32716e1818e1ae1cf5e14892a639bc35f15894610e619afe648ba554',
      verified: true,
    },
    {
      key: 'positionProtectionBook',
      name: 'PositionProtectionBook',
      category: 'orders',
      role: 'Manages armed on-chain take-profit and stop-loss protection triggers.',
      address: '0x81e3b03cf29865b6c0f85925839567e2bc61073d',
      expectedCodeHash: '0xe15987a99e503d6bbd5d871090b9bfea06383bb37fa7af8dfd9cf457fdc4eed5',
      actualCodeHash: '0xe15987a99e503d6bbd5d871090b9bfea06383bb37fa7af8dfd9cf457fdc4eed5',
      verified: true,
    },
    {
      key: 'housePool',
      name: 'HousePool',
      category: 'liquidity',
      role: 'Counterparty liquidity pool managing Senior/Junior tranche accounting and hourly LP epochs.',
      address: '0xeb5604c393843b2f3c347ec24fa9ddad6ab2131e',
      expectedCodeHash: '0x5e684cc82bfc5b697a24c80ef26910cfbe25efbe6fb2ca184c421808b5ced957',
      actualCodeHash: '0x5e684cc82bfc5b697a24c80ef26910cfbe25efbe6fb2ca184c421808b5ced957',
      verified: true,
      usdgBalanceAssets: '760000000000',
    },
    {
      key: 'seniorVault',
      name: 'Senior Vault',
      category: 'liquidity',
      role: 'ERC-7540 async Senior LP tranche vault with protected priority coupon.',
      address: '0x5dc42ba5a1e8dd3e1b6868d10f66a9bf8fb5b83d',
      expectedCodeHash: '0x1ef72eae70097e0207bbe8b4399d0b0e79910c40d6665a42f2c933b037355d8a',
      actualCodeHash: '0x1ef72eae70097e0207bbe8b4399d0b0e79910c40d6665a42f2c933b037355d8a',
      verified: true,
      usdgBalanceAssets: '42000000000',
    },
    {
      key: 'juniorVault',
      name: 'Junior Vault',
      category: 'liquidity',
      role: 'ERC-7540 async Junior LP tranche vault absorbing trader PnL and fee revenue.',
      address: '0x4a8b67184a1e3224c11a191cc69f22c50eef734e',
      expectedCodeHash: '0x8d00d3d0273b7a321cbf61232ea79ef0b503fe0d83bb52609321765a0a04f3bf',
      actualCodeHash: '0x8d00d3d0273b7a321cbf61232ea79ef0b503fe0d83bb52609321765a0a04f3bf',
      verified: true,
      usdgBalanceAssets: '18000000000',
    },
    {
      key: 'pletherOracle',
      name: 'PletherOracle',
      category: 'oracle',
      role: 'Verifies 6-component Pyth DXY basket updates, staleness bounds, and FAD windows.',
      address: '0x4f2c62a0e03657f52c633c19c11f88d8f0897bb2',
      expectedCodeHash: '0x1ecb1ca760e7ba51d958697eeefe06ff7ba1a3ad162b3e7978af3df30393bc13',
      actualCodeHash: '0x1ecb1ca760e7ba51d958697eeefe06ff7ba1a3ad162b3e7978af3df30393bc13',
      verified: true,
    },
    {
      key: 'perpsPublicLens',
      name: 'PerpsPublicLens',
      category: 'oracle',
      role: 'Aggregated public read-only lens for protocol status and tranche metrics.',
      address: '0xe1c9c8d3c068c49d933396c9591a55126f29d10e',
      expectedCodeHash: '0xd8db2962bbc7bfde5d38d8ef022325b9b06dca5a213b15159fb7a840cdc9d73b',
      actualCodeHash: '0xd8db2962bbc7bfde5d38d8ef022325b9b06dca5a213b15159fb7a840cdc9d73b',
      verified: true,
    },
    {
      key: 'emergencyPauseCoordinator',
      name: 'EmergencyPauseCoordinator',
      category: 'safety',
      role: 'Coordinates emergency pause actions across trading and LP settlement surfaces.',
      address: '0x736ead1f7a23f980e9fb51f023822c53d739ca2c',
      expectedCodeHash: '0x5dfb50481c44f5bb0e47c51ccf0d6d9b7b7eb4c93cc5e13ee23062aebe5594f6',
      actualCodeHash: '0x5dfb50481c44f5bb0e47c51ccf0d6d9b7b7eb4c93cc5e13ee23062aebe5594f6',
      verified: true,
    },
  ],
  releaseHistory: [
    {
      version: 'v1.2.3',
      status: 'superseded',
      date: '2026-09-01',
      summary: 'Initial Sepolia Core perpetuals release with deterministic trade receipts, VPI pricing, and September competition accounting.',
    },
    {
      version: 'v1.3.1',
      status: 'superseded',
      date: '2026-10-05',
      summary: 'Hardened LP epoch settlement math, Senior/Junior async vaults, and expanded execution policy controls.',
    },
    {
      version: 'v2.0.0',
      status: 'active',
      date: '2026-10-28',
      deploymentBlock: '323658447',
      summary: 'Schema-4 USDG settlement migration, bounded V3 signed order intents, PositionProtectionBook TP/SL triggers, and 30-contract runtime code-hash pinning.',
    },
  ],
}

function ProtocolStoryFrame({ manifest }: { manifest: ProtocolManifestResponse }) {
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
      latestIndexedBlock: Number(manifest.verifiedAtBlock),
      latestIndexedAt: manifest.observedAt,
      chainId: manifest.network.chainId,
    })
    client.setQueryData(['insights', 'divisions', 'live'], {
      status: 'ready',
      snapshotBlock: manifest.verifiedAtBlock,
      snapshotAt: manifest.observedAt,
      scoringRelease: { version: manifest.network.releaseVersion, assetSymbol: 'USDG' },
      walletCount: 42,
      standings: [],
    })
    client.setQueryData(['insights', 'protocol', 'manifest'], manifest)
    return client
  }, [manifest])

  return (
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/protocol']}>
        <main className="min-h-screen bg-app-bg px-4 py-8 text-content-primary sm:px-8">
          <div className="mx-auto max-w-7xl">
            <ProtocolPage />
          </div>
        </main>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta = {
  title: 'Insights/Stage 4 — Protocol Contracts & Parameters',
  component: ProtocolStoryFrame,
  parameters: {
    layout: 'fullscreen',
  },
} satisfies Meta<typeof ProtocolStoryFrame>

export default meta
type Story = StoryObj<typeof meta>

export const AllVerifiedLive: Story = {
  args: {
    manifest: BASE_MANIFEST,
  },
}

export const ParameterDriftDetected: Story = {
  args: {
    manifest: {
      ...BASE_MANIFEST,
      verification: {
        ...BASE_MANIFEST.verification,
        matchedParameters: 9,
        driftedParameters: 2,
      },
      parameters: BASE_MANIFEST.parameters.map((param) => {
        if (param.key === 'executionFeeBps') {
          return { ...param, liveValue: '6', status: 'drifted' as const }
        }
        if (param.key === 'fadMarginBps') {
          return { ...param, liveValue: '400', status: 'drifted' as const }
        }
        return param
      }),
    },
  },
}

export const EmergencyPauseAndMismatch: Story = {
  args: {
    manifest: {
      ...BASE_MANIFEST,
      safety: {
        ...BASE_MANIFEST.safety,
        phase: 'paused',
        tradingActive: false,
        oracleFrozen: true,
        housePoolPaused: true,
        lpEpochSettlementPaused: true,
        fadWindow: true,
        degradedMode: true,
      },
      verification: {
        ...BASE_MANIFEST.verification,
        verifiedCount: 11,
        mismatchCount: 1,
      },
      contracts: BASE_MANIFEST.contracts.map((c) =>
        c.key === 'orderRouter'
          ? {
              ...c,
              actualCodeHash: '0xdeadbeef00000000000000000000000000000000000000000000000000001337',
              verified: false,
            }
          : c,
      ),
    },
  },
}

export const RpcVerificationUnavailable: Story = {
  args: {
    manifest: {
      ...BASE_MANIFEST,
      rpcAvailable: false,
      safety: {
        available: false,
        phase: null,
        lastMarkPrice: null,
        lastMarkTime: null,
        tradingActive: null,
        withdrawalLive: null,
        lpEpochSettlementPaused: null,
        oracleFrozen: null,
        fadWindow: null,
        housePoolPaused: null,
        degradedMode: null,
      },
      verification: {
        totalContracts: 12,
        verifiedCount: 0,
        mismatchCount: 0,
        uncheckedCount: 12,
        matchedParameters: 0,
        driftedParameters: 0,
        unavailableParameters: 14,
      },
      parameters: BASE_MANIFEST.parameters.map((p) => ({
        ...p,
        liveValue: null,
        status: 'unavailable' as const,
      })),
      contracts: BASE_MANIFEST.contracts.map((c) => ({
        ...c,
        actualCodeHash: null,
        verified: null,
      })),
    },
  },
}
