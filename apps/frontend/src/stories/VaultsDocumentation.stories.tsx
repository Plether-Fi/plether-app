import type { Meta, StoryObj } from '@storybook/react-vite'
import { MemoryRouter } from 'react-router-dom'
import type { VaultHistory } from '../api'
import { createVaultAssetHistory } from './fixtures/vaultAssetHistory'
import {
  ActivityTab,
  OverviewTab,
  VaultDetailView,
  VaultPreviewModal,
  VaultsOverview,
  VAULT_TRANCHES,
} from '../pages/Vaults'

type VaultsOverviewProps = Parameters<typeof VaultsOverview>[0]
type VaultDetailViewProps = Parameters<typeof VaultDetailView>[0]
type VaultsSnapshot = VaultsOverviewProps['snapshot']
type VaultActivityViewState = VaultDetailViewProps['vaultActivity']
type VaultRequestsViewState = VaultDetailViewProps['vaultRequests']

const ASSETS = 1_000_000n
const SHARES = 1_000_000_000n
const MAX_UINT256 = (1n << 256n) - 1n
const FIXED_EPOCH_COUNTDOWN_SECONDS = 25 * 60 + 4

function assets(value: number): bigint {
  return BigInt(value) * ASSETS
}

function shares(value: number): bigint {
  return BigInt(value) * SHARES
}

function noop() {}

const snapshot = {
  status: 'live',
  pool: {
    totalAssets: assets(120_000_000),
    freeAssets: assets(90_000_000),
    withdrawalReservedAssets: assets(30_000_000),
    pendingRecapitalizationAssets: 0n,
    pendingTradingRevenueAssets: assets(480_000),
    seniorPrincipalAssets: assets(70_000_000),
    juniorPrincipalAssets: assets(50_000_000),
    seniorHighWaterMarkAssets: assets(70_000_000),
    currentTerminalDeficitAssets: 0n,
    markFresh: true,
    oracleFrozen: false,
    degradedMode: false,
    seniorImpaired: false,
    seniorImpairmentGapAssets: 0n,
    seniorPoolWithdrawCapAssets: assets(70_000_000),
    juniorPoolWithdrawCapAssets: 26_666_666_666_666n,
    maxSeniorExposureAssets: assets(100_000_000),
    maxSeniorShareBps: 7_500n,
    seniorRateBps: 800n,
    seniorDepositCapacityAssets: assets(25_000_000),
    reservedSeniorDepositAssets: assets(5_000_000),
    seniorReservationsWithinLimits: true,
    minTrancheDepositAssets: assets(1),
    markPrice: 100_000_000n,
    longOpenInterest: 40_000_000n * 10n ** 18n,
    shortOpenInterest: 30_000_000n * 10n ** 18n,
    longOpenCapacityAssets: assets(38_400_000),
    shortOpenCapacityAssets: assets(31_200_000),
  },
  walletAssets: assets(25_000),
  hasLivePoolData: true,
  tranches: {
    senior: {
      totalAssets: assets(70_000_000),
      totalSupply: shares(35_000_000),
      effectiveTotalSupply: shares(35_000_000),
      pendingMaintenanceFeeShares: 0n,
      maintenanceFeeAprBps: 0n,
      maintenanceFeeRecipient: '0x0000000000000000000000000000000000000000',
      userShares: shares(12_500),
      maxRequestDeposit: assets(25_000_000),
      maxRequestRedeem: shares(12_500),
      withdrawalCooldownEndsAt: 0n,
      allowance: assets(50_000),
      currentEpoch: 500_000n,
      nextRequestEpoch: 500_001n,
      nextRequestCutoffTime: 1_800_003_300n,
      depositBacklog: false,
      redeemBacklog: false,
      settlementLive: true,
      poolPaused: false,
      lpEpochSettlementPaused: false,
      frozenLpFeeBps: 0n,
      depositEnabled: true,
      withdrawEnabled: true,
      poolWithdrawCapAssets: assets(70_000_000),
      sharePrice: 2,
      hasCoreData: true,
      hasDepositData: true,
      hasUserData: true,
    },
    junior: {
      totalAssets: assets(50_000_000),
      totalSupply: shares(50_000_000),
      effectiveTotalSupply: shares(50_000_000) + 99_985_740_167_002n,
      pendingMaintenanceFeeShares: 99_985_740_167_002n,
      maintenanceFeeAprBps: 100n,
      maintenanceFeeRecipient: '0x0000000000000000000000000000000000000004',
      userShares: shares(8_500),
      maxRequestDeposit: MAX_UINT256,
      maxRequestRedeem: shares(8_500),
      withdrawalCooldownEndsAt: 0n,
      allowance: assets(50_000),
      currentEpoch: 500_000n,
      nextRequestEpoch: 500_001n,
      nextRequestCutoffTime: 1_800_003_300n,
      depositBacklog: false,
      redeemBacklog: false,
      settlementLive: true,
      poolPaused: false,
      lpEpochSettlementPaused: false,
      frozenLpFeeBps: 0n,
      depositEnabled: true,
      withdrawEnabled: true,
      poolWithdrawCapAssets: 26_666_666_666_666n,
      sharePrice: 50_000_000 / 50_099_985.740167,
      hasCoreData: true,
      hasDepositData: true,
      hasUserData: true,
    },
  },
  refresh: noop,
} satisfies VaultsSnapshot

const historyEnd = 1_800_000_000
const historyStart = historyEnd - 7 * 24 * 60 * 60

function historyPoint(
  timestamp: number,
  blockNumber: string,
  totalAssets: bigint,
  totalSupply: bigint,
  pendingFeeShares = 0n,
) {
  const effectiveTotalSupply = totalSupply + pendingFeeShares
  const sharePrice = (totalAssets + 1n) * 1_000_000_000_000_000_000_000n
    / (effectiveTotalSupply + 1_000n)

  return {
    timestamp,
    blockNumber,
    sharePrice: sharePrice.toString(),
    totalAssets: totalAssets.toString(),
    totalSupply: totalSupply.toString(),
    markFresh: true,
  }
}

function historySeries(
  startAssets: bigint,
  endAssets: bigint,
  totalSupply: bigint,
  pendingFeeShares = 0n,
) {
  return Array.from({ length: 169 }, (_, index) => {
    const elapsedHours = BigInt(index)
    const remainingHours = 168n - elapsedHours
    const totalAssets = (
      startAssets * remainingHours + endAssets * elapsedHours
    ) / 168n
    const blockNumber = 302_800_000 + Math.floor(index * 140_000 / 168)

    return historyPoint(
      historyStart + index * 60 * 60,
      blockNumber.toString(),
      totalAssets,
      totalSupply,
      pendingFeeShares,
    )
  })
}

const seniorSupply = shares(35_000_000)
const juniorSupply = shares(50_000_000)
const juniorPendingFeeShares = 99_985_740_167_002n
const seniorEndAssets = assets(70_000_000)
const seniorStartAssets = (seniorEndAssets + 1n) * 1_000n / 1_001n - 1n
const juniorEndAssets = assets(50_000_000)
const juniorStartAssets = (juniorEndAssets + 1n) * 400n / 399n - 1n

const history = {
  range: '7d',
  intervalSeconds: 3600,
  deployment: {
    chainId: 421614,
    housePool: '0x587c9863bb46fd5282a4ca2ea99589e3e8543962',
    seniorVault: '0xee755754319be980dd0f7df77107720485fe9d20',
    juniorVault: '0x1d4beed4d594cadfe4c3c5bdc8174b0fbfc4aaf0',
  },
  coverage: { start: historyStart, end: historyEnd, complete: true },
  senior: {
    apy7d: Math.pow(1.001, 365 / 7) - 1,
    return7d: 0.001,
    points: historySeries(seniorStartAssets, seniorEndAssets, seniorSupply),
  },
  junior: {
    apy7d: Math.pow(0.9975, 365 / 7) - 1,
    return7d: -0.0025,
    points: historySeries(
      juniorStartAssets,
      juniorEndAssets,
      juniorSupply,
      juniorPendingFeeShares,
    ),
  },
} satisfies VaultHistory

const assetHistory = createVaultAssetHistory()

const activity = {
  holders: [
    {
      address: '0x1111111111111111111111111111111111111111',
      currentNavAssets: assets(18_500_000),
      shareOfVaultNav: 45,
      seniorNavAssets: assets(15_000_000),
      juniorNavAssets: assets(3_500_000),
    },
    {
      address: '0x2222222222222222222222222222222222222222',
      currentNavAssets: assets(12_000_000),
      shareOfVaultNav: 30,
      seniorNavAssets: assets(10_000_000),
      juniorNavAssets: assets(2_000_000),
    },
    {
      address: '0x3333333333333333333333333333333333333333',
      currentNavAssets: assets(10_000_000),
      shareOfVaultNav: 25,
      seniorNavAssets: assets(8_000_000),
      juniorNavAssets: assets(2_000_000),
    },
  ],
  activity: [
    {
      id: 'senior-deposit-1',
      kind: 'deposit',
      tranche: 'senior',
      account: '0x4444444444444444444444444444444444444444',
      requestId: 500_000n,
      amountAssets: assets(25_000),
      amountIsEstimate: false,
      timestamp: '2027-01-15T07:40:46.000Z',
      blockNumber: 302_932_837,
      transactionHash: '0x1111111111111111111111111111111111111111111111111111111111111111',
    },
    {
      id: 'senior-withdraw-1',
      kind: 'withdraw',
      tranche: 'senior',
      account: '0x5555555555555555555555555555555555555555',
      requestId: 499_999n,
      amountAssets: assets(8_400),
      shares: shares(4_200),
      amountIsEstimate: true,
      timestamp: '2027-01-15T06:18:11.000Z',
      blockNumber: 302_925_100,
      transactionHash: '0x2222222222222222222222222222222222222222222222222222222222222222',
    },
  ],
  isLoading: false,
  isError: false,
} satisfies VaultActivityViewState

const emptyRequests = {
  depositRequests: [],
  redeemRequests: [],
  isLoading: false,
  discoveryError: false,
  discoveryStale: false,
  refresh: noop,
} satisfies VaultRequestsViewState

const pendingRequests = {
  depositRequests: [
    {
      requestId: 500_001n,
      targetTimestamp: 1_800_003_600,
      pendingAssets: assets(10_000),
      directRedeemableShares: 0n,
      pendingSharesEstimate: shares(5_000),
      claimableAssets: 0n,
      claimableShares: 0n,
      refundableAssets: 0n,
      matured: false,
    },
    {
      requestId: 499_998n,
      targetTimestamp: 1_799_992_800,
      activationTimestamp: 1_799_992_920,
      cooldownEndsAt: 1_799_996_520n,
      directRedeemableShares: shares(1_250),
      pendingAssets: 0n,
      pendingSharesEstimate: 0n,
      claimableAssets: assets(2_500),
      claimableShares: shares(1_250),
      refundableAssets: 0n,
      matured: true,
    },
  ],
  redeemRequests: [
    {
      requestId: 500_001n,
      targetTimestamp: 1_800_003_600,
      pendingShares: shares(800),
      pendingAssetsEstimate: 1_599_999_999n,
      claimableShares: 0n,
      claimableAssets: 0n,
      refundableShares: 0n,
      refundPending: false,
      matured: false,
    },
  ],
  isLoading: false,
  discoveryError: false,
  discoveryStale: false,
  refresh: noop,
} satisfies VaultRequestsViewState

function DocumentationVaults() {
  return null
}

const meta: Meta<typeof DocumentationVaults> = {
  title: 'Documentation/Vaults',
  component: DocumentationVaults,
  decorators: [
    (Story) => (
      <MemoryRouter initialEntries={['/vaults']}>
        <Story />
      </MemoryRouter>
    ),
  ],
  parameters: {
    layout: 'fullscreen',
    controls: { disable: true },
  },
}

export default meta
type Story = StoryObj<typeof meta>

function PageFrame({ children, maxWidth = 'max-w-7xl' }: {
  children: React.ReactNode
  maxWidth?: string
}) {
  return (
    <main className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className={`mx-auto ${maxWidth}`}>{children}</div>
    </main>
  )
}

export const Overview: Story = {
  render: () => (
    <PageFrame>
      <VaultsOverview
        snapshot={snapshot}
        history={history}
        epochCountdownSeconds={FIXED_EPOCH_COUNTDOWN_SECONDS}
      />
    </PageFrame>
  ),
}

export const SeniorVaultDetail: Story = {
  render: () => (
    <PageFrame>
      <VaultDetailView
        tranche={VAULT_TRANCHES.senior}
        snapshot={snapshot}
        history={history}
        assetHistory={assetHistory}
        isConnected
        isWrongNetwork={false}
        onConnect={noop}
        onSwitchNetwork={noop}
        isSwitchingNetwork={false}
        vaultActivity={activity}
        vaultRequests={emptyRequests}
        epochCountdownSeconds={FIXED_EPOCH_COUNTDOWN_SECONDS}
      />
    </PageFrame>
  ),
}

export const JuniorVaultDetail: Story = {
  render: () => (
    <PageFrame>
      <VaultDetailView
        tranche={VAULT_TRANCHES.junior}
        snapshot={snapshot}
        history={history}
        assetHistory={assetHistory}
        isConnected
        isWrongNetwork={false}
        onConnect={noop}
        onSwitchNetwork={noop}
        isSwitchingNetwork={false}
        vaultActivity={activity}
        vaultRequests={emptyRequests}
        epochCountdownSeconds={FIXED_EPOCH_COUNTDOWN_SECONDS}
      />
    </PageFrame>
  ),
}

export const SeniorOverviewSection: Story = {
  render: () => (
    <PageFrame maxWidth="max-w-6xl">
      <OverviewTab
        tranche={VAULT_TRANCHES.senior}
        liveData={snapshot.tranches.senior}
        snapshot={snapshot}
        isConnected
        epochCountdownSeconds={FIXED_EPOCH_COUNTDOWN_SECONDS}
      />
    </PageFrame>
  ),
}

export const JuniorOverviewSection: Story = {
  render: () => (
    <PageFrame maxWidth="max-w-6xl">
      <OverviewTab
        tranche={VAULT_TRANCHES.junior}
        liveData={snapshot.tranches.junior}
        snapshot={snapshot}
        isConnected
        epochCountdownSeconds={FIXED_EPOCH_COUNTDOWN_SECONDS}
      />
    </PageFrame>
  ),
}

export const Position: Story = {
  render: () => (
    <PageFrame maxWidth="max-w-5xl">
      <ActivityTab
        tranche={VAULT_TRANCHES.senior}
        liveData={snapshot.tranches.senior}
        snapshot={snapshot}
        isConnected
        isWrongNetwork={false}
        depositRequests={[]}
        redeemRequests={[]}
        requestsLoading={false}
        requestDiscoveryError={false}
        requestDiscoveryStale={false}
        onRefreshRequests={noop}
        onSwitchNetwork={noop}
      />
    </PageFrame>
  ),
}

export const PendingActivity: Story = {
  render: () => (
    <PageFrame maxWidth="max-w-5xl">
      <ActivityTab
        tranche={VAULT_TRANCHES.senior}
        liveData={snapshot.tranches.senior}
        snapshot={snapshot}
        isConnected
        isWrongNetwork={false}
        depositRequests={pendingRequests.depositRequests}
        redeemRequests={pendingRequests.redeemRequests}
        requestsLoading={false}
        requestDiscoveryError={false}
        requestDiscoveryStale={false}
        onRefreshRequests={noop}
        onSwitchNetwork={noop}
      />
    </PageFrame>
  ),
}

function ModalCanvas() {
  return <div className="min-h-screen bg-app-bg" />
}

export const DepositPreview: Story = {
  render: () => (
    <>
      <ModalCanvas />
      <VaultPreviewModal
        isOpen
        onClose={noop}
        onReset={noop}
        onViewRequest={noop}
        mode="deposit"
        tranche={VAULT_TRANCHES.senior}
        amount="10,000.00"
        estimatedShares={5_000}
        depositMode="Open for deposits"
        sharePrice={2}
        pendingActivationTimestamp={1_800_003_600}
        canSubmit
        needsApproval
        transactionStatus="idle"
        transactionPhase="idle"
        transactionSteps={[]}
        currentTransactionStep={-1}
        onSubmit={noop}
      />
    </>
  ),
}

export const WithdrawalPreview: Story = {
  render: () => (
    <>
      <ModalCanvas />
      <VaultPreviewModal
        isOpen
        onClose={noop}
        onReset={noop}
        onViewRequest={noop}
        mode="withdraw"
        tranche={VAULT_TRANCHES.senior}
        amount="1,250.00"
        estimatedShares={625.000000001}
        depositMode="Open for deposits"
        sharePrice={2}
        pendingActivationTimestamp={1_800_003_600}
        canSubmit
        needsApproval={false}
        transactionStatus="idle"
        transactionPhase="idle"
        transactionSteps={[]}
        currentTransactionStep={-1}
        onSubmit={noop}
      />
    </>
  ),
}
