import { act, fireEvent, render, screen, waitFor, within } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { Vaults } from './Vaults'

const mocks = vi.hoisted(() => ({
  account: {
    address: undefined as `0x${string}` | undefined,
    isConnected: false,
  },
  appKitOpen: vi.fn(),
  chainId: 421614,
  clearSwitchError: vi.fn(),
  quoteContractsData: undefined as readonly {
    status: 'failure' | 'success'
    result?: unknown
  }[] | undefined,
  quoteRefetch: vi.fn(),
  readContractsArgs: vi.fn(),
  readContractsData: undefined as readonly {
    status: 'failure' | 'success'
    result?: unknown
  }[] | undefined,
  refetch: vi.fn(),
  depositRequests: [] as {
    requestId: bigint
    targetTimestamp: number
    pendingAssets: bigint
    pendingSharesEstimate: bigint
    claimableAssets: bigint
    claimableShares: bigint
    refundableAssets: bigint
    matured: boolean
  }[],
  redeemRequests: [] as {
    requestId: bigint
    targetTimestamp: number
    pendingShares: bigint
    pendingAssetsEstimate: bigint
    claimableShares: bigint
    claimableAssets: bigint
    refundableShares: bigint
    refundPending: boolean
    matured: boolean
  }[],
  pendingRefresh: vi.fn(),
  pendingDiscoveryError: false,
  vaultHolders: [] as {
    address: `0x${string}`
    currentNavUsdc: bigint
    shareOfVaultNav: number
    seniorNavUsdc: bigint
    juniorNavUsdc: bigint
  }[],
  vaultOverviewActivity: [] as {
    id: string
    kind: 'deposit' | 'withdraw'
    tranche: 'senior' | 'junior'
    account: `0x${string}`
    requestId: bigint
    amountUsdc?: bigint
    shares?: bigint
    amountIsEstimate: boolean
    timestamp: string
    blockNumber: number
    transactionHash: `0x${string}`
  }[],
  vaultHistory: undefined as {
    range: '7d'
    intervalSeconds: 3600
    deployment: {
      chainId: number
      housePool: string
      seniorVault: string
      juniorVault: string
    }
    coverage: { start: number | null; end: number | null; complete: boolean }
    senior: {
      apy7d: number | null
      return7d: number | null
      points: {
        timestamp: number
        blockNumber: string
        markFresh: boolean
        sharePrice: string
        totalAssets: string
        totalSupply: string
      }[]
    }
    junior: {
      apy7d: number | null
      return7d: number | null
      points: {
        timestamp: number
        blockNumber: string
        markFresh: boolean
        sharePrice: string
        totalAssets: string
        totalSupply: string
      }[]
    }
  } | undefined,
  switchToArbitrumSepolia: vi.fn(),
  vaultCancelPendingDeposit: vi.fn(),
  vaultCancelRedeemRequest: vi.fn(),
  vaultClaimDepositShares: vi.fn(),
  vaultClaimRedeem: vi.fn(),
  vaultClaimRedeemRefund: vi.fn(),
  vaultRequestDeposit: vi.fn(),
  vaultRequestRedeem: vi.fn(),
  vaultReset: vi.fn(),
  scrollIntoView: vi.fn(),
}))

vi.mock('wagmi', () => ({
  useAccount: () => mocks.account,
  useChainId: () => mocks.chainId,
  useReadContracts: (args: unknown) => {
    mocks.readContractsArgs(args)
    const config = args as {
      contracts?: readonly {
        functionName?: string
      }[]
    }
    const isQuoteRead = config.contracts?.[0]?.functionName === 'estimateDepositShares'

    return {
      data: isQuoteRead ? mocks.quoteContractsData : mocks.readContractsData,
      isLoading: false,
      refetch: isQuoteRead ? mocks.quoteRefetch : mocks.refetch,
    }
  },
}))

vi.mock('../config/wagmi', () => ({
  openAppKit: mocks.appKitOpen,
}))

vi.mock('../api', () => ({
  usePerpsVaultHistory: () => ({
    data: mocks.vaultHistory === undefined ? undefined : { data: mocks.vaultHistory },
  }),
}))

vi.mock('../hooks', () => ({
  useVaultActivity: () => ({
    holders: mocks.vaultHolders,
    activity: mocks.vaultOverviewActivity,
    isLoading: false,
    isError: false,
    refetch: vi.fn(),
  }),
  useVaultRequests: () => ({
    depositRequests: mocks.depositRequests,
    redeemRequests: mocks.redeemRequests,
    isLoading: false,
    discoveryError: mocks.pendingDiscoveryError,
    refresh: mocks.pendingRefresh,
  }),
  useSwitchToArbitrumSepolia: () => ({
    switchToArbitrumSepolia: mocks.switchToArbitrumSepolia,
    isSwitching: false,
    switchError: null,
    clearSwitchError: mocks.clearSwitchError,
  }),
  useVaultTransactions: () => ({
    requestDeposit: mocks.vaultRequestDeposit,
    requestRedeem: mocks.vaultRequestRedeem,
    cancelPendingDeposit: mocks.vaultCancelPendingDeposit,
    cancelRedeemRequest: mocks.vaultCancelRedeemRequest,
    claimDepositShares: mocks.vaultClaimDepositShares,
    claimRedeem: mocks.vaultClaimRedeem,
    claimRedeemRefund: mocks.vaultClaimRedeemRefund,
    isRunning: false,
    isSuccess: false,
    isError: false,
    status: 'idle',
    phase: 'idle',
    steps: [],
    currentStepIndex: -1,
    hash: null,
    error: null,
    reset: mocks.vaultReset,
  }),
}))

function renderVaults(path = '/vaults') {
  return render(
    <MemoryRouter initialEntries={[path]}>
      <Routes>
        <Route path="/vaults" element={<Vaults />} />
        <Route path="/vaults/:trancheId" element={<Vaults />} />
      </Routes>
    </MemoryRouter>
  )
}

function usdc(value: number): bigint {
  return BigInt(value) * 1_000_000n
}

function shares(value: number): bigint {
  return BigInt(value) * 1_000_000_000n
}

function success(result: unknown) {
  return { status: 'success' as const, result }
}

function liveReadFixture({
  degradedMode = false,
  markFresh = true,
  oracleFrozen = false,
  terminalDeficit = 0,
  juniorMaxRequestDeposit = 10_000,
  seniorHighWaterMark = 72_000_000,
  seniorMaxRequestDeposit = 10_000,
  seniorPrincipal = 70_000_000,
  settlementPaused = false,
  poolPaused = false,
  juniorMaintenanceFeeAprBps = 200,
  juniorPendingMaintenanceFeeShares = 0,
  seniorUserShares = 250,
  seniorMaxRequestRedeem = seniorUserShares,
  seniorLastDepositTime = 0,
  seniorWithdrawalCooldown = 3_600,
  seniorRateBps = 800,
  juniorLastDepositTime = 0,
  juniorWithdrawalCooldown = 3_600,
  walletUsdc = 1_000,
}: {
  degradedMode?: boolean
  markFresh?: boolean
  oracleFrozen?: boolean
  terminalDeficit?: number
  juniorMaxRequestDeposit?: number
  seniorHighWaterMark?: number
  seniorMaxRequestDeposit?: number
  seniorPrincipal?: number
  settlementPaused?: boolean
  poolPaused?: boolean
  juniorMaintenanceFeeAprBps?: number
  juniorPendingMaintenanceFeeShares?: number
  seniorUserShares?: number
  seniorMaxRequestRedeem?: number
  seniorLastDepositTime?: number
  seniorWithdrawalCooldown?: number
  seniorRateBps?: number
  juniorLastDepositTime?: number
  juniorWithdrawalCooldown?: number
  walletUsdc?: number
} = {}) {
  return [
    success([
      usdc(120_000_000),
      usdc(90_000_000),
      usdc(30_000_000),
      0n,
      0n,
      usdc(seniorPrincipal),
      usdc(50_000_000),
      usdc(seniorHighWaterMark),
      usdc(terminalDeficit),
      markFresh,
      oracleFrozen,
      degradedMode,
    ]),
    success(usdc(70_000_000)),
    success(shares(35_000_000)),
    success(shares(seniorUserShares)),
    success(usdc(seniorMaxRequestDeposit)),
    success(shares(seniorMaxRequestRedeem)),
    success(usdc(50_000_000)),
    success(shares(50_000_000)),
    success(shares(100)),
    success(usdc(juniorMaxRequestDeposit)),
    success(shares(100)),
    success(usdc(walletUsdc)),
    success(0n),
    success(0n),
    success([500_001n, 1_800_003_300n]),
    success([500_001n, 1_800_003_300n]),
    success(2n * 10n ** 24n),
    success(10n ** 24n),
    success([0n, 10n ** 18n, 0n, false, false, true, true, settlementPaused]),
    success([0n, 40n * 10n ** 18n, 0n, 0n]),
    success([0n, 30n * 10n ** 18n, 0n, 0n]),
    success([0n, 5n * 10n ** 17n, 0n, 0n, 0n, 0n, 0n, 0n]),
    success([
      usdc(70_000_000),
      shares(35_000_000),
      shares(35_000_000),
      0n,
      0n,
      '0x0000000000000000000000000000000000000003',
      2n * 10n ** 15n,
      usdc(70_000_000),
      25n,
      !poolPaused && !oracleFrozen && !degradedMode && terminalDeficit === 0 && markFresh,
      true,
      oracleFrozen,
    ]),
    success([
      usdc(50_000_000),
      shares(50_000_000),
      shares(50_000_000 + juniorPendingMaintenanceFeeShares),
      shares(juniorPendingMaintenanceFeeShares),
      BigInt(juniorMaintenanceFeeAprBps),
      '0x0000000000000000000000000000000000000004',
      10n ** 15n,
      usdc(20_000_000),
      25n,
      !poolPaused && !oracleFrozen && !degradedMode && terminalDeficit === 0 && markFresh,
      true,
      oracleFrozen,
    ]),
    success(['0x0000000000000000000000000000000000000001', 500_000n, 500_000n, 500_001n, 1_800_003_300n, 0n, 0n, 0n, 0n, false, false, true, poolPaused, settlementPaused]),
    success(['0x0000000000000000000000000000000000000002', 500_000n, 500_000n, 500_001n, 1_800_003_300n, 0n, 0n, 0n, 0n, false, false, true, poolPaused, settlementPaused]),
    success([usdc(70_000_000), usdc(50_000_000), usdc(70_000_000), usdc(20_000_000)]),
    success(usdc(100_000_000)),
    success(7_500n),
    success(usdc(30_000_000)),
    success(usdc(5_000_000)),
    success(true),
    success(usdc(1)),
    success(BigInt(seniorLastDepositTime)),
    success(BigInt(seniorWithdrawalCooldown)),
    success(BigInt(juniorLastDepositTime)),
    success(BigInt(juniorWithdrawalCooldown)),
    success(BigInt(seniorRateBps)),
  ]
}

function completeHistoryFixture() {
  const start = 1_800_000_000
  const end = start + 7 * 24 * 60 * 60
  const point = (timestamp: number, blockNumber: string, price: string) => ({
    timestamp,
    blockNumber,
    markFresh: true,
    sharePrice: price,
    totalAssets: '1000000000000',
    totalSupply: '1000000000000',
  })

  return {
    range: '7d' as const,
    intervalSeconds: 3600 as const,
    deployment: {
      chainId: 421614,
      housePool: '0x86939a377A78EDe8EEe5445765ac77c9016E35E2',
      seniorVault: '0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710',
      juniorVault: '0xdf306B52eaC722D5994E2cc93D2818F391d68Adb',
    },
    coverage: { start, end, complete: true },
    senior: {
      apy7d: 0.0524,
      return7d: 0.001,
      points: [
        point(start, '100', '1000000000000000000'),
        point(start + 3 * 24 * 60 * 60, '200', '1000500000000000000'),
        point(end, '300', '1001000000000000000'),
      ],
    },
    junior: {
      apy7d: -0.125,
      return7d: -0.0025,
      points: [
        point(start, '100', '1000000000000000000'),
        point(start + 3 * 24 * 60 * 60, '200', '999000000000000000'),
        point(end, '300', '997500000000000000'),
      ],
    },
  }
}

describe('Vaults page', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    window.history.replaceState(null, '', '/')
    Object.defineProperty(HTMLElement.prototype, 'scrollIntoView', {
      configurable: true,
      value: mocks.scrollIntoView,
    })
    mocks.account.address = undefined
    mocks.account.isConnected = false
    mocks.chainId = 421614
    mocks.quoteContractsData = undefined
    mocks.quoteRefetch.mockImplementation(async () => ({
      data: mocks.quoteContractsData,
    }))
    mocks.readContractsData = undefined
    mocks.depositRequests = []
    mocks.redeemRequests = []
    mocks.pendingDiscoveryError = false
    mocks.vaultHolders = []
    mocks.vaultOverviewActivity = []
    mocks.vaultHistory = undefined
  })

  it('shows both tranche choices and opens the Senior detail route', () => {
    renderVaults()

    expect(screen.getByRole('heading', { name: /Provide liquidity that keeps the market running/i })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'View Senior Vault' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'View Junior Vault' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: /Learn how the vaults work/i })).toHaveAttribute(
      'href',
      'https://docs.plether.com/get-started/liquidity-provider-quickstart'
    )
    expect(screen.queryByRole('button', { name: 'Refresh' })).not.toBeInTheDocument()

    const seniorCard = screen.getByRole('link', { name: 'View Senior Vault' })
    const juniorCard = screen.getByRole('link', { name: 'View Junior Vault' })
    expect(seniorCard).toHaveClass('h-full')
    expect(juniorCard).toHaveClass('h-full')
    expect(seniorCard.querySelector('article')).toHaveClass('flex', 'h-full', 'flex-col')
    expect(juniorCard.querySelector('article')).toHaveClass('flex', 'h-full', 'flex-col')
    expect(within(seniorCard).queryByText('check')).not.toBeInTheDocument()
    expect(within(juniorCard).queryByText('check')).not.toBeInTheDocument()
    expect(within(seniorCard).getByText('Loss order')).toBeInTheDocument()
    expect(within(seniorCard).getByText('Return')).toBeInTheDocument()
    expect(within(seniorCard).getByText('Withdrawals')).toBeInTheDocument()
    expect(within(juniorCard).getByText('Loss order')).toBeInTheDocument()
    expect(within(juniorCard).getByText('Return')).toBeInTheDocument()
    expect(within(juniorCard).getByText('Withdrawals')).toBeInTheDocument()
    expect(within(seniorCard).getByText('Fee')).toBeInTheDocument()
    expect(within(seniorCard).getByText('Zero fees')).toBeInTheDocument()
    expect(within(juniorCard).getByText('Fee')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('link', { name: 'View Senior Vault' }))

    expect(screen.getByRole('heading', { name: 'Senior Vault', level: 1 })).toBeInTheDocument()
    expect(screen.getByRole('heading', { name: 'Deposit USDC' })).toBeInTheDocument()
    expect(screen.getAllByText('Deposit status unavailable').length).toBeGreaterThan(0)
  })

  it('counts down to the next shared hourly vault epoch', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-08-20T12:34:56Z'))

    const { unmount } = renderVaults()

    expect(screen.getByText('Next processing time in')).toBeInTheDocument()
    expect(screen.getByText('25:04')).toBeInTheDocument()
    expect(
      screen.getByText('Deposits and withdrawals submitted during the final five minutes are processed the following hour.'),
    ).toBeInTheDocument()

    act(() => {
      vi.advanceTimersByTime(1_000)
    })
    expect(screen.getByText('25:03')).toBeInTheDocument()

    unmount()
    vi.useRealTimers()
  })

  it('shows the current Junior maintenance fee on the overview card', () => {
    mocks.readContractsData = liveReadFixture({ juniorMaintenanceFeeAprBps: 275 })

    renderVaults()

    const juniorCard = screen.getByRole('link', { name: 'View Junior Vault' })
    expect(within(juniorCard).getByText('Fee')).toBeInTheDocument()
    expect(within(juniorCard).getByText('2.75% annual maintenance fee, paid by issuing new shares')).toBeInTheDocument()
  })

  it('shows tranche-scoped holder distribution and requests on an individual vault page', () => {
    mocks.readContractsData = liveReadFixture()
    mocks.vaultHolders = [{
      address: '0x1111111111111111111111111111111111111111',
      currentNavUsdc: usdc(12_500_000),
      shareOfVaultNav: 10.42,
      seniorNavUsdc: usdc(10_000_000),
      juniorNavUsdc: usdc(2_500_000),
    }]
    mocks.vaultOverviewActivity = [{
      id: 'request-1',
      kind: 'deposit',
      tranche: 'junior',
      account: '0x2222222222222222222222222222222222222222',
      requestId: 496_647n,
      amountUsdc: usdc(5_000),
      amountIsEstimate: false,
      timestamp: '2026-08-28T14:40:46.000Z',
      blockNumber: 302_932_837,
      transactionHash: '0x1111111111111111111111111111111111111111111111111111111111111111',
    }]

    renderVaults('/vaults/junior')

    const section = screen.getByRole('region', { name: 'Holders and recent activity' })
    expect(within(section).getByRole('heading', { name: 'Holder distribution' })).toBeInTheDocument()
    expect(within(section).getAllByText('2,500,000.00').length).toBeGreaterThan(0)
    expect(within(section).getAllByText('100.00%').length).toBeGreaterThan(0)
    expect(within(section).getByRole('columnheader', { name: '% of attributed value' })).toBeInTheDocument()
    expect(within(section).getByText(/attributed to its deposit and withdrawal requests/i)).toBeInTheDocument()
    expect(within(section).getByText(/Pending and refundable redeem shares remain attributed/i)).toBeInTheDocument()
    expect(within(section).getByRole('heading', { name: 'Recent deposits and withdrawals' })).toBeInTheDocument()
    expect(within(section).getAllByText('Deposit submitted').length).toBeGreaterThan(0)
    expect(within(section).getAllByText('5,000.00').length).toBeGreaterThan(0)
    expect(within(section).getAllByRole('link', { name: /0x1111/i }).length).toBeGreaterThan(0)
    expect(within(section).getAllByRole('link', { name: /View transaction/i }).length).toBeGreaterThan(0)
  })

  it('compacts large position metrics while retaining their exact accessible values', () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({ seniorUserShares: 10_000_000 })

    renderVaults('/vaults/senior')

    expect(screen.getAllByText('20M').length).toBeGreaterThan(0)
    expect(screen.getAllByText('10M').length).toBeGreaterThan(0)
    expect(screen.getAllByText('psLP').some((label) => label.classList.contains('border'))).toBe(true)
    expect(screen.getAllByLabelText('20,000,000 USDC').length).toBeGreaterThan(0)
    expect(screen.getAllByLabelText('10,000,000 psLP').length).toBeGreaterThan(0)
  })

  it('exposes detail sections, deposit and withdrawal modes, and wallet connection', () => {
    const juniorDetail = renderVaults('/vaults/junior')

    expect(screen.getByRole('button', { name: 'Overview' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Your position' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Activity' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'deposit' })).toHaveAttribute('aria-pressed', 'true')
    expect(screen.getByRole('button', { name: 'deposit' })).toHaveClass('bg-brand-peach', 'text-app-bg')
    expect(screen.queryByRole('tab', { name: 'Risk' })).not.toBeInTheDocument()
    expect(screen.queryByText('Return position')).not.toBeInTheDocument()
    expect(screen.queryByText('Risk position')).not.toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Read: How the shared pool protects Senior and Junior vaults' })).toHaveAttribute(
      'href',
      'https://docs.plether.com/how-plether-works/the-liquidity-pool-and-tranche-waterfall',
    )
    const timelocks = screen.getByRole('table', { name: 'Delayed settings changes' })
    expect(within(timelocks).getByText('Pool risk settings')).toBeInTheDocument()
    expect(within(timelocks).getByText('Junior fee settings')).toBeInTheDocument()
    expect(within(timelocks).getByText('Trading and pricing settings')).toBeInTheDocument()
    expect(within(timelocks).getAllByText('48 hours')).toHaveLength(3)
    expect(within(timelocks).getAllByRole('columnheader')).toHaveLength(2)
    expect(within(timelocks).queryByText('What it governs')).not.toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: /withdraw/i }))
    expect(screen.getByRole('button', { name: 'withdraw' })).toHaveAttribute('aria-pressed', 'true')
    expect(screen.getByRole('button', { name: 'withdraw' })).toHaveClass('bg-brand-peach', 'text-app-bg')
    expect(screen.getByRole('heading', { name: 'Withdraw USDC' })).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Connect wallet' }))
    expect(mocks.appKitOpen).toHaveBeenCalledTimes(1)
    expect(mocks.clearSwitchError).toHaveBeenCalledTimes(1)

    juniorDetail.unmount()
    renderVaults('/vaults/senior')
    expect(screen.queryByRole('tab', { name: 'Risk' })).not.toBeInTheDocument()
    expect(screen.queryByText('Return position')).not.toBeInTheDocument()
    expect(screen.queryByText('Risk position')).not.toBeInTheDocument()
  })

  it('uses the same activation threshold when scrolling down and up', async () => {
    let activityTop = 217
    const bounds = (top: number) => ({
      x: 0,
      y: top,
      top,
      right: 100,
      bottom: top + 100,
      left: 0,
      width: 100,
      height: 100,
      toJSON: () => ({}),
    }) as DOMRect
    const rectSpy = vi.spyOn(HTMLElement.prototype, 'getBoundingClientRect')
      .mockImplementation(function sectionBounds() {
        if (this.id === 'overview') return bounds(-200)
        if (this.id === 'your-position') return bounds(-50)
        if (this.id === 'activity') return bounds(activityTop)
        return bounds(1_000)
      })

    renderVaults('/vaults/senior')

    const positionButton = screen.getByRole('button', { name: 'Your position' })
    const activityButton = screen.getByRole('button', { name: 'Activity' })
    await waitFor(() => {
      expect(positionButton).toHaveAttribute('aria-current', 'location')
    })
    expect(activityButton).not.toHaveAttribute('aria-current')

    activityTop = 216
    fireEvent.scroll(window)
    expect(activityButton).toHaveAttribute('aria-current', 'location')

    activityTop = 217
    fireEvent.scroll(window)
    expect(positionButton).toHaveAttribute('aria-current', 'location')
    expect(activityButton).not.toHaveAttribute('aria-current')

    rectSpy.mockRestore()
  })

  it('handles an unknown tranche route', () => {
    renderVaults('/vaults/mezzanine')

    expect(screen.getByRole('heading', { name: 'Vault not found' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'View all vaults' })).toHaveAttribute('href', '/vaults')
  })

  it('maps the live HousePool and vault reads into tranche metrics', () => {
    mocks.readContractsData = liveReadFixture()

    renderVaults()

    expect(screen.queryByText('Live onchain')).not.toBeInTheDocument()
    const seniorCard = screen.getByRole('link', { name: 'View Senior Vault' })
    const juniorCard = screen.getByRole('link', { name: 'View Junior Vault' })
    expect(within(seniorCard).getByText('70M')).toBeInTheDocument()
    expect(within(seniorCard).getByText('2.0000')).toBeInTheDocument()
    expect(within(juniorCard).getByText('50M')).toBeInTheDocument()
    expect(within(juniorCard).getByText('1.0000')).toBeInTheDocument()
    expect(within(seniorCard).getAllByText('USDC').length).toBeGreaterThanOrEqual(2)
    expect(within(juniorCard).getAllByText('USDC').length).toBeGreaterThanOrEqual(2)

    expect(screen.queryByRole('button', { name: 'Pool liquidity details' })).not.toBeInTheDocument()
    expect(screen.getByRole('region', { name: 'Trading capacity and loss protection' })).toBeInTheDocument()
    expect(screen.getByText('Shared pool liquidity')).toBeInTheDocument()
    expect(screen.getByRole('heading', { name: 'Trading capacity and loss protection' })).toBeInTheDocument()
    expect(screen.getByText('Estimated LONG trading capacity')).toBeInTheDocument()
    expect(screen.getByText('Estimated SHORT trading capacity')).toBeInTheDocument()
    expect(screen.getByText('Junior · absorbs losses first')).toBeInTheDocument()
    expect(screen.getByText('Senior · protected by Junior')).toBeInTheDocument()
    expect(screen.queryByRole('heading', { name: 'One pool, two economic claims' })).not.toBeInTheDocument()
    expect(screen.queryByRole('heading', { name: 'When the pool loses' })).not.toBeInTheDocument()
    expect(screen.queryByRole('heading', { name: 'When the pool earns' })).not.toBeInTheDocument()
    expect(screen.queryByRole('heading', { name: 'When LPs withdraw' })).not.toBeInTheDocument()

    const readConfig = mocks.readContractsArgs.mock.calls[0][0] as {
      contracts: {
        chainId: number
        functionName: string
      }[]
      query: {
        refetchInterval: number
      }
    }
    expect(readConfig.query.refetchInterval).toBe(60_000)
    expect(readConfig.contracts).toHaveLength(38)
    expect(readConfig.contracts.every(({ chainId }) => chainId === 421614)).toBe(true)
    expect(readConfig.contracts.map(({ functionName }) => functionName)).toEqual([
      'getPoolLiquidityView',
      'totalAssets',
      'totalSupply',
      'balanceOf',
      'maxRequestDeposit',
      'maxRequestRedeem',
      'totalAssets',
      'totalSupply',
      'balanceOf',
      'maxRequestDeposit',
      'maxRequestRedeem',
      'balanceOf',
      'allowance',
      'allowance',
      'getRequestEpochWindow',
      'getRequestEpochWindow',
      'convertToAssets',
      'convertToAssets',
      'getProtocolStatus',
      'sides',
      'sides',
      'riskParams',
      'getSeniorTranche',
      'getJuniorTranche',
      'getTrancheQueues',
      'getTrancheQueues',
      'getPendingTrancheState',
      'maxSeniorExposureUsdc',
      'maxSeniorShareBps',
      'getSeniorDepositCapacity',
      'reservedSeniorDepositAssetsUsdc',
      'areSeniorDepositReservationsWithinLimits',
      'minTrancheDepositUsdc',
      'lastDepositTime',
      'DEPOSIT_COOLDOWN',
      'lastDepositTime',
      'DEPOSIT_COOLDOWN',
      'seniorRateBps',
    ])
    expect((readConfig.contracts[16] as { args?: bigint[] }).args).toEqual([10n ** 27n])
    expect((readConfig.contracts[17] as { args?: bigint[] }).args).toEqual([10n ** 27n])
  })

  it('omits every performance surface until complete deployment-matched history exists', () => {
    mocks.vaultHistory = {
      ...completeHistoryFixture(),
      coverage: { start: null, end: null, complete: false },
      senior: { apy7d: null, return7d: null, points: [] },
      junior: { apy7d: null, return7d: null, points: [] },
    }

    const overview = renderVaults()
    expect(screen.queryByText(/7d APY/i)).not.toBeInTheDocument()
    expect(screen.queryByText(/history unavailable|not indexed|indexer unavailable/i)).not.toBeInTheDocument()

    overview.unmount()
    renderVaults('/vaults/senior')
    expect(screen.queryByRole('button', { name: 'Performance' })).not.toBeInTheDocument()
    expect(screen.queryByText(/7d APY/i)).not.toBeInTheDocument()
    expect(screen.queryByText(/history unavailable|not indexed|indexer unavailable/i)).not.toBeInTheDocument()
  })

  it('shows signed APY and seven-day charts only for complete matching history', () => {
    mocks.vaultHistory = completeHistoryFixture()

    const overview = renderVaults()
    const seniorCard = screen.getByRole('link', { name: 'View Senior Vault' })
    const juniorCard = screen.getByRole('link', { name: 'View Junior Vault' })
    expect(within(seniorCard).getByText('+5.24%')).toHaveClass('text-positive')
    expect(within(juniorCard).getByText('-12.50%')).toHaveClass('text-brand-orange')
    expect(seniorCard.querySelector('path[data-vault-performance-series]')).toHaveAttribute(
      'stroke',
      '#FFAB96',
    )
    expect(juniorCard.querySelector('path[data-vault-performance-series]')).toHaveAttribute(
      'stroke',
      '#FFAB96',
    )
    const seniorMiniChart = within(seniorCard).getByRole('img', {
      name: 'Senior Vault seven-day share price chart',
    })
    expect(seniorMiniChart.querySelector('[data-vault-chart-axis="x"]')).toBeInTheDocument()
    expect(seniorMiniChart.querySelector('[data-vault-chart-axis="y"]')).toHaveAttribute('x1', '540')
    const overviewYTicks = [...seniorMiniChart.querySelectorAll('[data-vault-chart-y-tick]')]
    expect(overviewYTicks).toHaveLength(3)
    expect(overviewYTicks.map((tick) => tick.textContent)).toContain('0.00%')
    overviewYTicks.forEach((tick) => {
      expect(tick).toHaveAttribute('text-anchor', 'start')
      expect(tick.textContent).toContain('%')
    })
    fireEvent.pointerMove(seniorMiniChart, { pointerType: 'mouse', clientX: 291 })
    expect(seniorCard.querySelector('[data-vault-chart-tooltip]')).toHaveTextContent('1.0005')
    expect(seniorCard.querySelector('[data-vault-chart-tooltip]')).toHaveAttribute('data-placement', 'below')
    fireEvent.pointerMove(seniorMiniChart, { pointerType: 'mouse', clientX: 8 })
    expect(seniorCard.querySelector('[data-vault-chart-tooltip]')).toHaveAttribute('data-placement', 'right')
    fireEvent.pointerMove(seniorMiniChart, { pointerType: 'mouse', clientX: 540 })
    expect(seniorCard.querySelector('[data-vault-chart-tooltip]')).toHaveAttribute('data-placement', 'left')
    fireEvent.pointerLeave(seniorMiniChart, { pointerType: 'mouse' })
    expect(seniorCard.querySelector('[data-vault-chart-tooltip]')).not.toBeInTheDocument()
    expect(within(seniorCard).getByText(/seven-day share price changed \+0.10%/i)).toBeInTheDocument()
    expect(within(juniorCard).getByText(/seven-day share price changed -0.25%/i)).toBeInTheDocument()

    overview.unmount()
    renderVaults('/vaults/senior')
    expect(screen.getByRole('button', { name: 'Performance' })).toBeInTheDocument()
    const seniorApyValues = screen.getAllByText('+5.24%')
    expect(seniorApyValues.length).toBeGreaterThanOrEqual(2)
    seniorApyValues.forEach((value) => {
      expect(value).toHaveClass('text-positive')
    })
    expect(screen.getByText('+0.10% actual 7d return')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Performance' }))
    expect(mocks.scrollIntoView).toHaveBeenCalledWith({ behavior: 'smooth', block: 'start' })
    expect(window.location.hash).toBe('#performance')
    const chart = screen.getByRole('img', {
      name: 'Senior Vault interactive seven-day share price chart',
    })
    const chartContainer = chart.parentElement
    const bounds = (left: number, top: number, width: number, height: number) => ({
      x: left,
      y: top,
      top,
      right: left + width,
      bottom: top + height,
      left,
      width,
      height,
      toJSON: () => ({}),
    }) as DOMRect
    const rectSpy = vi.spyOn(Element.prototype, 'getBoundingClientRect')
      .mockImplementation(function chartBounds() {
        if (this === chart) return bounds(120, 220, 760, 256)
        if (this === chartContainer) return bounds(100, 200, 800, 280)
        return bounds(0, 0, 0, 0)
      })
    expect(chart.querySelector('[data-vault-chart-axis="x"]')).toBeInTheDocument()
    expect(chart.querySelector('[data-vault-chart-axis="y"]')).toBeInTheDocument()
    expect(screen.getAllByText('7d realized APY').length).toBeGreaterThanOrEqual(1)
    expect(screen.getByText('7d return')).toBeInTheDocument()
    expect(screen.getByText('Start share price')).toBeInTheDocument()
    expect(screen.getByText('Current share price')).toBeInTheDocument()
    expect(screen.queryByText(/30d/i)).not.toBeInTheDocument()

    fireEvent.focus(chart)
    expect(screen.getByText('+0.10% since start')).toBeInTheDocument()
    expect(screen.getByRole('status')).toHaveAttribute('data-placement', 'left')
    expect(parseFloat(screen.getByRole('status').style.left)).toBeCloseTo(90.27, 1)
    fireEvent.keyDown(chart, { key: 'Home' })
    expect(screen.getByText('0.00% since start')).toBeInTheDocument()
    expect(screen.getByRole('status')).toHaveAttribute('data-placement', 'right')
    fireEvent.keyDown(chart, { key: 'ArrowRight' })
    expect(screen.getByText('+0.05% since start')).toBeInTheDocument()
    fireEvent.pointerDown(chart, { pointerType: 'touch', clientX: 860 })
    expect(screen.getByText('+0.10% since start')).toBeInTheDocument()
    // This cursor remains closest to the first point after excluding the SVG's
    // aspect-ratio gutter; mapping the full element width selects the middle point.
    fireEvent.pointerMove(chart, { pointerType: 'mouse', clientX: 345 })
    expect(screen.getByText('0.00% since start')).toBeInTheDocument()
    fireEvent.resize(window)
    expect(chart).toHaveAttribute('viewBox', '0 0 712.5 240')
    expect(chart.querySelector('[data-vault-chart-axis="x"]')).toHaveAttribute('x2', '694.5')
    rectSpy.mockRestore()
  })

  it('explains accurate share-value factors with documentation links for both vaults', () => {
    mocks.vaultHistory = completeHistoryFixture()

    const seniorDetail = renderVaults('/vaults/senior')
    const seniorIncreaseFactors = screen.getByRole('heading', {
      name: 'What can increase share value',
    }).closest('section') as HTMLElement
    const seniorReduceFactors = screen.getByRole('heading', {
      name: 'What can reduce share value',
    }).closest('section') as HTMLElement
    expect(seniorIncreaseFactors.parentElement).toHaveClass('gap-3')
    expect(within(seniorIncreaseFactors).getByText('Targeted return funded by Junior')).toBeInTheDocument()
    expect(within(seniorIncreaseFactors).getByText('Recovery of earlier Senior losses')).toBeInTheDocument()
    expect(within(seniorIncreaseFactors).getByText('Frozen-price withdrawal surcharges')).toBeInTheDocument()
    expect(within(seniorReduceFactors).getByText('Liquidation shortfalls and bad debt after Junior is exhausted')).toBeInTheDocument()
    expect(within(seniorReduceFactors).queryByText(/Unpaid trader losses/i)).not.toBeInTheDocument()

    fireEvent.focus(screen.getByLabelText(
      'Learn more about Targeted return funded by Junior',
    ))
    const seniorTooltip = screen.getByRole('tooltip')
    expect(seniorTooltip).toHaveTextContent(/capped by what Junior can fund/i)
    expect(within(seniorTooltip).getByRole('link', {
      name: 'Read: How the shared pool protects Senior and Junior vaults',
    })).toHaveAttribute(
      'href',
      'https://docs.plether.com/how-plether-works/the-liquidity-pool-and-tranche-waterfall',
    )

    seniorDetail.unmount()
    renderVaults('/vaults/junior')
    const juniorIncreaseFactors = screen.getByRole('heading', {
      name: 'What can increase share value',
    }).closest('section') as HTMLElement
    const juniorReduceFactors = screen.getByRole('heading', {
      name: 'What can reduce share value',
    }).closest('section') as HTMLElement
    expect(within(juniorIncreaseFactors).getByText('Collectible marked and collected trader losses')).toBeInTheDocument()
    expect(within(juniorIncreaseFactors).getByText('Carry paid by traders to LPs')).toBeInTheDocument()
    expect(within(juniorIncreaseFactors).getByText('LP share of collected liquidation fees')).toBeInTheDocument()
    expect(within(juniorIncreaseFactors).getByText('Frozen-price withdrawal surcharges')).toBeInTheDocument()
    expect(within(juniorReduceFactors).getByText('Trader profits paid or owed')).toBeInTheDocument()
    expect(within(juniorReduceFactors).getByText('Annual maintenance fee dilution')).toBeInTheDocument()
    expect(within(juniorIncreaseFactors).queryByText(/Trading fees paid for positions/i)).not.toBeInTheDocument()

    fireEvent.focus(screen.getByLabelText(
      'Learn more about LP share of collected liquidation fees',
    ))
    const juniorTooltip = screen.getByRole('tooltip')
    expect(juniorTooltip).toHaveTextContent(/keeper receives the bounty/i)
    expect(juniorTooltip).not.toHaveTextContent(/protocol/i)
    expect(within(juniorTooltip).getByRole('link', {
      name: 'Read: Understand LP returns and share value',
    })).toHaveAttribute(
      'href',
      'https://docs.plether.com/providing-liquidity/understand-lp-returns-and-share-value',
    )
  })

  it('formats a near-zero negative APY without showing negative zero', () => {
    const history = completeHistoryFixture()
    history.senior.apy7d = -0.000001
    mocks.vaultHistory = history

    renderVaults()
    const seniorCard = screen.getByRole('link', { name: 'View Senior Vault' })
    const apyLabel = within(seniorCard).getByText('7d APY')
    const apyValue = apyLabel.parentElement?.querySelector('dd')
    expect(apyValue).toHaveTextContent('0.00%')
    expect(apyValue).not.toHaveTextContent('-0.00%')
  })

  it('does not add redundant freshness labels to carried-forward performance observations', () => {
    const history = completeHistoryFixture()
    history.senior.points[history.senior.points.length - 1].markFresh = false
    mocks.vaultHistory = history

    renderVaults('/vaults/senior')
    fireEvent.click(screen.getByRole('button', { name: 'Performance' }))

    expect(screen.queryByText(/last fresh valuation/i)).not.toBeInTheDocument()
    const chart = screen.getByRole('img', {
      name: 'Senior Vault interactive seven-day share price chart',
    })
    fireEvent.focus(chart)
    expect(screen.getByRole('status')).toHaveTextContent('1.001')
    expect(screen.getByRole('status')).toHaveTextContent('+0.10% since start')
    expect(screen.queryByText(/last fresh valuation/i)).not.toBeInTheDocument()
  })

  it('rejects otherwise complete history from a different deployment', () => {
    mocks.vaultHistory = {
      ...completeHistoryFixture(),
      deployment: {
        ...completeHistoryFixture().deployment,
        housePool: '0x0000000000000000000000000000000000000001',
      },
    }

    renderVaults('/vaults/junior')
    expect(screen.queryByRole('button', { name: 'Performance' })).not.toBeInTheDocument()
    expect(screen.queryByText(/7d APY/i)).not.toBeInTheDocument()
  })

  it('routes deposits through the epoch queue and omits obsolete detail labels', () => {
    mocks.readContractsData = liveReadFixture({ seniorHighWaterMark: 70_000_000 })
    renderVaults('/vaults/senior')
    expect(screen.getAllByText('Open for deposits').length).toBeGreaterThan(0)
    expect(screen.getByText(/Current Senior capacity:/)).toBeInTheDocument()
    expect(screen.getByText(/Current hourly window ends in/)).toBeInTheDocument()
    expect(screen.getByText('5 minutes before each hour')).toBeInTheDocument()
    expect(screen.queryByText('Immediate deposit max')).not.toBeInTheDocument()
    expect(screen.queryByText('Lower relative risk')).not.toBeInTheDocument()
    expect(screen.queryByText('Live onchain')).not.toBeInTheDocument()
    expect(screen.queryByText('Onchain action')).not.toBeInTheDocument()
    expect(screen.queryByText(/2 epoch IDs/i)).not.toBeInTheDocument()
    expect(screen.queryByText(/Withdrawal cooldown/i)).not.toBeInTheDocument()
  })

  it('shows the live withdrawal cooldown wherever withdrawal availability matters', () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({
      seniorHighWaterMark: 70_000_000,
      seniorMaxRequestRedeem: 0,
      seniorLastDepositTime: Math.floor(Date.now() / 1_000) - 600,
      seniorWithdrawalCooldown: 3_600,
    })

    renderVaults('/vaults/senior')

    expect(screen.getAllByText('Available in').length).toBeGreaterThan(0)
    expect(screen.getAllByLabelText(/seconds until withdrawals are available/i).length)
      .toBeGreaterThan(0)

    fireEvent.click(screen.getByRole('button', { name: 'withdraw' }))
    expect(screen.getByText('Withdrawal cooldown active')).toBeInTheDocument()
    expect(screen.getByText(/Receiving more psLP shares in your wallet restarts this one-hour cooldown/i))
      .toBeInTheDocument()
  })

  it('shows Junior maintenance fee metrics without the explanatory notice', () => {
    mocks.readContractsData = liveReadFixture({
      juniorMaintenanceFeeAprBps: 275,
      juniorPendingMaintenanceFeeShares: 500,
    })

    renderVaults('/vaults/junior')

    expect(screen.getByText('Annual vault fee')).toBeInTheDocument()
    expect(screen.getByText('2.75%')).toBeInTheDocument()
    const accruedFeeShares = screen.getByText('Accrued fee shares').closest('div')
    expect(accruedFeeShares).not.toBeNull()
    expect(within(accruedFeeShares!).getByText('500')).toBeInTheDocument()
    expect(within(accruedFeeShares!).getByText('pjLP')).toHaveClass('border', 'font-mono')
    expect(screen.getByRole('link', { name: /0x0000.*0004/i })).toHaveAttribute(
      'href',
      'https://sepolia.arbiscan.io/address/0x0000000000000000000000000000000000000004',
    )
    expect(screen.queryByText('How the Junior maintenance fee works')).not.toBeInTheDocument()
    expect(screen.queryByText(/paid by minting shares/i)).not.toBeInTheDocument()
  })

  it('shows the live Senior nominal APR from HousePool', () => {
    mocks.readContractsData = liveReadFixture({ seniorRateBps: 825 })

    renderVaults('/vaults/senior')

    expect(screen.getByText('Target nominal APR')).toBeInTheDocument()
    expect(screen.getByText('8.25%')).toBeInTheDocument()
    expect(screen.queryByRole('heading', { name: 'The market exposure' })).not.toBeInTheDocument()
  })

  it('shows Junior market exposure between Overview and Performance', () => {
    mocks.vaultHistory = completeHistoryFixture()
    mocks.readContractsData = liveReadFixture()
    renderVaults('/vaults/junior')

    const section = screen.getByRole('region', { name: 'The market exposure' })
    expect(section.previousElementSibling).toHaveAttribute('id', 'overview')
    expect(section.nextElementSibling).toHaveAttribute('id', 'performance')
    expect(screen.getByRole('button', { name: 'Market exposure' })).toBeInTheDocument()
    expect(within(section).getByText('41.67%')).toBeInTheDocument()
    expect(within(section).getByText('11.20%')).toBeInTheDocument()
    expect(within(section).getByText('2.40%')).toBeInTheDocument()
    expect(within(section).getByRole('img', { name: 'Cash unavailable for LP withdrawals: 25.00%' })).toBeInTheDocument()
  })

  it('withholds market sensitivity when the pool mark is stale', () => {
    mocks.readContractsData = liveReadFixture({ markFresh: false })
    renderVaults('/vaults/junior')

    const section = screen.getByRole('region', { name: 'The market exposure' })
    expect(within(section).getByText(/Market sensitivity is unavailable/)).toBeInTheDocument()
    expect(within(section).getByText(/live pricing is unavailable/)).toBeInTheDocument()
    expect(within(section).getByText('11.20%')).toBeInTheDocument()
  })

  it('surfaces a settlement hold without disabling new requests or existing request actions', () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({ settlementPaused: true })
    mocks.quoteContractsData = [success(shares(2)), success(shares(2))]

    const overview = renderVaults()
    expect(screen.getByText('Hourly processing paused')).toBeInTheDocument()
    expect(screen.getByText(/still submit deposits or withdrawals, move ready funds to your wallet/i)).toBeInTheDocument()
    overview.unmount()

    renderVaults('/vaults/junior')
    expect(screen.getAllByText('Hourly processing paused').length).toBeGreaterThan(0)
    fireEvent.change(screen.getByLabelText('Amount to deposit'), { target: { value: '2' } })
    expect(screen.getByRole('button', { name: 'Review deposit' })).toBeEnabled()
    fireEvent.click(screen.getByRole('button', { name: /withdraw/i }))
    fireEvent.change(screen.getByLabelText('Amount to withdraw'), { target: { value: '1' } })
    expect(screen.getByRole('button', { name: 'Review withdrawal' })).toBeEnabled()
  })

  it('keeps emergency pool pause asymmetric by disabling deposits but allowing redemptions', () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({ poolPaused: true })
    mocks.quoteContractsData = [success(shares(2)), success(shares(2))]

    renderVaults('/vaults/senior')
    fireEvent.change(screen.getByLabelText('Amount to deposit'), { target: { value: '2' } })
    expect(screen.getByRole('button', { name: 'Review deposit' })).toBeDisabled()
    expect(screen.getAllByText('Safety pause active').length).toBeGreaterThan(0)

    fireEvent.click(screen.getByRole('button', { name: /withdraw/i }))
    fireEvent.change(screen.getByLabelText('Amount to withdraw'), { target: { value: '1' } })
    expect(screen.getByRole('button', { name: 'Review withdrawal' })).toBeEnabled()
  })

  it('shows one specific deposit-closure reason and its reopening condition', () => {
    // Keep the assertion inside the recurring weekend closure. An earlier
    // countdown test enables fake timers for this file at a weekday instant.
    vi.setSystemTime(new Date('2026-08-30T20:00:00Z'))
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({
      oracleFrozen: true,
      seniorHighWaterMark: 70_000_000,
    })

    renderVaults('/vaults/senior')

    expect(screen.getAllByText('Deposits unavailable')).toHaveLength(1)
    expect(screen.getByText(/live FX market is closed/i)).toBeInTheDocument()
    expect(screen.getByText('Available again:')).toBeInTheDocument()
    expect(screen.getByText(/fresh live price is published/i)).toBeInTheDocument()
    expect(screen.queryByText(/not accepting new funded deposit requests/i)).not.toBeInTheDocument()
    expect(screen.queryByText(/current pool or vault safety state/i)).not.toBeInTheDocument()
    expect(screen.getAllByText('0.25% active')).toHaveLength(2)
    screen.getAllByText('0.25% active').forEach((value) => {
      expect(value).toHaveClass('text-brand-orange')
    })
    expect(screen.getByText(/Wait for pricing to resume before withdrawing when possible/i)).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: /withdraw/i }))
    expect(screen.getByText('Temporary withdrawal surcharge active')).toBeInTheDocument()
    expect(screen.getByText(/when your withdrawal is processed, more shares will be needed/i)).toBeInTheDocument()
    expect(screen.getByText(/wait for live pricing to return/i)).toBeInTheDocument()
  })

  it('submits a funded pending request when the vault selects the epoch route', async () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({ seniorHighWaterMark: 70_000_000 })
    mocks.quoteContractsData = [success(shares(2)), success(shares(2))]

    renderVaults('/vaults/senior')
    fireEvent.change(screen.getByLabelText('Amount to deposit'), { target: { value: '2' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review deposit' }))

    const queueButton = await screen.findByRole('button', { name: 'Confirm deposit' })
    expect(queueButton).toBeEnabled()
    fireEvent.click(queueButton)
    expect(mocks.vaultRequestDeposit).toHaveBeenCalledWith(usdc(2))
    expect(screen.getByRole('dialog', { name: 'Deposit flow' })).toBeInTheDocument()
  })

  it('manages asynchronous deposit and withdrawal requests without user finalization', () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({ seniorHighWaterMark: 70_000_000 })
    mocks.depositRequests = [{
      requestId: 500_002n,
      targetTimestamp: 1_800_007_200,
      pendingAssets: usdc(25),
      pendingSharesEstimate: shares(12),
      claimableAssets: 0n,
      claimableShares: 0n,
      refundableAssets: 0n,
      matured: false,
    }]
    const { unmount } = renderVaults('/vaults/senior')
    fireEvent.click(screen.getByRole('button', { name: 'Your position' }))
    const pendingDepositSection = screen.getByRole('heading', { name: 'Pending deposits' }).closest('section')
    expect(within(pendingDepositSection!).getByText('Expected processing')).toBeInTheDocument()
    expect(within(pendingDepositSection!).getByText('Estimated shares')).toBeInTheDocument()
    expect(within(pendingDepositSection!).queryByText('Eligible since')).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Cancel deposit' }))
    expect(mocks.vaultCancelPendingDeposit).not.toHaveBeenCalled()
    const cancelDepositFlow = screen.getByRole('dialog', { name: 'Cancel deposit flow' })
    expect(within(cancelDepositFlow).getByText('Review').closest('li')).toHaveAttribute('aria-current', 'step')
    expect(within(cancelDepositFlow).getByText('Complete')).toBeInTheDocument()
    fireEvent.click(within(cancelDepositFlow).getByRole('button', { name: 'Cancel deposit' }))
    expect(mocks.vaultCancelPendingDeposit).toHaveBeenCalledWith(500_002n)

    unmount()
    mocks.depositRequests = [{
      requestId: 500_002n,
      targetTimestamp: 1_800_007_200,
      pendingAssets: usdc(25),
      pendingSharesEstimate: shares(12),
      claimableAssets: usdc(25),
      claimableShares: shares(12),
      refundableAssets: 0n,
      matured: true,
    }]
    const claimView = renderVaults('/vaults/senior')
    fireEvent.click(screen.getByRole('button', { name: 'Your position' }))
    const claimDepositSection = screen.getByRole('heading', { name: 'Pending deposits' }).closest('section')
    expect(within(claimDepositSection!).getByText('Eligible since')).toBeInTheDocument()
    expect(within(claimDepositSection!).queryByText('Estimated shares')).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: /Finalize/i })).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Move shares to wallet' }))
    expect(mocks.vaultClaimDepositShares).not.toHaveBeenCalled()
    const claimDepositFlow = screen.getByRole('dialog', { name: 'Move shares flow' })
    expect(within(claimDepositFlow).getByText(
      /starts or restarts a one-hour cooldown for every psLP share/i
    )).toBeInTheDocument()
    fireEvent.click(within(claimDepositFlow).getByRole('button', { name: 'Move shares' }))
    expect(mocks.vaultClaimDepositShares).toHaveBeenCalledWith(500_002n)

    claimView.unmount()
    mocks.depositRequests = []
    mocks.redeemRequests = [{
      requestId: 500_003n,
      targetTimestamp: 1_800_010_800,
      pendingShares: 0n,
      pendingAssetsEstimate: usdc(10),
      claimableShares: shares(5),
      claimableAssets: usdc(10),
      refundableShares: 0n,
      refundPending: false,
      matured: true,
    }]
    renderVaults('/vaults/senior')
    fireEvent.click(screen.getByRole('button', { name: 'Your position' }))
    const claimWithdrawalSection = screen.getByRole('heading', { name: 'Pending withdrawals' }).closest('section')
    expect(within(claimWithdrawalSection!).getByText('Eligible since')).toBeInTheDocument()
    expect(within(claimWithdrawalSection!).queryByText('Estimated USDC')).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Move USDC to wallet' }))
    expect(mocks.vaultClaimRedeem).not.toHaveBeenCalled()
    const claimWithdrawalFlow = screen.getByRole('dialog', { name: 'Move USDC flow' })
    fireEvent.click(within(claimWithdrawalFlow).getByRole('button', { name: 'Move USDC' }))
    expect(mocks.vaultClaimRedeem).toHaveBeenCalledWith(500_003n, shares(5))
  })

  it('reviews recovery, withdrawal cancellation, and share reclaim actions in the step flow', () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({ seniorHighWaterMark: 70_000_000 })
    mocks.depositRequests = [{
      requestId: 500_004n,
      targetTimestamp: 1_800_007_200,
      pendingAssets: 0n,
      pendingSharesEstimate: 0n,
      claimableAssets: 0n,
      claimableShares: 0n,
      refundableAssets: usdc(7),
      matured: true,
    }]

    const recoveryView = renderVaults('/vaults/senior')
    fireEvent.click(screen.getByRole('button', { name: 'Your position' }))
    const recoverDepositSection = screen.getByRole('heading', { name: 'Pending deposits' }).closest('section')
    expect(within(recoverDepositSection!).getByText('Eligible since')).toBeInTheDocument()
    expect(within(recoverDepositSection!).queryByText('Estimated shares')).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Return USDC to wallet' }))
    expect(mocks.vaultCancelPendingDeposit).not.toHaveBeenCalled()
    const recoverFlow = screen.getByRole('dialog', { name: 'Return USDC flow' })
    expect(within(recoverFlow).getByText('Complete')).toBeInTheDocument()
    fireEvent.click(within(recoverFlow).getByRole('button', { name: 'Return USDC' }))
    expect(mocks.vaultCancelPendingDeposit).toHaveBeenCalledWith(500_004n)

    recoveryView.unmount()
    mocks.depositRequests = []
    mocks.redeemRequests = [{
      requestId: 500_005n,
      targetTimestamp: 1_800_010_800,
      pendingShares: shares(3),
      pendingAssetsEstimate: usdc(6),
      claimableShares: 0n,
      claimableAssets: 0n,
      refundableShares: 0n,
      refundPending: false,
      matured: false,
    }]

    const cancelView = renderVaults('/vaults/senior')
    fireEvent.click(screen.getByRole('button', { name: 'Your position' }))
    fireEvent.click(screen.getByRole('button', { name: 'Cancel withdrawal' }))
    expect(mocks.vaultCancelRedeemRequest).not.toHaveBeenCalled()
    const cancelFlow = screen.getByRole('dialog', { name: 'Cancel withdrawal flow' })
    fireEvent.click(within(cancelFlow).getByRole('button', { name: 'Cancel withdrawal' }))
    expect(mocks.vaultCancelRedeemRequest).toHaveBeenCalledWith(500_005n)

    cancelView.unmount()
    mocks.redeemRequests = [{
      requestId: 500_006n,
      targetTimestamp: 1_800_010_800,
      pendingShares: 0n,
      pendingAssetsEstimate: 0n,
      claimableShares: 0n,
      claimableAssets: 0n,
      refundableShares: shares(2),
      refundPending: true,
      matured: true,
    }]

    renderVaults('/vaults/senior')
    fireEvent.click(screen.getByRole('button', { name: 'Your position' }))
    const reclaimWithdrawalSection = screen.getByRole('heading', { name: 'Pending withdrawals' }).closest('section')
    expect(within(reclaimWithdrawalSection!).getByText('Eligible since')).toBeInTheDocument()
    expect(within(reclaimWithdrawalSection!).queryByText('Estimated USDC')).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Return shares to wallet' }))
    expect(mocks.vaultClaimRedeemRefund).not.toHaveBeenCalled()
    const reclaimFlow = screen.getByRole('dialog', { name: 'Return shares flow' })
    fireEvent.click(within(reclaimFlow).getByRole('button', { name: 'Return shares' }))
    expect(mocks.vaultClaimRedeemRefund).toHaveBeenCalledWith(500_006n)
  })

  it('reviews valid amounts on the correct network and switches a wrong-network wallet', async () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({ seniorHighWaterMark: 70_000_000 })
    mocks.quoteContractsData = [success(shares(50)), success(shares(50))]
    mocks.vaultHistory = completeHistoryFixture()

    const { unmount } = renderVaults('/vaults/senior')
    const amountInput = screen.getByPlaceholderText('0.00')
    const reviewButton = screen.getByRole('button', { name: 'Review deposit' })
    expect(reviewButton).toBeDisabled()

    fireEvent.change(amountInput, { target: { value: '100' } })
    expect(reviewButton).toBeEnabled()
    fireEvent.click(reviewButton)
    await waitFor(() => {
      expect(screen.getByRole('dialog', { name: 'Deposit flow' })).toBeInTheDocument()
    })
    const preview = screen.getByRole('dialog', { name: 'Deposit flow' })
    expect(within(preview).getByText('Review').closest('li')).toHaveAttribute('aria-current', 'step')
    expect(within(preview).getByText('Wallet')).toBeInTheDocument()
    expect(within(preview).getByText('Submitted')).toBeInTheDocument()
    expect(within(preview).getByText('7d realized APY')).toBeInTheDocument()
    expect(within(preview).getByText('+5.24%')).toBeInTheDocument()
    expect(within(preview).getByText('psLP')).toHaveClass('border', 'font-mono')
    expect(mocks.quoteRefetch).toHaveBeenCalledTimes(1)
    fireEvent.click(screen.getByRole('button', { name: 'Confirm deposit' }))
    expect(mocks.vaultReset).toHaveBeenCalledTimes(2)
    expect(mocks.vaultRequestDeposit).toHaveBeenCalledWith(usdc(100))
    expect(screen.getByRole('dialog', { name: 'Deposit flow' })).toBeInTheDocument()

    unmount()
    mocks.chainId = 1
    renderVaults('/vaults/senior')
    fireEvent.click(screen.getByRole('button', { name: 'Switch to Arbitrum Sepolia' }))
    expect(mocks.switchToArbitrumSepolia).toHaveBeenCalledTimes(1)
    expect(mocks.appKitOpen).not.toHaveBeenCalled()
  })

  it('blocks excess deposits but still permits withdrawal requests in degraded mode', () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({
      degradedMode: true,
      seniorHighWaterMark: 70_000_000,
      walletUsdc: 50,
    })
    mocks.quoteContractsData = [success(shares(1)), success(shares(1))]

    renderVaults('/vaults/junior')
    const amountInput = screen.getByPlaceholderText('0.00')
    fireEvent.change(amountInput, { target: { value: '100' } })
    expect(screen.getByText('Exceeds available balance.')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review deposit' })).toBeDisabled()

    fireEvent.click(screen.getByRole('button', { name: /withdraw/i }))
    fireEvent.change(screen.getByPlaceholderText('0.00'), { target: { value: '0.5' } })
    expect(screen.getByRole('button', { name: 'Review withdrawal' })).toBeEnabled()
    expect(screen.queryByText(/Withdrawals below 1 USDC/i)).not.toBeInTheDocument()
    expect(screen.queryByText(/Withdrawals are unavailable while the protocol is in degraded mode/i)).not.toBeInTheDocument()
  })
})
