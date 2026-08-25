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
}))

vi.mock('@reown/appkit/react', () => ({
  useAppKit: () => ({
    open: mocks.appKitOpen,
  }),
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
  syncAppKitModalStyleOverrides: vi.fn(),
}))

vi.mock('../api', () => ({
  usePerpsVaultHistory: () => ({
    data: mocks.vaultHistory === undefined ? undefined : { data: mocks.vaultHistory },
  }),
}))

vi.mock('../hooks', () => ({
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
  juniorMaxRequestDeposit = 10_000,
  seniorHighWaterMark = 72_000_000,
  seniorMaxRequestDeposit = 10_000,
  seniorPrincipal = 70_000_000,
  walletUsdc = 1_000,
}: {
  degradedMode?: boolean
  juniorMaxRequestDeposit?: number
  seniorHighWaterMark?: number
  seniorMaxRequestDeposit?: number
  seniorPrincipal?: number
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
      0n,
      true,
      false,
      degradedMode,
    ]),
    { status: 'failure' as const },
    success(usdc(70_000_000)),
    success(shares(35_000_000)),
    success(shares(250)),
    success(usdc(seniorMaxRequestDeposit)),
    success(shares(250)),
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
    success([0, 10n ** 18n, 0n, false, false, true, true]),
    success([0n, 40n * 10n ** 18n, 0n, 0n]),
    success([0n, 30n * 10n ** 18n, 0n, 0n]),
    success([0n, 5n * 10n ** 17n, 0n, 0n, 0n, 0n, 0n, 0n]),
    success([usdc(70_000_000), shares(35_000_000), 2n * 10n ** 18n, usdc(70_000_000), 25n, true, true, false]),
    success([usdc(50_000_000), shares(50_000_000), 10n ** 18n, usdc(20_000_000), 25n, true, true, false]),
    success(['0x0000000000000000000000000000000000000001', 500_000n, 500_000n, 500_001n, 1_800_003_300n, 0n, 0n, 0n, 0n, false, false, true, false]),
    success(['0x0000000000000000000000000000000000000002', 500_000n, 500_000n, 500_001n, 1_800_003_300n, 0n, 0n, 0n, 0n, false, false, true, false]),
    success([usdc(70_000_000), usdc(50_000_000), usdc(70_000_000), usdc(20_000_000)]),
    success(usdc(100_000_000)),
    success(7_500n),
    success(usdc(30_000_000)),
    success(usdc(5_000_000)),
    success(true),
    success(usdc(1)),
  ]
}

function completeHistoryFixture() {
  const start = 1_800_000_000
  const end = start + 7 * 24 * 60 * 60
  const point = (timestamp: number, blockNumber: string, price: string) => ({
    timestamp,
    blockNumber,
    sharePrice: price,
    totalAssets: '1000000000000',
    totalSupply: '1000000000000',
  })

  return {
    range: '7d' as const,
    intervalSeconds: 3600 as const,
    deployment: {
      chainId: 421614,
      housePool: '0xFA654f4c548130F09C3Fb962AbD4bE32c0357C18',
      seniorVault: '0x4bAb5448C1BD9A48B978ABcb014F1a8F80F100A8',
      juniorVault: '0x7258d6E91fbEFB8a16751575adbe9bBB3086D458',
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
    mocks.vaultHistory = undefined
  })

  it('shows both tranche choices and opens the Senior detail route', () => {
    renderVaults()

    expect(screen.getByRole('heading', { name: /Supply the balance sheet behind the market/i })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'View Senior Vault' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'View Junior Vault' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: /Read the LP guide/i })).toHaveAttribute(
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

    fireEvent.click(screen.getByRole('link', { name: 'View Senior Vault' }))

    expect(screen.getByRole('heading', { name: 'Senior Vault', level: 1 })).toBeInTheDocument()
    expect(screen.getByRole('heading', { name: 'Deposit USDC' })).toBeInTheDocument()
    expect(screen.getAllByText('Availability unavailable').length).toBeGreaterThan(0)
  })

  it('counts down to the next shared hourly vault epoch', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-08-20T12:34:56Z'))

    const { unmount } = renderVaults()

    expect(screen.getByText('Request cutoff')).toBeInTheDocument()
    expect(screen.getByText('25:04')).toBeInTheDocument()
    expect(
      screen.getByText('Requests after this timer join the following hourly batch'),
    ).toBeInTheDocument()

    act(() => {
      vi.advanceTimersByTime(1_000)
    })
    expect(screen.getByText('25:03')).toBeInTheDocument()

    unmount()
    vi.useRealTimers()
  })

  it('exposes detail tabs, deposit and withdrawal modes, and wallet connection', () => {
    renderVaults('/vaults/junior')

    expect(screen.getByRole('button', { name: 'deposit' })).toHaveAttribute('aria-pressed', 'true')
    fireEvent.click(screen.getByRole('tab', { name: 'Risk' }))
    expect(screen.getByText('Junior Vault is not principal-protected')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: /withdraw/i }))
    expect(screen.getByRole('button', { name: 'withdraw' })).toHaveAttribute('aria-pressed', 'true')
    expect(screen.getByRole('heading', { name: 'Withdraw USDC' })).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Connect wallet' }))
    expect(mocks.appKitOpen).toHaveBeenCalledTimes(1)
    expect(mocks.clearSwitchError).toHaveBeenCalledTimes(1)
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
    expect(screen.getByRole('region', { name: 'Capacity and capital waterfall' })).toBeInTheDocument()
    expect(screen.getByText('HousePool liquidity')).toBeInTheDocument()
    expect(screen.getByRole('heading', { name: 'Capital waterfall' })).toBeInTheDocument()
    expect(screen.getByText('Estimated LONG capacity')).toBeInTheDocument()
    expect(screen.getByText('Estimated SHORT capacity')).toBeInTheDocument()
    expect(screen.getByText('Junior · first loss')).toBeInTheDocument()
    expect(screen.getByText('Senior · last loss')).toBeInTheDocument()
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
    expect(readConfig.contracts).toHaveLength(34)
    expect(readConfig.contracts.every(({ chainId }) => chainId === 421614)).toBe(true)
    expect(readConfig.contracts.map(({ functionName }) => functionName)).toEqual([
      'getPoolLiquidityView',
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
    ])
    expect((readConfig.contracts[17] as { args?: bigint[] }).args).toEqual([10n ** 27n])
    expect((readConfig.contracts[18] as { args?: bigint[] }).args).toEqual([10n ** 27n])
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
    expect(screen.queryByRole('tab', { name: 'Performance' })).not.toBeInTheDocument()
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
    fireEvent.pointerLeave(seniorMiniChart, { pointerType: 'mouse' })
    expect(seniorCard.querySelector('[data-vault-chart-tooltip]')).not.toBeInTheDocument()
    expect(within(seniorCard).getByText(/seven-day share price changed \+0.10%/i)).toBeInTheDocument()
    expect(within(juniorCard).getByText(/seven-day share price changed -0.25%/i)).toBeInTheDocument()

    overview.unmount()
    renderVaults('/vaults/senior')
    expect(screen.getByRole('tab', { name: 'Performance' })).toBeInTheDocument()
    const seniorApyValues = screen.getAllByText('+5.24%')
    expect(seniorApyValues.length).toBeGreaterThanOrEqual(2)
    seniorApyValues.forEach((value) => {
      expect(value).toHaveClass('text-positive')
    })
    expect(screen.getByText('+0.10% actual 7d return')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('tab', { name: 'Performance' }))
    const chart = screen.getByRole('img', {
      name: 'Senior Vault interactive seven-day share price chart',
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
    fireEvent.keyDown(chart, { key: 'Home' })
    expect(screen.getByText('0.00% since start')).toBeInTheDocument()
    fireEvent.keyDown(chart, { key: 'ArrowRight' })
    expect(screen.getByText('+0.05% since start')).toBeInTheDocument()
    fireEvent.pointerDown(chart, { pointerType: 'touch', clientX: 620 })
    expect(screen.getByText('+0.10% since start')).toBeInTheDocument()
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

  it('rejects otherwise complete history from a different deployment', () => {
    mocks.vaultHistory = {
      ...completeHistoryFixture(),
      deployment: {
        ...completeHistoryFixture().deployment,
        housePool: '0x0000000000000000000000000000000000000001',
      },
    }

    renderVaults('/vaults/junior')
    expect(screen.queryByRole('tab', { name: 'Performance' })).not.toBeInTheDocument()
    expect(screen.queryByText(/7d APY/i)).not.toBeInTheDocument()
  })

  it('routes deposits through the epoch queue and omits obsolete detail labels', () => {
    mocks.readContractsData = liveReadFixture({ seniorHighWaterMark: 70_000_000 })
    renderVaults('/vaults/senior')
    expect(screen.getAllByText('Queued deposits open').length).toBeGreaterThan(0)
    expect(screen.getByText(/Current Senior capacity:/)).toBeInTheDocument()
    expect(screen.getByText(/Current request window closes in/)).toBeInTheDocument()
    expect(screen.getByText('5 minutes before each hour')).toBeInTheDocument()
    expect(screen.queryByText('Immediate deposit max')).not.toBeInTheDocument()
    expect(screen.queryByText('Lower relative risk')).not.toBeInTheDocument()
    expect(screen.queryByText('Live onchain')).not.toBeInTheDocument()
    expect(screen.queryByText('Onchain action')).not.toBeInTheDocument()
    expect(screen.queryByText(/2 epoch IDs/i)).not.toBeInTheDocument()
    expect(screen.queryByText(/Withdrawal cooldown/i)).not.toBeInTheDocument()
  })

  it('submits a funded pending request when the vault selects the epoch route', async () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({ seniorHighWaterMark: 70_000_000 })
    mocks.quoteContractsData = [success(shares(2)), success(shares(2))]

    renderVaults('/vaults/senior')
    fireEvent.change(screen.getByLabelText('Amount to deposit'), { target: { value: '2' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review deposit' }))

    const queueButton = await screen.findByRole('button', { name: 'Approve & queue' })
    expect(queueButton).toBeEnabled()
    fireEvent.click(queueButton)
    expect(mocks.vaultRequestDeposit).toHaveBeenCalledWith(usdc(2))
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
    fireEvent.click(screen.getByRole('tab', { name: 'Your position' }))
    fireEvent.click(screen.getByRole('button', { name: 'Cancel request' }))
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
    fireEvent.click(screen.getByRole('tab', { name: 'Your position' }))
    expect(screen.queryByRole('button', { name: /Finalize/i })).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Claim shares' }))
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
    fireEvent.click(screen.getByRole('tab', { name: 'Your position' }))
    fireEvent.click(screen.getByRole('button', { name: 'Claim USDC' }))
    expect(mocks.vaultClaimRedeem).toHaveBeenCalledWith(500_003n, shares(5))
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
      expect(screen.getByRole('dialog', { name: 'Deposit preview' })).toBeInTheDocument()
    })
    const preview = screen.getByRole('dialog', { name: 'Deposit preview' })
    expect(within(preview).getByText('7d realized APY')).toBeInTheDocument()
    expect(within(preview).getByText('+5.24%')).toBeInTheDocument()
    expect(mocks.quoteRefetch).toHaveBeenCalledTimes(1)
    fireEvent.click(screen.getByRole('button', { name: 'Approve & queue' }))
    expect(mocks.vaultReset).toHaveBeenCalledTimes(1)
    expect(mocks.vaultRequestDeposit).toHaveBeenCalledWith(usdc(100))

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
    expect(screen.getByRole('button', { name: 'Review withdraw' })).toBeEnabled()
    expect(screen.queryByText(/Withdrawals below 1 USDC/i)).not.toBeInTheDocument()
    expect(screen.queryByText(/Withdrawals are unavailable while the protocol is in degraded mode/i)).not.toBeInTheDocument()
  })
})
