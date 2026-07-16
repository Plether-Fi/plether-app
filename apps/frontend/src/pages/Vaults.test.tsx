import { fireEvent, render, screen, waitFor, within } from '@testing-library/react'
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
  switchToArbitrumSepolia: vi.fn(),
  vaultDeposit: vi.fn(),
  vaultReset: vi.fn(),
  vaultWithdraw: vi.fn(),
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
    const isQuoteRead = config.contracts?.[0]?.functionName === 'previewDeposit'

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

vi.mock('../hooks', () => ({
  useSwitchToArbitrumSepolia: () => ({
    switchToArbitrumSepolia: mocks.switchToArbitrumSepolia,
    isSwitching: false,
    switchError: null,
    clearSwitchError: mocks.clearSwitchError,
  }),
  useVaultTransactions: () => ({
    deposit: mocks.vaultDeposit,
    withdraw: mocks.vaultWithdraw,
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
  juniorMaxDeposit = 0,
  juniorMaxRequestDeposit = 10_000,
  seniorHighWaterMark = 72_000_000,
  seniorMaxDeposit = 0,
  seniorMaxRequestDeposit = 10_000,
  seniorPrincipal = 70_000_000,
  walletUsdc = 1_000,
}: {
  degradedMode?: boolean
  juniorMaxDeposit?: number
  juniorMaxRequestDeposit?: number
  seniorHighWaterMark?: number
  seniorMaxDeposit?: number
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
      true,
      false,
      degradedMode,
    ]),
    success(usdc(70_000_000)),
    success(shares(35_000_000)),
    success(shares(250)),
    success(usdc(seniorMaxDeposit)),
    success(usdc(400)),
    success(usdc(50_000_000)),
    success(shares(50_000_000)),
    success(shares(100)),
    success(usdc(juniorMaxDeposit)),
    success(usdc(150)),
    success(usdc(walletUsdc)),
    success(usdc(seniorMaxRequestDeposit)),
    success(usdc(juniorMaxRequestDeposit)),
    success(0n),
    success(0n),
  ]
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
  })

  it('shows both tranche choices and opens the Senior detail route', () => {
    renderVaults()

    expect(screen.getByRole('heading', { name: /Supply the balance sheet behind the market/i })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'View Senior Vault' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'View Junior Vault' })).toBeInTheDocument()

    fireEvent.click(screen.getByRole('link', { name: 'View Senior Vault' }))

    expect(screen.getByRole('heading', { name: 'Senior Vault', level: 1 })).toBeInTheDocument()
    expect(screen.getByRole('heading', { name: 'Deposit USDC' })).toBeInTheDocument()
    expect(screen.getAllByText('Availability unavailable').length).toBeGreaterThan(0)
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

    expect(screen.getByText('Live onchain')).toBeInTheDocument()
    const seniorCard = screen.getByRole('link', { name: 'View Senior Vault' })
    const juniorCard = screen.getByRole('link', { name: 'View Junior Vault' })
    expect(within(seniorCard).getByText('$70M')).toBeInTheDocument()
    expect(within(seniorCard).getByText('$2.0000')).toBeInTheDocument()
    expect(within(juniorCard).getByText('$50M')).toBeInTheDocument()
    expect(within(juniorCard).getByText('$1.0000')).toBeInTheDocument()

    const readConfig = mocks.readContractsArgs.mock.calls[0][0] as {
      contracts: {
        chainId: number
        functionName: string
      }[]
    }
    expect(readConfig.contracts).toHaveLength(16)
    expect(readConfig.contracts.every(({ chainId }) => chainId === 421614)).toBe(true)
    expect(readConfig.contracts.map(({ functionName }) => functionName)).toEqual([
      'getPoolLiquidityView',
      'totalAssets',
      'totalSupply',
      'balanceOf',
      'maxDeposit',
      'maxWithdraw',
      'totalAssets',
      'totalSupply',
      'balanceOf',
      'maxDeposit',
      'maxWithdraw',
      'balanceOf',
      'maxRequestDeposit',
      'maxRequestDeposit',
      'allowance',
      'allowance',
    ])
  })

  it('distinguishes a shared pending path from mixed tranche availability', () => {
    mocks.readContractsData = liveReadFixture({ seniorHighWaterMark: 70_000_000 })
    const { unmount } = renderVaults()
    expect(screen.getByText('Pending epoch')).toBeInTheDocument()

    unmount()
    mocks.readContractsData = liveReadFixture({
      seniorHighWaterMark: 70_000_000,
      seniorMaxDeposit: 1_000,
    })
    renderVaults()
    expect(screen.getByText('Varies by tranche')).toBeInTheDocument()
  })

  it('keeps funded pending requests disabled until the full epoch lifecycle is available', async () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({ seniorHighWaterMark: 70_000_000 })
    mocks.quoteContractsData = [success(shares(2)), success(shares(2))]

    renderVaults('/vaults/senior')
    fireEvent.change(screen.getByLabelText('Amount to deposit'), { target: { value: '2' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review deposit' }))

    const lifecycleButton = await screen.findByRole('button', { name: 'Lifecycle coming soon' })
    expect(lifecycleButton).toBeDisabled()
    expect(screen.getByText('Pending lifecycle not enabled')).toBeInTheDocument()
    expect(mocks.vaultDeposit).not.toHaveBeenCalled()
  })

  it('reviews valid amounts on the correct network and switches a wrong-network wallet', async () => {
    mocks.account.address = '0x1111111111111111111111111111111111111111'
    mocks.account.isConnected = true
    mocks.readContractsData = liveReadFixture({
      seniorHighWaterMark: 70_000_000,
      seniorMaxDeposit: 1_000,
    })
    mocks.quoteContractsData = [success(shares(50)), success(shares(50))]

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
    expect(mocks.quoteRefetch).toHaveBeenCalledTimes(1)
    fireEvent.click(screen.getByRole('button', { name: 'Approve & deposit' }))
    expect(mocks.vaultReset).toHaveBeenCalledTimes(1)
    expect(mocks.vaultDeposit).toHaveBeenCalledWith(usdc(100))

    unmount()
    mocks.chainId = 1
    renderVaults('/vaults/senior')
    fireEvent.click(screen.getByRole('button', { name: 'Switch to Arbitrum Sepolia' }))
    expect(mocks.switchToArbitrumSepolia).toHaveBeenCalledTimes(1)
    expect(mocks.appKitOpen).not.toHaveBeenCalled()
  })

  it('blocks unsafe or unavailable previews', () => {
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
    expect(screen.getByRole('button', { name: 'Review withdraw' })).toBeDisabled()
    expect(screen.getByText('Withdrawals below 1 USDC are only allowed for a complete residual exit.')).toBeInTheDocument()
    expect(screen.getByText('Withdrawals are unavailable while the protocol is in degraded mode.')).toBeInTheDocument()
  })
})
