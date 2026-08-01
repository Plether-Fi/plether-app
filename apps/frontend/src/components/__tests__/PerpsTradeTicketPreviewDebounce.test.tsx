import { act, fireEvent, render, screen, within } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { PerpsTradeTicket } from '../PerpsTradeTicket'

const wagmiMocks = vi.hoisted(() => ({
  useReadContracts: vi.fn(),
}))

vi.mock('../../perps-aa', () => {
  const address = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'

  return {
    findBundlerRequestError: () => undefined,
    usePerpsIdentity: () => ({
      status: 'ready',
      ownerAddress: address,
      accountAddress: address,
      chainId: 421614,
      isAaManifestConfigured: false,
      sponsorshipEnabled: false,
      manifest: null,
      identity: null,
      proposedIdentity: null,
      changedIdentityFields: [],
      error: null,
      confirmIdentityAfterContinuityCheck: () => false,
      reloadIdentity: () => undefined,
    }),
    useSponsoredOperationStore: (selector: (state: { operations: readonly unknown[] }) => unknown) => (
      selector({ operations: [] })
    ),
  }
})

vi.mock('@reown/appkit/react', () => ({
  createAppKit: vi.fn(),
  useAppKit: () => ({ open: vi.fn() }),
  useAppKitNetwork: () => ({ switchNetwork: vi.fn() }),
}))

vi.mock('wagmi', () => ({
  http: vi.fn(() => ({})),
  useAccount: () => ({
    address: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
    isConnected: true,
  }),
  useChainId: () => 421614,
  useReadContracts: wagmiMocks.useReadContracts,
  useSimulateContract: () => ({
    error: null,
    isFetching: false,
    isLoading: false,
  }),
  useSwitchChain: () => ({ switchChain: vi.fn() }),
}))

vi.mock('../../hooks', () => ({
  usePerpsTrading: () => ({
    cleanupExpiredOrder: vi.fn(),
    commitOrder: vi.fn(),
    depositMargin: vi.fn(),
    executeOrder: vi.fn(),
    fundTradingAccount: vi.fn(),
    withdrawMargin: vi.fn(),
  }),
  useSwitchToArbitrumSepolia: () => ({
    switchToArbitrumSepolia: vi.fn(),
    switchError: null,
  }),
  waitForPerpsOrderTerminal: vi.fn(),
}))

interface ReadContractsOptions {
  contracts: readonly {
    functionName: string
    args?: readonly unknown[]
  }[]
  query: {
    enabled: boolean
  }
}

const openPreviewResult = {
  valid: true,
  invalidReason: 0,
  failureCategory: 0,
  executionPrice: 99_000_000n,
  sizeDelta: 100n * 10n ** 18n,
  notionalUsdc: 100_000_000n,
  marginDeltaUsdc: 20_000_000n,
  vpiUsdc: 0n,
  executionFeeUsdc: 40_000n,
  tradeCostUsdc: 40_000n,
  poolRebatePayoutUsdc: 0n,
  pendingCarryUsdc: 0n,
  initialMarginRequirementUsdc: 20_000_000n,
  maintenanceMarginUsdc: 5_000_000n,
  postSize: 100n * 10n ** 18n,
  postMarginUsdc: 20_000_000n,
  postEntryPrice: 99_000_000n,
  postVpiAccrued: 0n,
  postUnrealizedPnlUsdc: 0n,
  postEquityUsdc: 20_000_000n,
  postHealthBps: 4_000n,
  postLiquidatable: false,
  hasLiquidationPrice: true,
  liquidationPrice: 94_000_000n,
}

function latestReadOptions(): ReadContractsOptions {
  const options = wagmiMocks.useReadContracts.mock.calls.at(-1)?.[0]
  if (!options) throw new Error('Expected useReadContracts to be called')
  return options as ReadContractsOptions
}

describe('Perps trade preview debounce', () => {
  beforeEach(() => {
    vi.useFakeTimers()
    wagmiMocks.useReadContracts.mockReset()
    wagmiMocks.useReadContracts.mockReturnValue({
      data: [{ status: 'success', result: openPreviewResult }],
      isFetching: false,
      isLoading: false,
    })
  })

  afterEach(() => {
    vi.useRealTimers()
  })

  it('waits for size, leverage, and direction edits to settle before enabling the preview read', async () => {
    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialSize="100"
        oraclePriceRaw={100_000_000n}
        oraclePublishTime={1_700_000_000}
        availableToTradeRaw={1_000_000_000n}
      />
    )

    const initialOptions = latestReadOptions()
    const initialContract = initialOptions.contracts[0]
    expect(initialOptions.query.enabled).toBe(true)
    expect(initialContract?.functionName).toBe('previewOpen')
    expect((screen.getByRole('button', { name: 'Review Long' }) as HTMLButtonElement).disabled).toBe(false)

    const callsBeforeEditing = wagmiMocks.useReadContracts.mock.calls.length
    const sizeInput = screen.getByRole('textbox')
    fireEvent.change(sizeInput, { target: { value: '101' } })

    expect((sizeInput as HTMLInputElement).value).toBe('101')
    expect(latestReadOptions()).toMatchObject({ contracts: [], query: { enabled: false } })

    await act(async () => {
      vi.advanceTimersByTime(100)
    })

    const leverageInput = screen.getByRole('spinbutton', { name: 'Leverage' })
    fireEvent.change(leverageInput, { target: { value: '6' } })

    expect((leverageInput as HTMLInputElement).value).toBe('6')
    expect(latestReadOptions()).toMatchObject({ contracts: [], query: { enabled: false } })

    await act(async () => {
      vi.advanceTimersByTime(100)
    })

    fireEvent.click(screen.getByRole('button', { name: 'Short plDXY Perp' }))

    const reviewButton = screen.getByRole('button', { name: 'Review Short' })
    expect((reviewButton as HTMLButtonElement).disabled).toBe(true)
    expect(latestReadOptions()).toMatchObject({ contracts: [], query: { enabled: false } })

    const previewPanel = screen.getByText('Preview').parentElement
    expect(previewPanel).not.toBeNull()
    expect(within(previewPanel!).getAllByText('Loading').length).toBeGreaterThan(0)

    await act(async () => {
      vi.advanceTimersByTime(299)
    })

    const editOptions = wagmiMocks.useReadContracts.mock.calls
      .slice(callsBeforeEditing)
      .map((call) => call[0] as ReadContractsOptions)
    expect(editOptions.some((options) => options.query.enabled)).toBe(false)

    await act(async () => {
      vi.advanceTimersByTime(1)
    })

    const finalOptions = latestReadOptions()
    const finalContract = finalOptions.contracts[0]
    const settledOptions = wagmiMocks.useReadContracts.mock.calls
      .slice(callsBeforeEditing)
      .map((call) => call[0] as ReadContractsOptions)
    expect(settledOptions.filter((options) => options.query.enabled)).toHaveLength(1)
    expect(finalOptions.query.enabled).toBe(true)
    expect(finalContract?.functionName).toBe('previewOpen')
    expect(finalContract?.args?.[1]).not.toBe(initialContract?.args?.[1])
    expect(finalContract?.args?.[2]).not.toBe(initialContract?.args?.[2])
    expect(finalContract?.args?.[3]).not.toBe(initialContract?.args?.[3])
    expect((reviewButton as HTMLButtonElement).disabled).toBe(false)
    expect(within(previewPanel!).queryByText('Loading')).toBeNull()
  })
})
