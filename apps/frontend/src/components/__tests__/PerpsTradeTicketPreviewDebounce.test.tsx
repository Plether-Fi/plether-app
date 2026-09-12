import { act, fireEvent, render, screen, within } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { prepared } from '../../test/fixtures/preparedOrder'
import { PerpsTradeTicket } from '../PerpsTradeTicket'

const wagmiMocks = vi.hoisted(() => ({
  useReadContracts: vi.fn(),
  accountAddress: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
  prepareOrder: vi.fn(),
  commitOrder: vi.fn(),
}))

// Max has an independent quote lifecycle; these assertions cover typed-size previews.
vi.mock('../../hooks/usePerpsMaxOpenQuote', () => ({
  usePerpsMaxOpenQuote: () => ({ quote: undefined, marginDelta: undefined, isPending: false, isFetching: false }),
}))

vi.mock('../../perps-aa', () => {
  const address = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'

  return {
    findBundlerRequestError: () => undefined,
    usePerpsIdentity: () => ({
      status: 'ready',
      ownerAddress: address,
      accountAddress: wagmiMocks.accountAddress,
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
    useSponsoredOperationStore: (selector: (state: { operations: readonly unknown[]; getActiveOperation: () => undefined }) => unknown) => (
      selector({ operations: [], getActiveOperation: () => undefined })
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
    prepareOrder: wagmiMocks.prepareOrder,
    cleanupExpiredOrder: vi.fn(),
    commitOrder: wagmiMocks.commitOrder,
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
  vpiUsdc: 25_000_000n,
  executionFeeUsdc: 40_000n,
  tradeCostUsdc: 40_000n,
  poolRebatePayoutUsdc: 0n,
  pendingCarryUsdc: 0n,
  initialMarginRequirementUsdc: 20_000_000n,
  maintenanceMarginUsdc: 5_000_000n,
  postSize: 100n * 10n ** 18n,
  postMarginUsdc: 20_000_000n,
  postEntryPrice: 99_000_000n,
  postVpiAccrued: 25_000_000n,
  postUnrealizedPnlUsdc: 0n,
  postEquityUsdc: 20_000_000n,
  postHealthBps: 4_000n,
  postLiquidatable: false,
  hasLiquidationPrice: true,
  liquidationPrice: 94_000_000n,
}

const existingLongPosition = {
  exists: true,
  side: 0,
  direction: 'long' as const,
  size: 1_000n * 10n ** 18n,
  entryPrice: 100_000_000n,
  marginUsdc: 200_000_000n,
  unrealizedPnlUsdc: 0n,
  maintenanceMarginUsdc: 10_000_000n,
  liquidatable: false,
  estimatedNotionalUsdc: 1_000_000_000n,
  entryNotionalUsdc: 1_000_000_000n,
  dxyExposureUsdc: 1_000_000_000n,
  vpiAccrued: 60_000_000n,
}

function latestReadOptions(): ReadContractsOptions {
  const options = wagmiMocks.useReadContracts.mock.calls.at(-1)?.[0]
  if (!options) throw new Error('Expected useReadContracts to be called')
  return options as ReadContractsOptions
}

describe('Perps trade preview debounce', () => {
  beforeEach(() => {
    vi.useFakeTimers()
    wagmiMocks.accountAddress = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
    wagmiMocks.useReadContracts.mockReset()
    wagmiMocks.commitOrder.mockReset()
    wagmiMocks.prepareOrder.mockReset().mockImplementation(() => new Promise(() => {}))
    wagmiMocks.useReadContracts.mockReturnValue({
      data: [{ status: 'success', result: openPreviewResult }],
      isFetching: false,
      isLoading: false,
    })
  })

  afterEach(() => {
    vi.useRealTimers()
  })

  it('prepares while the ticket is idle, reuses the result on opening, and never submits in the background', async () => {
    const result = prepared()
    result.account = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
    result.request.sizeDelta = 100n * 10n ** 18n
    wagmiMocks.prepareOrder.mockResolvedValue(result)
    render(<PerpsTradeTicket enableLiveTrading initialOrderQuantity="100" oraclePriceRaw={100_000_000n}
      oraclePublishTime={1_700_000_000} availableToTradeRaw={1_000_000_000n} />)
    await act(async () => { await vi.advanceTimersByTimeAsync(500) })
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledTimes(1)
    expect(wagmiMocks.commitOrder).not.toHaveBeenCalled()
    fireEvent.click(screen.getByRole('button', { name: 'Review Long' }))
    expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeEnabled()
    expect(screen.getByRole('dialog')).toHaveFocus()
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledTimes(1)
    expect(wagmiMocks.commitOrder).not.toHaveBeenCalled()
  })

  it('refreshes automatically and displays exact changed terms before enabling updated confirmation', async () => {
    const first = prepared(20)
    first.account = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
    first.request.sizeDelta = 100n * 10n ** 18n
    let resolveRefresh!: (value: typeof first) => void
    wagmiMocks.prepareOrder.mockResolvedValueOnce(first).mockImplementationOnce(() => new Promise(resolve => { resolveRefresh = resolve }))
    render(<PerpsTradeTicket enableLiveTrading initialOrderQuantity="100" initialReviewOpen oraclePriceRaw={100_000_000n}
      oraclePublishTime={1_700_000_000} availableToTradeRaw={1_000_000_000n} />)
    await act(async () => { await vi.advanceTimersByTimeAsync(0) })
    expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeEnabled()
    await act(async () => { await vi.advanceTimersByTimeAsync(10_000) })
    expect(screen.getByRole('button', { name: 'Updating review…' })).toBeDisabled()
    const updated = { ...first, protection: { ...first.protection, validUntil: BigInt(Math.floor(Date.now() / 1000) + 60) },
      request: { ...first.request, marginDelta: first.request.marginDelta + 1n } }
    await act(async () => { resolveRefresh(updated) })
    expect(screen.getByRole('button', { name: 'Confirm updated order' })).toBeEnabled()
    expect(screen.getByText('Required margin: 20 USDC → 20.000001 USDC')).toBeInTheDocument()
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledTimes(2)
    expect(wagmiMocks.commitOrder).not.toHaveBeenCalled()
  })

  it('closes a review after an account switch and ignores its late response', async () => {
    let resolve!: (value: ReturnType<typeof prepared>) => void
    wagmiMocks.prepareOrder.mockImplementation(() => new Promise(yes => { resolve = yes }))
    const props = { enableLiveTrading: true, initialReviewOpen: true, initialOrderQuantity: '100', oraclePriceRaw: 100_000_000n,
      oraclePublishTime: 1_700_000_000, availableToTradeRaw: 1_000_000_000n }
    const view = render(<PerpsTradeTicket {...props} />)
    expect(screen.getByRole('dialog')).toBeInTheDocument()
    wagmiMocks.accountAddress = '0x0000000000000000000000000000000000000002'
    view.rerender(<PerpsTradeTicket {...props} />)
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument()
    await act(async () => { resolve(prepared()) })
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument()
    expect(wagmiMocks.commitOrder).not.toHaveBeenCalled()
  })

  it('refreshes a frozen close with Exact slippage while retaining the selected quantity', async () => {
    const result = prepared()
    result.account = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
    result.request.isClose = true
    result.request.sizeDelta = 100n * 10n ** 18n
    result.request.marginDelta = 0n
    wagmiMocks.prepareOrder.mockResolvedValue(result)
    const props = { enableLiveTrading: true, initialReviewOpen: true, initialReduceOnly: true, initialOrderQuantity: '100',
      currentPosition: existingLongPosition, oraclePriceRaw: 100_000_000n, oraclePublishTime: 1_700_000_000, availableToTradeRaw: 1_000_000_000n }
    const view = render(<PerpsTradeTicket {...props} oracleFrozen={false} />)
    await act(async () => { await vi.advanceTimersByTimeAsync(0) })
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledTimes(1)
    view.rerender(<PerpsTradeTicket {...props} oracleFrozen />)
    await act(async () => { await vi.advanceTimersByTimeAsync(0) })
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledTimes(2)
    expect(wagmiMocks.prepareOrder).toHaveBeenLastCalledWith(expect.objectContaining({ slippagePercent: 0, sizeDelta: 100n * 10n ** 18n, isClose: true }))
    expect(wagmiMocks.commitOrder).not.toHaveBeenCalled()
  })

  it('uses the opening preview margin and entry for TP/SL and rejects a stop beyond preview liquidation', async () => {
    wagmiMocks.useReadContracts.mockReturnValue({
      data: [{ status: 'success', result: { ...openPreviewResult, liquidationPrice: 110_000_000n } }],
      isFetching: false, isLoading: false,
    })
    render(<PerpsTradeTicket enableLiveTrading initialDirection="long" initialOrderQuantity="100"
      oraclePriceRaw={100_000_000n} oraclePublishTime={1_700_000_000} availableToTradeRaw={1_000_000_000n}
      protectionCapPrice={200_000_000n} protectionConfiguration={{ enabled: true, triggerBountyUsdc: 200_000n, executionBountyUsdc: 200_000n }} />)
    fireEvent.click(screen.getByRole('checkbox', { name: 'Take profit / stop loss' }))
    fireEvent.change(screen.getByLabelText('Stop loss (%)'), { target: { value: '25' } })
    expect(screen.getByLabelText('Stop loss (USDC)')).toHaveValue('0.96') // 5 USDC loss / 20 USDC preview margin.
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeEnabled()
    fireEvent.change(screen.getByLabelText('Stop loss (%)'), { target: { value: '90' } })
    expect(screen.getByLabelText('Stop loss (%)')).toHaveAttribute('aria-invalid', 'true')
    expect(screen.getByRole('alert')).toHaveTextContent('liquidation price')
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeDisabled()

    fireEvent.change(screen.getByLabelText('Stop loss (%)'), { target: { value: '25' } })
    fireEvent.change(screen.getByRole('spinbutton', { name: 'Leverage' }), { target: { value: '6' } })
    expect(screen.getByLabelText('Stop loss (USDC)')).toHaveValue('')
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeDisabled()
    wagmiMocks.useReadContracts.mockReturnValue({
      data: [{ status: 'success', result: { ...openPreviewResult, postMarginUsdc: 40_000_000n, liquidationPrice: 110_000_000n } }],
      isFetching: false, isLoading: false,
    })
    await act(async () => { vi.advanceTimersByTime(300) })
    expect(screen.getByLabelText('Stop loss (USDC)')).toHaveValue('0.91')
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeEnabled()
  })

  it('revalidates the fixed reviewed stop when the opening preview liquidation changes', () => {
    wagmiMocks.useReadContracts.mockReturnValue({
      data: [{ status: 'success', result: { ...openPreviewResult, liquidationPrice: 110_000_000n } }],
      isFetching: false, isLoading: false,
    })
    const props = { enableLiveTrading: true, initialDirection: 'long' as const, initialOrderQuantity: '100',
      oraclePriceRaw: 100_000_000n, oraclePublishTime: 1_700_000_000, availableToTradeRaw: 1_000_000_000n,
      protectionCapPrice: 200_000_000n, protectionConfiguration: { enabled: true, triggerBountyUsdc: 200_000n, executionBountyUsdc: 200_000n } }
    const view = render(<PerpsTradeTicket {...props} />)
    fireEvent.click(screen.getByRole('checkbox', { name: 'Take profit / stop loss' }))
    fireEvent.change(screen.getByLabelText('Stop loss (%)'), { target: { value: '25' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review Long' }))
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledWith(expect.objectContaining({ positionProtection: { takeProfitTriggerPrice: 0n, stopLossTriggerPrice: 104_000_000n } }))
    // Recomputing 25% with this new margin would produce a valid .99 stop,
    // but the reviewed .96 stop is now beyond the .97 liquidation boundary.
    wagmiMocks.useReadContracts.mockReturnValue({
      data: [{ status: 'success', result: { ...openPreviewResult, postEntryPrice: 100_000_000n, postMarginUsdc: 4_000_000n, liquidationPrice: 103_000_000n } }],
      isFetching: false, isLoading: false,
    })
    view.rerender(<PerpsTradeTicket {...props} />)
    expect(screen.getByRole('alert')).toHaveTextContent('liquidation price')
    expect(screen.getByRole('button', { name: 'Checking order…' })).toBeDisabled()
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledTimes(1)
  })

  it('waits for size, leverage, and direction edits to settle before enabling the preview read', async () => {
    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialOrderQuantity="100"
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
    fireEvent.change(sizeInput, { target: { value: '200' } })

    expect((sizeInput as HTMLInputElement).value).toBe('200')
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
    expect(within(previewPanel!).queryByText('Position VPI balance')).not.toBeInTheDocument()
    expect(within(previewPanel!).getByLabelText('Pay 25.0 USDC')).toBeInTheDocument()
    expect(within(previewPanel!).queryByText('Maximum future VPI credit')).not.toBeInTheDocument()
  })

  it('keeps the order quantity input while deriving its order exposure', () => {
    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialOrderQuantity="4900"
        oraclePriceRaw={98_580_000n}
        oraclePublishTime={1_700_000_000}
        availableToTradeRaw={10_000_000_000n}
      />
    )

    const options = latestReadOptions()
    expect(options.contracts[0]?.functionName).toBe('previewOpen')
    expect(options.contracts[0]?.args?.[2]).toBe(4_900n * 10n ** 18n)
    expect(screen.getByRole('textbox', { name: 'Order quantity' }))
      .toHaveValue('4900')
    expect(screen.queryByText(/^Rounded down to/)).not.toBeInTheDocument()

    const previewPanel = screen.getByText('Preview').parentElement
    expect(previewPanel).not.toBeNull()
    const preview = within(previewPanel!)
    const orderExposureRow = preview.getByText('Order exposure').closest('div')

    expect(within(orderExposureRow!).getByText('4 969.58')).toBeInTheDocument()
    expect(within(orderExposureRow!).getByText('USDC')).toBeInTheDocument()
    expect(preview.queryByText('Order quantity')).not.toBeInTheDocument()
  })

  it('opens Commit Preview on the dialog without opening the oracle tooltip', () => {
    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialOrderQuantity="4900"
        oraclePriceRaw={98_580_000n}
        oraclePublishTime={1_700_000_000}
        oracleFreshness="fresh"
        oracleFreshnessTooltip="validated oracle basket updated 18s ago"
        availableToTradeRaw={10_000_000_000n}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: 'Review Long' }))

    expect(screen.getByRole('dialog')).toHaveFocus()
    expect(screen.queryByRole('tooltip')).not.toBeInTheDocument()
  })

  it('blocks a partial close quantity outside the 100 plDXY protocol quantum', () => {
    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialReduceOnly
        initialOrderQuantity="550"
        oraclePriceRaw={98_580_000n}
        oraclePublishTime={1_700_000_000}
        currentPosition={existingLongPosition}
      />
    )

    expect(wagmiMocks.useReadContracts.mock.calls.some((call) => (
      (call[0] as ReadContractsOptions).contracts[0]?.functionName === 'previewClose'
    ))).toBe(false)
    expect(screen.getByText('Order quantity must be a multiple of 100 plDXY.')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Reduce' })).toBeDisabled()
  })

  it('blocks a non-quantized order in fixture mode', () => {
    render(
      <PerpsTradeTicket
        initialDirection="long"
        initialOrderQuantity="4950"
        oraclePriceRaw={98_580_000n}
        oraclePublishTime={1_700_000_000}
        availableToTradeRaw={10_000_000_000n}
      />
    )

    expect(screen.getByText('Order quantity must be a multiple of 100 plDXY.')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeDisabled()
  })

  it('allows an exact non-quantized full close in fixture mode', () => {
    render(
      <PerpsTradeTicket
        initialDirection="long"
        initialReduceOnly
        initialOrderQuantity="9550"
        oraclePriceRaw={98_580_000n}
        oraclePublishTime={1_700_000_000}
        currentPosition={{
          ...existingLongPosition,
          size: 9_550n * 10n ** 18n,
        }}
      />
    )

    expect(screen.queryByText('Order quantity must be a multiple of 100 plDXY.')).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Close' })).toBeEnabled()
  })

  it('shows the resulting position VPI balance when increasing an existing position', () => {
    const increasePreviewResult = {
      ...openPreviewResult,
      postSize: 1_100n * 10n ** 18n,
      postMarginUsdc: 220_000_000n,
      postVpiAccrued: 85_000_000n,
    }

    render(
      <PerpsTradeTicket
        initialReviewOpen
        initialDirection="long"
        initialOrderQuantity="100"
        oraclePriceRaw={100_000_000n}
        oraclePublishTime={1_700_000_000}
        availableToTradeRaw={1_000_000_000n}
        currentPosition={existingLongPosition}
        openPreviewFixture={increasePreviewResult}
      />
    )

    expect(screen.getAllByLabelText('Net paid 85.0 USDC')).toHaveLength(2)
    expect(screen.getAllByLabelText('Pay 25.0 USDC')).toHaveLength(2)

    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))

    const confirmation = screen.getByRole('dialog')
    expect(within(confirmation).getByLabelText('Net paid 85.0 USDC')).toBeInTheDocument()
    expect(within(confirmation).getByLabelText('Pay 25.0 USDC')).toBeInTheDocument()
  })

  it('keeps the reviewed VPI in a fresh opening confirmation without adding a balance row', () => {
    render(
      <PerpsTradeTicket
        initialReviewOpen
        initialDirection="long"
        initialOrderQuantity="100"
        currentPositionAmount="0"
        oraclePriceRaw={100_000_000n}
        oraclePublishTime={1_700_000_000}
        availableToTradeRaw={1_000_000_000n}
        openPreviewFixture={openPreviewResult}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))

    const confirmation = screen.getByRole('dialog')
    expect(within(confirmation).queryByText('Position VPI balance')).not.toBeInTheDocument()
    expect(within(confirmation).getByLabelText('Pay 25.0 USDC')).toBeInTheDocument()
  })
})
