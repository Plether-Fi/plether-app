import { act, fireEvent, render, screen, within } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { relaxedProtectionCloseLimits } from '../../contracts/positionProtection'
import { prepared } from '../../test/fixtures/preparedOrder'
import { PerpsTradeTicket } from '../PerpsTradeTicket'

const wagmiMocks = vi.hoisted(() => ({
  useReadContracts: vi.fn(),
  accountAddress: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
  prepareOrder: vi.fn(),
  commitOrder: vi.fn(),
  maxQuote: vi.fn(),
  openMargin: vi.fn(),
}))

// Keep the Max quote lifecycle independent from typed-size preview reads.
vi.mock('../../hooks/usePerpsOpenMargin', () => ({
  usePerpsOpenMargin: wagmiMocks.openMargin,
}))

vi.mock('../../hooks/usePerpsMaxOpenQuote', () => ({
  usePerpsMaxOpenQuote: wagmiMocks.maxQuote,
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

vi.mock('../../wallet/useWalletActions', () => ({
  useWalletActions: () => ({ connect: vi.fn(), ready: true, configured: true }),
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
  notionalAssets: 100_000_000n,
  marginDeltaAssets: 20_000_000n,
  vpiAssets: 25_000_000n,
  executionFeeAssets: 40_000n,
  tradeCostAssets: 40_000n,
  poolRebatePayoutAssets: 0n,
  pendingCarryAssets: 0n,
  initialMarginRequirementAssets: 20_000_000n,
  maintenanceMarginAssets: 5_000_000n,
  postSize: 100n * 10n ** 18n,
  postMarginAssets: 20_000_000n,
  postEntryPrice: 99_000_000n,
  postVpiAccrued: 25_000_000n,
  postUnrealizedPnlAssets: 0n,
  postEquityAssets: 20_000_000n,
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
  marginAssets: 200_000_000n,
  unrealizedPnlAssets: 0n,
  maintenanceMarginAssets: 10_000_000n,
  liquidatable: false,
  estimatedNotionalAssets: 1_000_000_000n,
  entryNotionalAssets: 1_000_000_000n,
  dxyExposureAssets: 1_000_000_000n,
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
    wagmiMocks.openMargin.mockReset().mockReturnValue({ preview: undefined, error: undefined, isPending: false })
    wagmiMocks.maxQuote.mockReset().mockReturnValue({ quote: undefined, marginDelta: undefined, isPending: false, isFetching: false })
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

  describe('funded opening margin validation', () => {
    const props = { enableLiveTrading: true, initialOrderQuantity: '100', initialLeverage: 10,
      oraclePriceRaw: 100_000_000n, oraclePublishTime: 1_700_000_000, availableToTradeRaw: 1_000_000_000n }

    function maximum() {
      wagmiMocks.maxQuote.mockReturnValue({ quote: { maxSizeDelta: 1_000n * 10n ** 18n, preview: { valid: true } },
        marginDelta: 999_800_000n, isPending: false, isFetching: false })
    }

    it.each(['manual', '25%', '50%', '75%', '100%'])('uses the funded quote in the preview and review for %s', async size => {
      maximum()
      wagmiMocks.openMargin.mockImplementation(({ sizeDelta }: { sizeDelta: bigint }) => {
        const equity = sizeDelta / 10n ** 13n
        return { preview: { ...openPreviewResult, postSize: sizeDelta, postEquityAssets: equity,
          marginDeltaAssets: equity + 10_500_000n, executionPrice: 100_000_000n }, isPending: false }
      })
      render(<PerpsTradeTicket {...props} />)
      if (size !== 'manual') fireEvent.click(within(screen.getByRole('group', { name: 'Order quantity shortcuts' })).getByRole('button', { name: size === '100%' ? 'Max' : size }))
      await act(async () => { await vi.advanceTimersByTimeAsync(600) })
      const quantity = size === 'manual' ? 100n : (1_000n * BigInt(size.slice(0, -1)) / 100n / 100n) * 100n
      const margin = quantity * 100_000n + 10_500_000n
      expect(latestReadOptions().contracts[0]?.args?.[3]).toBe(margin)
      expect(screen.getByRole('button', { name: 'Review Long' })).toBeEnabled()
      fireEvent.click(screen.getByRole('button', { name: 'Review Long' }))
      expect(wagmiMocks.prepareOrder).toHaveBeenCalledWith(expect.objectContaining({
        marginAssets: margin, sizeDelta: quantity * 10n ** 18n,
      }))
    })

    it.each(['25%', '50%', '75%', '100%'])('blocks %s when its exact funded preview is rejected', async size => {
      maximum()
      wagmiMocks.useReadContracts.mockReturnValue({
        data: [{ status: 'success', result: { ...openPreviewResult, valid: false, invalidReason: 9 } }],
        isFetching: false, isLoading: false,
      })
      render(<PerpsTradeTicket {...props} />)
      fireEvent.click(within(screen.getByRole('group', { name: 'Order quantity shortcuts' })).getByRole('button', { name: size === '100%' ? 'Max' : size }))
      await act(async () => { await vi.advanceTimersByTimeAsync(600) })
      expect(screen.getByText(/cannot fund the required liquidation reserve/)).toBeInTheDocument()
      expect(screen.getByRole('button', { name: 'Review Long' })).toBeDisabled()
      expect(wagmiMocks.prepareOrder).not.toHaveBeenCalled()
    })

    it('keeps 100% disabled while margin is being quoted', async () => {
      maximum()
      wagmiMocks.openMargin.mockReturnValue({ preview: undefined, isPending: true })
      render(<PerpsTradeTicket {...props} />)
      fireEvent.click(screen.getByRole('button', { name: 'Max' }))
      await act(async () => { await vi.advanceTimersByTimeAsync(600) })
      expect(screen.getByRole('button', { name: 'Review Long' })).toBeDisabled()
      expect(wagmiMocks.prepareOrder).not.toHaveBeenCalled()
    })
  })

  describe('Order quantity shortcuts', () => {
    const props = { enableLiveTrading: true, initialLeverage: 10, oraclePriceRaw: 100_000_000n,
      oraclePublishTime: 1_700_000_000, availableToTradeRaw: 1_000_000_000n }
    const maximum = (tokens: bigint) => ({
      quote: { maxSizeDelta: tokens * 10n ** 18n, preview: { valid: true, executionPrice: 100_000_000n } },
      marginDelta: 999_800_000n, isPending: false, isFetching: false,
    })
    const shortcuts = () => within(screen.getByRole('group', { name: 'Order quantity shortcuts' }))

    it('fills each percentage of the quoted maximum', () => {
      wagmiMocks.maxQuote.mockReturnValue(maximum(405_200n))
      render(<PerpsTradeTicket {...props} />)
      for (const [percentage, amount] of [['25%', '101 300'], ['50%', '202 600'], ['75%', '303 900'], ['Max', '405 200']]) {
        fireEvent.click(shortcuts().getByRole('button', { name: percentage === 'Max' ? 'Max' : percentage }))
        expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue(amount)
      }
    })

    it('shows a plain Max link that fills the full token quantity', () => {
      const value = maximum(700n)
      wagmiMocks.maxQuote.mockReturnValue({ ...value,
        quote: { ...value.quote, preview: { ...value.quote.preview, executionPrice: 80_000_000n } },
      })
      render(<PerpsTradeTicket {...props} oraclePriceRaw={80_000_000n} />)
      const group = shortcuts()
      expect(group.getAllByRole('button').map(button => button.textContent)).toEqual(['25%', '50%', '75%', 'Max'])
      expect(group.queryByRole('button', { name: '100%' })).not.toBeInTheDocument()
      fireEvent.click(group.getByRole('button', { name: 'Max' }))
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('700')
      expect(group.getByRole('button', { name: 'Max', pressed: true })).toHaveClass('border-brand-orange/60', 'bg-brand-orange/10')
      fireEvent.change(screen.getByRole('textbox', { name: 'Order quantity' }), { target: { value: '600' } })
      expect(group.getByRole('button', { name: 'Max', pressed: false })).not.toHaveClass('bg-brand-orange/10')
    })

    it('rounds fractional lots down without exceeding the requested percentage', () => {
      wagmiMocks.maxQuote.mockReturnValue(maximum(700n))
      render(<PerpsTradeTicket {...props} />)
      for (const [percentage, amount] of [['25%', '100'], ['50%', '300'], ['75%', '500'], ['Max', '700']]) {
        fireEvent.click(shortcuts().getByRole('button', { name: percentage === 'Max' ? 'Max' : percentage }))
        expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue(amount)
      }
    })

    it('retains the filled Max and shortcut appearance while a new price quote loads', async () => {
      const previous = maximum(700n)
      wagmiMocks.maxQuote.mockReturnValue(previous)
      const view = render(<PerpsTradeTicket {...props} />)
      fireEvent.click(shortcuts().getByRole('button', { name: 'Max' }))
      wagmiMocks.maxQuote.mockReturnValue({ quote: undefined, marginDelta: undefined,
        displayQuote: previous.quote, isPending: true, isFetching: true })
      view.rerender(<PerpsTradeTicket {...props} oraclePriceRaw={100_001_000n} />)
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('700')
      expect(screen.getByRole('button', { name: 'Max' })).toBeDisabled()
      expect(screen.getByRole('button', { name: 'Max', pressed: true })).toHaveClass('bg-brand-orange/10')
      expect(screen.queryByText('Loading…')).not.toBeInTheDocument()
      expect(screen.getByRole('group', { name: 'Order quantity shortcuts' })).toHaveAttribute('aria-busy', 'true')
      for (const button of shortcuts().getAllByRole('button')) {
        expect(button).toBeDisabled()
        expect(button).not.toHaveClass('disabled:opacity-50')
      }
      fireEvent.click(shortcuts().getByRole('button', { name: '50%' }))
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('700')
      await act(async () => { await vi.advanceTimersByTimeAsync(500) })
      expect(wagmiMocks.prepareOrder).not.toHaveBeenCalled()
      wagmiMocks.maxQuote.mockReturnValue(maximum(600n))
      view.rerender(<PerpsTradeTicket {...props} oraclePriceRaw={100_001_000n} />)
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('600')
      expect(shortcuts().getByRole('button', { name: 'Max' })).toBeEnabled()
    })

    it('keeps shortcuts visually steady during a periodic refresh while disabling clicks', () => {
      const previous = maximum(700n)
      wagmiMocks.maxQuote.mockReturnValue(previous)
      const view = render(<PerpsTradeTicket {...props} />)
      wagmiMocks.maxQuote.mockReturnValue({ ...previous, isFetching: true })
      view.rerender(<PerpsTradeTicket {...props} />)
      const max = screen.getByRole('button', { name: 'Max' })
      expect(max).toBeDisabled()
      expect(max).not.toHaveClass('disabled:opacity-50')
      expect(shortcuts().getByRole('button', { name: 'Max' })).not.toHaveClass('disabled:opacity-50')
    })

    it.each([
      ['loading', { isPending: true }],
      ['refreshing', { isFetching: true }],
      ['failed', { error: new Error('RPC unavailable') }],
      ['zero capacity', { quote: { maxSizeDelta: 0n, preview: { valid: true } } }],
      ['invalid', { quote: { maxSizeDelta: 700n * 10n ** 18n, preview: { valid: false } } }],
    ])('disables all shortcuts while Max is %s', (_state, override) => {
      wagmiMocks.maxQuote.mockReturnValue({ ...maximum(700n), ...override })
      render(<PerpsTradeTicket {...props} />)
      for (const button of shortcuts().getAllByRole('button')) {
        expect(button).toBeDisabled()
        fireEvent.click(button)
      }
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('0')
    })

    it('disables fractions that cannot fund one lot while keeping the exact maximum usable', () => {
      wagmiMocks.maxQuote.mockReturnValue(maximum(100n))
      render(<PerpsTradeTicket {...props} />)
      for (const percentage of ['25%', '50%', '75%']) {
        expect(shortcuts().getByRole('button', { name: percentage === 'Max' ? 'Max' : percentage })).toBeDisabled()
      }
      expect(shortcuts().getByRole('button', { name: 'Max' })).toBeEnabled()
      fireEvent.click(shortcuts().getByRole('button', { name: 'Max' }))
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('100')
    })

    it('keeps 100% dynamic but switches a smaller percentage back to a fixed ordinary order', async () => {
      wagmiMocks.maxQuote.mockImplementation(({ selectedMaxLeverageBps }: { selectedMaxLeverageBps: number }) => ({
        ...maximum(0n),
        quote: { maxSizeDelta: BigInt(selectedMaxLeverageBps) * 10n ** 17n, preview: { valid: true } },
      }))
      render(<PerpsTradeTicket {...props} />)
      expect(shortcuts().getByRole('button', { name: 'Max', pressed: false })).toBeInTheDocument()
      fireEvent.click(shortcuts().getByRole('button', { name: 'Max' }))
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('10 000')
      expect(shortcuts().getByRole('button', { name: 'Max', pressed: true })).toBeInTheDocument()
      fireEvent.change(screen.getByRole('slider', { name: 'Leverage slider' }), { target: { value: '5' } })
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('5 000')
      expect(shortcuts().getByRole('button', { name: 'Max', pressed: true })).toBeInTheDocument()
      fireEvent.click(shortcuts().getByRole('button', { name: '50%' }))
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('2 500')
      expect(shortcuts().getByRole('button', { name: 'Max', pressed: false })).toBeInTheDocument()
      fireEvent.change(screen.getByRole('slider', { name: 'Leverage slider' }), { target: { value: '10' } })
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('2 500')
      await act(async () => { await vi.advanceTimersByTimeAsync(500) })
      fireEvent.click(screen.getByRole('button', { name: 'Review Long' }))
      expect(wagmiMocks.prepareOrder).toHaveBeenLastCalledWith(expect.objectContaining({
        sizeDelta: 2_500n * 10n ** 18n, maxSize: undefined,
      }))
    })

    it('uses only the unreserved closing size and clears full-close intent for a partial shortcut', () => {
      const pendingClose = (tokens: bigint) => [{
        orderId: 33n, side: 0, direction: 'long' as const, sizeDelta: tokens * 10n ** 18n,
        marginDeltaAssets: 0n, acceptablePrice: 100_000_000n, isReduceOnly: true, status: 1,
      }]
      const closeProps = { initialReduceOnly: true, currentPosition: existingLongPosition,
        oraclePriceRaw: 100_000_000n, oraclePublishTime: 1_700_000_000 }
      const view = render(<PerpsTradeTicket {...closeProps} pendingOrders={pendingClose(300n)} />)
      for (const [percentage, amount] of [['25%', '100'], ['50%', '300'], ['75%', '500'], ['Max', '700']]) {
        fireEvent.click(shortcuts().getByRole('button', { name: percentage === 'Max' ? 'Max' : percentage }))
        expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue(amount)
      }
      view.rerender(<PerpsTradeTicket {...closeProps} pendingOrders={pendingClose(400n)} />)
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('600')
      fireEvent.click(shortcuts().getByRole('button', { name: '50%' }))
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('300')
      view.rerender(<PerpsTradeTicket {...closeProps} pendingOrders={pendingClose(500n)} />)
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('300')
    })

    it('preserves an exact non-quantized full close for 100%', () => {
      render(<PerpsTradeTicket initialReduceOnly currentPosition={{ ...existingLongPosition, size: 9_550n * 10n ** 18n }}
        oraclePriceRaw={98_580_000n} oraclePublishTime={1_700_000_000} />)
      fireEvent.click(shortcuts().getByRole('button', { name: '25%' }))
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('2 300')
      fireEvent.click(shortcuts().getByRole('button', { name: 'Max' }))
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('9 550')
      expect(screen.queryByText('Order quantity must be a multiple of 100 plDXY.')).not.toBeInTheDocument()
      expect(screen.getByRole('button', { name: 'Review Close' })).toBeEnabled()
    })
  })

  it('passes selected leverage to Max and updates a filled maximum when leverage changes', async () => {
    wagmiMocks.maxQuote.mockImplementation(({ selectedMaxLeverageBps }: { selectedMaxLeverageBps: number }) => ({
      quote: { maxSizeDelta: BigInt(selectedMaxLeverageBps) * 10n ** 17n, preview: { valid: true } },
      marginDelta: 999_800_000n, isPending: false, isFetching: false,
    }))
    render(<PerpsTradeTicket enableLiveTrading initialLeverage={10} oraclePriceRaw={100_000_000n}
      oraclePublishTime={1_700_000_000} availableToTradeRaw={1_000_000_000n} />)
    fireEvent.click(screen.getByRole('button', { name: 'Max' }))
    expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('10 000')
    fireEvent.change(screen.getByRole('slider', { name: 'Leverage slider' }), { target: { value: '5' } })
    expect(wagmiMocks.maxQuote).toHaveBeenLastCalledWith(expect.objectContaining({ selectedMaxLeverageBps: 50_000 }))
    expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('5 000')
    fireEvent.change(screen.getByRole('textbox', { name: 'Order quantity' }), { target: { value: '100' } })
    fireEvent.change(screen.getByRole('spinbutton', { name: 'Leverage' }), { target: { value: '20' } })
    expect(wagmiMocks.maxQuote).toHaveBeenLastCalledWith(expect.objectContaining({ selectedMaxLeverageBps: 200_000 }))
    expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('100')
    fireEvent.click(screen.getByRole('button', { name: 'Max' }))
    fireEvent.change(screen.getByRole('slider', { name: 'Leverage slider' }), { target: { value: '10' } })
    const quantity = screen.getByRole('textbox', { name: 'Order quantity' })
    await act(async () => { await vi.advanceTimersByTimeAsync(500) })
    fireEvent.click(screen.getByRole('button', { name: 'Review Long' }))
    expect(screen.getByRole('dialog')).toBeInTheDocument()
    expect(quantity).toHaveValue('10 000')
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

  it('does not repeatedly prepare while an untouched position and oracle poll, but rechecks fresh margin on review', async () => {
    const warm = prepared()
    warm.account = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
    warm.request.sizeDelta = 100n * 10n ** 18n
    const fresh = { ...warm, request: { ...warm.request, marginDelta: 30_000_000n } }
    wagmiMocks.prepareOrder.mockResolvedValueOnce(warm).mockResolvedValue(fresh)
    const props = { enableLiveTrading: true, initialOrderQuantity: '100', initialDirection: 'long' as const,
      oraclePriceRaw: 100_000_000n, oraclePublishTime: 1_700_000_000, availableToTradeRaw: 1_000_000_000n,
      currentPosition: existingLongPosition }
    const view = render(<PerpsTradeTicket {...props} />)
    await act(async () => { await vi.advanceTimersByTimeAsync(500) })
    for (let tick = 1; tick <= 30; tick++) {
      view.rerender(<PerpsTradeTicket {...props} oraclePublishTime={props.oraclePublishTime + tick * 2}
        currentPosition={{ ...existingLongPosition, unrealizedPnlAssets: BigInt(tick) * 1_000n }} />)
      await act(async () => { await vi.advanceTimersByTimeAsync(2000) })
    }
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledTimes(1)
    expect(wagmiMocks.commitOrder).not.toHaveBeenCalled()
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeEnabled()
    fireEvent.click(screen.getByRole('button', { name: 'Review Long' }))
    await act(async () => { await vi.advanceTimersByTimeAsync(0) })
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledTimes(2)
    expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeEnabled()
    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))
    expect(wagmiMocks.commitOrder).toHaveBeenCalledWith(expect.objectContaining({ preparedOrder: fresh }))
  })

  it('keeps reviewed terms and confirmation available after the provisional deadline', async () => {
    const first = prepared(55)
    first.account = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
    first.request.sizeDelta = 100n * 10n ** 18n
    wagmiMocks.prepareOrder.mockResolvedValue(first)
    wagmiMocks.commitOrder.mockImplementation(() => new Promise(() => {}))
    render(<PerpsTradeTicket enableLiveTrading initialOrderQuantity="100" initialReviewOpen oraclePriceRaw={100_000_000n}
      oraclePublishTime={1_700_000_000} availableToTradeRaw={1_000_000_000n} />)
    await act(async () => { await vi.advanceTimersByTimeAsync(0) })
    expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeEnabled()
    await act(async () => { await vi.advanceTimersByTimeAsync(60_000) })
    expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeEnabled()
    expect(screen.queryByText(/This quote is no longer valid/)).not.toBeInTheDocument()
    expect(within(screen.getByRole('dialog')).getByText('Required margin').closest('div')).toHaveTextContent('20.0USDG')
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledTimes(1)
    expect(wagmiMocks.commitOrder).not.toHaveBeenCalled()
    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))
    await act(async () => { await vi.advanceTimersByTimeAsync(0) })
    expect(wagmiMocks.commitOrder).toHaveBeenCalledWith(expect.objectContaining({ preparedOrder: first }))
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledTimes(1)
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
      protectionCapPrice={200_000_000n} protectionConfiguration={{ enabled: true, triggerBountyAssets: 200_000n, executionBountyAssets: 200_000n }} />)
    fireEvent.click(screen.getByRole('checkbox', { name: 'Take profit / stop loss' }))
    await act(async () => { vi.advanceTimersByTime(300) })
    fireEvent.change(screen.getByLabelText('Stop loss (%)'), { target: { value: '25' } })
    expect(screen.getByLabelText('Stop loss (USDG)')).toHaveValue('0.96') // 5 USDG loss / 20 USDG preview margin.
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeEnabled()
    fireEvent.change(screen.getByLabelText('Stop loss (%)'), { target: { value: '90' } })
    expect(screen.getByLabelText('Stop loss (%)')).toHaveAttribute('aria-invalid', 'true')
    expect(screen.getByRole('alert')).toHaveTextContent('liquidation price')
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeDisabled()

    fireEvent.change(screen.getByLabelText('Stop loss (%)'), { target: { value: '25' } })
    fireEvent.change(screen.getByRole('spinbutton', { name: 'Leverage' }), { target: { value: '6' } })
    expect(screen.getByLabelText('Stop loss (USDG)')).toHaveValue('')
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeDisabled()
    wagmiMocks.useReadContracts.mockReturnValue({
      data: [{ status: 'success', result: { ...openPreviewResult, postMarginAssets: 40_000_000n, liquidationPrice: 110_000_000n } }],
      isFetching: false, isLoading: false,
    })
    await act(async () => { vi.advanceTimersByTime(300) })
    expect(screen.getByLabelText('Stop loss (USDG)')).toHaveValue('0.91')
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeEnabled()
  })

  it('revalidates the fixed reviewed stop when the opening preview liquidation changes', async () => {
    wagmiMocks.useReadContracts.mockReturnValue({
      data: [{ status: 'success', result: { ...openPreviewResult, liquidationPrice: 110_000_000n } }],
      isFetching: false, isLoading: false,
    })
    const props = { enableLiveTrading: true, initialDirection: 'long' as const, initialOrderQuantity: '100',
      oraclePriceRaw: 100_000_000n, oraclePublishTime: 1_700_000_000, availableToTradeRaw: 1_000_000_000n,
      protectionCapPrice: 200_000_000n, protectionConfiguration: { enabled: true, triggerBountyAssets: 200_000n, executionBountyAssets: 200_000n } }
    const view = render(<PerpsTradeTicket {...props} />)
    fireEvent.click(screen.getByRole('checkbox', { name: 'Take profit / stop loss' }))
    await act(async () => { vi.advanceTimersByTime(300) })
    fireEvent.change(screen.getByLabelText('Stop loss (%)'), { target: { value: '25' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review Long' }))
    expect(wagmiMocks.prepareOrder).toHaveBeenCalledWith(expect.objectContaining({ positionProtection: { takeProfitTriggerPrice: 0n, stopLossTriggerPrice: 104_000_000n, closeLimits: relaxedProtectionCloseLimits() } }))
    // Recomputing 25% with this new margin would produce a valid .99 stop,
    // but the reviewed .96 stop is now beyond the .97 liquidation boundary.
    wagmiMocks.useReadContracts.mockReturnValue({
      data: [{ status: 'success', result: { ...openPreviewResult, postEntryPrice: 100_000_000n, postMarginAssets: 4_000_000n, liquidationPrice: 103_000_000n } }],
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
    expect(within(previewPanel!).getByLabelText('Pay 25.0 USDG')).toBeInTheDocument()
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
    expect(within(orderExposureRow!).getByText('USDG')).toBeInTheDocument()
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
      postMarginAssets: 220_000_000n,
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

    expect(screen.getAllByLabelText('Net paid 85.0 USDG')).toHaveLength(2)
    expect(screen.getAllByLabelText('Pay 25.0 USDG')).toHaveLength(2)

    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))

    const confirmation = screen.getByRole('dialog')
    expect(within(confirmation).getByLabelText('Net paid 85.0 USDG')).toBeInTheDocument()
    expect(within(confirmation).getByLabelText('Pay 25.0 USDG')).toBeInTheDocument()
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
    expect(within(confirmation).getByLabelText('Pay 25.0 USDG')).toBeInTheDocument()
  })
})

vi.mock('../../perps-aa/deadlineClock', () => ({ deadlineNow: () => Date.now(), refreshDeadlineClock: async () => ({ now: () => Date.now() }), observeDeadlineResponse: () => {} }))
