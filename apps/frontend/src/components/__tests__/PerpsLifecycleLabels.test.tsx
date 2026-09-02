import { useState } from 'react'
import { act, fireEvent, render, screen, waitFor, within } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'

const identityMocks = vi.hoisted(() => ({
  isAaManifestConfigured: false,
  usdcSupportsEip3009: false,
}))

const wagmiMocks = vi.hoisted(() => ({
  readContractsData: undefined as readonly unknown[] | undefined,
}))

vi.mock('../../perps-aa', async () => {
  const { useSponsoredOperationStore } = await import('../../perps-aa/operationStore')
  const {
    BundlerRequestError,
    findBundlerRequestError,
  } = await import('../../perps-aa/errors')
  const address = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
  return {
    BundlerRequestError,
    findBundlerRequestError,
    useSponsoredOperationStore,
    usePerpsIdentity: () => ({
      status: 'ready',
      ownerAddress: address,
      accountAddress: address,
      chainId: 421614,
      isAaManifestConfigured: identityMocks.isAaManifestConfigured,
      sponsorshipEnabled: identityMocks.isAaManifestConfigured,
      manifest: identityMocks.isAaManifestConfigured
        ? {
            smartAccountMode: 'simple',
            usdcSupportsEip3009: identityMocks.usdcSupportsEip3009,
            userOperationExplorerUrlTemplate:
              'https://arbitrum-sepolia.blockscout.com/op/{userOperationHash}',
          }
        : null,
      identity: null,
      proposedIdentity: null,
      changedIdentityFields: [],
      error: null,
      confirmIdentityAfterContinuityCheck: () => false,
      reloadIdentity: () => undefined,
    }),
  }
})
import { PerpsAccountPanel } from '../PerpsAccountPanel'
import { PerpsTradeTicket } from '../PerpsTradeTicket'
import {
  BundlerRequestError,
  useSponsoredOperationStore,
} from '../../perps-aa'
import { PerpsOrderFundingShortfallError } from '../../contracts/preparePerpsOrderV2'
import type { PerpsExecutionAssessment, PreparedPerpsOrderV2 } from '../../contracts/perpsOrderV2'

const V2_ACCOUNT = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B' as const
const V2_CLIENT_ORDER_ID = `0x${'12'.repeat(32)}` as `0x${string}`
const V2_RECEIPT_HASH = `0x${'34'.repeat(32)}` as `0x${string}`

vi.mock('@reown/appkit/react', () => ({
  createAppKit: vi.fn(),
  useAppKit: () => ({
    open: vi.fn(),
  }),
  useAppKitNetwork: () => ({
    switchNetwork: vi.fn(),
  }),
}))

let mockIsConnected = false
const perpsTradingMocks = vi.hoisted(() => ({
  fundTradingAccount: vi.fn(),
  depositMargin: vi.fn(),
  withdrawMargin: vi.fn(),
  addPositionMargin: vi.fn(),
  findMaxOpenOrder: vi.fn(),
  prepareOrder: vi.fn(),
  commitOrder: vi.fn(),
  readOrderLifecycleOutcome: vi.fn(),
  executeOrder: vi.fn(),
  cleanupExpiredOrder: vi.fn(),
  waitForPerpsOrderTerminal: vi.fn(),
}))

vi.mock('wagmi', () => ({
  http: vi.fn(() => ({})),
  useAccount: () => ({
    address: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
    isConnected: mockIsConnected,
  }),
  useChainId: () => 421614,
  useReadContracts: () => ({
    data: wagmiMocks.readContractsData,
  }),
  useSimulateContract: () => ({
    error: null,
    isFetching: false,
    isLoading: false,
  }),
  useSwitchChain: () => ({
    switchChain: vi.fn(),
  }),
}))

vi.mock('../../hooks', () => ({
  usePerpsTrading: () => ({
    fundTradingAccount: perpsTradingMocks.fundTradingAccount,
    depositMargin: perpsTradingMocks.depositMargin,
    withdrawMargin: perpsTradingMocks.withdrawMargin,
    addPositionMargin: perpsTradingMocks.addPositionMargin,
    findMaxOpenOrder: perpsTradingMocks.findMaxOpenOrder,
    prepareOrder: perpsTradingMocks.prepareOrder,
    commitOrder: perpsTradingMocks.commitOrder,
    readOrderLifecycleOutcome: perpsTradingMocks.readOrderLifecycleOutcome,
    executeOrder: perpsTradingMocks.executeOrder,
    cleanupExpiredOrder: perpsTradingMocks.cleanupExpiredOrder,
  }),
  useSwitchToArbitrumSepolia: () => ({
    switchToArbitrumSepolia: vi.fn(),
    isSwitching: false,
    switchError: null,
    clearSwitchError: vi.fn(),
  }),
  waitForPerpsOrderTerminal: perpsTradingMocks.waitForPerpsOrderTerminal,
}))

describe('perps lifecycle labels', () => {
  beforeEach(() => {
    globalThis.localStorage.clear()
    mockIsConnected = false
    identityMocks.isAaManifestConfigured = false
    identityMocks.usdcSupportsEip3009 = false
    wagmiMocks.readContractsData = undefined
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    vi.useRealTimers()
    Object.values(perpsTradingMocks).forEach((mock) => {
      mock.mockReset()
    })
    perpsTradingMocks.readOrderLifecycleOutcome.mockResolvedValue(undefined)
    perpsTradingMocks.prepareOrder.mockResolvedValue({
      account: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
      manifestVersion: 'perps-aa-arbitrum-sepolia-v2',
      orderRouter: '0x1111111111111111111111111111111111111111',
      orderLifecycleBook: '0x2222222222222222222222222222222222222222',
      request: {
        clientOrderId: `0x${'12'.repeat(32)}`,
        side: 0,
        sizeDelta: 100_000_000n,
        marginDelta: 20_000_000n,
        targetPrice: 100_100_000n,
        isClose: false,
        bounds: {
          validUntil: 1_700_000_300n,
          allowedExecutionModes: 1,
          expectedConfigHash: `0x${'34'.repeat(32)}`,
          maxExecutionBountyUsdc: 10_000n,
          maxExecutionNotionalUsdc: 100_000_000n,
          maxGrossAccountDebitUsdc: 120_010_000n,
          maxActionChargeUsdc: 1_000_000n,
          maxExplicitFeesUsdc: 1_000_000n,
          maxPostPositionSize: 100_000_000n,
          minPostSettlementBalanceUsdc: 800_000_000n,
          minPostPositionEquityUsdc: 20_000_000n,
          maxPostLeverageBps: 50_000,
        },
      },
      executionBountyUsdc: 10_000n,
      reviewedBlockNumber: 123n,
      reviewedBlockHash: `0x${'56'.repeat(32)}`,
      reviewedPrice: 100_000_000n,
      protection: {
        validUntil: 1_700_000_300n,
        executionMode: 1,
        executionBountyUsdc: 10_000n,
      },
    })
  })

  async function startDelayedSponsoredCommit() {
    mockIsConnected = true
    identityMocks.isAaManifestConfigured = true
    wagmiMocks.readContractsData = [{
      status: 'success',
      result: { valid: true },
    }]
    perpsTradingMocks.commitOrder.mockReturnValue(new Promise<never>(() => undefined))

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialReviewOpen
        initialOrderQuantity="100"
        oraclePriceRaw={100_000_000n}
        oraclePublishTime={Math.floor(Date.now() / 1_000)}
        availableToTradeRaw={1_000_000_000n}
      />
    )

    await act(async () => undefined)
    expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeEnabled()
    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))
    expect(perpsTradingMocks.commitOrder).toHaveBeenCalledOnce()

    const commitInput = perpsTradingMocks.commitOrder.mock.calls[0]?.[0] as {
      onStatus?: (
        status: 'awaiting-signature' | 'submitting' | 'confirming'
      ) => void
    } | undefined
    if (!commitInput?.onStatus) {
      throw new Error('Expected commitOrder to receive an onStatus callback')
    }
    return commitInput.onStatus
  }

  it('clears the delayed wallet warning when the signed operation starts submitting', async () => {
    vi.useFakeTimers()
    const onStatus = await startDelayedSponsoredCommit()

    act(() => onStatus('awaiting-signature'))
    expect(screen.getByText('Waiting for wallet confirmation')).toBeInTheDocument()

    await act(async () => {
      vi.advanceTimersByTime(14_999)
    })
    expect(screen.queryByText(/No wallet response yet/)).not.toBeInTheDocument()

    await act(async () => {
      vi.advanceTimersByTime(1)
    })
    expect(screen.getByText(/No wallet response yet/)).toBeInTheDocument()

    await act(async () => {
      onStatus('submitting')
    })
    await act(async () => {
      vi.advanceTimersByTime(0)
    })
    expect(screen.getByText('Submitting sponsored transaction')).toBeInTheDocument()
    expect(screen.queryByText(/No wallet response yet/)).not.toBeInTheDocument()

    await act(async () => {
      vi.advanceTimersByTime(15_000)
    })
    expect(screen.queryByText(/No wallet response yet/)).not.toBeInTheDocument()

    await act(async () => {
      onStatus('confirming')
    })
    await act(async () => {
      vi.advanceTimersByTime(0)
    })
    expect(screen.getByText('Waiting for on-chain confirmation')).toBeInTheDocument()

    await act(async () => {
      vi.advanceTimersByTime(15_000)
    })
    expect(screen.queryByText(/No wallet response yet/)).not.toBeInTheDocument()
  })

  it('cancels the wallet warning timer when signing finishes before the threshold', async () => {
    vi.useFakeTimers()
    const onStatus = await startDelayedSponsoredCommit()

    act(() => onStatus('awaiting-signature'))
    await act(async () => {
      vi.advanceTimersByTime(10_000)
    })

    act(() => onStatus('submitting'))
    await act(async () => {
      vi.advanceTimersByTime(30_000)
    })

    expect(screen.getByText('Submitting sponsored transaction')).toBeInTheDocument()
    expect(screen.queryByText(/No wallet response yet/)).not.toBeInTheDocument()
  })

  it('keeps indexed inclusion through a safe timeout and revokes it if the row disappears', async () => {
    mockIsConnected = true
    identityMocks.isAaManifestConfigured = true
    wagmiMocks.readContractsData = [{
      status: 'success',
      result: { valid: true },
    }]
    perpsTradingMocks.waitForPerpsOrderTerminal.mockReturnValue(
      new Promise(() => {})
    )

    let rejectSafeConfirmation: (error: Error) => void = () => {}
    perpsTradingMocks.commitOrder.mockReturnValue(new Promise((_, reject) => {
      rejectSafeConfirmation = reject
    }))
    const onAccountRefresh = vi.fn()
    const baseProps = {
      enableLiveTrading: true,
      initialReviewOpen: true,
      initialOrderQuantity: '100',
      oraclePriceRaw: 100_000_000n,
      oraclePublishTime: Math.floor(Date.now() / 1_000),
      availableToTradeRaw: 1_000_000_000n,
      onAccountRefresh,
    }
    const terminalOrder = {
      orderId: 9177n,
      time: '30 Jul, 19:05',
      market: 'plDXY Perp',
      side: 'Long',
      type: 'Open',
      price: '1.0000',
      size: '100',
      status: 'Executed' as const,
      account:
        '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B' as const,
      clientOrderId: `0x${'12'.repeat(32)}` as `0x${string}`,
      commitTxHash:
        '0xf4a07414941a4d90b5be13743db20f451e58fcf27ceaba670eac26e5d0b4822e' as const,
      revealTxHash:
        '0x77f23300000000000000000000000000000000000000000000000000000067d1' as const,
      receiptHash:
        '0x88f23300000000000000000000000000000000000000000000000000000067d1' as const,
      executionPriceRaw: 100_000_000n,
      executionEconomicsVersion: 2,
    }

    const { rerender } = render(
      <PerpsTradeTicket {...baseProps} orderHistory={[]} />
    )

    await waitFor(() => {
      expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeEnabled()
    })
    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))
    const commitInput = perpsTradingMocks.commitOrder.mock.calls[0]?.[0] as {
      onStatus?: (
        status: 'awaiting-signature' | 'confirming'
      ) => void
      onIncluded?: (result: {
        account: string
        clientOrderId: string
        hash: `0x${string}`
        orderId: bigint
      }) => void
    } | undefined
    expect(commitInput?.onStatus).toBeTypeOf('function')
    expect(commitInput?.onIncluded).toBeTypeOf('function')

    await act(async () => {
      commitInput?.onStatus?.('awaiting-signature')
      commitInput?.onStatus?.('confirming')
    })
    expect(
      screen.getByText('Waiting for on-chain confirmation')
    ).toBeInTheDocument()

    await act(async () => {
      commitInput?.onIncluded?.({
        account: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
        clientOrderId: `0x${'12'.repeat(32)}`,
        hash: terminalOrder.commitTxHash,
        orderId: terminalOrder.orderId,
      })
    })

    expect(
      screen.queryByText('Waiting for on-chain confirmation')
    ).not.toBeInTheDocument()
    expect(
      screen.getByText('Waiting for verified market data')
    ).toBeInTheDocument()
    expect(perpsTradingMocks.waitForPerpsOrderTerminal).toHaveBeenCalledWith(
      expect.objectContaining({ orderId: terminalOrder.orderId })
    )

    rerender(
      <PerpsTradeTicket
        {...baseProps}
        orderHistory={[terminalOrder]}
      />
    )
    await waitFor(() => {
      expect(screen.getByText('Final Result')).toBeInTheDocument()
    })

    await act(async () => {
      rejectSafeConfirmation(
        new Error(
          'Timed out reconciling the locally persisted UserOperation hash with Pimlico',
          {
            cause: new BundlerRequestError({
              message:
                'Timed out reconciling the locally persisted UserOperation hash with Pimlico',
              retryable: false,
              terminalStatus: 'receipt-timeout',
            }),
          }
        )
      )
    })

    expect(screen.getByText('Final Result')).toBeInTheDocument()
    expect(screen.queryByText('Commit transaction failed')).not.toBeInTheDocument()
    expect(onAccountRefresh).toHaveBeenCalledTimes(2)

    rerender(
      <PerpsTradeTicket
        {...baseProps}
        orderHistory={[]}
      />
    )
    await waitFor(() => {
      expect(screen.getByText('Commit transaction failed')).toBeInTheDocument()
    })
    expect(
      screen.getByText(
        'Timed out reconciling the locally persisted UserOperation hash with Pimlico'
      )
    ).toBeInTheDocument()
    expect(screen.queryByText('Final Result')).not.toBeInTheDocument()
  })

  it('does not suppress a safe-head timeout after indexed evidence disappears', async () => {
    mockIsConnected = true
    identityMocks.isAaManifestConfigured = true
    wagmiMocks.readContractsData = [{
      status: 'success',
      result: { valid: true },
    }]
    perpsTradingMocks.waitForPerpsOrderTerminal.mockReturnValue(
      new Promise(() => {})
    )

    let rejectSafeConfirmation: (error: Error) => void = () => {}
    perpsTradingMocks.commitOrder.mockReturnValue(new Promise((_, reject) => {
      rejectSafeConfirmation = reject
    }))
    const baseProps = {
      enableLiveTrading: true,
      initialReviewOpen: true,
      initialOrderQuantity: '100',
      oraclePriceRaw: 100_000_000n,
      oraclePublishTime: Math.floor(Date.now() / 1_000),
      availableToTradeRaw: 1_000_000_000n,
    }
    const indexedOrder = {
      orderId: 9177n,
      time: '30 Jul, 19:05',
      market: 'plDXY Perp',
      side: 'Long',
      type: 'Open',
      price: '1.0000',
      size: '100',
      status: 'Committed' as const,
      account: V2_ACCOUNT,
      clientOrderId: V2_CLIENT_ORDER_ID,
      commitTxHash:
        '0xf4a07414941a4d90b5be13743db20f451e58fcf27ceaba670eac26e5d0b4822e' as const,
    }

    const { rerender } = render(
      <PerpsTradeTicket {...baseProps} orderHistory={[]} />
    )

    await waitFor(() => {
      expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeEnabled()
    })
    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))
    const commitInput = perpsTradingMocks.commitOrder.mock.calls[0]?.[0] as {
      onIncluded?: (result: {
        account: string
        clientOrderId: string
        hash: `0x${string}`
        orderId: bigint
      }) => void
    } | undefined

    await act(async () => {
      commitInput?.onIncluded?.({
        account: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
        clientOrderId: `0x${'12'.repeat(32)}`,
        hash: indexedOrder.commitTxHash,
        orderId: indexedOrder.orderId,
      })
    })
    expect(
      screen.getByText('Waiting for verified market data')
    ).toBeInTheDocument()

    rerender(
      <PerpsTradeTicket {...baseProps} orderHistory={[indexedOrder]} />
    )
    rerender(
      <PerpsTradeTicket {...baseProps} orderHistory={[]} />
    )

    await act(async () => {
      rejectSafeConfirmation(
        new Error(
          'Timed out reconciling the locally persisted UserOperation hash with Pimlico',
          {
            cause: new BundlerRequestError({
              message:
                'Timed out reconciling the locally persisted UserOperation hash with Pimlico',
              retryable: false,
              terminalStatus: 'receipt-timeout',
            }),
          }
        )
      )
    })

    expect(screen.getByText('Commit transaction failed')).toBeInTheDocument()
    expect(
      screen.getByText(
        'Timed out reconciling the locally persisted UserOperation hash with Pimlico'
      )
    ).toBeInTheDocument()
    expect(
      screen.queryByText('Waiting for verified market data')
    ).not.toBeInTheDocument()
  })

  it('lets an authoritative safe-head revert override optimistic inclusion', async () => {
    mockIsConnected = true
    identityMocks.isAaManifestConfigured = true
    wagmiMocks.readContractsData = [{
      status: 'success',
      result: { valid: true },
    }]
    perpsTradingMocks.waitForPerpsOrderTerminal.mockReturnValue(
      new Promise(() => {})
    )

    let rejectSafeConfirmation: (error: Error) => void = () => {}
    perpsTradingMocks.commitOrder.mockReturnValue(new Promise((_, reject) => {
      rejectSafeConfirmation = reject
    }))

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialReviewOpen
        initialOrderQuantity="100"
        oraclePriceRaw={100_000_000n}
        oraclePublishTime={Math.floor(Date.now() / 1_000)}
        availableToTradeRaw={1_000_000_000n}
      />
    )

    await waitFor(() => {
      expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeEnabled()
    })
    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))
    const commitInput = perpsTradingMocks.commitOrder.mock.calls[0]?.[0] as {
      onStatus?: (
        status: 'awaiting-signature' | 'confirming'
      ) => void
      onIncluded?: (result: {
        account: string
        clientOrderId: string
        hash: `0x${string}`
        orderId: bigint
      }) => void
    } | undefined

    await act(async () => {
      commitInput?.onStatus?.('awaiting-signature')
      commitInput?.onStatus?.('confirming')
      commitInput?.onIncluded?.({
        account: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
        clientOrderId: `0x${'12'.repeat(32)}`,
        hash:
          '0xf4a07414941a4d90b5be13743db20f451e58fcf27ceaba670eac26e5d0b4822e',
        orderId: 9177n,
      })
    })
    expect(
      screen.getByText('Waiting for verified market data')
    ).toBeInTheDocument()

    await act(async () => {
      rejectSafeConfirmation(
        new Error('UserOperation reverted at the safe head', {
          cause: new BundlerRequestError({
            message: 'UserOperation reverted at the safe head',
            retryable: false,
            terminalStatus: 'execution-reverted',
          }),
        })
      )
    })

    expect(screen.getByText('Commit transaction failed')).toBeInTheDocument()
    expect(
      screen.getByText('UserOperation reverted at the safe head')
    ).toBeInTheDocument()
    expect(
      screen.queryByText('Waiting for verified market data')
    ).not.toBeInTheDocument()
  })

  it('distinguishes plDXY Perp exposure from order quantity and execution notional', () => {
    render(
      <>
        <PerpsTradeTicket
          initialLifecycleState="executed"
          initialReviewOpen
          initialDirection="long"
          initialOrderQuantity="2 000"
        />
        <PerpsAccountPanel
          isConnected
          position={{
            exists: true,
            side: 0,
            direction: 'long',
            size: 2_000n * 10n ** 18n,
            entryPrice: 98300000n,
            marginUsdc: 400000000n,
            unrealizedPnlUsdc: -250000n,
            maintenanceMarginUsdc: 0n,
            liquidatable: false,
            estimatedNotionalUsdc: 1999920000n,
            entryNotionalUsdc: 2000000000n,
            dxyExposureUsdc: 2069380000n,
            pendingCarryUsdc: 1250000n,
          }}
        />
      </>
    )

    expect(screen.getAllByText('plDXY Perp exposure').length).toBeGreaterThan(0)
    expect(screen.queryByText('Contract notional')).not.toBeInTheDocument()
    expect(screen.getAllByText('Order quantity').length).toBeGreaterThan(0)
    expect(screen.getByText('Trade executed at 1.0089 USDC')).toBeInTheDocument()
    const initialConfetti = document.querySelector('[data-finalization-confetti]')
    expect(initialConfetti).toHaveAttribute('aria-hidden', 'true')

    const celebrationCard = screen.getByRole('button', { name: 'Replay celebration confetti' })
    vi.spyOn(celebrationCard, 'getBoundingClientRect').mockReturnValue({
      bottom: 218,
      height: 208,
      left: 20,
      right: 420,
      top: 10,
      width: 400,
      x: 20,
      y: 10,
      toJSON: () => ({}),
    })

    fireEvent.click(celebrationCard, { clientX: 140, clientY: 90, detail: 1 })

    const replayedConfetti = document.querySelector('[data-finalization-confetti]')
    expect(replayedConfetti).toHaveAttribute('aria-hidden', 'true')
    expect(replayedConfetti).not.toBe(initialConfetti)
    expect(document.querySelector('[data-confetti-origin]')).toHaveStyle({ left: '120px', top: '80px' })

    fireEvent.click(celebrationCard, { clientX: 360, clientY: 210, detail: 1 })

    expect(document.querySelector('[data-confetti-origin]')).toHaveStyle({
      left: '340px',
      top: '200px',
      transform: 'rotate(180deg)',
    })
    const finalResult = screen.getByText('Final Result').closest('div')?.parentElement
    expect(finalResult).toBeInTheDocument()
    const finalResultQueries = within(finalResult!)
    expect(finalResultQueries.getByText('Order quantity')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Execution plDXY Perp exposure')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Order quantity')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Margin posted')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Protocol execution fee')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Execution reward')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Execution plDXY Perp exposure is the committed Order quantity valued at the final displayed price.')).toBeInTheDocument()
    expect(finalResultQueries.queryByText('Estimated protocol execution fee')).not.toBeInTheDocument()
    expect(finalResultQueries.queryByText('Estimated execution reward')).not.toBeInTheDocument()

    const positionQuantityLabel = screen.getAllByText('Order quantity').find((label) =>
      label.parentElement?.classList.contains('uppercase')
    )
    expect(positionQuantityLabel).toBeDefined()
    expect(positionQuantityLabel?.closest('div')?.parentElement).toHaveTextContent('2 000')
    expect(positionQuantityLabel?.closest('div')?.parentElement).toHaveTextContent('plDXY')
    expect(screen.getByText('Entry price')).toBeInTheDocument()
    expect(screen.getByText('1.0170')).toBeInTheDocument()
    expect(screen.queryByText('0.9830')).not.toBeInTheDocument()
    expect(screen.getAllByText('Unrealized PnL').length).toBeGreaterThan(0)
    expect(screen.getByText('Cost of carry')).toBeInTheDocument()
    expect(screen.getByText('1.25')).toBeInTheDocument()
    expect(screen.getByText(/Order quantity stays fixed between size-changing trades\. plDXY Perp exposure moves with the current price\./)).toBeInTheDocument()
  })

  it('uses terminal post-position evidence instead of committed full-close intent', () => {
    render(
      <PerpsTradeTicket
        initialLifecycleState="executed"
        initialReviewOpen
        initialDirection="short"
        initialReduceOnly
        initialOrderQuantity="1 014.2"
        initialOrderId={81n}
        initialCommittedIsFullClose
        initialCommittedSizeDelta={1_000n * 10n ** 18n}
        initialFinalExecutionPrice={98_300_000n}
        orderHistory={[{
          orderId: 81n,
          time: '31 Aug, 00:15',
          market: 'plDXY Perp',
          side: 'Long',
          type: 'Close',
          price: '1.0170',
          size: '1 000',
          status: 'Executed',
          account: V2_ACCOUNT,
          clientOrderId: V2_CLIENT_ORDER_ID,
          receiptHash: V2_RECEIPT_HASH,
          receiptEconomics: {
            postPositionSize: (100n * 10n ** 18n).toString(),
          },
          executionEconomicsVersion: 2,
        }]}
      />
    )

    expect(screen.getByText(/Long plDXY Perp position reduced at/)).toBeInTheDocument()
    expect(screen.queryByText(/Long plDXY Perp position closed at/)).not.toBeInTheDocument()
    expect(screen.getByText('Executed reduction exposure')).toBeInTheDocument()
  })

  it('uses zero terminal post-position evidence to identify an actual full close', () => {
    render(
      <PerpsTradeTicket
        initialLifecycleState="executed"
        initialReviewOpen
        initialDirection="short"
        initialReduceOnly
        initialOrderQuantity="1 014.2"
        initialOrderId={82n}
        initialCommittedIsFullClose={false}
        initialCommittedSizeDelta={1_000n * 10n ** 18n}
        initialFinalExecutionPrice={98_300_000n}
        orderHistory={[{
          orderId: 82n,
          time: '31 Aug, 00:16',
          market: 'plDXY Perp',
          side: 'Long',
          type: 'Close',
          price: '1.0170',
          size: '1 000',
          status: 'Executed',
          account: V2_ACCOUNT,
          clientOrderId: V2_CLIENT_ORDER_ID,
          receiptHash: V2_RECEIPT_HASH,
          receiptEconomics: {
            postPositionSize: '0',
          },
          executionEconomicsVersion: 2,
        }]}
      />
    )

    expect(screen.getByText(/Long plDXY Perp position closed at/)).toBeInTheDocument()
    expect(screen.queryByText(/Long plDXY Perp position reduced at/)).not.toBeInTheDocument()
    expect(screen.getByText('Executed close exposure')).toBeInTheDocument()
  })

  it('resets the review modal lifecycle when it closes', () => {
    render(
      <PerpsTradeTicket
        initialLifecycleState="executed"
        initialReviewOpen
        initialDirection="long"
        initialOrderQuantity="2 000"
      />
    )

    expect(screen.getByText('Final Result')).toBeInTheDocument()

    fireEvent.keyDown(document, { key: 'Escape' })
    fireEvent.click(screen.getByRole('button', { name: 'Review Long' }))

    expect(screen.getByText('Commit Preview')).toBeInTheDocument()
    expect(screen.queryByText('Final Result')).not.toBeInTheDocument()
    expect(document.querySelector('[data-finalization-confetti]')).not.toBeInTheDocument()
  })

  it('keeps the leverage field and slider in sync', () => {
    render(<PerpsTradeTicket />)

    const leverageInput = screen.getByRole('spinbutton', { name: 'Leverage' })
    const leverageSlider = screen.getByRole('slider', { name: 'Leverage slider' })

    expect(leverageInput).toHaveValue(5)
    expect((leverageSlider as HTMLInputElement).value).toBe('5')

    fireEvent.change(leverageInput, { target: { value: '12' } })

    expect(leverageInput).toHaveValue(12)
    expect((leverageSlider as HTMLInputElement).value).toBe('12')

    fireEvent.change(leverageInput, { target: { value: '99' } })
    fireEvent.blur(leverageInput)

    expect(leverageInput).toHaveValue(33)
    expect((leverageSlider as HTMLInputElement).value).toBe('33')
  })

  it('keeps advanced preview rows behind a show-more control', () => {
    render(<PerpsTradeTicket />)

    const previewPanel = screen.getByText('Preview').parentElement
    expect(previewPanel).not.toBeNull()
    const preview = within(previewPanel!)

    expect(preview.getByText('Required margin')).toBeInTheDocument()
    expect(preview.getByText('Execution limit')).toBeInTheDocument()
    expect(preview.getByText('Liquidation price')).toBeInTheDocument()
    expect(preview.getByText('Estimated fee')).toBeInTheDocument()
    expect(preview.queryByText('Contract notional')).not.toBeInTheDocument()
    expect(preview.queryByText('Order quantity')).not.toBeInTheDocument()
    expect(preview.queryByText('Maintenance margin')).not.toBeInTheDocument()
    expect(preview.queryByText('Estimated execution reward')).not.toBeInTheDocument()

    const showMoreButton = preview.getByRole('button', { name: 'Show more...' })
    expect(showMoreButton).toHaveAttribute('aria-expanded', 'false')
    fireEvent.click(showMoreButton)

    expect(preview.queryByText('Contract notional')).not.toBeInTheDocument()
    expect(preview.queryByText('Order quantity')).not.toBeInTheDocument()
    expect(preview.getByText('Maintenance margin')).toBeInTheDocument()
    expect(preview.getByText('Estimated execution reward')).toBeInTheDocument()
    expect(preview.getByRole('button', { name: 'Show less' }))
      .toHaveAttribute('aria-expanded', 'true')
  })

  it('renders order and transaction history tabs from live rows', () => {
    render(
      <PerpsAccountPanel
        isConnected
        orderHistory={[
          {
            orderId: 30n,
            time: '10 Jun, 14:05',
            market: 'plDXY Perp',
            side: 'Long',
            type: 'Open',
            price: '1.0170',
            size: '1 999.67',
            status: 'Executed',
            account: V2_ACCOUNT,
            clientOrderId: V2_CLIENT_ORDER_ID,
            receiptHash: V2_RECEIPT_HASH,
            executionEconomicsVersion: 2,
            commitTxHash: '0x9d4b00000000000000000000000000000000f953',
            revealTxHash: '0x6c0d00000000000000000000000000000000b7d3',
          },
          {
            orderId: 31n,
            time: '10 Jun, 14:07',
            market: 'plDXY Perp',
            side: 'Long',
            type: 'Commit',
            price: 'Not executed',
            size: 'Not executed',
            status: 'Failed: Slippage exceeded',
            account: V2_ACCOUNT,
            clientOrderId: `0x${'13'.repeat(32)}`,
            receiptHash: `0x${'35'.repeat(32)}`,
            commitTxHash: '0x9d4b00000000000000000000000000000000f954',
            revealTxHash: '0x6c0d00000000000000000000000000000000b7d4',
            terminalReason: 'Slippage',
          },
        ]}
        tradeHistory={[
          {
            time: '10 Jun, 14:06',
            market: 'plDXY Perp',
            side: 'Open Long',
            price: '1.0170',
            size: '1 999.67',
            txHash: '0x6c0d00000000000000000000000000000000b7d3',
          },
          {
            time: '10 Jun, 13:55',
            market: 'Margin Account',
            side: 'Deposit',
            price: '--',
            size: '500',
            txHash: '0x5e7100000000000000000000000000000000d005',
          },
          {
            time: '10 Jun, 13:58',
            market: 'plDXY Perp',
            side: 'Add margin',
            price: '--',
            size: '25',
            txHash: '0xadad000000000000000000000000000000000add',
          },
          {
            time: '10 Jun, 13:40',
            market: 'plDXY Perp',
            side: 'Liquidated Long',
            price: '1.0300',
            size: '1 000',
            pnl: 'Liquidation reward 0.2',
            txHash: '0x1d1000000000000000000000000000000000001d0',
          },
        ]}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: 'Order History' }))
    expect(screen.getByText('30')).toBeInTheDocument()
    expect(screen.getByText('Executed')).toBeInTheDocument()
    expect(screen.getByText('1 999.67')).toBeInTheDocument()
    expect(screen.getByText('Failed: Slippage exceeded')).toBeInTheDocument()
    expect(screen.getAllByText('Not executed')).toHaveLength(2)
    expect(screen.getAllByText('Commit').length).toBeGreaterThan(0)
    expect(screen.getByText('Reveal')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Transaction History' }))
    expect(screen.getByText('Open Long')).toBeInTheDocument()
    expect(screen.getByText('1.0170')).toBeInTheDocument()
    expect(screen.queryByText('0.9830')).not.toBeInTheDocument()
    expect(screen.getByText('1 999.67')).toBeInTheDocument()
    expect(screen.getByText('Action')).toBeInTheDocument()
    expect(screen.getByText('Result')).toBeInTheDocument()
    expect(screen.getByText('Deposit')).toBeInTheDocument()
    expect(screen.getByText('Margin Account')).toBeInTheDocument()
    expect(screen.getByText('Add margin')).toBeInTheDocument()
    expect(screen.getByText('Liquidated Long')).toBeInTheDocument()
    expect(screen.getByText('Liquidation reward 0.2')).toBeInTheDocument()
  })

  it('fills current position and max with the exact plDXY order quantity', () => {
    render(
      <PerpsTradeTicket
        initialDirection="short"
        currentPosition={{
          exists: true,
          side: 0,
          direction: 'long',
          size: 1_500n * 10n ** 18n,
          entryPrice: 98300000n,
          marginUsdc: 300000000n,
          unrealizedPnlUsdc: 0n,
          maintenanceMarginUsdc: 0n,
          liquidatable: false,
          estimatedNotionalUsdc: 1499470000n,
          entryNotionalUsdc: 1500000000n,
          dxyExposureUsdc: 1553249999n,
        }}
      />
    )

    expect(screen.getAllByText('1 553.25').length).toBeGreaterThan(0)

    fireEvent.click(screen.getByRole('button', { name: /Max:/ }))

    expect(screen.getByRole('textbox')).toHaveValue('1 500')

    fireEvent.change(screen.getByRole('textbox'), { target: { value: '0' } })
    fireEvent.click(screen.getByRole('button', { name: /Current Position/ }))

    expect(screen.getByRole('textbox')).toHaveValue('1 500')
  })

  it('calculates an executable opening Max before filling the quantity', async () => {
    mockIsConnected = true
    wagmiMocks.readContractsData = [{
      status: 'success',
      result: { valid: true },
    }]
    let resolveMaxOrder!: (value: { sizeDelta: bigint }) => void
    perpsTradingMocks.findMaxOpenOrder.mockReturnValue(
      new Promise((resolve) => {
        resolveMaxOrder = resolve
      }),
    )

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialOrderQuantity="0"
        oraclePriceRaw={100_000_000n}
        oraclePublishTime={Math.floor(Date.now() / 1_000)}
        availableToTradeRaw={100_000_000n}
        longOpenCapacityUsdc={1_000_000_000n}
        minOpenNotionalUsdc={100_000_000n}
        minNewPositionNotionalUsdc={100_000_000n}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: 'Calculate executable Max' }))
    expect(screen.getByRole('button', { name: 'Calculating executable Max…' })).toBeDisabled()

    await act(async () => {
      resolveMaxOrder({ sizeDelta: 400n * 10n ** 18n })
    })

    await waitFor(() => {
      expect(screen.getByRole('textbox', { name: 'Order quantity' })).toHaveValue('400')
    })
    expect(screen.getByRole('button', { name: /Max: 400 plDXY/ })).toBeEnabled()
    expect(perpsTradingMocks.findMaxOpenOrder).toHaveBeenCalledWith(expect.objectContaining({
      direction: 'long',
      selectedMaxLeverageBps: 50_000,
    }))
  })

  it('shows protected margin and exact shortfall without enabling confirmation', async () => {
    mockIsConnected = true
    identityMocks.isAaManifestConfigured = true
    wagmiMocks.readContractsData = [{
      status: 'success',
      result: { valid: true },
    }]
    const reviewSummary = {
      requiredMarginUsdc: 30_000_000n,
      executionBountyUsdc: 10_000n,
      requiredFundingUsdc: 30_010_000n,
      availableFundingUsdc: 20_020_000n,
      worstPostLeverageBps: 50_000n,
      reviewedBlockNumber: 123n,
      reviewedBlockHash: `0x${'56'.repeat(32)}` as `0x${string}`,
      reviewedPrice: 100_000_000n,
      currentAssessment: {
        executionFeeUsdc: 40_000n,
        vpiUsdc: 1_000_000n,
      } as PerpsExecutionAssessment,
    }
    const reviewedPreparedOrder = {
      account: V2_ACCOUNT,
      manifestVersion: 'perps-aa-arbitrum-sepolia-v2',
      orderRouter: '0x1111111111111111111111111111111111111111',
      orderLifecycleBook: '0x2222222222222222222222222222222222222222',
      request: {
        clientOrderId: V2_CLIENT_ORDER_ID,
        side: 0,
        sizeDelta: 100n * 10n ** 18n,
        marginDelta: 30_000_000n,
        targetPrice: 100_100_000n,
        isClose: false,
        bounds: {
          validUntil: BigInt(Math.floor(Date.now() / 1_000) + 60),
          allowedExecutionModes: 1,
          expectedConfigHash: `0x${'34'.repeat(32)}`,
          maxExecutionBountyUsdc: 10_000n,
          maxExecutionNotionalUsdc: (1n << 256n) - 1n,
          maxGrossAccountDebitUsdc: (1n << 256n) - 1n,
          maxActionChargeUsdc: (1n << 256n) - 1n,
          maxExplicitFeesUsdc: (1n << 256n) - 1n,
          maxPostPositionSize: (1n << 256n) - 1n,
          minPostSettlementBalanceUsdc: 0n,
          minPostPositionEquityUsdc: 0n,
          maxPostLeverageBps: 0xffff_ffff,
        },
      },
      executionBountyUsdc: 10_000n,
      reviewedBlockNumber: 123n,
      reviewedBlockHash: `0x${'56'.repeat(32)}`,
      reviewedPrice: 100_000_000n,
      protection: {
        validUntil: BigInt(Math.floor(Date.now() / 1_000) + 60),
        executionMode: 1,
        executionBountyUsdc: 10_000n,
      },
      reviewSummary,
    } satisfies PreparedPerpsOrderV2
    perpsTradingMocks.prepareOrder.mockRejectedValueOnce(
      new PerpsOrderFundingShortfallError(
        { preparedOrder: reviewedPreparedOrder, reviewSummary },
        9_990_000n
      )
    )

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialReviewOpen
        initialOrderQuantity="100"
        oraclePriceRaw={100_000_000n}
        oraclePublishTime={Math.floor(Date.now() / 1_000)}
        availableToTradeRaw={20_020_000n}
      />
    )

    const dialog = screen.getByRole('dialog')
    await waitFor(() => {
      expect(within(dialog).getByText(/Deposit 9.99 USDC more or reduce the order/)).toBeInTheDocument()
    })
    expect(within(dialog).getByText('Required margin').closest('div')).toHaveTextContent('30.0USDC')
    expect(within(dialog).getByText('Total funding required').closest('div')).toHaveTextContent('30.0USDC')
    expect(within(dialog).getByText('Available account funding').closest('div')).toHaveTextContent('20.0USDC')
    expect(within(dialog).getByText('Resulting leverage').closest('div')).toHaveTextContent('5x')
    expect(within(dialog).getByRole('button', { name: 'Confirm Commit' })).toBeDisabled()
  })

  it('shows resulting position leverage in the margin action modal', () => {
    mockIsConnected = true

    render(
      <PerpsTradeTicket
        enableLiveTrading
        walletUsdcRaw={1000000000n}
        portfolioValueRaw={1000000000n}
        currentPosition={{
          exists: true,
          side: 0,
          direction: 'long',
          size: 0n,
          entryPrice: 98300000n,
          marginUsdc: 400000000n,
          unrealizedPnlUsdc: 0n,
          maintenanceMarginUsdc: 0n,
          liquidatable: false,
          estimatedNotionalUsdc: 2000000000n,
          entryNotionalUsdc: 2000000000n,
          dxyExposureUsdc: 2069380000n,
        }}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: 'Deposit' }))

    expect(screen.getByText('Deposit Margin')).toBeInTheDocument()
    expect(screen.getByText('Position margin')).toBeInTheDocument()
    expect(screen.getByText(/Deposit and withdraw change free margin only/i)).toBeInTheDocument()
    expect(screen.queryByText('Current leverage')).not.toBeInTheDocument()

    fireEvent.change(screen.getByDisplayValue(''), { target: { value: '500' } })

    expect(screen.queryByText('Resulting leverage')).not.toBeInTheDocument()
    expect(screen.queryByText('2.22x')).not.toBeInTheDocument()
  })

  it('combines owner and Trading Account USDC in the supported deposit flow', () => {
    mockIsConnected = true
    identityMocks.isAaManifestConfigured = true

    render(
      <PerpsTradeTicket
        enableLiveTrading
        availableToTradeRaw={0n}
        availableToTradeAmount="0"
        ownerWalletUsdcRaw={100000000000n}
        tradingAccountUsdcRaw={100000000000n}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: 'Deposit' }))

    const dialog = screen.getByRole('dialog')
    expect(within(dialog).getByText('Available to deposit')).toBeInTheDocument()
    expect(within(dialog).getByText('Available to trade')).toBeInTheDocument()
    expect(within(dialog).getByText('Owner Wallet USDC')).toBeInTheDocument()
    expect(within(dialog).queryByText('Trading Account balance')).not.toBeInTheDocument()
    expect(within(dialog).getByText('Trading Account USDC')).toBeInTheDocument()
    expect(within(dialog).getByRole('button', { name: /Max:/ })).toHaveTextContent('200 000')
  })

  it('transfers only the Trading Account shortfall before depositing margin', async () => {
    mockIsConnected = true
    identityMocks.isAaManifestConfigured = true
    perpsTradingMocks.fundTradingAccount.mockResolvedValue('0xtransfer')
    perpsTradingMocks.depositMargin.mockResolvedValue('0xdeposit')

    render(
      <PerpsTradeTicket
        enableLiveTrading
        availableToTradeRaw={0n}
        availableToTradeAmount="0"
        ownerWalletUsdcRaw={100000000000n}
        tradingAccountUsdcRaw={25000000000n}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: 'Deposit' }))
    const dialog = screen.getByRole('dialog')
    fireEvent.change(within(dialog).getByRole('textbox'), {
      target: { value: '50 000' },
    })

    const fundingNotice = within(dialog).getByText(/requires ETH for network gas/i)
    expect(fundingNotice.parentElement).toHaveTextContent('Transfer')
    expect(fundingNotice.parentElement).toHaveTextContent('25 000')
    expect(fundingNotice.parentElement).toHaveTextContent('from Owner Wallet')
    fireEvent.click(within(dialog).getByRole('button', { name: 'Transfer & Deposit' }))

    await waitFor(() => {
      expect(perpsTradingMocks.fundTradingAccount).toHaveBeenCalledWith(25000000000n)
      expect(perpsTradingMocks.depositMargin).toHaveBeenCalledWith(
        50000000000n,
        undefined,
        'account'
      )
    })
  })

  it('does not transfer twice when the sponsored deposit needs a retry', async () => {
    mockIsConnected = true
    identityMocks.isAaManifestConfigured = true
    perpsTradingMocks.fundTradingAccount.mockResolvedValue('0xtransfer')
    perpsTradingMocks.depositMargin
      .mockRejectedValueOnce(new Error('Sponsorship is temporarily unavailable.'))
      .mockResolvedValueOnce('0xdeposit')

    render(
      <PerpsTradeTicket
        enableLiveTrading
        availableToTradeRaw={0n}
        availableToTradeAmount="0"
        ownerWalletUsdcRaw={50000000000n}
        tradingAccountUsdcRaw={0n}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: 'Deposit' }))
    const dialog = screen.getByRole('dialog')
    fireEvent.change(within(dialog).getByRole('textbox'), {
      target: { value: '50 000' },
    })
    fireEvent.click(within(dialog).getByRole('button', { name: 'Transfer & Deposit' }))

    expect(await within(dialog).findByText(/The transfer succeeded, but the Margin Account deposit failed/))
      .toBeInTheDocument()
    fireEvent.click(within(dialog).getByRole('button', { name: 'Deposit' }))

    await waitFor(() => {
      expect(perpsTradingMocks.depositMargin).toHaveBeenCalledTimes(2)
    })
    expect(perpsTradingMocks.fundTradingAccount).toHaveBeenCalledTimes(1)
  })

  it('uses the engine new-position minimum when opening from zero', () => {
    mockIsConnected = true

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialOrderQuantity="100"
        oraclePriceRaw={98434897n}
        oraclePublishTime={1781267148}
        minOpenNotionalUsdc={100000000n}
        minNewPositionNotionalUsdc={1000000000n}
      />
    )

    expect(screen.getByText('Minimum new position is 1 117.22 USDC.')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeDisabled()
  })

  it('explains skew-limited capacity and directs a new trader to the opposing side', () => {
    mockIsConnected = true

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialOrderQuantity="1 000"
        oraclePriceRaw={100_000_000n}
        oraclePublishTime={1_781_267_148}
        longOpenCapacityUsdc={0n}
        shortOpenCapacityUsdc={2_000_000_000n}
        minOpenNotionalUsdc={100_000_000n}
        minNewPositionNotionalUsdc={1_000_000_000n}
      />
    )

    expect(screen.getByText(
      'Long plDXY Perp positions are temporarily unavailable because there is not enough remaining Long capacity to fit the minimum position size of 1 000 USDC. Opening more Long exposure would worsen the market imbalance. You can open a Short plDXY Perp position instead, which helps rebalance the market.'
    )).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeDisabled()
  })

  it('tells a trader with an existing position to close it before switching sides', () => {
    mockIsConnected = true

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialOrderQuantity="100"
        oraclePriceRaw={100_000_000n}
        oraclePublishTime={1_781_267_148}
        longOpenCapacityUsdc={0n}
        shortOpenCapacityUsdc={2_000_000_000n}
        minOpenNotionalUsdc={100_000_000n}
        minNewPositionNotionalUsdc={1_000_000_000n}
        currentPosition={{
          exists: true,
          side: 0,
          direction: 'long',
          size: 1_000_000_000_000_000_000_000n,
          entryPrice: 100_000_000n,
          marginUsdc: 100_000_000n,
          unrealizedPnlUsdc: 0n,
          maintenanceMarginUsdc: 10_000_000n,
          liquidatable: false,
          estimatedNotionalUsdc: 1_000_000_000n,
          dxyExposureUsdc: 1_000_000_000n,
        }}
      />
    )

    expect(screen.getByText(
      'Long plDXY Perp positions are temporarily unavailable because there is not enough remaining Long capacity to fit the minimum increase size of 100 USDC. Opening more Long exposure would worsen the market imbalance. You can reduce or close your current Long position. After closing it, you can open a Short plDXY Perp position, which helps rebalance the market.'
    )).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeDisabled()
  })

  it('explains when a pending full close already reserves the position', () => {
    mockIsConnected = true
    const fullPositionSize = 1526359014354277024332n

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="short"
        initialOrderQuantity="1 553.25"
        oraclePriceRaw={98240000n}
        oraclePublishTime={1781118120}
        currentPosition={{
          exists: true,
          side: 0,
          direction: 'long',
          size: fullPositionSize,
          entryPrice: 98309486n,
          marginUsdc: 299399778n,
          unrealizedPnlUsdc: 1083653n,
          maintenanceMarginUsdc: 14994720n,
          liquidatable: false,
          estimatedNotionalUsdc: 1499470000n,
          entryNotionalUsdc: 1500556701n,
          dxyExposureUsdc: 1553250000n,
        }}
        pendingOrders={[
          {
            orderId: 33n,
            side: 0,
            direction: 'long',
            sizeDelta: fullPositionSize,
            marginDeltaUsdc: 0n,
            acceptablePrice: 98336728n,
            isReduceOnly: true,
            status: 1,
            expiryTime: BigInt(Math.floor(Date.now() / 1000) + 120),
          },
        ]}
        pendingOrderCount={1}
      />
    )

    expect(screen.getByText(/Order #33 is already closing the full current position/)).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Reduce' })).toBeDisabled()
    expect(screen.getByRole('button', { name: /Max:/ })).toBeDisabled()
    expect(screen.getByRole('button', { name: /Max:/ })).toHaveTextContent('Max: 0')

    fireEvent.change(screen.getByRole('textbox'), { target: { value: '0' } })
    fireEvent.click(screen.getByRole('button', { name: /Current Position/ }))
    expect(screen.getByRole('textbox')).toHaveValue('0')
  })

  it('does not blame pending orders when a manual reduce exceeds the latest exposure', () => {
    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialReduceOnly
        initialOrderQuantity="2 100"
        oraclePriceRaw={98_300_000n}
        oraclePublishTime={1_784_705_538}
        currentPosition={{
          exists: true,
          side: 0,
          direction: 'long',
          size: 2_000n * 10n ** 18n,
          entryPrice: 98_300_000n,
          marginUsdc: 400_000_000n,
          unrealizedPnlUsdc: 48_250_000n,
          maintenanceMarginUsdc: 20_000_000n,
          liquidatable: false,
          estimatedNotionalUsdc: 1_966_000_000n,
          entryNotionalUsdc: 1_966_000_000n,
          dxyExposureUsdc: 2_034_000_000n,
        }}
      />
    )

    expect(screen.getByText('Only 2 000 plDXY is available to reduce.')).toBeInTheDocument()
    expect(screen.queryByText(/already reserved by pending close orders/)).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Reduce' })).toBeDisabled()
  })

  it('keeps a near-max reduction partial until Max selects the exact full close', () => {
    const positionSize = 5_000n * 10n ** 18n
    const positionExposureUsdc = 5_071_000_000n

    render(
      <PerpsTradeTicket
        initialDirection="long"
        initialReduceOnly
        initialOrderQuantity="4900"
        oraclePriceRaw={98_580_000n}
        currentPosition={{
          exists: true,
          side: 0,
          direction: 'long',
          size: positionSize,
          entryPrice: 98_580_000n,
          marginUsdc: 1_000_000_000n,
          unrealizedPnlUsdc: 0n,
          maintenanceMarginUsdc: 50_000_000n,
          liquidatable: false,
          estimatedNotionalUsdc: 4_929_000_000n,
          entryNotionalUsdc: 4_929_000_000n,
          dxyExposureUsdc: positionExposureUsdc,
        }}
      />
    )

    expect(screen.getByRole('button', { name: 'Review Reduce' })).toBeEnabled()

    fireEvent.click(screen.getByRole('button', { name: /Max:/ }))

    expect(screen.getByRole('textbox', { name: 'Order quantity' }))
      .toHaveValue('5 000')
    expect(screen.getByRole('button', { name: 'Review Close' })).toBeEnabled()
  })

  it('blocks a floored reduction below the partial minimum but exempts an exact full close', async () => {
    mockIsConnected = true
    wagmiMocks.readContractsData = [{
      status: 'success',
      result: {
        valid: true,
        invalidReason: 0,
        executionPrice: 98_580_000n,
        sizeDelta: 900n * 10n ** 18n,
        remainingSize: 4_100n * 10n ** 18n,
        remainingMargin: 800_000_000n,
      },
    }]

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialReduceOnly
        initialOrderQuantity="1000"
        oraclePriceRaw={98_580_000n}
        oraclePublishTime={1_700_000_000}
        minNewPositionNotionalUsdc={1_000_000_000n}
        currentPosition={{
          exists: true,
          side: 0,
          direction: 'long',
          size: 5_000n * 10n ** 18n,
          entryPrice: 98_580_000n,
          marginUsdc: 1_000_000_000n,
          unrealizedPnlUsdc: 0n,
          maintenanceMarginUsdc: 50_000_000n,
          liquidatable: false,
          estimatedNotionalUsdc: 4_929_000_000n,
          entryNotionalUsdc: 4_929_000_000n,
          dxyExposureUsdc: 5_071_000_000n,
        }}
      />
    )

    expect(screen.getByText(
      'Minimum partial reduction is 1 115.62 USDC. Use Max to close the full position.'
    )).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Reduce' })).toBeDisabled()

    fireEvent.click(screen.getByRole('button', { name: /Max:/ }))

    expect(screen.queryByText(/Minimum partial reduction/)).not.toBeInTheDocument()
    await waitFor(() => {
      expect(screen.getByRole('button', { name: 'Review Close' })).toBeEnabled()
    })
  })

  it('does not treat a sub-minimum projected residual behind a pending order as a guaranteed full close', () => {
    mockIsConnected = true
    wagmiMocks.readContractsData = [{
      status: 'success',
      result: {
        valid: true,
        invalidReason: 0,
        executionPrice: 98_580_000n,
        sizeDelta: 800n * 10n ** 18n,
        remainingSize: 1_200n * 10n ** 18n,
        remainingMargin: 400_000_000n,
      },
    }]

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialReduceOnly
        initialOrderQuantity="0"
        oraclePriceRaw={98_580_000n}
        oraclePublishTime={1_700_000_000}
        minNewPositionNotionalUsdc={1_000_000_000n}
        currentPosition={{
          exists: true,
          side: 0,
          direction: 'long',
          size: 2_000n * 10n ** 18n,
          entryPrice: 98_580_000n,
          marginUsdc: 400_000_000n,
          unrealizedPnlUsdc: 0n,
          maintenanceMarginUsdc: 20_000_000n,
          liquidatable: false,
          estimatedNotionalUsdc: 1_971_600_000n,
          entryNotionalUsdc: 1_971_600_000n,
          dxyExposureUsdc: 2_028_400_000n,
        }}
        pendingOrders={[{
          orderId: 77n,
          side: 0,
          direction: 'long',
          sizeDelta: 1_200n * 10n ** 18n,
          marginDeltaUsdc: 0n,
          acceptablePrice: 98_580_000n,
          isReduceOnly: true,
          status: 1,
        }]}
        pendingOrderCount={1}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: /Max:/ }))

    expect(screen.getByRole('textbox', { name: 'Order quantity' }))
      .toHaveValue('800')
    expect(screen.getByText(
      'Minimum partial reduction is 1 115.62 USDC. Finalize or clean up earlier pending orders before closing a smaller projected remainder.'
    )).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Reduce' })).toBeDisabled()
    expect(screen.queryByRole('button', { name: 'Review Close' })).not.toBeInTheDocument()
  })

  it('allows a profitable full close with no free buying power when the close preview is valid', () => {
    mockIsConnected = true
    wagmiMocks.readContractsData = [{
      status: 'success',
      result: {
        valid: true,
        invalidReason: 0,
        executionPrice: 97_190_495n,
        sizeDelta: 3_389_329_558_583_534_648_693_500n,
        realizedPnlUsdc: 5_751_556_687n,
        executionFeeUsdc: 200_000n,
        remainingSize: 0n,
        remainingMargin: 0n,
      },
    }]
    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="short"
        initialOrderQuantity="3 389 329.5585835346486935"
        oraclePriceRaw={97_190_495n}
        oraclePublishTime={1_784_656_207}
        availableToTradeRaw={0n}
        availableToTradeAmount="0"
        currentPosition={{
          exists: true,
          side: 0,
          direction: 'long',
          size: 3_389_329_558_583_534_648_693_500n,
          entryPrice: 97_360_191n,
          marginUsdc: 99_884_165_044n,
          unrealizedPnlUsdc: 5_751_556_687n,
          maintenanceMarginUsdc: 9_882_318_525n,
          liquidatable: false,
          estimatedNotionalUsdc: 3_294_106_175_168n,
          entryNotionalUsdc: 3_299_857_731_856n,
          dxyExposureUsdc: 3_484_552_941_998n,
        }}
      />
    )

    expect(screen.queryByText('Deposit 0.2 USDC more before committing this order.')).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Close' })).toBeEnabled()
  })

  it('requires confirmation before enabling the margin call simulator', () => {
    render(
      <PerpsTradeTicket
        initialOrderQuantity="1 000"
        maintenanceMarginBps={10n}
        initialMarginBps={20n}
        executionFeeBps={4n}
      />
    )

    const simulatorCheckbox = screen.getByLabelText('Margin Call Simulator')

    expect(simulatorCheckbox).not.toBeChecked()
    expect(screen.getByText('33x')).toBeInTheDocument()

    fireEvent.click(simulatorCheckbox)

    expect(simulatorCheckbox).not.toBeChecked()
    expect(screen.getByText('Enable Margin Call Simulator?')).toBeInTheDocument()
    expect(screen.getByText('Maintenance boundary (not an entry cap)')).toBeInTheDocument()
    expect(screen.getByText('Initial-margin boundary')).toBeInTheDocument()
    expect(screen.getByText('Estimated simulator entry cap')).toBeInTheDocument()
    expect(screen.getByText(/When the market closes, this setting may expire or become stricter/i)).toBeInTheDocument()
    expect(screen.getByText('1000x')).toBeInTheDocument()
    expect(screen.getByText('500x')).toBeInTheDocument()
    expect(screen.getAllByText('416x').length).toBeGreaterThan(0)
    expect(screen.getByText('floor(10 000 / max(10, 20 + 4))')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Enable Simulator' }))

    expect(simulatorCheckbox).toBeChecked()
    expect(screen.queryByText('Enable Margin Call Simulator?')).not.toBeInTheDocument()
    expect(screen.getByText('416x')).toBeInTheDocument()
    expect(screen.getByLabelText('Leverage slider')).toHaveAttribute('max', '416')
  })

  it('opens the edit position margin modal from the leverage pencil', () => {
    render(
      <PerpsAccountPanel
        isConnected
        freeBuyingPowerUsdc={250000000n}
        position={{
          exists: true,
          side: 0,
          direction: 'long',
          size: 0n,
          entryPrice: 101240000n,
          marginUsdc: 400000000n,
          unrealizedPnlUsdc: 0n,
          maintenanceMarginUsdc: 0n,
          liquidatable: false,
          estimatedNotionalUsdc: 2000000000n,
          entryNotionalUsdc: 2000000000n,
          dxyExposureUsdc: 2096930000n,
          pendingCarryUsdc: 0n,
        }}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: 'Edit position margin' }))

    expect(screen.getByText('Edit Position Margin')).toBeInTheDocument()
    expect(screen.getByText('This locks free USDC into the current position margin bucket. It does not change position size.')).toBeInTheDocument()
    expect(screen.getByText(/Direct margin removal is not supported/i)).toBeInTheDocument()
    expect(screen.queryByText(/by the current contracts/i)).not.toBeInTheDocument()
    expect(screen.getByText('Current position margin')).toBeInTheDocument()
    expect(screen.getByText('Resulting leverage')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: /Max:/ }))

    expect(screen.getByRole('textbox')).toHaveValue('250')
    expect(screen.getByText('3.08x')).toBeInTheDocument()
  })

  it('opens a reduce-only full-close review from the position panel', async () => {
    const position = {
      exists: true,
      side: 0,
      direction: 'long' as const,
      size: 2_000n * 10n ** 18n,
      entryPrice: 98_300_000n,
      marginUsdc: 400_000_000n,
      unrealizedPnlUsdc: 48_250_000n,
      maintenanceMarginUsdc: 20_000_000n,
      liquidatable: false,
      estimatedNotionalUsdc: 2_000_000_000n,
      entryNotionalUsdc: 2_000_000_000n,
      dxyExposureUsdc: 2_034_000_000n,
      displayDxyPrice: 101_700_000n,
      pendingCarryUsdc: 1_250_000n,
    }

    function ClosePositionFlow() {
      const [requestId, setRequestId] = useState(0)

      return (
        <>
          <PerpsAccountPanel
            isConnected
            position={position}
            onClosePosition={() => {
              setRequestId((currentRequestId) => currentRequestId + 1)
            }}
          />
          <PerpsTradeTicket
            closePositionRequestId={requestId}
            currentPosition={position}
            oraclePriceRaw={98_300_000n}
          />
        </>
      )
    }

    render(<ClosePositionFlow />)

    fireEvent.click(screen.getByRole('button', { name: 'Close position' }))

    await waitFor(() => {
      expect(screen.getByRole('dialog')).toBeInTheDocument()
    })
    expect(screen.getByRole('checkbox', { name: 'Reduce only' })).toBeChecked()
    expect(screen.getByRole('dialog')).toHaveTextContent('You are closing your Long plDXY Perp position.')
  })

  it('keeps a position-panel full close valid when the oracle price refreshes', async () => {
    mockIsConnected = true
    const positionSize = 2_000n * 10n ** 18n
    const positionSizeToUsdcScale = 10n ** 20n
    const priceCap = 200_000_000n

    wagmiMocks.readContractsData = [{
      status: 'success',
      result: {
        valid: true,
        invalidReason: 0,
        executionPrice: 98_300_000n,
        sizeDelta: positionSize,
        realizedPnlUsdc: 48_250_000n,
        executionFeeUsdc: 200_000n,
        remainingSize: 0n,
        remainingMargin: 0n,
      },
    }]
    perpsTradingMocks.commitOrder.mockResolvedValue({
      hash: '0xc105e00000000000000000000000000000000000000000000000000000000000',
      orderId: 42n,
    })

    function FullCloseWithOracleRefresh() {
      const [requestId, setRequestId] = useState(0)
      const [oraclePrice, setOraclePrice] = useState(98_300_000n)
      const dxyExposureUsdc = (positionSize * (priceCap - oraclePrice)) / positionSizeToUsdcScale
      const position = {
        exists: true,
        side: 0,
        direction: 'long' as const,
        size: positionSize,
        entryPrice: 98_300_000n,
        marginUsdc: 400_000_000n,
        unrealizedPnlUsdc: 48_250_000n,
        maintenanceMarginUsdc: 20_000_000n,
        liquidatable: false,
        estimatedNotionalUsdc: (positionSize * oraclePrice) / positionSizeToUsdcScale,
        entryNotionalUsdc: 1_966_000_000n,
        dxyExposureUsdc,
        displayDxyPrice: priceCap - oraclePrice,
        pendingCarryUsdc: 1_250_000n,
      }

      return (
        <>
          <button type="button" onClick={() => setOraclePrice(98_400_000n)}>
            Refresh oracle price
          </button>
          <PerpsAccountPanel
            isConnected
            position={position}
            onClosePosition={() => setRequestId((currentRequestId) => currentRequestId + 1)}
          />
          <PerpsTradeTicket
            enableLiveTrading
            closePositionRequestId={requestId}
            currentPosition={position}
            oraclePriceRaw={oraclePrice}
            oraclePublishTime={1_784_705_538}
            availableToTradeRaw={0n}
            availableToTradeAmount="0"
          />
        </>
      )
    }

    render(<FullCloseWithOracleRefresh />)

    fireEvent.click(screen.getByRole('button', { name: 'Close position' }))

    await waitFor(() => {
      expect(screen.getByRole('dialog')).toBeInTheDocument()
    })
    expect(screen.getByRole('textbox')).toHaveValue('2 000')
    expect(screen.getByRole('button', { name: 'Review Close' })).toBeEnabled()

    fireEvent.click(screen.getByRole('button', { name: 'Refresh oracle price' }))

    await waitFor(() => {
      expect(screen.getByRole('textbox')).toHaveValue('2 000')
    })
    expect(screen.queryByText(/already reserved by pending close orders/)).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Close' })).toBeEnabled()
    await waitFor(() => {
      expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeEnabled()
    })

    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))

    await waitFor(() => {
      expect(perpsTradingMocks.commitOrder).toHaveBeenCalledOnce()
    })
    expect(perpsTradingMocks.commitOrder.mock.calls[0]?.[0]).toMatchObject({
      direction: 'long',
      sizeDelta: positionSize,
      isClose: true,
    })
  })

  it('uses the short accent color for a short current-position badge', () => {
    render(
      <PerpsAccountPanel
        isConnected
        position={{
          exists: true,
          side: 1,
          direction: 'short',
          size: 0n,
          entryPrice: 101240000n,
          marginUsdc: 400000000n,
          unrealizedPnlUsdc: 0n,
          maintenanceMarginUsdc: 0n,
          liquidatable: false,
          estimatedNotionalUsdc: 2000000000n,
          entryNotionalUsdc: 2000000000n,
          dxyExposureUsdc: 2096930000n,
          pendingCarryUsdc: 0n,
        }}
      />
    )

    expect(screen.getByText('Short')).toHaveClass('text-brand-orange')
    expect(screen.getByText('Short')).not.toHaveClass('text-positive')
  })

  it('does not show position margin edit when there is no connected live position', () => {
    render(<PerpsAccountPanel isConnected={false} />)

    expect(screen.queryByRole('button', { name: 'Edit position margin' })).not.toBeInTheDocument()
  })

  it('labels initial position loading without reporting that position data is absent', () => {
    render(<PerpsAccountPanel isConnected isLoading />)

    expect(screen.getByText('Loading position data...')).toBeInTheDocument()
    expect(screen.queryByText('No position data')).not.toBeInTheDocument()
  })

  it('reports when transaction history becomes active and inactive', () => {
    const onActiveTabChange = vi.fn()
    render(<PerpsAccountPanel onActiveTabChange={onActiveTabChange} />)

    expect(onActiveTabChange).toHaveBeenLastCalledWith('position')
    expect(onActiveTabChange).toHaveBeenCalledTimes(1)

    fireEvent.click(screen.getByRole('button', { name: 'Transaction History' }))
    expect(onActiveTabChange).toHaveBeenLastCalledWith('tradeHistory')
    expect(onActiveTabChange).toHaveBeenCalledTimes(2)

    fireEvent.click(screen.getByRole('button', { name: 'Order History' }))
    expect(onActiveTabChange).toHaveBeenLastCalledWith('orderHistory')
    expect(onActiveTabChange).toHaveBeenCalledTimes(3)
  })

  it('hides manual finalization during the automatic finalization grace period', async () => {
    vi.useFakeTimers()
    const onAccountRefresh = vi.fn()

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialLifecycleState="revealPending"
        initialReviewOpen
        onAccountRefresh={onAccountRefresh}
      />
    )

    expect(screen.getByText('Waiting for verified market data')).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Finalize Trade' })).not.toBeInTheDocument()
    expect(screen.getByRole('progressbar', { name: 'Price finalization progress' })).toBeInTheDocument()
    expect(screen.queryByText(/This panel will show the confirmation automatically/i)).not.toBeInTheDocument()

    await act(async () => {
      vi.advanceTimersByTime(19_000)
    })

    expect(screen.queryByRole('button', { name: 'Finalize Trade' })).not.toBeInTheDocument()
    expect(screen.getByRole('progressbar', { name: 'Price finalization progress' })).toHaveAttribute('aria-valuenow', '95')
    expect(onAccountRefresh).not.toHaveBeenCalled()

    await act(async () => {
      vi.advanceTimersByTime(1_000)
    })

    expect(screen.getByRole('button', { name: 'Finalize Trade' })).toBeInTheDocument()
  })

  it('keeps manual finalization disabled for sponsored Trading Accounts', async () => {
    vi.useFakeTimers()
    identityMocks.isAaManifestConfigured = true

    render(
      <PerpsTradeTicket
        initialLifecycleState="revealPending"
        initialReviewOpen
        showFinalizationProgress
      />
    )

    expect(
      screen.getByText('Waiting for verified market data')
    ).toBeInTheDocument()

    await act(async () => {
      vi.advanceTimersByTime(21_000)
    })

    expect(
      screen.queryByRole('button', { name: 'Finalize Trade' })
    ).not.toBeInTheDocument()
    expect(
      screen.getByRole('progressbar', {
        name: 'Price finalization progress',
      })
    ).toHaveAttribute('aria-valuenow', '100')
  })

  it('shows the 20-second finalization progress circle in story mode without backend waiting', async () => {
    vi.useFakeTimers()
    const randomSpy = vi.spyOn(Math, 'random').mockReturnValue(0.7)

    try {
      render(
        <PerpsTradeTicket
          initialLifecycleState="revealPending"
          initialReviewOpen
          initialOrderId={62n}
          showFinalizationProgress
        />
      )

      expect(screen.getByText('Waiting for verified market data')).toBeInTheDocument()
      expect(screen.queryByText('Finalizing execution price')).not.toBeInTheDocument()
      expect(screen.getByText('Using signed oracle data for the order window before settling the trade.')).toBeInTheDocument()
      expect(screen.getByRole('progressbar', { name: 'Price finalization progress' })).toHaveAttribute('aria-valuenow', '0')
      expect(screen.getByText('Available in 20s')).toBeInTheDocument()
      expect(screen.queryByRole('button', { name: 'Show Manual Option' })).not.toBeInTheDocument()
      expect(perpsTradingMocks.waitForPerpsOrderTerminal).not.toHaveBeenCalled()

      await act(async () => {
        vi.advanceTimersByTime(4_000)
      })
      await act(async () => {
        vi.advanceTimersByTime(1_380)
      })

      expect(screen.getByText('Verifying solvency after execution')).toBeInTheDocument()
      expect(screen.getByText('Checking the account remains properly collateralized after settlement.')).toBeInTheDocument()

      await act(async () => {
        vi.advanceTimersByTime(4_620)
      })

      expect(screen.getByRole('progressbar', { name: 'Price finalization progress' })).toHaveAttribute('aria-valuenow', '50')
    } finally {
      randomSpy.mockRestore()
    }
  })

  it('shows the execution confirmation if automatic finalization settles before the manual option is available', async () => {
    mockIsConnected = true
    perpsTradingMocks.waitForPerpsOrderTerminal.mockResolvedValue({
      timedOut: false,
      order: {
        orderId: 58n,
        time: '22 Jun, 12:02',
        market: 'plDXY Perp',
        side: 'Long',
        type: 'Open',
        price: '0.9733',
        size: '1 000',
        status: 'Executed',
        account: V2_ACCOUNT,
        clientOrderId: V2_CLIENT_ORDER_ID,
        receiptHash: V2_RECEIPT_HASH,
        commitTxHash: '0x46cb000000000000000000000000000000001cbb',
        revealTxHash: '0x6c0d00000000000000000000000000000000b7d3',
        executionPriceRaw: 97_330_315n,
        executionEconomicsVersion: 2,
      },
    })

    const baseProps = {
      enableLiveTrading: true,
      initialLifecycleState: 'revealPending' as const,
      initialReviewOpen: true,
      initialDirection: 'long' as const,
      initialOrderQuantity: '1 000',
      initialOrderId: 58n,
      oraclePriceRaw: 97_330_315n,
      oraclePublishTime: Math.floor(Date.now() / 1000),
      availableToTradeRaw: 2_000_000_000n,
      walletUsdcRaw: 2_000_000_000n,
      portfolioValueRaw: 2_000_000_000n,
      withdrawableUsdcRaw: 2_000_000_000n,
      minOpenNotionalUsdc: 100_000_000n,
      minNewPositionNotionalUsdc: 100_000_000n,
    }

    render(<PerpsTradeTicket {...baseProps} />)

    expect(screen.getByText('Waiting for verified market data')).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Finalize Trade' })).not.toBeInTheDocument()

    await waitFor(() => {
      expect(screen.getByText('Final Result')).toBeInTheDocument()
    })
    const finalResult = screen.getByText('Final Result').closest('div')?.parentElement
    expect(finalResult).toBeInTheDocument()
    expect(within(finalResult!).getByText('0x6c0d...b7d3')).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Finalize Trade' })).not.toBeInTheDocument()
  })

  it('updates the account position before publishing the execution result', async () => {
    mockIsConnected = true
    let resolveAccountRefresh = () => {}
    const accountRefreshGate = new Promise<void>((resolve) => {
      resolveAccountRefresh = resolve
    })
    const onAccountRefresh = vi.fn(async () => {
      await accountRefreshGate
    })
    perpsTradingMocks.waitForPerpsOrderTerminal.mockResolvedValue({
      timedOut: false,
      order: {
        orderId: 59n,
        time: '22 Jun, 12:02',
        market: 'plDXY Perp',
        side: 'Long',
        type: 'Open',
        price: '0.9733',
        size: '1 000',
        status: 'Executed',
        account: V2_ACCOUNT,
        clientOrderId: V2_CLIENT_ORDER_ID,
        receiptHash: V2_RECEIPT_HASH,
        commitTxHash: '0x46cb000000000000000000000000000000001cbb',
        revealTxHash: '0x6c0d00000000000000000000000000000000b7d3',
        executionPriceRaw: 97_330_315n,
        executionOracleFrozen: false,
        oracleDerivationVersion: 1,
        executionEconomicsVersion: 2,
      },
    })

    function PositionRefreshHarness() {
      const [position, setPosition] = useState('No position')

      return (
        <>
          <output aria-label="Current position">{position}</output>
          <PerpsTradeTicket
            enableLiveTrading
            initialLifecycleState="revealPending"
            initialReviewOpen
            initialDirection="long"
            initialOrderQuantity="1 000"
            initialOrderId={59n}
            oraclePriceRaw={97_330_315n}
            oraclePublishTime={Math.floor(Date.now() / 1000)}
            availableToTradeRaw={2_000_000_000n}
            walletUsdcRaw={2_000_000_000n}
            portfolioValueRaw={2_000_000_000n}
            withdrawableUsdcRaw={2_000_000_000n}
            minOpenNotionalUsdc={100_000_000n}
            minNewPositionNotionalUsdc={100_000_000n}
            onAccountRefresh={async () => {
              await onAccountRefresh()
              setPosition('1 000 USDC')
            }}
          />
        </>
      )
    }

    render(<PositionRefreshHarness />)

    await waitFor(() => {
      expect(onAccountRefresh).toHaveBeenCalledOnce()
    })
    expect(screen.getByLabelText('Current position')).toHaveTextContent('No position')
    expect(screen.queryByText('Final Result')).not.toBeInTheDocument()

    await act(async () => {
      resolveAccountRefresh()
    })

    await waitFor(() => {
      expect(screen.getByLabelText('Current position')).toHaveTextContent('1 000 USDC')
      expect(screen.getByText('Final Result')).toBeInTheDocument()
    })
  })

  it('keeps settlement hashes compact and links both hashes to Blockscout', () => {
    mockIsConnected = true
    identityMocks.isAaManifestConfigured = true
    perpsTradingMocks.waitForPerpsOrderTerminal.mockReturnValue(new Promise(() => {}))

    const userOperationHash =
      '0x13a03bd38e5603cf4be51d9adf9c5fc25b4ba529c60da857615b36b8393cc92b'
    const commitTxHash =
      '0xd7f7a49e3fc3e9286b84b8fbcb02763f00b7ab7867338ceec6f5f4bce44e1507'
    const now = Date.now()

    useSponsoredOperationStore.setState({
      operations: [{
        id: 'settlement-layout',
        ownerAddress: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
        accountAddress: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
        chainId: 421614,
        accountMode: 'simple',
        manifestVersion: 'perps-aa-arbitrum-sepolia-v2',
        action: 'place-order',
        lane: 'default',
        status: 'confirmed',
        sponsorshipAccepted: true,
        userOperationHash,
        transactionHash: commitTxHash,
        retryCount: 0,
        createdAt: now,
        updatedAt: now,
        statusTimestamps: { confirmed: now },
      }],
      activeLanes: {},
    })

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialLifecycleState="revealPending"
        initialReviewOpen
        initialOrderId={52n}
        initialCommitTxHash={commitTxHash}
      />
    )

    const dialog = screen.getByRole('dialog')
    const abbreviatedHash = `${userOperationHash.slice(0, 6)}...${userOperationHash.slice(-4)}`
    const hashText = within(dialog).getByText(abbreviatedHash)

    expect(hashText).toHaveClass('truncate')
    expect(hashText).toHaveAttribute('title', userOperationHash)
    expect(within(dialog).queryByText(userOperationHash)).not.toBeInTheDocument()
    expect(
      within(dialog).getByRole('link', {
        name: 'Open UserOperation in block explorer',
      })
    ).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/op/${userOperationHash}`
    )
    expect(
      within(dialog).getByRole('link', {
        name: 'Open tx in block explorer',
      })
    ).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/tx/${commitTxHash}`
    )
  })

  it('preserves lagging committed history and rewinds only after an indexed reorg proof', async () => {
    mockIsConnected = true
    perpsTradingMocks.waitForPerpsOrderTerminal.mockReturnValue(new Promise(() => {}))
    const onAccountRefresh = vi.fn()
    const terminalOrder = {
      orderId: 72n,
      time: '23 Jun, 11:14',
      market: 'plDXY Perp',
      side: 'Long',
      type: 'Open',
      price: '1.0286',
      size: '1 000',
      status: 'Executed' as const,
      account: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B' as const,
      clientOrderId: `0x${'12'.repeat(32)}` as `0x${string}`,
      commitTxHash: '0x971c00000000000000000000000000000000eeab',
      revealTxHash: '0xec0c00000000000000000000000000000000d745',
      receiptHash: `0x${'34'.repeat(32)}` as `0x${string}`,
      terminalBlockNumberRaw: 190_002_345n,
      executionPriceRaw: 97_138_163n,
      executionOraclePriceRaw: 97_330_315n,
      executionMode: 'Live',
      oracleDerivationVersion: 1,
      vpiUsdcRaw: 12_345_678n,
      executionEconomicsVersion: 2,
    }
    const baseProps = {
      enableLiveTrading: true,
      initialLifecycleState: 'revealPending' as const,
      initialReviewOpen: true,
      initialDirection: 'long' as const,
      initialOrderQuantity: '1 000',
      initialOrderId: 72n,
      oraclePriceRaw: 97_330_315n,
      oraclePublishTime: Math.floor(Date.now() / 1000),
      availableToTradeRaw: 2_000_000_000n,
      walletUsdcRaw: 2_000_000_000n,
      portfolioValueRaw: 2_000_000_000n,
      withdrawableUsdcRaw: 2_000_000_000n,
      minOpenNotionalUsdc: 100_000_000n,
      minNewPositionNotionalUsdc: 100_000_000n,
      onAccountRefresh,
    }

    const { rerender } = render(<PerpsTradeTicket {...baseProps} orderHistory={[]} />)

    expect(screen.getByText('Waiting for verified market data')).toBeInTheDocument()

    rerender(
      <PerpsTradeTicket
        {...baseProps}
        orderHistory={[terminalOrder]}
      />
    )

    await waitFor(() => {
      expect(screen.getByText('Final Result')).toBeInTheDocument()
    })
    const finalResult = screen.getByText('Final Result').closest('div')?.parentElement
    expect(finalResult).toBeInTheDocument()
    expect(within(finalResult!).getByText('0xec0c...d745')).toBeInTheDocument()
    expect(within(finalResult!).getByText('VPI')).toBeInTheDocument()
    expect(within(finalResult!).getByText('12.3')).toBeInTheDocument()
    const oracleSpreadRow = within(finalResult!).getByText('Oracle confidence spread').closest('div')
    expect(oracleSpreadRow?.querySelector('dd')).toHaveTextContent('~0.1974%')
    expect(onAccountRefresh).toHaveBeenCalledTimes(1)

    rerender(
      <PerpsTradeTicket
        {...baseProps}
        oraclePriceRaw={98_000_000n}
        orderHistory={[{ ...terminalOrder }]}
      />
    )

    await act(async () => {})
    expect(oracleSpreadRow?.querySelector('dd')).toHaveTextContent('~0.1974%')
    expect(onAccountRefresh).toHaveBeenCalledTimes(1)
    expect(perpsTradingMocks.waitForPerpsOrderTerminal).toHaveBeenCalledTimes(1)

    rerender(
      <PerpsTradeTicket
        {...baseProps}
        orderHistory={[{
          ...terminalOrder,
          status: 'Committed',
          revealTxHash: undefined,
          terminalBlockNumberRaw: undefined,
          terminalBlockHash: undefined,
          executionPriceRaw: undefined,
          executionOraclePriceRaw: undefined,
          executionOracleFrozen: undefined,
          oracleDerivationVersion: undefined,
          vpiUsdcRaw: undefined,
          executionEconomicsVersion: undefined,
        }]}
      />
    )

    await waitFor(() => {
      expect(screen.getByText('Final Result')).toBeInTheDocument()
      expect(within(finalResult!).getByText('12.3')).toBeInTheDocument()
      expect(oracleSpreadRow?.querySelector('dd')).toHaveTextContent('~0.1974%')
      expect(perpsTradingMocks.waitForPerpsOrderTerminal).toHaveBeenCalledTimes(2)
    })
    expect(onAccountRefresh).toHaveBeenCalledTimes(1)

    rerender(
      <PerpsTradeTicket
        {...baseProps}
        ordersIndexedThroughBlockRaw={190_002_345n}
        orderHistory={[]}
      />
    )

    await act(async () => {})
    expect(screen.getByText('Final Result')).toBeInTheDocument()
    expect(within(finalResult!).getByText('12.3')).toBeInTheDocument()
    const waitCallsBeforeReorgProof =
      perpsTradingMocks.waitForPerpsOrderTerminal.mock.calls.length
    perpsTradingMocks.waitForPerpsOrderTerminal.mockResolvedValue({
      timedOut: false,
      order: terminalOrder,
    })

    rerender(
      <PerpsTradeTicket
        {...baseProps}
        ordersIndexedThroughBlockRaw={190_002_345n}
        orderHistory={[{
          ...terminalOrder,
          status: 'Committed',
          revealTxHash: undefined,
          terminalBlockNumberRaw: undefined,
          terminalBlockHash: undefined,
          executionPriceRaw: undefined,
          executionOraclePriceRaw: undefined,
          executionOracleFrozen: undefined,
          oracleDerivationVersion: undefined,
          vpiUsdcRaw: undefined,
          executionEconomicsVersion: undefined,
        }]}
      />
    )

    await waitFor(() => {
      expect(screen.queryByText('Final Result')).not.toBeInTheDocument()
      expect(screen.getByText('Waiting for verified market data')).toBeInTheDocument()
      expect(
        perpsTradingMocks.waitForPerpsOrderTerminal.mock.calls.length
      ).toBeGreaterThan(waitCallsBeforeReorgProof)
    })

    await act(async () => {})
    expect(screen.queryByText('Final Result')).not.toBeInTheDocument()

    rerender(
      <PerpsTradeTicket
        {...baseProps}
        ordersIndexedThroughBlockRaw={190_002_346n}
        orderHistory={[{
          ...terminalOrder,
          terminalBlockHash:
            '0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
        }]}
      />
    )

    await waitFor(() => {
      expect(screen.getByText('Final Result')).toBeInTheDocument()
      expect(screen.getByText('12.3')).toBeInTheDocument()
    })
  })

  it('polls and hydrates exact execution evidence after the terminal response', async () => {
    mockIsConnected = true
    const terminalOrder = {
      orderId: 73n,
      time: '30 Jul, 20:57',
      market: 'plDXY Perp',
      side: 'Short',
      type: 'Close',
      price: '0.9839',
      size: '100 000',
      status: 'Executed' as const,
      account: V2_ACCOUNT,
      clientOrderId: V2_CLIENT_ORDER_ID,
      commitTxHash: '0x54237f181c19e86acfd661fd217e219fd6570227dc5f0b9815589a9d278f6104' as const,
      revealTxHash: '0xebbbf75e5b32d516e9e0398d9a7b1647a1dcf434b385c0e90b123b815957eaed' as const,
      terminalBlockNumberRaw: 190_002_346n,
      terminalBlockHash: '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' as const,
      executionPriceRaw: 98_391_251n,
      executionMode: 'Live',
      receiptHash: V2_RECEIPT_HASH,
      vpiUsdcRaw: 182_822_887n,
      executionEconomicsVersion: 2,
    }
    perpsTradingMocks.waitForPerpsOrderTerminal
      .mockResolvedValueOnce({
        timedOut: false,
        order: terminalOrder,
      })
      .mockResolvedValue({
        timedOut: false,
        order: {
          ...terminalOrder,
          executionOraclePriceRaw: 98_391_482n,
          executionOracleFrozen: false,
          oracleDerivationVersion: 1,
        },
      })
    const onAccountRefresh = vi.fn()
    const baseProps = {
      enableLiveTrading: true,
      initialLifecycleState: 'revealPending' as const,
      initialReviewOpen: true,
      initialDirection: 'short' as const,
      initialOrderQuantity: '100 000',
      initialOrderId: 73n,
      oraclePriceRaw: 98_391_482n,
      oraclePublishTime: Math.floor(Date.now() / 1000),
      availableToTradeRaw: 200_000_000_000n,
      walletUsdcRaw: 2_000_000_000n,
      portfolioValueRaw: 200_000_000_000n,
      withdrawableUsdcRaw: 2_000_000_000n,
      minOpenNotionalUsdc: 100_000_000n,
      minNewPositionNotionalUsdc: 100_000_000n,
      onAccountRefresh,
    }

    const { rerender } = render(
      <PerpsTradeTicket {...baseProps} orderHistory={[]} />
    )

    await waitFor(() => {
      expect(screen.getByText('Final Result')).toBeInTheDocument()
    })
    const finalResult = screen.getByText('Final Result').closest('div')?.parentElement
    expect(finalResult).toBeInTheDocument()
    const vpiRow = within(finalResult!).getByText('VPI').closest('div')
    expect(within(vpiRow!).getByLabelText('Paid 182.8 USDC')).toBeInTheDocument()
    expect(onAccountRefresh).toHaveBeenCalledTimes(1)

    await waitFor(() => {
      expect(within(vpiRow!).getByLabelText('Paid 182.8 USDC')).toBeInTheDocument()
      expect(perpsTradingMocks.waitForPerpsOrderTerminal).toHaveBeenCalledTimes(2)
    }, { timeout: 4_000 })
    expect(onAccountRefresh).toHaveBeenCalledTimes(1)

    const oracleSpreadRow = within(finalResult!).getByText('Oracle confidence spread').closest('div')
    expect(oracleSpreadRow?.querySelector('dd')).toHaveTextContent('~0.0002%')
    perpsTradingMocks.waitForPerpsOrderTerminal.mockReturnValue(
      new Promise(() => {})
    )
    rerender(
      <PerpsTradeTicket
        {...baseProps}
        orderHistory={[{
          ...terminalOrder,
          terminalBlockHash: '0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
          executionPriceRaw: 98_400_000n,
          executionOraclePriceRaw: undefined,
          executionOracleFrozen: false,
          oracleDerivationVersion: 1,
          vpiUsdcRaw: undefined,
          executionEconomicsVersion: 2,
        }]}
      />
    )

    await waitFor(() => {
      expect(vpiRow?.querySelector('dd')).toHaveTextContent('Unavailable')
      expect(oracleSpreadRow?.querySelector('dd')).toHaveTextContent('Unavailable')
    })
    expect(onAccountRefresh).toHaveBeenCalledTimes(2)
  })

  it('keeps the execution-evidence deadline bounded across history rerenders', async () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-07-30T20:00:00Z'))
    mockIsConnected = true
    perpsTradingMocks.waitForPerpsOrderTerminal.mockReturnValue(
      new Promise(() => {})
    )
    const onAccountRefresh = vi.fn()
    const terminalOrder = {
      orderId: 74n,
      time: '30 Jul, 22:00',
      market: 'plDXY Perp',
      side: 'Long',
      type: 'Open',
      price: '0.9839',
      size: '1 000',
      status: 'Executed' as const,
      account: V2_ACCOUNT,
      clientOrderId: V2_CLIENT_ORDER_ID,
      commitTxHash:
        '0x1111111111111111111111111111111111111111111111111111111111111111' as const,
      revealTxHash:
        '0x2222222222222222222222222222222222222222222222222222222222222222' as const,
      terminalBlockNumberRaw: 190_002_347n,
      terminalBlockHash:
        '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' as const,
      executionPriceRaw: 98_391_251n,
    }
    const baseProps = {
      enableLiveTrading: true,
      initialLifecycleState: 'revealPending' as const,
      initialReviewOpen: true,
      initialOrderId: 74n,
      initialOrderQuantity: '1 000',
      oraclePriceRaw: 98_391_482n,
      availableToTradeRaw: 2_000_000_000n,
      walletUsdcRaw: 2_000_000_000n,
      portfolioValueRaw: 2_000_000_000n,
      withdrawableUsdcRaw: 2_000_000_000n,
      minOpenNotionalUsdc: 100_000_000n,
      minNewPositionNotionalUsdc: 100_000_000n,
      onAccountRefresh,
    }
    const { rerender } = render(
      <PerpsTradeTicket {...baseProps} orderHistory={[terminalOrder]} />
    )

    await act(async () => {
      vi.advanceTimersByTime(0)
    })
    expect(screen.getByText('Final Result')).toBeInTheDocument()
    expect(perpsTradingMocks.waitForPerpsOrderTerminal).toHaveBeenCalledTimes(1)

    await act(async () => {
      vi.advanceTimersByTime(30_000)
    })
    rerender(
      <PerpsTradeTicket
        {...baseProps}
        orderHistory={[{ ...terminalOrder }]}
      />
    )
    await act(async () => {
      vi.advanceTimersByTime(0)
    })
    expect(perpsTradingMocks.waitForPerpsOrderTerminal).toHaveBeenCalledTimes(2)

    await act(async () => {
      vi.advanceTimersByTime(30_000)
    })
    const callsAtDeadline =
      perpsTradingMocks.waitForPerpsOrderTerminal.mock.calls.length
    rerender(
      <PerpsTradeTicket
        {...baseProps}
        orderHistory={[{ ...terminalOrder }]}
      />
    )
    await act(async () => {
      vi.advanceTimersByTime(0)
    })

    expect(
      perpsTradingMocks.waitForPerpsOrderTerminal.mock.calls.length
    ).toBe(callsAtDeadline)
  })

  it('keeps waiting for indexed lifecycle finalization after the first wait times out', async () => {
    mockIsConnected = true
    perpsTradingMocks.waitForPerpsOrderTerminal
      .mockResolvedValueOnce({ timedOut: true, order: undefined })
      .mockResolvedValueOnce({
        timedOut: false,
        order: {
          orderId: 72n,
          time: '23 Jun, 11:14',
          market: 'plDXY Perp',
          side: 'Long',
          type: 'Open',
          price: '1.0286',
          size: '1 000',
          status: 'Executed',
          account: V2_ACCOUNT,
          clientOrderId: V2_CLIENT_ORDER_ID,
          receiptHash: V2_RECEIPT_HASH,
          commitTxHash: '0x971c00000000000000000000000000000000eeab',
          revealTxHash: '0xec0c00000000000000000000000000000000d745',
          executionPriceRaw: 97_138_163n,
          executionEconomicsVersion: 2,
        },
      })

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialLifecycleState="revealPending"
        initialReviewOpen
        initialDirection="long"
        initialOrderQuantity="1 000"
        initialOrderId={72n}
        oraclePriceRaw={97_330_315n}
        oraclePublishTime={Math.floor(Date.now() / 1000)}
        availableToTradeRaw={2_000_000_000n}
        walletUsdcRaw={2_000_000_000n}
        portfolioValueRaw={2_000_000_000n}
        withdrawableUsdcRaw={2_000_000_000n}
        minOpenNotionalUsdc={100_000_000n}
        minNewPositionNotionalUsdc={100_000_000n}
      />
    )

    await waitFor(() => {
      expect(perpsTradingMocks.waitForPerpsOrderTerminal).toHaveBeenCalledTimes(2)
    }, { timeout: 4_000 })

    await waitFor(() => {
      expect(screen.getByText('Final Result')).toBeInTheDocument()
    })
  })

  it('stops waiting if the order leaves pending state without executing', async () => {
    mockIsConnected = true
    perpsTradingMocks.waitForPerpsOrderTerminal.mockResolvedValue({
      timedOut: false,
      order: {
        orderId: 60n,
        time: '22 Jun, 12:03',
        market: 'plDXY Perp',
        side: 'Long',
        type: 'Cleanup',
        price: '--',
        size: '--',
        status: 'Expired / Cleaned up',
        account: V2_ACCOUNT,
        clientOrderId: V2_CLIENT_ORDER_ID,
        receiptHash: V2_RECEIPT_HASH,
        commitTxHash: '0x46cb000000000000000000000000000000001cbb',
        revealTxHash: '0x6c0d00000000000000000000000000000000b7d3',
        terminalReason: 'Expired',
      },
    })

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialLifecycleState="revealPending"
        initialReviewOpen
        initialDirection="long"
        initialOrderQuantity="1 000"
        initialOrderId={60n}
        oraclePriceRaw={97_330_315n}
        oraclePublishTime={Math.floor(Date.now() / 1000)}
        availableToTradeRaw={2_000_000_000n}
        walletUsdcRaw={2_000_000_000n}
        portfolioValueRaw={2_000_000_000n}
        withdrawableUsdcRaw={2_000_000_000n}
        minOpenNotionalUsdc={100_000_000n}
        minNewPositionNotionalUsdc={100_000_000n}
      />
    )

    expect(screen.getByText('Waiting for verified market data')).toBeInTheDocument()

    await waitFor(() => {
      expect(screen.getByText('Order failed')).toBeInTheDocument()
    })

    expect(screen.getByText(/The order expired before execution/i)).toBeInTheDocument()
    expect(screen.getAllByText('Unavailable').length).toBeGreaterThan(0)
    expect(screen.queryByRole('button', { name: 'Retry Finalizing' })).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Back to Preview' })).toBeInTheDocument()
  })

  it('shows a direct lifecycle constraint failure while indexed history is behind', async () => {
    mockIsConnected = true
    perpsTradingMocks.readOrderLifecycleOutcome.mockResolvedValue({
      orderId: 12n,
      account: V2_ACCOUNT,
      clientOrderId: V2_CLIENT_ORDER_ID,
      status: 3,
      terminalReason: 8,
      executionMode: 1,
      terminalBlock: 11_604_786n,
      terminalTime: 1_788_167_807n,
      executionPrice: 98_750_341n,
      failedConstraint: 2,
      receiptHash: V2_RECEIPT_HASH,
    })

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialLifecycleState="revealPending"
        initialReviewOpen
        initialDirection="long"
        initialOrderQuantity="5 000"
        initialOrderId={12n}
        initialCommitTxHash="0xd184242bd9852d24639e40d83bf0fdb3b79e12e2adcf03c26fa51c62b7be285c"
        oraclePriceRaw={98_750_339n}
        oraclePublishTime={Math.floor(Date.now() / 1_000)}
        availableToTradeRaw={15_000_000_000n}
        orderHistory={[]}
        ordersIndexedThroughBlockRaw={303_662_124n}
      />
    )

    await waitFor(() => {
      expect(screen.getByText('Order failed')).toBeInTheDocument()
    })
    expect(screen.getByText(/Execution violated an onchain financial bound/i))
      .toBeInTheDocument()
    expect(screen.getByText(/Failed constraint: Execution notional/i))
      .toBeInTheDocument()
    expect(screen.queryByText('Keeper processing')).not.toBeInTheDocument()
  })

  it('waits for indexed terminal order confirmation after manual finalization submits', async () => {
    mockIsConnected = true
    let resolveWait: (value: {
      timedOut: boolean
      order?: {
        orderId: bigint
        time: string
        market: string
        side: string
        type: string
        price: string
        size: string
        status: string
        account: `0x${string}`
        clientOrderId: `0x${string}`
        commitTxHash: `0x${string}`
        receiptHash?: `0x${string}`
        revealTxHash?: `0x${string}`
        executionPriceRaw?: bigint
        executionEconomicsVersion?: number
      }
    }) => void = () => {}
    perpsTradingMocks.waitForPerpsOrderTerminal.mockReturnValue(
      new Promise((resolve) => {
        resolveWait = resolve
      })
    )
    perpsTradingMocks.executeOrder.mockResolvedValue({
      hash: '0x9e1f00000000000000000000000000000000cafe',
      executionPrice: 97_330_315n,
    })

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialLifecycleState="selfExecuteAvailable"
        initialReviewOpen
        initialDirection="long"
        initialOrderQuantity="1 000"
        initialOrderId={63n}
        initialCommitTxHash="0x46cb000000000000000000000000000000001cbb"
        oraclePriceRaw={97_330_315n}
        oraclePublishTime={Math.floor(Date.now() / 1000)}
        availableToTradeRaw={2_000_000_000n}
        walletUsdcRaw={2_000_000_000n}
        portfolioValueRaw={2_000_000_000n}
        withdrawableUsdcRaw={2_000_000_000n}
        minOpenNotionalUsdc={100_000_000n}
        minNewPositionNotionalUsdc={100_000_000n}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: 'Finalize Trade' }))

    await waitFor(() => {
      expect(perpsTradingMocks.executeOrder).toHaveBeenCalledWith(63n)
    })
    expect(screen.getByText('Finalizing trade')).toBeInTheDocument()
    expect(screen.queryByText('Final Result')).not.toBeInTheDocument()

    await act(async () => {
      resolveWait({
        timedOut: false,
        order: {
          orderId: 63n,
          time: '22 Jun, 12:05',
          market: 'plDXY Perp',
          side: 'Long',
          type: 'Open',
          price: '0.9733',
          size: '1 000',
          status: 'Executed',
          account: V2_ACCOUNT,
          clientOrderId: V2_CLIENT_ORDER_ID,
          receiptHash: V2_RECEIPT_HASH,
          commitTxHash: '0x46cb000000000000000000000000000000001cbb',
          revealTxHash: '0x9e1f00000000000000000000000000000000cafe',
          executionPriceRaw: 97_330_315n,
          executionEconomicsVersion: 2,
        },
      })
    })

    await waitFor(() => {
      expect(screen.getByText('Final Result')).toBeInTheDocument()
    })
  })
})
