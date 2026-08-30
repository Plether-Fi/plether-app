import { fireEvent, render, screen, waitFor } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'

vi.mock('../../perps-aa', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../../perps-aa')>()
  const address = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'
  return {
    ...actual,
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
  }
})
import { PerpsTradeTicket } from '../PerpsTradeTicket'

let mockIsConnected = true
let mockChainId = 421614

const analyticsMocks = vi.hoisted(() => ({
  trackPerpsButtonClicked: vi.fn(),
  trackPerpsMarginLifecycle: vi.fn(),
  trackPerpsModalClosed: vi.fn(),
  trackPerpsModalOpened: vi.fn(),
  trackPerpsOrderLifecycle: vi.fn(),
  trackPerpsValidationBlocked: vi.fn(),
}))

const tradingMocks = vi.hoisted(() => ({
  prepareOrder: vi.fn(),
  commitOrder: vi.fn(),
  cleanupExpiredOrder: vi.fn(),
  depositMargin: vi.fn(),
  executeOrder: vi.fn(),
  switchToArbitrumSepolia: vi.fn(),
  waitForPerpsOrderTerminal: vi.fn(),
  withdrawMargin: vi.fn(),
}))

vi.mock('@reown/appkit/react', () => ({
  createAppKit: vi.fn(),
  useAppKit: () => ({
    open: vi.fn(),
  }),
}))

vi.mock('wagmi', () => ({
  http: vi.fn(() => ({})),
  useAccount: () => ({
    address: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
    isConnected: mockIsConnected,
  }),
  useChainId: () => mockChainId,
  useReadContracts: () => ({
    data: [{
      status: 'success',
      result: {
        valid: true,
        invalidReason: 0,
        failureCategory: 0,
        executionPrice: 99_000_000n,
        sizeDelta: 100_000_000n,
        notionalUsdc: 100_000_000n,
        marginDeltaUsdc: 20_000_000n,
        vpiUsdc: 0n,
        executionFeeUsdc: 40_000n,
        initialMarginRequirementUsdc: 20_000_000n,
        maintenanceMarginUsdc: 5_000_000n,
        postSize: 100_000_000n,
        postMarginUsdc: 20_000_000n,
        postEntryPrice: 99_000_000n,
        postVpiAccrued: 0n,
        postUnrealizedPnlUsdc: 0n,
        postEquityUsdc: 20_000_000n,
        postHealthBps: 4000n,
        postLiquidatable: false,
        hasLiquidationPrice: true,
        liquidationPrice: 94_000_000n,
      },
    }],
    isFetching: false,
    isLoading: false,
  }),
}))

vi.mock('../../hooks', () => ({
  usePerpsTrading: () => ({
    prepareOrder: tradingMocks.prepareOrder,
    cleanupExpiredOrder: tradingMocks.cleanupExpiredOrder,
    commitOrder: tradingMocks.commitOrder,
    depositMargin: tradingMocks.depositMargin,
    executeOrder: tradingMocks.executeOrder,
    withdrawMargin: tradingMocks.withdrawMargin,
  }),
  useSwitchToArbitrumSepolia: () => ({
    switchToArbitrumSepolia: tradingMocks.switchToArbitrumSepolia,
    switchError: null,
  }),
  waitForPerpsOrderTerminal: tradingMocks.waitForPerpsOrderTerminal,
}))

vi.mock('../../analytics/perps', () => ({
  perpsChainState: (isConnected: boolean, isCorrectChain: boolean) => (
    !isConnected ? 'unknown' : isCorrectChain ? 'correct_chain' : 'wrong_chain'
  ),
  perpsConnectedState: (isConnected: boolean) => isConnected ? 'connected' : 'disconnected',
  perpsErrorCategory: () => 'unknown',
  perpsSizeBucket: () => '100_999',
  trackPerpsButtonClicked: analyticsMocks.trackPerpsButtonClicked,
  trackPerpsMarginLifecycle: analyticsMocks.trackPerpsMarginLifecycle,
  trackPerpsModalClosed: analyticsMocks.trackPerpsModalClosed,
  trackPerpsModalOpened: analyticsMocks.trackPerpsModalOpened,
  trackPerpsOrderLifecycle: analyticsMocks.trackPerpsOrderLifecycle,
  trackPerpsValidationBlocked: analyticsMocks.trackPerpsValidationBlocked,
}))

function renderTicket(overrides: Partial<Parameters<typeof PerpsTradeTicket>[0]> = {}) {
  return render(
    <PerpsTradeTicket
      initialSize="200"
      oraclePriceRaw={99_000_000n}
      oraclePublishTime={1_700_000_000}
      availableToTradeRaw={1_000_000_000n}
      marginAllowanceUsdc={1_000_000_000n}
      marketPhase="open"
      {...overrides}
    />
  )
}

describe('Perps analytics', () => {
  beforeEach(() => {
    mockIsConnected = true
    mockChainId = 421614
    Object.values(analyticsMocks).forEach((mock) => { mock.mockReset() })
    Object.values(tradingMocks).forEach((mock) => { mock.mockReset() })
    tradingMocks.prepareOrder.mockResolvedValue({
      account: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
      manifestVersion: 'perps-aa-arbitrum-sepolia-v2',
      orderRouter: '0x1111111111111111111111111111111111111111',
      orderLifecycleBook: '0x2222222222222222222222222222222222222222',
      request: {
        clientOrderId: `0x${'12'.repeat(32)}`,
        side: 0,
        sizeDelta: 100_000_000n,
        marginDelta: 20_000_000n,
        targetPrice: 99_099_000n,
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
      reviewedPrice: 99_000_000n,
      protection: {
        validUntil: 1_700_000_300n,
        executionMode: 1,
        executionBountyUsdc: 10_000n,
        maxGrossAccountDebitUsdc: 120_010_000n,
        maxActionChargeUsdc: 1_000_000n,
        maxExplicitFeesUsdc: 1_000_000n,
        maxPostLeverageBps: 50_000,
        minPostSettlementBalanceUsdc: 800_000_000n,
        minPostPositionEquityUsdc: 20_000_000n,
      },
    })
  })

  it('tracks review modal opens with safe properties', async () => {
    renderTicket()

    fireEvent.click(screen.getByRole('button', { name: 'Review Long' }))

    expect(analyticsMocks.trackPerpsButtonClicked).toHaveBeenCalledWith('review_trade', expect.objectContaining({
      chain_state: 'correct_chain',
      connected_state: 'connected',
      direction: 'long',
      size_bucket: '100_999',
    }))
    await waitFor(() => {
      expect(analyticsMocks.trackPerpsModalOpened).toHaveBeenCalledWith('trade_review', expect.not.objectContaining({
        address: expect.anything(),
        amount: expect.anything(),
        tx_hash: expect.anything(),
      }))
    })
  })

  it('tracks margin modal and margin lifecycle events', async () => {
    renderTicket()

    fireEvent.click(screen.getByRole('button', { name: 'Deposit' }))

    expect(analyticsMocks.trackPerpsMarginLifecycle).toHaveBeenCalledWith('deposit_opened', expect.objectContaining({
      direction: 'long',
      size_bucket: '100_999',
    }))
    await waitFor(() => {
      expect(analyticsMocks.trackPerpsModalOpened).toHaveBeenCalledWith('deposit_margin', expect.objectContaining({
        chain_state: 'correct_chain',
      }))
    })
  })

  it('tracks commit failures without hashes or order ids', async () => {
    tradingMocks.commitOrder.mockRejectedValue(new Error('wallet rejected 0x1111111111111111111111111111111111111111111111111111111111111111'))
    renderTicket({
      enableLiveTrading: true,
      initialReviewOpen: true,
    })

    await waitFor(() => {
      expect(screen.getByRole('button', { name: 'Confirm Commit' })).toBeEnabled()
    })
    fireEvent.click(screen.getByRole('button', { name: 'Confirm Commit' }))

    await waitFor(() => {
      expect(analyticsMocks.trackPerpsOrderLifecycle).toHaveBeenCalledWith('commit_failed', expect.objectContaining({
        chain_state: 'correct_chain',
        error_category: 'unknown',
      }))
    })
    expect(analyticsMocks.trackPerpsOrderLifecycle).not.toHaveBeenCalledWith('commit_failed', expect.objectContaining({
      tx_hash: expect.anything(),
      order_id: expect.anything(),
    }))
  })

  it('tracks validation blocked with a category instead of the display message', async () => {
    renderTicket({
      enableLiveTrading: true,
      oraclePriceRaw: undefined,
    })

    await waitFor(() => {
      expect(analyticsMocks.trackPerpsValidationBlocked).toHaveBeenCalledWith('oracle_unavailable', expect.objectContaining({
        chain_state: 'correct_chain',
      }))
    })
    expect(analyticsMocks.trackPerpsValidationBlocked).not.toHaveBeenCalledWith(
      expect.stringContaining('plDXY'),
      expect.anything()
    )
  })
})
