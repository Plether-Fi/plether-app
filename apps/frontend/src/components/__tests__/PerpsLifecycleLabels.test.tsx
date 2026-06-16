import { fireEvent, render, screen, within } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { PerpsAccountPanel } from '../PerpsAccountPanel'
import { PerpsTradeTicket } from '../PerpsTradeTicket'

vi.mock('@reown/appkit/react', () => ({
  useAppKit: () => ({
    open: vi.fn(),
  }),
}))

let mockIsConnected = false

vi.mock('wagmi', () => ({
  useAccount: () => ({
    address: '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B',
    isConnected: mockIsConnected,
  }),
  useChainId: () => 421614,
  useReadContracts: () => ({
    data: undefined,
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

vi.mock('../../hooks', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../../hooks')>()
  return {
    ...actual,
    usePerpsTrading: () => ({
      depositMargin: vi.fn(),
      withdrawMargin: vi.fn(),
      commitOrder: vi.fn(),
      executeOrder: vi.fn(),
      cleanupExpiredOrder: vi.fn(),
    }),
  }
})

describe('perps lifecycle labels', () => {
  beforeEach(() => {
    mockIsConnected = false
  })

  it('distinguishes plDXY Perp exposure from contract and entry notionals', () => {
    render(
      <>
        <PerpsTradeTicket
          initialLifecycleState="executed"
          initialReviewOpen
          initialDirection="long"
          initialSize="2 000"
        />
        <PerpsAccountPanel
          isConnected
          position={{
            exists: true,
            side: 0,
            direction: 'long',
            size: 0n,
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
    expect(screen.getAllByText('Contract notional').length).toBeGreaterThan(0)
    expect(screen.getByText('Trade executed at 1.0089 USDC')).toBeInTheDocument()
    const finalResult = screen.getByText('Final Result').closest('div')?.parentElement
    expect(finalResult).toBeInTheDocument()
    const finalResultQueries = within(finalResult!)
    expect(finalResultQueries.getByText('Target plDXY Perp exposure')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Execution plDXY Perp exposure')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Margin posted')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Protocol execution fee')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Keeper bounty')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Target plDXY Perp exposure is what you submitted. Execution plDXY Perp exposure is the committed size valued with the displayed plDXY Perp price at reveal.')).toBeInTheDocument()
    expect(finalResultQueries.queryByText('Estimated protocol execution fee')).not.toBeInTheDocument()
    expect(finalResultQueries.queryByText('Estimated keeper bounty')).not.toBeInTheDocument()

    expect(screen.getByText('Entry notional')).toBeInTheDocument()
    expect(screen.getByText('Entry price')).toBeInTheDocument()
    expect(screen.getByText('1.0170')).toBeInTheDocument()
    expect(screen.queryByText('0.9830')).not.toBeInTheDocument()
    expect(screen.getAllByText('Unrealized PnL').length).toBeGreaterThan(0)
    expect(screen.getByText('Cost of carry')).toBeInTheDocument()
    expect(screen.getByText('1.25')).toBeInTheDocument()
    expect(screen.getByText('Entry notional is the executed order size. plDXY Perp exposure is current displayed exposure.')).toBeInTheDocument()
  })

  it('resets the review modal lifecycle when it closes', () => {
    render(
      <PerpsTradeTicket
        initialLifecycleState="executed"
        initialReviewOpen
        initialDirection="long"
        initialSize="2 000"
      />
    )

    expect(screen.getByText('Final Result')).toBeInTheDocument()

    fireEvent.keyDown(document, { key: 'Escape' })
    fireEvent.click(screen.getByRole('button', { name: 'Review Long' }))

    expect(screen.getByText('Commit Preview')).toBeInTheDocument()
    expect(screen.queryByText('Final Result')).not.toBeInTheDocument()
  })

  it('renders order and trade history tabs from live rows', () => {
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
            commitTxHash: '0x9d4b00000000000000000000000000000000f953',
            revealTxHash: '0x6c0d00000000000000000000000000000000b7d3',
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
        ]}
      />
    )

    fireEvent.click(screen.getByRole('button', { name: 'Order History' }))
    expect(screen.getByText('30')).toBeInTheDocument()
    expect(screen.getByText('Executed')).toBeInTheDocument()
    expect(screen.getByText('1 999.67')).toBeInTheDocument()
    expect(screen.getByText('Commit')).toBeInTheDocument()
    expect(screen.getByText('Reveal')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Trade History' }))
    expect(screen.getByText('Open Long')).toBeInTheDocument()
    expect(screen.getByText('1.0170')).toBeInTheDocument()
    expect(screen.queryByText('0.9830')).not.toBeInTheDocument()
    expect(screen.getByText('1 999.67')).toBeInTheDocument()
  })

  it('fills current position and max with exact plDXY Perp exposure instead of rounded display value', () => {
    render(
      <PerpsTradeTicket
        initialDirection="short"
        currentPosition={{
          exists: true,
          side: 0,
          direction: 'long',
          size: 0n,
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

    expect(screen.getByRole('textbox')).toHaveValue('1 553.249999')

    fireEvent.change(screen.getByRole('textbox'), { target: { value: '0' } })
    fireEvent.click(screen.getByRole('button', { name: /Current Position/ }))

    expect(screen.getByRole('textbox')).toHaveValue('1 553.249999')
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

  it('uses the engine new-position minimum when opening from zero', () => {
    mockIsConnected = true

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="long"
        initialSize="104"
        oraclePriceRaw={98434897n}
        oraclePublishTime={1781267148}
        minOpenNotionalUsdc={100000000n}
        minNewPositionNotionalUsdc={1000000000n}
      />
    )

    expect(screen.getByText('Minimum new position is 1 031.8 USDC.')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review Long' })).toBeDisabled()
  })

  it('explains when a pending full close already reserves the position', () => {
    mockIsConnected = true
    const fullPositionSize = 1526359014354277024332n

    render(
      <PerpsTradeTicket
        enableLiveTrading
        initialDirection="short"
        initialSize="1 553.25"
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
})
