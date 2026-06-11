import { fireEvent, render, screen, within } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'
import { PerpsAccountPanel } from '../PerpsAccountPanel'
import { PerpsTradeTicket } from '../PerpsTradeTicket'

vi.mock('@reown/appkit/react', () => ({
  useAppKit: () => ({
    open: vi.fn(),
  }),
}))

vi.mock('wagmi', () => ({
  useAccount: () => ({
    isConnected: false,
  }),
  useChainId: () => 421614,
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
  it('distinguishes target, execution, and mark notionals', () => {
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
          }}
        />
      </>
    )

    expect(screen.getAllByText('Target notional').length).toBeGreaterThan(0)
    const finalResult = screen.getByText('Final Result').closest('div')?.parentElement
    expect(finalResult).toBeInTheDocument()
    const finalResultQueries = within(finalResult!)
    expect(finalResultQueries.getByText('Execution notional')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Margin posted')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Protocol execution fee')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Keeper bounty')).toBeInTheDocument()
    expect(finalResultQueries.getByText('Target notional is what you submitted. Execution notional is the committed size valued by contract accounting at reveal.')).toBeInTheDocument()
    expect(finalResultQueries.queryByText('Estimated protocol execution fee')).not.toBeInTheDocument()
    expect(finalResultQueries.queryByText('Estimated keeper bounty')).not.toBeInTheDocument()

    expect(screen.getByText('Mark notional')).toBeInTheDocument()
    expect(screen.getByText('Entry price')).toBeInTheDocument()
    expect(screen.getByText('1.0170')).toBeInTheDocument()
    expect(screen.queryByText('0.9830')).not.toBeInTheDocument()
    expect(screen.getAllByText('Unrealized PnL').length).toBeGreaterThan(0)
    expect(screen.getByText('Mark notional is current exposure. Unrealized PnL shows whether the position is currently up or down.')).toBeInTheDocument()
  })

  it('renders order and trade history tabs from live rows', () => {
    render(
      <PerpsAccountPanel
        isConnected
        orderHistory={[
          {
            orderId: 30n,
            time: '10 Jun, 14:05',
            market: 'DXY Perp',
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
            market: 'DXY Perp',
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
})
