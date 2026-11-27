import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import type { BalanceHistoryPoint } from '../api/wallets'
import { WalletBalanceChart } from './WalletBalanceChart'

const NOW = 1_800_000_000
const OBSERVED = new Date(NOW * 1000).toISOString()

const SAMPLE_POINTS: BalanceHistoryPoint[] = [
  {
    occurredAt: new Date((NOW - 60 * 86_400) * 1000).toISOString(),
    timestamp: NOW - 60 * 86_400,
    blockNumber: '330000000',
    balanceAssets: '1000000000', // 1,000 USDG
    eventType: 'Deposit',
    source: 'transfer',
  },
  {
    occurredAt: new Date((NOW - 15 * 86_400) * 1000).toISOString(),
    timestamp: NOW - 15 * 86_400,
    blockNumber: '331500000',
    balanceAssets: '1250000000', // 1,250 USDG
    eventType: 'Close',
    source: 'receipt',
  },
  {
    occurredAt: new Date((NOW - 2 * 86_400) * 1000).toISOString(),
    timestamp: NOW - 2 * 86_400,
    blockNumber: '332700000',
    balanceAssets: '1400000000', // 1,400 USDG
    eventType: 'Close',
    source: 'receipt',
  },
  {
    occurredAt: OBSERVED,
    timestamp: NOW,
    blockNumber: '332885000',
    balanceAssets: '1400000000',
    netEquityAssets: '1450000000',
    eventType: 'Live',
    source: 'ledger',
  },
]

describe('WalletBalanceChart', () => {
  it('switches between 7D, 30D, and 90D timeframes and updates period deltas accurately', () => {
    render(
      <WalletBalanceChart
        points={SAMPLE_POINTS}
        observedAt={OBSERVED}
        releaseVersion="v2.0.0"
      />,
    )

    // Default 30D: baseline from 60d ago (1,000 USDG) -> latest 1,400 USDG (+400.00 USDG, +40.00%)
    expect(screen.getByRole('button', { name: '30D' })).toHaveAttribute('aria-pressed', 'true')
    expect(screen.getByText(/\+400\.00 USDG \(\+40\.00%\)/)).toBeInTheDocument()
    expect(screen.getByRole('img', { name: 'Wallet balance chart over 30d' })).toBeInTheDocument()

    // Switch to 7D: baseline from 15d ago (1,250 USDG) -> latest 1,400 USDG (+150.00 USDG, +12.00%)
    fireEvent.click(screen.getByRole('button', { name: '7D' }))
    expect(screen.getByRole('button', { name: '7D' })).toHaveAttribute('aria-pressed', 'true')
    expect(screen.getByText(/\+150\.00 USDG \(\+12\.00%\)/)).toBeInTheDocument()
    expect(screen.getByRole('img', { name: 'Wallet balance chart over 7d' })).toBeInTheDocument()

    // Switch to 90D: starts at 1,000 USDG -> 1,400 USDG (+400.00 USDG, +40.00%)
    fireEvent.click(screen.getByRole('button', { name: '90D' }))
    expect(screen.getByRole('button', { name: '90D' })).toHaveAttribute('aria-pressed', 'true')
    expect(screen.getByText(/\+400\.00 USDG \(\+40\.00%\)/)).toBeInTheDocument()
  })

  it('fails closed cleanly when awaiting backend deploy or when no points exist', () => {
    render(<WalletBalanceChart points={[]} awaitingDeploy />)
    expect(screen.getByText('Wallet balance history awaiting Stage 3 backend')).toBeInTheDocument()
  })

  it('toggles cleanly between Balance and Cumulative PnL views with win rate stats', () => {
    render(
      <WalletBalanceChart
        points={SAMPLE_POINTS}
        activity={[
          {
            id: '0x1:1',
            activityType: 'Close',
            side: 'long',
            pnlAssets: '250000000',
            txHash: `0x${'a'.repeat(64)}`,
            blockNumber: '331500000',
            occurredAt: new Date((NOW - 15 * 86_400) * 1000).toISOString(),
          },
          {
            id: '0x1:2',
            activityType: 'Close',
            side: 'short',
            pnlAssets: '-50000000',
            txHash: `0x${'b'.repeat(64)}`,
            blockNumber: '332700000',
            occurredAt: new Date((NOW - 2 * 86_400) * 1000).toISOString(),
          },
        ]}
        observedAt={OBSERVED}
        releaseVersion="v2.0.0"
      />,
    )

    fireEvent.click(screen.getByRole('button', { name: 'Cumulative PnL' }))
    expect(screen.getByRole('button', { name: 'Cumulative PnL' })).toHaveAttribute('aria-pressed', 'true')
    expect(screen.getByRole('img', { name: 'Cumulative realized PnL chart over 30d' })).toBeInTheDocument()
    expect(screen.getByText('+200.00 USDG')).toBeInTheDocument()
    expect(screen.getByText(/1W \/ 1L \(50\.0%\)/)).toBeInTheDocument()
  })
})
