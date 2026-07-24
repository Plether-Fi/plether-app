import { render, screen, within } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { WalletPage } from './WalletPage'

const apiMocks = vi.hoisted(() => ({
  useWallet: vi.fn(),
}))

vi.mock('../api', async (importOriginal) => ({
  ...await importOriginal<typeof import('../api')>(),
  useWallet: apiMocks.useWallet,
}))

const address = '0x1111111111111111111111111111111111111111'

beforeEach(() => {
  apiMocks.useWallet.mockReturnValue({
    data: {
      competition: {
        id: 'competition-1',
        slug: 'testnet-trading-2026',
        name: 'Testnet Trading Competition',
        status: 'live',
        startsAt: '2026-07-20T16:00:00Z',
        tradingCutoffAt: '2026-08-03T16:00:00Z',
        resultsAt: '2026-08-05T12:00:00Z',
        startingBalance: '100000000000',
        pnlEligibilityThreshold: '1000000000',
        minActiveDays: 5,
        prizes: [],
        latestIndexedBlock: 123,
        latestIndexedAt: '2026-07-20T12:00:00Z',
      },
      wallet: {
        rank: 1,
        address,
        displayName: 'Trader',
        pnl: '1000000',
        realizedPnl: '1000000',
        roiBps: 1,
        volume: '1000000000',
        trades: 2,
        activeDays: 1,
        liquidations: 0,
        prizePlace: null,
        prizePlaces: [],
        prizeAmountUsdc: null,
        eligible: false,
        eligibilityStatus: 'pending',
        eligibilityReasons: [],
        equity: '100001000000',
        position: null,
      },
      activity: [
        {
          id: 'open',
          type: 'Open',
          occurredAt: '2026-07-20T12:00:00Z',
          market: 'DXY',
          side: 'long',
          size: '4412651342500',
          sizeDelta: '1',
          price: '1.03058',
          pnl: null,
          executionFee: '1765060537',
          vpi: '4854090357',
          txHash: null,
        },
        {
          id: 'close',
          type: 'Close',
          occurredAt: '2026-07-20T13:00:00Z',
          market: 'DXY',
          side: 'long',
          size: '28200367500',
          sizeDelta: '1',
          price: '1.03090',
          pnl: '9497545',
          executionFee: '11280147',
          vpi: '-30992947',
          txHash: null,
        },
        {
          id: 'deposit',
          type: 'Deposit',
          occurredAt: '2026-07-20T14:00:00Z',
          market: null,
          side: null,
          size: '100000000',
          sizeDelta: null,
          price: null,
          pnl: null,
          executionFee: null,
          vpi: null,
          txHash: null,
        },
      ],
    },
    isError: false,
    isLoading: false,
  })
})

describe('WalletPage activity costs', () => {
  it('shows protocol fee and signed VPI on trade rows', () => {
    render(
      <MemoryRouter initialEntries={[`/competitions/testnet-trading-2026/wallets/${address}`]}>
        <Routes>
          <Route path="/competitions/:slug/wallets/:address" element={<WalletPage />} />
        </Routes>
      </MemoryRouter>,
    )

    const [headerRow, openRow, closeRow, depositRow] = screen.getAllByRole('row')
    expect(within(headerRow).getByText('Protocol fee')).toBeInTheDocument()
    expect(within(headerRow).getByText('VPI')).toHaveAttribute(
      'title',
      'Positive VPI is a charge; negative VPI is a rebate.',
    )
    expect(within(openRow).getByText('1,765.06 USDC')).toBeInTheDocument()
    expect(within(openRow).getByText('+4,854.09 USDC')).toHaveClass('text-brand-orange')
    expect(within(closeRow).getByText('11.28 USDC')).toBeInTheDocument()
    expect(within(closeRow).getByText('-30.99 USDC')).toHaveClass('text-positive')
    expect(within(depositRow).queryByText('11.28 USDC')).not.toBeInTheDocument()
  })
})
