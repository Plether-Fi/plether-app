import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { useState } from 'react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { queryKeys, type WalletResponse } from '../../../insights/src/api'
import { WalletPage } from '../../../insights/src/pages/WalletPage'
import '../../../insights/src/index.css'

const competitionSlug = 'testnet-trading-2026'
const walletAddress = '0xaD3C5bBbaB13D3E9d89c229e90e46e805610024F'

const walletResponse: WalletResponse = {
  competition: {
    id: 'competition-story',
    slug: competitionSlug,
    name: 'Plether Testnet Trading Competition',
    status: 'live',
    startsAt: '2026-07-20T16:00:00.000Z',
    tradingCutoffAt: '2026-08-03T16:00:00.000Z',
    resultsAt: '2026-08-10T16:00:00.000Z',
    startingBalance: '100000000000',
    pnlEligibilityThreshold: '1000000000',
    minActiveDays: 5,
    prizes: [
      { place: 1, amount: '600000000' },
      { place: 2, amount: '300000000' },
      { place: 3, amount: '100000000' },
    ],
    latestIndexedBlock: 149874321,
    latestIndexedAt: '2026-07-22T09:30:00.000Z',
  },
  wallet: {
    rank: 4,
    address: walletAddress,
    displayName: '@profile_trader',
    pnl: '430270000',
    realizedPnl: '313180000',
    roiBps: 43,
    equity: '100430270000',
    volume: '999890000000',
    trades: 8,
    activeDays: 3,
    liquidations: 0,
    prizePlace: null,
    prizePlaces: [],
    prizeAmountUsdc: null,
    eligible: false,
    eligibilityStatus: 'pending',
    eligibilityReasons: ['3 of 5 active days', 'Integrity review pending'],
    position: {
      market: 'plDXY Perp',
      side: 'short',
      size: '999890000000',
      sizeDelta: '-1027300',
      margin: '99571930000',
      entryPrice: '0.97329',
      markPrice: '0.97244',
      unrealizedPnl: '874930000',
      liquidatable: false,
    },
  },
  activity: [
    {
      id: 'activity-close',
      type: 'decrease_position',
      occurredAt: '2026-07-22T08:52:00.000Z',
      market: 'plDXY Perp',
      side: 'short',
      size: '250000000000',
      sizeDelta: '256862',
      price: '0.97261',
      pnl: '313180000',
      txHash: '0x8b3b735d1f629943362f2ae67bac89996571a7cc2c0be40ad73b7d50507ca122',
    },
    {
      id: 'activity-open',
      type: 'open_position',
      occurredAt: '2026-07-21T03:33:00.000Z',
      market: 'plDXY Perp',
      side: 'short',
      size: '999890000000',
      sizeDelta: '-1027300',
      price: '0.97329',
      pnl: null,
      txHash: '0xb9a6a7c2d94b48e299ef4399ebc8608294de5626704be96559459195857b00ab',
    },
    {
      id: 'activity-deposit',
      type: 'deposit',
      occurredAt: '2026-07-20T16:12:00.000Z',
      market: null,
      side: null,
      size: '100000000000',
      sizeDelta: null,
      price: null,
      pnl: null,
      txHash: '0x7b821ad5822352950d2ef179be204db011f5f236ef28887895d4126215916c5b',
    },
  ],
}

function ApplicantProfilePreview() {
  const route = `/competitions/${competitionSlug}/wallets/${walletAddress}`
  const [queryClient] = useState(() => {
    const client = new QueryClient({
      defaultOptions: {
        queries: {
          retry: false,
          refetchOnWindowFocus: false,
        },
      },
    })
    client.setQueryData(queryKeys.wallet(competitionSlug, walletAddress.toLowerCase()), walletResponse)
    return client
  })

  return (
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={[route]}>
        <main className="min-h-screen bg-app-bg px-4 py-8 text-content-primary sm:px-8">
          <div className="mx-auto max-w-7xl">
            <Routes>
              <Route path="/competitions/:slug/wallets/:address" element={<WalletPage />} />
            </Routes>
          </div>
        </main>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta = {
  title: 'Insights/Applicant profile/Trading activity',
  component: ApplicantProfilePreview,
  parameters: {
    layout: 'fullscreen',
  },
} satisfies Meta<typeof ApplicantProfilePreview>

export default meta
type Story = StoryObj<typeof meta>

export const ProfileWithOpenPositionAndActivity: Story = {}
