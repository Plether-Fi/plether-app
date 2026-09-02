import type { Meta, StoryObj } from '@storybook/react-vite'
import { MemoryRouter } from 'react-router-dom'
import { Leaderboard, LeaderboardTitle } from '../../../insights/src/components/Competition'
import { Panel } from '../../../insights/src/components/ui'
import type { Standing } from '../../../insights/src/api'
import '../../../insights/src/index.css'

const competitionSlug = 'testnet-trading-2026'

const standings: Standing[] = [
  {
    rank: 1,
    address: '0x1111111111111111111111111111111111111111',
    displayName: '@profitable_trader',
    pnl: '2480750000',
    realizedPnl: '2480750000',
    roiBps: 248,
    volume: '425000000000',
    trades: 18,
    activeDays: 5,
    liquidations: 0,
    prizePlace: null,
    prizePlaces: [],
    prizeAmountUsdc: null,
    eligible: false,
    eligibilityStatus: 'pending',
    eligibilityReasons: ['Integrity review pending'],
  },
  {
    rank: 2,
    address: '0x2222222222222222222222222222222222222222',
    displayName: '@small_loss',
    pnl: '-125500000',
    realizedPnl: '-125500000',
    roiBps: -13,
    volume: '218500000000',
    trades: 9,
    activeDays: 3,
    liquidations: 0,
    prizePlace: null,
    prizePlaces: [],
    prizeAmountUsdc: null,
    eligible: false,
    eligibilityStatus: 'pending',
    eligibilityReasons: ['Below the +1.00 USDC prize threshold'],
  },
  {
    rank: 3,
    address: '0x3333333333333333333333333333333333333333',
    displayName: '@larger_loss',
    pnl: '-3210000000',
    realizedPnl: '-3210000000',
    roiBps: -321,
    volume: '805000000000',
    trades: 24,
    activeDays: 4,
    liquidations: 1,
    prizePlace: null,
    prizePlaces: [],
    prizeAmountUsdc: null,
    eligible: false,
    eligibilityStatus: 'pending',
    eligibilityReasons: ['Below the +1.00 USDC prize threshold'],
  },
  {
    rank: null,
    address: '0x4444444444444444444444444444444444444444',
    displayName: '@no_trades_yet',
    pnl: '0',
    realizedPnl: '0',
    roiBps: 0,
    volume: '0',
    trades: 0,
    activeDays: 0,
    liquidations: 0,
    prizePlace: null,
    prizePlaces: [],
    prizeAmountUsdc: null,
    eligible: false,
    eligibilityStatus: 'pending',
    eligibilityReasons: ['Integrity review pending'],
  },
  {
    rank: null,
    address: '0x5555555555555555555555555555555555555555',
    displayName: '@also_inactive',
    pnl: '0',
    realizedPnl: '0',
    roiBps: 0,
    volume: '0',
    trades: 0,
    activeDays: 0,
    liquidations: 0,
    prizePlace: null,
    prizePlaces: [],
    prizeAmountUsdc: null,
    eligible: false,
    eligibilityStatus: 'pending',
    eligibilityReasons: ['Integrity review pending'],
  },
]

function InsightsLeaderboardPreview() {
  return (
    <MemoryRouter>
      <main className="min-h-screen bg-app-bg px-4 py-8 text-content-primary sm:px-8">
        <div className="mx-auto max-w-6xl space-y-4">
          <LeaderboardTitle count={standings.length} competitionSlug={competitionSlug} />
          <Panel>
            <Leaderboard standings={standings} search="" competitionSlug={competitionSlug} />
          </Panel>
        </div>
      </main>
    </MemoryRouter>
  )
}

const meta = {
  title: 'Insights/Leaderboard/Zero-trade ranking',
  component: InsightsLeaderboardPreview,
  parameters: {
    layout: 'fullscreen',
  },
} satisfies Meta<typeof InsightsLeaderboardPreview>

export default meta
type Story = StoryObj<typeof meta>

export const ActiveTradersAboveUnrankedAccounts: Story = {}
