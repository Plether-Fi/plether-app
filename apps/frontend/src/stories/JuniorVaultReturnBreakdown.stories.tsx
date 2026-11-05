import type { Meta, StoryObj } from '@storybook/react-vite'
import { JuniorVaultReturnBreakdown } from '../components/JuniorVaultReturnBreakdown'
import { PerformanceTab, VAULT_TRANCHES } from '../pages/Vaults'
import { buildJuniorReturnBreakdown } from '../utils/juniorReturnAttribution'
import type { JuniorReturnAttribution } from '../api/types'
import { juniorReturnExample, juniorReturnExamplePeriod } from './fixtures/juniorReturnAttribution'
import { createVaultAssetHistory } from './fixtures/vaultAssetHistory'

const meta = {
  title: 'Vaults/Junior Return Breakdown',
  component: JuniorVaultReturnBreakdown,
  tags: ['autodocs'],
  parameters: { layout: 'fullscreen' },
  decorators: [(Story) => <div className="min-h-screen bg-app-bg p-4 sm:p-8"><div className="mx-auto max-w-6xl">
    <p className="mb-4 text-sm text-content-secondary">Illustrative data · not audited historical component values</p><Story />
  </div></div>],
  args: { period: juniorReturnExamplePeriod, attribution: juniorReturnExample },
} satisfies Meta<typeof JuniorVaultReturnBreakdown>
export default meta
type Story = StoryObj<typeof meta>

function performanceExample(attribution?: JuniorReturnAttribution | null) {
  const history = createVaultAssetHistory()
  const period = {
    ...juniorReturnExamplePeriod,
    startTimestamp: history.junior.points[0].timestamp,
    endTimestamp: history.junior.points[history.junior.points.length - 1].timestamp,
  }
  const startPrice = BigInt(period.startDilutedSharePrice)
  const change = BigInt(period.endDilutedSharePrice) - startPrice
  history.junior.points = history.junior.points.map((point, index) => ({
    ...point,
    sharePrice: (startPrice + change * BigInt(index) / 168n).toString(),
  }))
  const metrics = buildJuniorReturnBreakdown(period)
  if (!metrics || metrics.apyPercent === null) throw new Error('Invalid performance fixture')
  history.junior.return7d = metrics.returnPp / 100
  history.junior.apy7d = metrics.apyPercent / 100
  history.junior.returnAttribution = attribution ? { ...attribution, ...period } : undefined
  return {
    history,
    performance: {
      apy7d: history.junior.apy7d,
      return7d: history.junior.return7d,
      periodStart: period.startTimestamp,
      periodEnd: period.endTimestamp,
      points: history.junior.points.map((point) => ({ ...point, sharePrice: Number(point.sharePrice) / 1e18 })),
    },
  }
}

export const BaselineExample: Story = {}
export const InVaultPerformance: Story = {
  render: (args) => <div className="space-y-6">
    <div>
      <p className="text-xs font-semibold uppercase tracking-[0.18em] text-content-secondary">Historical results</p>
      <h2 className="mt-1 text-2xl font-semibold text-content-primary">Performance</h2>
    </div>
    <PerformanceTab tranche={VAULT_TRANCHES.junior} {...performanceExample(args.attribution)} />
  </div>,
}
export const MissingAttribution: Story = { args: { attribution: undefined } }
export const MissingAttributionInVaultPerformance: Story = {
  ...InVaultPerformance,
  args: { attribution: undefined },
}
export const Loading: Story = { args: { period: undefined, attribution: undefined, isLoading: true } }
export const Unavailable: Story = { args: { period: undefined, attribution: undefined, isError: true } }
export const ReconciliationDifference: Story = {
  args: { attribution: { ...juniorReturnExample, components: {
    ...juniorReturnExample.components, netVpi: { contributionPp: 0.25, amountUsdg: '3500000000000' },
  } } },
}
export const NegativeReturn: Story = {
  args: {
    period: { ...juniorReturnExamplePeriod, endDilutedSharePrice: '995227311518081856' },
    attribution: {
      ...juniorReturnExample, endDilutedSharePrice: '995227311518081856', netReturnUsdg: '-6353333000000',
      components: { ...juniorReturnExample.components, settledTraderPnl: { contributionPp: -0.913, amountUsdg: '-12173333000000' } },
    },
  },
}
export const Mobile: Story = { decorators: [(Story) => <div style={{ width: 360, maxWidth: '100%' }}><Story /></div>] }
