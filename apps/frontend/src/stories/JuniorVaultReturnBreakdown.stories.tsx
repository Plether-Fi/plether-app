import type { Meta, StoryObj } from '@storybook/react-vite'
import { JuniorVaultReturnBreakdown } from '../components/JuniorVaultReturnBreakdown'
import { juniorReturnExample, juniorReturnExamplePeriod } from './fixtures/juniorReturnAttribution'

const meta = {
  title: 'Vaults/Junior Return Breakdown',
  component: JuniorVaultReturnBreakdown,
  tags: ['autodocs'],
  parameters: { layout: 'fullscreen' },
  decorators: [(Story) => <div className="min-h-screen bg-[#080f1b] p-4 sm:p-8"><div className="mx-auto max-w-6xl">
    <p className="mb-4 text-sm text-slate-400">Illustrative data · not audited historical component values</p><Story />
  </div></div>],
  args: { period: juniorReturnExamplePeriod, attribution: juniorReturnExample },
} satisfies Meta<typeof JuniorVaultReturnBreakdown>
export default meta
type Story = StoryObj<typeof meta>

export const BaselineExample: Story = {}
export const MissingAttribution: Story = { args: { attribution: undefined } }
export const Loading: Story = { args: { period: undefined, attribution: undefined, isLoading: true } }
export const Unavailable: Story = { args: { period: undefined, attribution: undefined, isError: true } }
export const ReconciliationDifference: Story = {
  args: { attribution: { ...juniorReturnExample, components: {
    ...juniorReturnExample.components, netVpi: { contributionPp: 0.25, amountUsdc: '3500000000000' },
  } } },
}
export const NegativeReturn: Story = {
  args: {
    period: { ...juniorReturnExamplePeriod, endDilutedSharePrice: '995227311518081856' },
    attribution: {
      ...juniorReturnExample, endDilutedSharePrice: '995227311518081856', netReturnUsdc: '-6353333000000',
      components: { ...juniorReturnExample.components, settledTraderPnl: { contributionPp: -0.913, amountUsdc: '-12173333000000' } },
    },
  },
}
export const Mobile: Story = { decorators: [(Story) => <div style={{ width: 360, maxWidth: '100%' }}><Story /></div>] }
