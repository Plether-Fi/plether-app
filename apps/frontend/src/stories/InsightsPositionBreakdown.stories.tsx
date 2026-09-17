import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, within } from 'storybook/test'
import { PositionBreakdown } from '../../../insights/src/components/PositionBreakdown'
import { TradeFee, TradeNotice, TradeVpi } from '../../../insights/src/components/TradeBreakdown'
import type { WalletActivity } from '../../../insights/src/api/types'
import examples from './fixtures/insights-position-breakdowns.json'
import './InsightsPositionBreakdown.css'

// Snapshot of indexed executions for the account used in the fee investigation.
// Keep stories offline and render the same components as the Insights activity page.
const [open, waivedClose, rebateClose] = examples as WalletActivity[]

function Comparison({ items }: { items: WalletActivity[] }) {
  return <main className="min-h-screen bg-app-bg px-4 py-8 text-content-primary sm:px-8">
    <div className="mx-auto max-w-6xl space-y-6">
      <header>
        <h1 className="text-2xl font-semibold">Position breakdowns</h1>
        <p className="mt-2 text-sm text-content-secondary">Compare open and close executions. Each View breakdown button opens the production Insights dialog with exact six-decimal amounts.</p>
      </header>
      <div className="grid gap-4 lg:grid-cols-3">
        {items.map(item => <section key={item.id} className="space-y-4 border border-brand-border/30 bg-surface-panel p-5">
          <header>
            <h2 className="text-lg font-semibold">{item.type} · {item.market} {item.side?.toUpperCase()}</h2>
            <p className="mt-1 text-xs text-content-secondary">{item.type === 'Open' ? 'Opening costs' : 'Closing costs and settlement'}</p>
          </header>
          <dl className="space-y-3 text-sm">
            <div className="flex justify-between gap-4"><dt className="text-content-secondary">Protocol fee assessed</dt><dd className="whitespace-nowrap tabular-nums"><TradeFee item={item} /></dd></div>
            <div className="flex justify-between gap-4"><dt className="text-content-secondary">VPI</dt><dd className="whitespace-nowrap tabular-nums"><TradeVpi item={item} /></dd></div>
          </dl>
          <TradeNotice item={item} />
          <PositionBreakdown item={item} />
        </section>)}
      </div>
      <p className="text-xs text-content-secondary">VPI is shown before fees and settlement adjustments. These are individual executions, not a combined position return.</p>
    </div>
  </main>
}

const meta = {
  title: 'Insights/Position breakdown',
  component: Comparison,
  parameters: { layout: 'fullscreen' },
  args: { items: [open, waivedClose, rebateClose] },
} satisfies Meta<typeof Comparison>

export default meta
type Story = StoryObj<typeof meta>

export const OpenAndClose: Story = {}

const showBreakdown: NonNullable<Story['play']> = async ({ canvasElement }) => {
  await userEvent.click(within(canvasElement).getByRole('button', { name: 'View breakdown' }))
  const dialog = within(canvasElement.ownerDocument.body).getByRole('dialog', { name: 'Position breakdown' })
  await expect(within(dialog).getByText('Actual account change')).toBeVisible()
  await expect(within(dialog).queryByRole('checkbox')).not.toBeInTheDocument()
}

export const OpenExecution: Story = { args: { items: [open] }, play: showBreakdown }
export const CloseWithWaivedFee: Story = { args: { items: [waivedClose] }, play: showBreakdown }
export const CloseWithRebate: Story = { args: { items: [rebateClose] }, play: showBreakdown }
