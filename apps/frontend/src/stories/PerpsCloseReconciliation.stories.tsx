import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsCloseReconciliationDisclosure } from '../components/PerpsCloseReconciliationDetails'
import { derivePerpsCloseReconciliation } from '../utils/perpsCloseReconciliation'
import { closeSettlementAdjustmentReceipt } from '../utils/__fixtures__/closeSettlementAdjustment'

const meta = {
  title: 'Perps/Close Reconciliation',
  component: PerpsCloseReconciliationDisclosure,
  parameters: { layout: 'fullscreen' },
  decorators: [(Story) => <div className="mx-auto max-w-xl bg-surface-panel p-4"><Story /></div>],
  args: {
    reconciliation: derivePerpsCloseReconciliation(closeSettlementAdjustmentReceipt)!,
  },
} satisfies Meta<typeof PerpsCloseReconciliationDisclosure>

export default meta
type Story = StoryObj<typeof meta>

export const Collapsed: Story = {}
export const Expanded: Story = { args: { initiallyExpanded: true } }
