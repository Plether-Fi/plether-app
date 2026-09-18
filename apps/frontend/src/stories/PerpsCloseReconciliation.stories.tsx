import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsCloseReconciliationDisclosure } from '../components/PerpsCloseReconciliationDetails'
import { derivePerpsCloseReconciliation } from '../utils/perpsCloseReconciliation'
import { closeOrder14Receipt } from '../utils/__fixtures__/closeOrder14'
import { closeSettlementAdjustmentReceipt } from '../utils/__fixtures__/closeSettlementAdjustment'

const meta = {
  title: 'Perps/Close Reconciliation',
  component: PerpsCloseReconciliationDisclosure,
  parameters: { layout: 'fullscreen' },
  decorators: [(Story) => <div className="mx-auto max-w-xl bg-surface-panel p-4"><Story /></div>],
  args: {
    reconciliation: derivePerpsCloseReconciliation({
      ...closeSettlementAdjustmentReceipt,
      totalPositionVpiUsdc: '148190000',
      positionLifetimeNetResultUsdc: '-8000000000',
      positionLifetimeTradesResultUsdc: '-7990000000',
      positionLifetimeAccountAdjustmentUsdc: '-10000000',
    }, { preExecutionPositionMarginUsdc: 5_000_000_000n })!,
  },
} satisfies Meta<typeof PerpsCloseReconciliationDisclosure>

export default meta
type Story = StoryObj<typeof meta>

export const Summary: Story = {}
export const Expanded: Story = {}

export const MissingPositionHistory: Story = {
  args: {
    reconciliation: derivePerpsCloseReconciliation(closeSettlementAdjustmentReceipt)!,
  },
}

export const PartialClose: Story = {
  args: {
    reconciliation: derivePerpsCloseReconciliation(closeOrder14Receipt, {
      preExecutionPositionMarginUsdc: 4_500_000_000n,
    })!,
  },
}
