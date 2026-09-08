import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsReviewFooter } from '../components/PerpsReviewFooter'
import { Modal } from '../components/ui'

const meta: Meta<typeof PerpsReviewFooter> = {
  title: 'Perps/Commit Preview Preparation',
  component: PerpsReviewFooter,
  parameters: { layout: 'fullscreen' },
  args: {
    preparing: false, refreshing: false, slow: false, changes: [], canConfirm: true, direction: 'long',
    onConfirm: () => undefined, onCancel: () => undefined, onRetry: () => undefined,
  },
  decorators: [(Story) => (
    <Modal isOpen onClose={() => undefined} title="Commit Preview" size="lg" initialFocus="dialog" footer={<Story />}>
      <div tabIndex={0} aria-label="Reviewed order details" className="space-y-5 text-content-primary">
        <p className="text-xl font-semibold">Open a Long plDXY Perp position</p>
        <dl className="grid grid-cols-2 gap-4 border border-brand-border/20 bg-app-bg p-4 text-sm">
          <dt>Order quantity</dt><dd className="text-right">1 200 plDXY</dd>
          <dt>Order exposure</dt><dd className="text-right">1 208.00 USDC</dd>
          <dt>Execution limit</dt><dd className="text-right">1.001 USDC</dd>
          <dt>Required funding</dt><dd className="text-right">242.48 USDC</dd>
          <dt>Highest reviewed leverage</dt><dd className="text-right">5×</dd>
        </dl>
      </div>
    </Modal>
  )],
}
export default meta
type Story = StoryObj<typeof meta>
export const Ready: Story = {}
export const Checking: Story = { args: { preparing: true, canConfirm: false } }
export const Slow: Story = { args: { preparing: true, slow: true, canConfirm: false } }
export const Updating: Story = { args: { preparing: true, refreshing: true, canConfirm: false } }
export const Changed: Story = { args: { changes: [
  { label: 'Execution limit', before: '1.00099999 USDC', after: '1.001 USDC' },
  { label: 'Required funding', before: '242.470001 USDC', after: '242.48 USDC' },
] } }
export const Failed: Story = { args: { error: 'Order checks took too long. Retry review.', canConfirm: false } }
