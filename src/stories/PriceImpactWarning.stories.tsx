import type { Meta, StoryObj } from '@storybook/react-vite'
import { useState } from 'react'
import { Modal, Button } from '../components/ui'

interface PriceImpactWarningArgs {
  priceImpact: number
  maxPriceImpact: number
}

function PriceImpactWarningModal({
  priceImpact,
  maxPriceImpact,
  isOpen,
  onClose,
  onConfirm,
}: PriceImpactWarningArgs & {
  isOpen: boolean
  onClose: () => void
  onConfirm: () => void
}) {
  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
      title="High Price Impact"
      size="sm"
    >
      <div className="space-y-4">
        <div className="bg-red-500/10 border border-red-500/30 p-4 text-center">
          <div className="text-3xl font-bold text-red-500">{priceImpact.toFixed(2)}%</div>
          <div className="text-sm text-cyber-text-secondary mt-1">Price Impact</div>
        </div>

        <p className="text-sm text-cyber-text-secondary">
          This trade has a price impact of <span className="text-red-500 font-medium">{priceImpact.toFixed(2)}%</span>,
          which exceeds your maximum threshold of <span className="text-cyber-text-primary font-medium">{maxPriceImpact}%</span>.
        </p>

        <p className="text-sm text-cyber-text-secondary">
          You will receive significantly less value than your input. Are you sure you want to proceed?
        </p>

        <div className="flex gap-3">
          <Button
            variant="secondary"
            onClick={onClose}
            className="flex-1"
          >
            Cancel
          </Button>
          <Button
            variant="primary"
            onClick={onConfirm}
            className="flex-1 !bg-red-500 hover:!bg-red-600"
          >
            Swap Anyway
          </Button>
        </div>
      </div>
    </Modal>
  )
}

function PriceImpactWarningDemo(args: PriceImpactWarningArgs) {
  const [isOpen, setIsOpen] = useState(true)

  return (
    <div className="p-8">
      <Button onClick={() => setIsOpen(true)}>
        Show Warning Modal
      </Button>
      <PriceImpactWarningModal
        {...args}
        isOpen={isOpen}
        onClose={() => setIsOpen(false)}
        onConfirm={() => {
          setIsOpen(false)
          alert('User confirmed high impact trade')
        }}
      />
    </div>
  )
}

const meta: Meta<PriceImpactWarningArgs> = {
  title: 'Components/PriceImpactWarning',
  component: PriceImpactWarningDemo,
  tags: ['autodocs'],
  argTypes: {
    priceImpact: {
      control: { type: 'number', min: 0, max: 100, step: 0.1 },
      description: 'Actual price impact of the trade',
    },
    maxPriceImpact: {
      control: { type: 'number', min: 0, max: 10, step: 0.5 },
      description: 'User configured max price impact threshold',
    },
  },
}

export default meta
type Story = StoryObj<PriceImpactWarningArgs>

export const ModeratePriceImpact: Story = {
  args: {
    priceImpact: 3.5,
    maxPriceImpact: 2.0,
  },
}

export const HighPriceImpact: Story = {
  args: {
    priceImpact: 8.2,
    maxPriceImpact: 2.0,
  },
}

export const ExtremePriceImpact: Story = {
  args: {
    priceImpact: 15.7,
    maxPriceImpact: 2.0,
  },
}
