import { useState } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { Button, Modal, Input } from '../components/ui'

const meta: Meta = {
  title: 'Components/SlippageSelector',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

const SLIPPAGE_PRESETS = [0.1, 0.5, 1.0]
const MAX_SLIPPAGE = 1.0

function MockSlippageSelector() {
  const [slippage, setSlippage] = useState(0.5)
  const [isOpen, setIsOpen] = useState(false)
  const [customValue, setCustomValue] = useState('')

  const handlePresetClick = (value: number) => {
    setSlippage(value)
    setCustomValue('')
  }

  const handleCustomChange = (value: string) => {
    setCustomValue(value)
    const num = parseFloat(value)
    if (!isNaN(num) && num > 0 && num <= MAX_SLIPPAGE) {
      setSlippage(num)
    }
  }

  const isCustom = !SLIPPAGE_PRESETS.includes(slippage)

  return (
    <>
      <button
        onClick={() => setIsOpen(true)}
        className="flex items-center gap-1 text-sm text-cyber-text-secondary hover:text-cyber-text-primary transition-colors"
      >
        <span className="material-symbols-outlined text-lg">settings</span>
        <span>{slippage}% slippage</span>
      </button>

      <Modal
        isOpen={isOpen}
        onClose={() => setIsOpen(false)}
        title="Slippage Settings"
        size="sm"
      >
        <div className="space-y-4">
          <p className="text-sm text-cyber-text-secondary">
            Your transaction will revert if the price changes unfavorably by more
            than this percentage.
          </p>

          <div className="flex gap-2">
            {SLIPPAGE_PRESETS.map((preset) => (
              <Button
                key={preset}
                variant={slippage === preset ? 'primary' : 'secondary'}
                size="sm"
                onClick={() => handlePresetClick(preset)}
                className="flex-1"
              >
                {preset}%
              </Button>
            ))}
          </div>

          <div>
            <label className="block text-sm text-cyber-text-secondary mb-1">
              Custom (max {MAX_SLIPPAGE}%)
            </label>
            <Input
              type="text"
              inputMode="decimal"
              value={customValue}
              onChange={(e) => handleCustomChange(e.target.value)}
              placeholder="0.5"
              rightElement={<span className="text-cyber-text-secondary">%</span>}
              className={isCustom ? 'ring-2 ring-cyber-neon-green' : ''}
            />
          </div>

          <Button
            variant="primary"
            onClick={() => setIsOpen(false)}
            className="w-full"
          >
            Save
          </Button>
        </div>
      </Modal>
    </>
  )
}

export const Default: Story = {
  render: () => <MockSlippageSelector />,
}

export const InContext: Story = {
  render: () => (
    <div className="bg-cyber-surface-dark p-4 border border-cyber-border-glow/30 max-w-md">
      <div className="flex items-center justify-between mb-4">
        <span className="text-cyber-text-primary font-medium">Swap Settings</span>
        <MockSlippageSelector />
      </div>
      <p className="text-sm text-cyber-text-secondary">
        Click the settings button to adjust slippage tolerance.
      </p>
    </div>
  ),
}
