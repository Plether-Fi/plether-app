import { useState } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { AdjustPositionModal } from '../components/AdjustPositionModal'
import type { LeveragePosition } from '../types'

const meta: Meta = {
  title: 'Components/AdjustPositionModal',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

const mockPosition: LeveragePosition = {
  id: '1',
  side: 'BEAR',
  leverage: 3,
  size: BigInt(15000 * 1e6),
  collateral: BigInt(5000 * 1e6),
  pnl: BigInt(1250 * 1e6),
  pnlPercentage: 25,
  entryPrice: BigInt(103 * 1e6),
  liquidationPrice: BigInt(115 * 1e6),
  healthFactor: 2.1,
}

const bullPosition: LeveragePosition = {
  ...mockPosition,
  id: '2',
  side: 'BULL',
}

function ModalWrapper({ position }: { position: LeveragePosition }) {
  const [isOpen, setIsOpen] = useState(true)
  return (
    <>
      <button
        onClick={() => setIsOpen(true)}
        className="px-4 py-2 bg-cyber-surface-light text-cyber-text-primary border border-cyber-border-glow/30"
      >
        Open Modal
      </button>
      <AdjustPositionModal isOpen={isOpen} onClose={() => setIsOpen(false)} position={position} collateralShares={BigInt(5000 * 1e18)} />
    </>
  )
}

export const BearPosition: Story = {
  render: () => <ModalWrapper position={mockPosition} />,
}

export const BullPosition: Story = {
  render: () => <ModalWrapper position={bullPosition} />,
}
