import { useState } from 'react'
import { Modal } from './ui'
import type { LeveragePosition } from '../types'

export interface AdjustPositionModalProps {
  isOpen: boolean
  onClose: () => void
  position: LeveragePosition
}

export function AdjustPositionModal({ isOpen, onClose, position }: AdjustPositionModalProps) {
  const [action, setAction] = useState<'add' | 'remove' | 'adjust'>('add')
  const [amount, setAmount] = useState('')

  return (
    <Modal isOpen={isOpen} onClose={onClose} title={`Adjust ${position.side} Position`}>
      <div className="space-y-4">
        <div className="flex gap-2">
          <button
            onClick={() => setAction('add')}
            className={`flex-1 py-2 px-3 text-sm font-medium transition-colors ${
              action === 'add'
                ? 'bg-cyber-neon-green/20 text-cyber-neon-green border border-cyber-neon-green/50'
                : 'bg-cyber-surface-light text-cyber-text-secondary border border-cyber-border-glow/30 hover:text-cyber-bright-blue'
            }`}
          >
            Add Collateral
          </button>
          <button
            onClick={() => setAction('remove')}
            className={`flex-1 py-2 px-3 text-sm font-medium transition-colors ${
              action === 'remove'
                ? 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia border border-cyber-electric-fuchsia/50'
                : 'bg-cyber-surface-light text-cyber-text-secondary border border-cyber-border-glow/30 hover:text-cyber-bright-blue'
            }`}
          >
            Remove
          </button>
          <button
            onClick={() => setAction('adjust')}
            className={`flex-1 py-2 px-3 text-sm font-medium transition-colors ${
              action === 'adjust'
                ? 'bg-cyber-bright-blue/20 text-cyber-bright-blue border border-cyber-bright-blue/50'
                : 'bg-cyber-surface-light text-cyber-text-secondary border border-cyber-border-glow/30 hover:text-cyber-bright-blue'
            }`}
          >
            Adjust Leverage
          </button>
        </div>

        {(action === 'add' || action === 'remove') && (
          <div className="relative">
            <input
              type="number"
              value={amount}
              onChange={(e) => setAmount(e.target.value)}
              placeholder="0.00"
              className="w-full bg-cyber-surface-light border border-cyber-border-glow/30 py-3 pl-4 pr-20 text-lg font-medium text-cyber-text-primary focus:ring-1 focus:ring-cyber-bright-blue focus:border-cyber-bright-blue outline-none"
            />
            <div className="absolute right-4 top-1/2 -translate-y-1/2">
              <span className="font-medium text-cyber-text-secondary">USDC</span>
            </div>
          </div>
        )}

        {action === 'adjust' && (
          <div>
            <label className="block text-sm text-cyber-text-secondary mb-2">New Leverage</label>
            <input
              type="range"
              min="1.1"
              max="5"
              step="0.1"
              defaultValue={position.leverage}
              className="w-full h-2 bg-cyber-surface-light appearance-none cursor-pointer accent-cyber-bright-blue"
            />
          </div>
        )}

        <div className="bg-cyber-surface-light p-3 space-y-2 text-sm border border-cyber-border-glow/30">
          <div className="flex justify-between">
            <span className="text-cyber-text-secondary">New Liquidation Price</span>
            <span className="text-cyber-warning-text">$92.50</span>
          </div>
          <div className="flex justify-between">
            <span className="text-cyber-text-secondary">New Health Factor</span>
            <span className="text-cyber-neon-green">2.1</span>
          </div>
        </div>

        <button className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-3 px-6 shadow-lg shadow-cyber-neon-green/40 transition-all">
          Confirm {action === 'add' ? 'Add' : action === 'remove' ? 'Remove' : 'Adjust'}
        </button>
      </div>
    </Modal>
  )
}
