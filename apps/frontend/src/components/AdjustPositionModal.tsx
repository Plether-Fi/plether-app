import { useState, useEffect } from 'react'
import { useAccount } from 'wagmi'
import { parseUnits, formatUnits } from 'viem'
import { Modal } from './ui'
import { useAdjustCollateral } from '../hooks'
import { useUserDashboard } from '../api'
import { formatUsd } from '../utils/formatters'
import { getDeadline } from '../utils/deadline'
import type { LeveragePosition } from '../types'

export interface AdjustPositionModalProps {
  isOpen: boolean
  onClose: () => void
  position: LeveragePosition
  collateralShares: bigint
  onSuccess?: () => void
}

export function AdjustPositionModal({ isOpen, onClose, position, collateralShares, onSuccess }: AdjustPositionModalProps) {
  const { address } = useAccount()

  const [action, setAction] = useState<'add' | 'remove'>('add')
  const [amount, setAmount] = useState('')

  const { data: dashboardData } = useUserDashboard(address)
  const usdcBalance = dashboardData ? BigInt(dashboardData.data.balances.usdc) : 0n

  const { addCollateral, removeCollateral, isPending, isSuccess, reset } = useAdjustCollateral(position.side, onSuccess)

  // For add: amount in USDC (6 decimals)
  // For remove: amount in tokens (18 decimals), then convert to shares (* 1000)
  const getAmountBigInt = () => {
    if (!amount) return 0n
    if (action === 'add') {
      return parseUnits(amount, 6)
    } else {
      return parseUnits(amount, 18) * 1000n
    }
  }

  const amountBigInt = getAmountBigInt()

  const insufficientBalance = action === 'add'
    ? amountBigInt > usdcBalance
    : amountBigInt > collateralShares

  const collateralTokens = collateralShares ? collateralShares / 1000n : 0n
  const formattedCollateral = formatUnits(collateralTokens, 18)

  useEffect(() => {
    if (isSuccess) {
      setAmount('')
      reset()
      onClose()
    }
  }, [isSuccess, reset, onClose])

  const handleConfirm = () => {
    if (!amountBigInt || amountBigInt <= 0n) return

    const maxSlippageBps = 100n
    const deadline = getDeadline(60)

    if (action === 'add') {
      onClose()
      void addCollateral(amountBigInt, maxSlippageBps)
    } else {
      setAmount('')
      onClose()
      void removeCollateral(amountBigInt, maxSlippageBps, deadline)
    }
  }

  const getButtonText = () => {
    if (isPending) return action === 'add' ? 'Adding...' : 'Removing...'
    if (insufficientBalance) return action === 'add' ? 'Insufficient USDC' : 'Insufficient Collateral'
    return `Confirm ${action === 'add' ? 'Add' : 'Remove'}`
  }

  const tokenSymbol = position.side === 'BEAR' ? 'plDXY-BEAR' : 'plDXY-BULL'
  const isDisabled = !amount || parseFloat(amount) <= 0 || isPending || insufficientBalance

  return (
    <Modal isOpen={isOpen} onClose={onClose} title={`Adjust ${position.side} Position`}>
      <div className="space-y-4">
        <div className="flex gap-2">
          <button
            onClick={() => { setAction('add'); setAmount('') }}
            className={`flex-1 py-2 px-3 text-sm font-medium transition-colors ${
              action === 'add'
                ? 'bg-cyber-neon-green/20 text-cyber-neon-green border border-cyber-neon-green/50'
                : 'bg-cyber-surface-light text-cyber-text-secondary border border-cyber-border-glow/30 hover:text-cyber-bright-blue'
            }`}
          >
            Add Collateral
          </button>
          <button
            onClick={() => { setAction('remove'); setAmount('') }}
            className={`flex-1 py-2 px-3 text-sm font-medium transition-colors ${
              action === 'remove'
                ? 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia border border-cyber-electric-fuchsia/50'
                : 'bg-cyber-surface-light text-cyber-text-secondary border border-cyber-border-glow/30 hover:text-cyber-bright-blue'
            }`}
          >
            Remove
          </button>
        </div>

        <div>
          {action === 'add' && (
            <div className="flex justify-between text-sm mb-2">
              <span className="text-cyber-text-secondary">Available USDC</span>
              <span className="text-cyber-text-primary">{formatUsd(usdcBalance)} USDC</span>
            </div>
          )}
          {action === 'remove' && (
            <div className="flex justify-between text-sm mb-2">
              <span className="text-cyber-text-secondary">Available Collateral</span>
              <span className="text-cyber-text-primary">
                {parseFloat(formattedCollateral).toFixed(2)} {tokenSymbol}
              </span>
            </div>
          )}
          <div className="relative">
            <input
              type="number"
              value={amount}
              onChange={(e) => { setAmount(e.target.value); }}
              placeholder="0.00"
              className="w-full bg-cyber-surface-light border border-cyber-border-glow/30 py-3 pl-4 pr-24 text-lg font-medium text-cyber-text-primary focus:ring-1 focus:ring-cyber-bright-blue focus:border-cyber-bright-blue outline-none"
            />
            <div className="absolute right-4 top-1/2 -translate-y-1/2">
              <span className="font-medium text-cyber-text-secondary">
                {action === 'add' ? 'USDC' : tokenSymbol.replace('plDXY-', '')}
              </span>
            </div>
          </div>
        </div>

        <div className="bg-cyber-surface-light p-3 space-y-2 text-sm border border-cyber-border-glow/30">
          <div className="flex justify-between">
            <span className="text-cyber-text-secondary">Current Health Factor</span>
            <span className="text-cyber-neon-green">{position.healthFactor.toFixed(2)}</span>
          </div>
          <div className="flex justify-between">
            <span className="text-cyber-text-secondary">Current Liquidation Price</span>
            <span className="text-cyber-warning-text">{(Number(position.liquidationPrice) / 1e6).toFixed(2)} USDC</span>
          </div>
        </div>

        <button
          onClick={handleConfirm}
          disabled={isDisabled}
          className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-3 px-6 shadow-lg shadow-cyber-neon-green/40 transition-all disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {getButtonText()}
        </button>
      </div>
    </Modal>
  )
}
