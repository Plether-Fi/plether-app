import { useState, useEffect, useCallback } from 'react'
import { useAccount } from 'wagmi'
import { parseUnits } from 'viem'
import { Modal } from './ui'
import { useAdjustCollateral, useApprovalFlow, useTokenBalances } from '../hooks'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
import { formatAmount } from '../utils/formatters'
import type { LeveragePosition } from '../types'

export interface AdjustPositionModalProps {
  isOpen: boolean
  onClose: () => void
  position: LeveragePosition
  onSuccess?: () => void
}

export function AdjustPositionModal({ isOpen, onClose, position, onSuccess }: AdjustPositionModalProps) {
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? DEFAULT_CHAIN_ID)

  const [action, setAction] = useState<'add' | 'remove'>('add')
  const [amount, setAmount] = useState('')

  const { usdcBalance } = useTokenBalances()
  const amountBigInt = amount ? parseUnits(amount, 6) : 0n

  const routerAddress = position.side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER
  const { addCollateral, removeCollateral, isPending, isSuccess, reset } = useAdjustCollateral(position.side)

  const {
    execute: executeWithApproval,
    needsApproval,
    isApproving,
    approvePending,
  } = useApprovalFlow({
    tokenAddress: addresses.USDC,
    spenderAddress: routerAddress,
  })

  const insufficientBalance = action === 'add' && amountBigInt > usdcBalance

  useEffect(() => {
    if (isSuccess) {
      onSuccess?.()
      setAmount('')
      reset()
      onClose()
    }
  }, [isSuccess, onSuccess, reset, onClose])

  const executeAddCollateral = useCallback(async (amt: bigint) => {
    await addCollateral(amt)
  }, [addCollateral])

  const handleConfirm = async () => {
    if (!amountBigInt || amountBigInt <= 0n) return

    if (action === 'add') {
      await executeWithApproval(amountBigInt, executeAddCollateral)
    } else {
      await removeCollateral(amountBigInt)
    }
  }

  const getButtonText = () => {
    if (isPending) return action === 'add' ? 'Adding...' : 'Removing...'
    if (approvePending) return 'Approving USDC...'
    if (insufficientBalance) return 'Insufficient USDC'
    if (action === 'add' && needsApproval(amountBigInt)) return 'Approve USDC'
    return `Confirm ${action === 'add' ? 'Add' : 'Remove'}`
  }

  const isActionPending = isPending || isApproving
  const isDisabled = !amount || parseFloat(amount) <= 0 || isActionPending || insufficientBalance

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
              <span className="text-cyber-text-primary">{formatAmount(usdcBalance, 6)}</span>
            </div>
          )}
          {action === 'remove' && (
            <div className="flex justify-between text-sm mb-2">
              <span className="text-cyber-text-secondary">Current Collateral</span>
              <span className="text-cyber-text-primary">{formatAmount(position.collateral, 6)}</span>
            </div>
          )}
          <div className="relative">
            <input
              type="number"
              value={amount}
              onChange={(e) => { setAmount(e.target.value); }}
              placeholder="0.00"
              className="w-full bg-cyber-surface-light border border-cyber-border-glow/30 py-3 pl-4 pr-20 text-lg font-medium text-cyber-text-primary focus:ring-1 focus:ring-cyber-bright-blue focus:border-cyber-bright-blue outline-none"
            />
            <div className="absolute right-4 top-1/2 -translate-y-1/2">
              <span className="font-medium text-cyber-text-secondary">USDC</span>
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
          onClick={() => void handleConfirm()}
          disabled={isDisabled}
          className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-3 px-6 shadow-lg shadow-cyber-neon-green/40 transition-all disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {getButtonText()}
        </button>
      </div>
    </Modal>
  )
}
