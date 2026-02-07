import { useState, useEffect } from 'react'
import { useAccount } from 'wagmi'
import { parseUnits, formatUnits } from 'viem'
import { Modal } from './ui'
import { useAdjustCollateral, useApprovalFlow } from '../hooks'
import { useUserDashboard } from '../api'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
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
  const { address, chainId } = useAccount()
  const addresses = getAddresses(chainId ?? DEFAULT_CHAIN_ID)

  const [action, setAction] = useState<'add' | 'remove'>('add')
  const [amount, setAmount] = useState('')
  const [isHidden, setIsHidden] = useState(false)

  const { data: dashboardData } = useUserDashboard(address)
  const usdcBalance = dashboardData ? BigInt(dashboardData.data.balances.usdc) : 0n

  const routerAddress = position.side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER
  const { addCollateral, removeCollateral, isPending, isSuccess, reset } = useAdjustCollateral(position.side, onSuccess)

  const {
    execute: executeWithApproval,
    needsApproval,
    isApproving,
    approvePending,
    isPending: approvalFlowPending,
  } = useApprovalFlow({
    tokenAddress: addresses.USDC,
    spenderAddress: routerAddress,
    actionTitle: 'Adding collateral',
    actionStepLabel: 'Add collateral',
  })

  // For add: amount in USDC (6 decimals)
  // For remove: amount in tokens (18 decimals), then convert to shares (* 1000)
  const getAmountBigInt = () => {
    if (!amount) return 0n
    if (action === 'add') {
      return parseUnits(amount, 6)
    } else {
      // User enters token amount, convert to staked shares (1000x offset)
      return parseUnits(amount, 18) * 1000n
    }
  }

  const amountBigInt = getAmountBigInt()

  // For add: check against USDC balance
  // For remove: check against collateral shares
  const insufficientBalance = action === 'add'
    ? amountBigInt > usdcBalance
    : amountBigInt > collateralShares

  // Format collateral shares to token amount for display (divide by 1000 offset, then format as 18 decimals)
  const collateralTokens = collateralShares ? collateralShares / 1000n : 0n
  const formattedCollateral = formatUnits(collateralTokens, 18)

  // Reset hidden state when modal opens
  useEffect(() => {
    if (isOpen) {
      setIsHidden(false)
    }
  }, [isOpen])

  // Close modal when approval flow completes without success (e.g., user rejected)
  useEffect(() => {
    if (isHidden && !approvalFlowPending && !isPending && !isSuccess) {
      setIsHidden(false)
      onClose()
    }
  }, [isHidden, approvalFlowPending, isPending, isSuccess, onClose])

  // Close modal when transaction succeeds
  useEffect(() => {
    if (isSuccess) {
      setAmount('')
      setIsHidden(false)
      reset()
      onClose()
    }
  }, [isSuccess, reset, onClose])

  const handleConfirm = () => {
    if (!amountBigInt || amountBigInt <= 0n) return

    const maxSlippageBps = 100n // 1% slippage
    const deadline = getDeadline(60)

    if (action === 'add') {
      // Hide modal but keep mounted - approval flow needs component to stay alive
      // useEffect will fully close when isSuccess becomes true
      setIsHidden(true)
      const addWithSlippage = (usdcAmount: bigint, txContext: { txId: string; stepIndex: number }) =>
        addCollateral(usdcAmount, maxSlippageBps, deadline, txContext)
      void executeWithApproval(amountBigInt, addWithSlippage)
    } else {
      // Remove doesn't need approval, close immediately
      setAmount('')
      onClose()
      void removeCollateral(amountBigInt, maxSlippageBps, deadline)
    }
  }

  const getButtonText = () => {
    if (isPending) return action === 'add' ? 'Adding...' : 'Removing...'
    if (approvePending) return 'Approving USDC...'
    if (insufficientBalance) return action === 'add' ? 'Insufficient USDC' : 'Insufficient Collateral'
    if (action === 'add' && needsApproval(amountBigInt)) return 'Approve USDC'
    return `Confirm ${action === 'add' ? 'Add' : 'Remove'}`
  }

  const tokenSymbol = position.side === 'BEAR' ? 'plDXY-BEAR' : 'plDXY-BULL'
  const isActionPending = isPending || isApproving
  const isDisabled = !amount || parseFloat(amount) <= 0 || isActionPending || insufficientBalance

  return (
    <Modal isOpen={isOpen && !isHidden} onClose={onClose} title={`Adjust ${position.side} Position`}>
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
