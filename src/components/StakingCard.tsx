import { useState, useEffect, useRef } from 'react'
import { TokenIcon, OutputDisplay } from './ui'
import { TokenInput } from './TokenInput'
import { formatAmount } from '../utils/formatters'
import { parseStakingAmount, getStakingDecimals, SHARE_DECIMALS } from '../utils/staking'
import { parseTransactionError } from '../utils/errors'
import { useStakeWithPermit, useUnstake, useStakedBalance, usePreviewDeposit, usePreviewRedeem } from '../hooks/useStaking'
import { useTransactionModal } from '../hooks'

type StakeMode = 'stake' | 'unstake'

export interface StakingCardProps {
  side: 'BEAR' | 'BULL'
  tokenBalance: bigint
}

export function StakingCard({ side, tokenBalance }: StakingCardProps) {
  const txModal = useTransactionModal()
  const [mode, setMode] = useState<StakeMode>('stake')
  const [amount, setAmount] = useState('')
  const stakeTriggeredRef = useRef(false)
  const unstakeTriggeredRef = useRef(false)

  const { shares: stakedBalance, refetch: refetchStaked } = useStakedBalance(side)
  const {
    stakeWithPermit,
    isPending: stakePending,
    isConfirming: stakeConfirming,
    isSigningPermit,
    isSuccess: stakeSuccess,
    error: stakeError,
    permitCompleted,
    reset: resetStake,
    hash: stakeHash,
  } = useStakeWithPermit(side)
  const {
    unstake,
    isPending: unstakePending,
    isConfirming: unstakeConfirming,
    isSuccess: unstakeSuccess,
    error: unstakeError,
    reset: resetUnstake,
    hash: unstakeHash,
  } = useUnstake(side)

  const decimals = getStakingDecimals(mode)
  const amountBigInt = parseStakingAmount(amount, mode)

  const { shares: previewShares, isLoading: previewDepositLoading } = usePreviewDeposit(side, mode === 'stake' ? amountBigInt : 0n)
  const { assets: previewAssets, isLoading: previewRedeemLoading } = usePreviewRedeem(side, mode === 'unstake' ? amountBigInt : 0n)

  useEffect(() => {
    if (stakeSuccess) {
      if (stakeHash) txModal.setSuccess(stakeHash)
      refetchStaked()
      setAmount('')
      resetStake()
      stakeTriggeredRef.current = false
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [stakeSuccess, stakeHash, refetchStaked, resetStake])

  useEffect(() => {
    if (unstakeSuccess) {
      if (unstakeHash) txModal.setSuccess(unstakeHash)
      refetchStaked()
      setAmount('')
      resetUnstake()
      unstakeTriggeredRef.current = false
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [unstakeSuccess, unstakeHash, refetchStaked, resetUnstake])

  useEffect(() => {
    const modal = useTransactionModal.getState()
    if (!modal.isOpen || !stakeTriggeredRef.current) return
    if (isSigningPermit) {
      modal.setStepInProgress(0)
    } else if (stakePending) {
      modal.setStepInProgress(1)
    } else if (stakeConfirming) {
      modal.setStepInProgress(2)
    } else if (stakeError) {
      const stepIndex = permitCompleted ? 1 : 0
      modal.setError(stepIndex, parseTransactionError(stakeError))
    }
  }, [isSigningPermit, stakePending, stakeConfirming, stakeError, permitCompleted])

  useEffect(() => {
    const modal = useTransactionModal.getState()
    if (!modal.isOpen || !unstakeTriggeredRef.current) return
    if (unstakePending) {
      modal.setStepInProgress(0)
    } else if (unstakeConfirming) {
      modal.setStepInProgress(1)
    } else if (unstakeError) {
      modal.setError(0, parseTransactionError(unstakeError))
    }
  }, [unstakePending, unstakeConfirming, unstakeError])

  const handleStake = async () => {
    stakeTriggeredRef.current = true
    txModal.open({
      title: `Staking DXY-${side}`,
      steps: ['Sign permit', `Stake DXY-${side}`, 'Awaiting confirmation'],
      onRetry: handleStake,
    })
    await stakeWithPermit(amountBigInt)
  }

  const handleUnstake = async () => {
    unstakeTriggeredRef.current = true
    txModal.open({
      title: `Unstaking sDXY-${side}`,
      steps: [`Unstake sDXY-${side}`, 'Awaiting confirmation'],
      onRetry: handleUnstake,
    })
    await unstake(amountBigInt)
  }

  const handleAction = mode === 'stake' ? handleStake : handleUnstake

  const isBear = side === 'BEAR'
  const textColor = isBear ? 'text-cyber-electric-fuchsia' : 'text-cyber-neon-green'
  const bgColor = isBear ? 'bg-cyber-electric-fuchsia' : 'bg-cyber-neon-green'
  const shadowColor = isBear ? 'shadow-cyber-electric-fuchsia' : 'shadow-cyber-neon-green'

  const balance = mode === 'stake' ? tokenBalance : stakedBalance
  const insufficientBalance = amountBigInt > balance

  const getButtonText = () => {
    if (mode === 'stake') {
      if (isSigningPermit) return 'Sign Permit...'
      if (stakePending) return 'Staking...'
      if (insufficientBalance) return 'Insufficient Balance'
      return `Stake DXY-${side}`
    } else {
      if (unstakePending) return 'Unstaking...'
      if (insufficientBalance) return 'Insufficient Balance'
      return `Unstake sDXY-${side}`
    }
  }

  const isDisabled = !amount || parseFloat(amount) <= 0 ||
    stakePending || unstakePending || insufficientBalance

  return (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg overflow-hidden">
      <div className={`px-6 py-4 border-b border-cyber-border-glow/30 ${isBear ? 'bg-cyber-electric-fuchsia/10' : 'bg-cyber-neon-green/10'}`}>
        <div className="flex items-center gap-3">
          <TokenIcon side={side} />
          <div>
            <h3 className={`font-semibold ${textColor}`}>DXY-{side} Staking</h3>
            <p className="text-xs text-cyber-text-secondary">Stake to use as collateral</p>
          </div>
        </div>
      </div>

      <div className="p-6 space-y-6">
        <div className={`bg-cyber-surface-light p-4 border ${isBear ? 'border-cyber-electric-fuchsia/30' : 'border-cyber-neon-green/30'}`}>
          <div className="flex justify-between items-center">
            <span className="text-cyber-text-secondary text-sm">Staked Balance</span>
            <span className={`${textColor} font-semibold`}>
              {formatAmount(stakedBalance, SHARE_DECIMALS)} sDXY-{side}
            </span>
          </div>
        </div>

        <div className="bg-cyber-surface-light p-1 flex text-sm font-medium border border-cyber-border-glow/30">
          <button
            onClick={() => { setMode('stake'); setAmount('') }}
            className={`flex-1 py-2 px-4 transition-all ${
              mode === 'stake'
                ? `bg-cyber-surface-dark ${textColor} shadow-sm ${shadowColor}/10 border border-${isBear ? 'cyber-electric-fuchsia' : 'cyber-neon-green'}/50`
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
            }`}
          >
            Stake
          </button>
          <button
            onClick={() => { setMode('unstake'); setAmount('') }}
            className={`flex-1 py-2 px-4 transition-all ${
              mode === 'unstake'
                ? `bg-cyber-surface-dark ${textColor} shadow-sm ${shadowColor}/10 border border-${isBear ? 'cyber-electric-fuchsia' : 'cyber-neon-green'}/50`
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
            }`}
          >
            Unstake
          </button>
        </div>

        <TokenInput
          label={mode === 'stake' ? `DXY-${side} to stake` : `sDXY-${side} to unstake`}
          value={amount}
          onChange={setAmount}
          token={{ symbol: mode === 'stake' ? `DXY-${side}` : `sDXY-${side}`, decimals }}
          balance={balance}
        />

        <OutputDisplay
          label="You will receive"
          value={mode === 'stake'
            ? (previewDepositLoading ? '...' : formatAmount(previewShares, SHARE_DECIMALS))
            : (previewRedeemLoading ? '...' : formatAmount(previewAssets, 18))
          }
          token={mode === 'stake' ? `sDXY-${side}` : `DXY-${side}`}
          variant={side}
        />

        <button
          onClick={handleAction}
          disabled={isDisabled}
          className={`w-full ${bgColor} hover:opacity-90 ${isBear ? 'text-cyber-text-primary' : 'text-cyber-bg'} font-semibold py-4 px-6 shadow-lg ${shadowColor}/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none`}
        >
          {getButtonText()}
        </button>
      </div>
    </div>
  )
}
