import { useState } from 'react'
import { TokenIcon, OutputDisplay } from './ui'
import { TokenInput } from './TokenInput'
import { formatAmount } from '../utils/formatters'
import { parseStakingAmount, getStakingDecimals, SHARE_DECIMALS } from '../utils/staking'
import { useProtocolStatus } from '../api'
import { useTransactionStore } from '../stores/transactionStore'
import { transactionManager } from '../services/transactionManager'

type StakeMode = 'stake' | 'unstake'

export interface StakingCardProps {
  side: 'BEAR' | 'BULL'
  tokenBalance: bigint
  stakedBalance: bigint
  onSuccess?: () => void
}

export function StakingCard({ side, tokenBalance, stakedBalance, onSuccess }: StakingCardProps) {
  const txStore = useTransactionStore()
  const [mode, setMode] = useState<StakeMode>('stake')
  const [amount, setAmount] = useState('')

  const stakeOpKey = `stake-${side}`
  const unstakeOpKey = `unstake-${side}`
  const stakeTransactionId = txStore.activeOperations[stakeOpKey]
  const unstakeTransactionId = txStore.activeOperations[unstakeOpKey]

  const stakeTx = stakeTransactionId
    ? txStore.transactions.find(t => t.id === stakeTransactionId)
    : null
  const unstakeTx = unstakeTransactionId
    ? txStore.transactions.find(t => t.id === unstakeTransactionId)
    : null

  const isStakePending = stakeTx?.status === 'pending' || stakeTx?.status === 'confirming'
  const isUnstakePending = unstakeTx?.status === 'pending' || unstakeTx?.status === 'confirming'

  const { data: protocolData } = useProtocolStatus()
  const stakingStats = protocolData?.data.staking[side === 'BEAR' ? 'bear' : 'bull']
  const totalAssets = stakingStats ? BigInt(stakingStats.totalAssets) : 0n
  const totalShares = stakingStats ? BigInt(stakingStats.totalShares) : 0n

  const decimals = getStakingDecimals(mode)
  const amountBigInt = parseStakingAmount(amount, mode)

  const previewShares = mode === 'stake' && amountBigInt > 0n && totalAssets > 0n
    ? (amountBigInt * totalShares) / totalAssets
    : 0n
  const previewAssets = mode === 'unstake' && amountBigInt > 0n && totalShares > 0n
    ? (amountBigInt * totalAssets) / totalShares
    : 0n

  const handleStake = () => {
    void transactionManager.executeStake(side, amountBigInt, {
      onRetry: handleStake,
    }).then(() => {
      onSuccess?.()
      setAmount('')
    })
  }

  const handleUnstake = () => {
    void transactionManager.executeUnstake(side, amountBigInt, {
      onRetry: handleUnstake,
    }).then(() => {
      onSuccess?.()
      setAmount('')
    })
  }

  const handleAction = mode === 'stake' ? handleStake : handleUnstake

  const isBear = side === 'BEAR'
  const textColor = isBear ? 'text-brand-orange' : 'text-positive'
  const bgColor = isBear ? 'bg-brand-orange' : 'bg-positive'
  const activeTextColor = isBear ? 'text-content-primary' : 'text-app-bg'
  const activeBorderColor = isBear ? 'border-brand-orange/50' : 'border-positive/50'

  const balance = mode === 'stake' ? tokenBalance : stakedBalance
  const insufficientBalance = amountBigInt > balance

  const getButtonText = () => {
    if (mode === 'stake') {
      if (isStakePending) return 'Staking...'
      if (insufficientBalance) return 'Insufficient Balance'
      return `Stake plDXY-${side}`
    } else {
      if (isUnstakePending) return 'Unstaking...'
      if (insufficientBalance) return 'Insufficient Balance'
      return `Unstake splDXY-${side}`
    }
  }

  const isDisabled = !amount || parseFloat(amount) <= 0 ||
    isStakePending || isUnstakePending || insufficientBalance

  return (
    <div className="min-w-0 overflow-hidden border border-brand-border/30 bg-surface-panel">
      <div className={`border-b border-brand-border/30 px-4 py-4 sm:px-6 ${isBear ? 'bg-brand-orange/10' : 'bg-positive/10'}`}>
        <div className="flex min-w-0 items-center gap-3">
          <span className="shrink-0">
            <TokenIcon side={side} />
          </span>
          <div className="min-w-0">
            <h3 className={`break-words font-semibold ${textColor}`}>plDXY-{side} Staking</h3>
            <p className="text-xs text-content-secondary">Stake to use as collateral</p>
          </div>
        </div>
      </div>

      <div className="min-w-0 space-y-5 p-4 sm:space-y-6 sm:p-6">
        <div className={`min-w-0 border bg-surface-muted p-3 sm:p-4 ${isBear ? 'border-brand-orange/30' : 'border-positive/30'}`}>
          <div className="flex min-w-0 flex-col items-start gap-1 sm:flex-row sm:items-center sm:justify-between sm:gap-3">
            <span className="shrink-0 text-sm text-content-secondary">Staked Balance</span>
            <span className={`min-w-0 break-words font-semibold [overflow-wrap:anywhere] sm:text-right ${textColor}`}>
              {formatAmount(stakedBalance, SHARE_DECIMALS)} splDXY-{side}
            </span>
          </div>
        </div>

        <div className="bg-surface-muted p-1 flex text-sm font-medium border border-brand-border/30">
          <button
            onClick={() => { setMode('stake'); setAmount('') }}
            className={`min-h-11 min-w-0 flex-1 px-3 py-2 transition-colors hover:underline hover:underline-offset-4 sm:px-4 ${
              mode === 'stake'
                ? `${bgColor} ${activeTextColor} border ${activeBorderColor}`
                : 'text-content-secondary hover:bg-[#3B212D] hover:text-[#FFAB96]'
            }`}
          >
            Stake
          </button>
          <button
            onClick={() => { setMode('unstake'); setAmount('') }}
            className={`min-h-11 min-w-0 flex-1 px-3 py-2 transition-colors hover:underline hover:underline-offset-4 sm:px-4 ${
              mode === 'unstake'
                ? `${bgColor} ${activeTextColor} border ${activeBorderColor}`
                : 'text-content-secondary hover:bg-[#3B212D] hover:text-[#FFAB96]'
            }`}
          >
            Unstake
          </button>
        </div>

        <TokenInput
          label={mode === 'stake' ? `plDXY-${side} to stake` : `splDXY-${side} to unstake`}
          value={amount}
          onChange={setAmount}
          token={{ symbol: mode === 'stake' ? `plDXY-${side}` : `splDXY-${side}`, decimals }}
          balance={balance}
        />

        <OutputDisplay
          label="You will receive"
          value={mode === 'stake'
            ? formatAmount(previewShares, SHARE_DECIMALS)
            : formatAmount(previewAssets, 18)
          }
          token={mode === 'stake' ? `splDXY-${side}` : `plDXY-${side}`}
          variant={side}
        />

        <button
          onClick={() => { handleAction() }}
          disabled={isDisabled}
          className={`min-h-12 w-full ${bgColor} ${isBear ? 'text-content-primary enabled:hover:bg-[#FF572D]' : 'text-app-bg enabled:hover:bg-[#00CC77]'} px-4 py-3 text-base font-semibold transition-colors enabled:hover:underline enabled:hover:underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50 sm:px-6 sm:py-4 sm:text-lg`}
        >
          {getButtonText()}
        </button>
      </div>
    </div>
  )
}
