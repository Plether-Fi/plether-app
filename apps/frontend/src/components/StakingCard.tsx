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
  const textColor = isBear ? 'text-cyber-electric-fuchsia' : 'text-cyber-neon-green'
  const bgColor = isBear ? 'bg-cyber-electric-fuchsia' : 'bg-cyber-neon-green'
  const shadowColor = isBear ? 'shadow-cyber-electric-fuchsia' : 'shadow-cyber-neon-green'

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
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg overflow-hidden">
      <div className={`px-6 py-4 border-b border-cyber-border-glow/30 ${isBear ? 'bg-cyber-electric-fuchsia/10' : 'bg-cyber-neon-green/10'}`}>
        <div className="flex items-center gap-3">
          <TokenIcon side={side} />
          <div>
            <h3 className={`font-semibold ${textColor}`}>plDXY-{side} Staking</h3>
            <p className="text-xs text-cyber-text-secondary">Stake to use as collateral</p>
          </div>
        </div>
      </div>

      <div className="p-6 space-y-6">
        <div className={`bg-cyber-surface-light p-4 border ${isBear ? 'border-cyber-electric-fuchsia/30' : 'border-cyber-neon-green/30'}`}>
          <div className="flex justify-between items-center">
            <span className="text-cyber-text-secondary text-sm">Staked Balance</span>
            <span className={`${textColor} font-semibold`}>
              {formatAmount(stakedBalance, SHARE_DECIMALS)} splDXY-{side}
            </span>
          </div>
        </div>

        <div className="bg-cyber-surface-light p-1 flex text-sm font-medium border border-cyber-border-glow/30">
          <button
            onClick={() => { setMode('stake'); setAmount('') }}
            className={`flex-1 py-2 px-4 transition-all cursor-pointer ${
              mode === 'stake'
                ? `bg-cyber-surface-dark ${textColor} shadow-sm ${shadowColor}/10 border border-${isBear ? 'cyber-electric-fuchsia' : 'cyber-neon-green'}/50`
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
            }`}
          >
            Stake
          </button>
          <button
            onClick={() => { setMode('unstake'); setAmount('') }}
            className={`flex-1 py-2 px-4 transition-all cursor-pointer ${
              mode === 'unstake'
                ? `bg-cyber-surface-dark ${textColor} shadow-sm ${shadowColor}/10 border border-${isBear ? 'cyber-electric-fuchsia' : 'cyber-neon-green'}/50`
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
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
          className={`w-full ${bgColor} hover:opacity-90 ${isBear ? 'text-cyber-text-primary' : 'text-cyber-bg'} font-semibold py-4 px-6 shadow-lg ${shadowColor}/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none`}
        >
          {getButtonText()}
        </button>
      </div>
    </div>
  )
}
