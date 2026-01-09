import { useState, useMemo, useEffect, useRef } from 'react'
import { useAccount } from 'wagmi'
import { parseUnits } from 'viem'
import { TokenIcon } from './ui'
import { TokenInput } from './TokenInput'
import { formatAmount } from '../utils/formatters'
import { useStake, useUnstake, useStakedBalance } from '../hooks/useStaking'
import { useAllowance, useApprove } from '../hooks'
import { getAddresses } from '../contracts/addresses'

type StakeMode = 'stake' | 'unstake'

const ASSET_DECIMALS = 18
const SHARE_DECIMALS = 21 // 18 + 3 offset for inflation attack protection

export interface StakingCardProps {
  side: 'BEAR' | 'BULL'
  tokenBalance: bigint
}

export function StakingCard({ side, tokenBalance }: StakingCardProps) {
  const { chainId } = useAccount()
  const [mode, setMode] = useState<StakeMode>('stake')
  const [amount, setAmount] = useState('')
  const [pendingStake, setPendingStake] = useState(false)
  const pendingAmountRef = useRef<bigint>(0n)
  const approveHandledRef = useRef(false)

  const addresses = getAddresses(chainId ?? 1)
  const tokenAddress = side === 'BEAR' ? addresses.DXY_BEAR : addresses.DXY_BULL
  const stakingAddress = side === 'BEAR' ? addresses.STAKING_BEAR : addresses.STAKING_BULL

  const { shares: stakedBalance, refetch: refetchStaked } = useStakedBalance(side)
  const { stake, isPending: stakePending, isSuccess: stakeSuccess, reset: resetStake } = useStake(side)
  const { unstake, isPending: unstakePending, isSuccess: unstakeSuccess, reset: resetUnstake } = useUnstake(side)
  const { allowance, refetch: refetchAllowance } = useAllowance(tokenAddress, stakingAddress)
  const { approve, isPending: approvePending, isSuccess: approveSuccess } = useApprove(tokenAddress, stakingAddress)

  const decimals = mode === 'stake' ? ASSET_DECIMALS : SHARE_DECIMALS

  const amountBigInt = useMemo(() => {
    if (!amount || isNaN(parseFloat(amount))) return 0n
    try {
      return parseUnits(amount, decimals)
    } catch {
      return 0n
    }
  }, [amount, decimals])

  const needsApproval = mode === 'stake' && amountBigInt > 0n && allowance < amountBigInt

  useEffect(() => {
    if (approveSuccess && !approveHandledRef.current) {
      approveHandledRef.current = true
      refetchAllowance()
      if (pendingStake && pendingAmountRef.current > 0n) {
        stake(pendingAmountRef.current)
        pendingAmountRef.current = 0n
        setPendingStake(false)
      }
    }
  }, [approveSuccess, refetchAllowance, pendingStake, stake])

  useEffect(() => {
    if (stakeSuccess) {
      refetchStaked()
      setAmount('')
      resetStake()
    }
  }, [stakeSuccess, refetchStaked, resetStake])

  useEffect(() => {
    if (unstakeSuccess) {
      refetchStaked()
      setAmount('')
      resetUnstake()
    }
  }, [unstakeSuccess, refetchStaked, resetUnstake])

  const handleStake = async () => {
    approveHandledRef.current = false
    if (needsApproval) {
      pendingAmountRef.current = amountBigInt
      setPendingStake(true)
      await approve(amountBigInt)
      return
    }
    await stake(amountBigInt)
  }

  const handleUnstake = async () => {
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
      if (stakePending) return 'Staking...'
      if (approvePending) return `Approving DXY-${side}...`
      if (insufficientBalance) return 'Insufficient Balance'
      if (needsApproval) return `Approve DXY-${side}`
      return `Stake DXY-${side}`
    } else {
      if (unstakePending) return 'Unstaking...'
      if (insufficientBalance) return 'Insufficient Balance'
      return `Unstake sDXY-${side}`
    }
  }

  const isDisabled = !amount || parseFloat(amount) <= 0 ||
    stakePending || unstakePending || approvePending || insufficientBalance

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
