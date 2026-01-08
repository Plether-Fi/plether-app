import { useState } from 'react'
import { TokenIcon } from './ui'
import { formatAmount } from '../utils/formatters'

type StakeMode = 'stake' | 'unstake'

export interface StakingCardProps {
  side: 'BEAR' | 'BULL'
  tokenBalance: bigint
  stakedBalance: bigint
}

export function StakingCard({ side, tokenBalance, stakedBalance }: StakingCardProps) {
  const [mode, setMode] = useState<StakeMode>('stake')
  const [amount, setAmount] = useState('')

  const isBear = side === 'BEAR'
  const textColor = isBear ? 'text-cyber-electric-fuchsia' : 'text-cyber-neon-green'
  const bgColor = isBear ? 'bg-cyber-electric-fuchsia' : 'bg-cyber-neon-green'
  const borderColor = isBear ? 'border-cyber-electric-fuchsia' : 'border-cyber-neon-green'
  const shadowColor = isBear ? 'shadow-cyber-electric-fuchsia' : 'shadow-cyber-neon-green'

  const handleAction = async () => {
    console.log(`${mode}:`, { side, amount })
  }

  const balance = mode === 'stake' ? tokenBalance : stakedBalance

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
              {formatAmount(stakedBalance, 18)} sDXY-{side}
            </span>
          </div>
        </div>

        <div className="bg-cyber-surface-light p-1 flex text-sm font-medium border border-cyber-border-glow/30">
          <button
            onClick={() => { setMode('stake'); setAmount('') }}
            className={`flex-1 py-2 px-4 transition-all ${
              mode === 'stake'
                ? `bg-cyber-surface-dark ${textColor} shadow-sm ${shadowColor}/10 border ${borderColor}/50`
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
            }`}
          >
            Stake
          </button>
          <button
            onClick={() => { setMode('unstake'); setAmount('') }}
            className={`flex-1 py-2 px-4 transition-all ${
              mode === 'unstake'
                ? `bg-cyber-surface-dark ${textColor} shadow-sm ${shadowColor}/10 border ${borderColor}/50`
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
            }`}
          >
            Unstake
          </button>
        </div>

        <div className="space-y-2">
          <div className="flex justify-between text-sm">
            <span className="text-cyber-text-secondary">
              {mode === 'stake' ? `DXY-${side} to stake` : `sDXY-${side} to unstake`}
            </span>
            <span className="text-cyber-text-secondary">
              Balance: <span className="text-cyber-text-primary">{formatAmount(balance, 18)}</span>
            </span>
          </div>
          <div className="relative">
            <input
              type="number"
              value={amount}
              onChange={(e) => setAmount(e.target.value)}
              placeholder="0.00"
              className="w-full bg-cyber-surface-light border border-cyber-border-glow/30 py-4 pl-4 pr-24 text-xl font-medium text-cyber-text-primary focus:ring-1 focus:ring-cyber-bright-blue focus:border-cyber-bright-blue outline-none transition-shadow shadow-sm shadow-cyber-border-glow/10"
            />
            <div className="absolute right-4 top-1/2 -translate-y-1/2 flex items-center gap-2">
              <button
                onClick={() => setAmount((Number(balance) / 1e18).toString())}
                className={`text-xs font-semibold ${textColor} hover:opacity-80 px-2 py-1 ${isBear ? 'bg-cyber-electric-fuchsia/10' : 'bg-cyber-neon-green/10'}`}
              >
                MAX
              </button>
            </div>
          </div>
        </div>

        <button
          onClick={handleAction}
          disabled={!amount || parseFloat(amount) <= 0}
          className={`w-full ${bgColor} hover:opacity-90 ${isBear ? 'text-cyber-text-primary' : 'text-cyber-bg'} font-semibold py-4 px-6 shadow-lg ${shadowColor}/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none`}
        >
          {mode === 'stake' ? 'Stake' : 'Unstake'} DXY-{side}
        </button>
      </div>
    </div>
  )
}
