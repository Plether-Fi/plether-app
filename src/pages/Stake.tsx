import { useState } from 'react'
import { useAccount } from 'wagmi'
import { Card, CardHeader, Button, Tabs } from '../components/ui'
import { TokenInput } from '../components/TokenInput'
import { formatAmount } from '../utils/formatters'

type StakeMode = 'stake' | 'unstake'

export function Stake() {
  const { isConnected } = useAccount()

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-2xl font-bold text-white">Stake</h1>
        <p className="text-gray-400">Stake your tokens to use as collateral</p>
      </div>

      <div className="grid md:grid-cols-2 gap-6">
        {/* BEAR Staking */}
        <StakingCard
          side="BEAR"
          tokenBalance={500n * 10n ** 18n}
          stakedBalance={200n * 10n ** 18n}
          isConnected={isConnected}
        />

        {/* BULL Staking */}
        <StakingCard
          side="BULL"
          tokenBalance={500n * 10n ** 18n}
          stakedBalance={150n * 10n ** 18n}
          isConnected={isConnected}
        />
      </div>
    </div>
  )
}

interface StakingCardProps {
  side: 'BEAR' | 'BULL'
  tokenBalance: bigint
  stakedBalance: bigint
  isConnected: boolean
}

function StakingCard({
  side,
  tokenBalance,
  stakedBalance,
  isConnected,
}: StakingCardProps) {
  const [mode, setMode] = useState<StakeMode>('stake')
  const [amount, setAmount] = useState('')

  const colorClass = side === 'BEAR' ? 'text-bear' : 'text-bull'
  const bgClass = side === 'BEAR' ? 'bg-bear/10' : 'bg-bull/10'
  const borderClass = side === 'BEAR' ? 'border-bear/30' : 'border-bull/30'

  const handleStake = async () => {
    console.log('Stake:', { side, amount })
  }

  const handleUnstake = async () => {
    console.log('Unstake:', { side, amount })
  }

  return (
    <Card>
      <CardHeader
        title={`DXY-${side} Staking`}
        subtitle={`Stake DXY-${side} to use as collateral`}
      />

      {/* Staking stats */}
      <div className={`${bgClass} ${borderClass} border rounded-lg p-4 mb-4`}>
        <div className="flex justify-between">
          <span className="text-gray-400 text-sm">Staked Balance</span>
          <span className={`${colorClass} font-medium`}>
            {formatAmount(stakedBalance, 18)} sDXY-{side}
          </span>
        </div>
      </div>

      {/* Stake/Unstake tabs */}
      <Tabs
        tabs={[
          { id: 'stake', label: 'Stake' },
          { id: 'unstake', label: 'Unstake' },
        ]}
        activeTab={mode}
        onChange={(id) => {
          setMode(id as StakeMode)
          setAmount('')
        }}
      />

      <div className="mt-4 space-y-4">
        <TokenInput
          value={amount}
          onChange={setAmount}
          token={{
            symbol: mode === 'stake' ? `DXY-${side}` : `sDXY-${side}`,
            decimals: 18,
          }}
          balance={isConnected ? (mode === 'stake' ? tokenBalance : stakedBalance) : undefined}
        />

        {isConnected ? (
          <Button
            variant="primary"
            className="w-full"
            disabled={!amount || parseFloat(amount) <= 0}
            onClick={mode === 'stake' ? handleStake : handleUnstake}
          >
            {mode === 'stake' ? 'Stake' : 'Unstake'} DXY-{side}
          </Button>
        ) : (
          <Button variant="secondary" className="w-full" disabled>
            Connect Wallet
          </Button>
        )}
      </div>
    </Card>
  )
}
