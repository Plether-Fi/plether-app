import { useAccount } from 'wagmi'
import { StakingCard } from '../components/StakingCard'

export function Stake() {
  const { isConnected } = useAccount()

  return (
    <div className="space-y-10">
      <div className="mb-8">
        <h1 className="text-3xl font-semibold text-cyber-text-primary mb-1">Stake</h1>
        <p className="text-cyber-text-secondary font-light">Stake your tokens to use as collateral</p>
      </div>

      {isConnected ? (
        <div className="grid md:grid-cols-2 gap-6">
          <StakingCard
            side="BEAR"
            tokenBalance={500n * 10n ** 18n}
            stakedBalance={200n * 10n ** 18n}
          />
          <StakingCard
            side="BULL"
            tokenBalance={500n * 10n ** 18n}
            stakedBalance={150n * 10n ** 18n}
          />
        </div>
      ) : (
        <div className="bg-cyber-surface-dark  p-12 text-center border border-cyber-border-glow/30 shadow-lg">
          <div className="w-16 h-16 mx-auto mb-4 rounded-full bg-cyber-surface-light flex items-center justify-center">
            <span className="material-symbols-outlined text-3xl text-cyber-text-secondary">lock</span>
          </div>
          <h2 className="text-xl font-semibold text-cyber-text-primary mb-2">Connect Your Wallet</h2>
          <p className="text-cyber-text-secondary mb-6 max-w-md mx-auto">
            Connect your wallet to stake DXY-BEAR and DXY-BULL tokens.
          </p>
        </div>
      )}
    </div>
  )
}
