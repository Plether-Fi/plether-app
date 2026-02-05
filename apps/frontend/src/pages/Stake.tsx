import { useCallback } from 'react'
import { useAccount } from 'wagmi'
import { StakingCard } from '../components/StakingCard'
import { ConnectWalletPrompt } from '../components/ConnectWalletPrompt'
import { useUserBalances, apiQueryKeys } from '../api'
import { useQueryClient } from '@tanstack/react-query'

export function Stake() {
  const { isConnected, address } = useAccount()
  const queryClient = useQueryClient()

  const { data: balancesData } = useUserBalances(address)
  const balances = balancesData?.data

  const bearBalance = balances ? BigInt(balances.bear) : 0n
  const bullBalance = balances ? BigInt(balances.bull) : 0n

  const handleSuccess = useCallback(() => {
    if (address) {
      void queryClient.invalidateQueries({ queryKey: apiQueryKeys.user.balances(address) })
    }
  }, [address, queryClient])

  return (
    <div className="space-y-10">
      <div className="mb-8">
        <h1 className="text-3xl font-semibold text-cyber-text-primary mb-1">Stake</h1>
        <p className="text-cyber-text-secondary font-light">Stake your tokens to use as collateral</p>
      </div>

      {isConnected ? (
        <div className="grid md:grid-cols-2 gap-6">
          <StakingCard
            side="BULL"
            tokenBalance={bullBalance}
            onSuccess={handleSuccess}
          />
          <StakingCard
            side="BEAR"
            tokenBalance={bearBalance}
            onSuccess={handleSuccess}
          />
        </div>
      ) : (
        <ConnectWalletPrompt description="Connect your wallet to stake plDXY-BEAR and plDXY-BULL tokens." />
      )}
    </div>
  )
}

export default Stake
