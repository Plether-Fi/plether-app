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
  const stakedBear = balances ? BigInt(balances.stakedBear) : 0n
  const stakedBull = balances ? BigInt(balances.stakedBull) : 0n

  const handleSuccess = useCallback(() => {
    if (address) {
      void queryClient.invalidateQueries({ queryKey: apiQueryKeys.user.balances(address) })
    }
  }, [address, queryClient])

  return (
    <div className="min-w-0 space-y-6 sm:space-y-10">
      <div className="mb-6 sm:mb-8">
        <h1 className="mb-1 text-2xl font-semibold text-content-primary sm:text-3xl">Stake</h1>
        <p className="text-sm font-light text-content-secondary sm:text-base">Stake your tokens to use as collateral</p>
      </div>

      {isConnected ? (
        <div className="grid min-w-0 gap-4 sm:gap-6 md:grid-cols-2">
          <StakingCard
            side="BULL"
            tokenBalance={bullBalance}
            stakedBalance={stakedBull}
            onSuccess={handleSuccess}
          />
          <StakingCard
            side="BEAR"
            tokenBalance={bearBalance}
            stakedBalance={stakedBear}
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
