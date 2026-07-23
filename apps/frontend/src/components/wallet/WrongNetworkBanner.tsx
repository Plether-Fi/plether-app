import { useChainId } from 'wagmi'
import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'
import { anvil } from '../../config/wagmi'
import { useSwitchToArbitrumSepolia } from '../../hooks'

const SUPPORTED_CHAIN_IDS: number[] = [mainnet.id, sepolia.id, arbitrumSepolia.id, anvil.id as number]

export function WrongNetworkBanner() {
  const chainId = useChainId()
  const { switchToArbitrumSepolia, isSwitching, switchError } = useSwitchToArbitrumSepolia()

  const isWrongNetwork = !SUPPORTED_CHAIN_IDS.includes(chainId)

  if (!isWrongNetwork) return null

  return (
    <div className="border-b border-brand-orange/50 bg-brand-orange/20 px-4 py-3">
      <div className="mx-auto flex max-w-7xl flex-col items-stretch justify-between gap-3 sm:flex-row sm:items-center sm:gap-4">
        <div className="flex min-w-0 items-start gap-3">
          <span className="material-symbols-outlined text-brand-orange">warning</span>
          <div className="space-y-1">
            <p className="text-brand-orange text-sm">
              Please connect to Ethereum Mainnet, Sepolia, or Arbitrum Sepolia to use Plether.
            </p>
            {switchError ? (
              <p className="max-w-3xl text-xs leading-4 text-[#FFAB96]">
                {switchError}
              </p>
            ) : null}
          </div>
        </div>
        <button
          onClick={() => { void switchToArbitrumSepolia() }}
          disabled={isSwitching}
          className="flex min-h-11 shrink-0 items-center justify-center gap-2 bg-brand-orange px-4 py-2 text-sm font-medium text-content-primary transition-colors enabled:hover:bg-[#FF572D] enabled:hover:underline enabled:hover:underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50"
        >
          {isSwitching ? (
            <>
              <div className="w-4 h-4 relative">
                <div className="absolute inset-0 rounded-full border-2 border-content-primary/30 border-t-content-primary animate-spin" />
              </div>
              Switching...
            </>
          ) : (
            <>
              <span className="material-symbols-outlined text-lg">swap_horiz</span>
              Switch to Arbitrum Sepolia
            </>
          )}
        </button>
      </div>
    </div>
  )
}
