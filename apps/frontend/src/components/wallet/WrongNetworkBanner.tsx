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
    <div className="bg-cyber-electric-fuchsia/20 border-b border-cyber-electric-fuchsia/50 px-4 py-3">
      <div className="max-w-7xl mx-auto flex items-center justify-between gap-4">
        <div className="flex items-start gap-3">
          <span className="material-symbols-outlined text-cyber-electric-fuchsia">warning</span>
          <div className="space-y-1">
            <p className="text-cyber-electric-fuchsia text-sm">
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
          className="flex items-center gap-2 px-4 py-2 bg-cyber-electric-fuchsia text-cyber-text-primary enabled:hover:bg-[#CC00AA] enabled:hover:underline enabled:hover:underline-offset-4  text-sm font-medium transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {isSwitching ? (
            <>
              <div className="w-4 h-4 relative">
                <div className="absolute inset-0 rounded-full border-2 border-cyber-text-primary/30 border-t-cyber-text-primary animate-spin" />
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
