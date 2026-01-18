import { useSwitchChain, useChainId } from 'wagmi'
import { mainnet, sepolia } from 'wagmi/chains'
import { anvil } from '../../config/wagmi'

const SUPPORTED_CHAIN_IDS = [mainnet.id, sepolia.id, anvil.id] as const

export function WrongNetworkBanner() {
  const chainId = useChainId()
  const { switchChain, isPending } = useSwitchChain()

  const isWrongNetwork = !SUPPORTED_CHAIN_IDS.includes(chainId)

  if (!isWrongNetwork) return null

  return (
    <div className="bg-cyber-electric-fuchsia/20 border-b border-cyber-electric-fuchsia/50 px-4 py-3">
      <div className="max-w-7xl mx-auto flex items-center justify-between gap-4">
        <div className="flex items-center gap-3">
          <span className="material-symbols-outlined text-cyber-electric-fuchsia">warning</span>
          <p className="text-cyber-electric-fuchsia text-sm">
            Please connect to Ethereum Mainnet or Sepolia to use Plether.
          </p>
        </div>
        <button
          onClick={() => { switchChain({ chainId: mainnet.id }) }}
          disabled={isPending}
          className="flex items-center gap-2 px-4 py-2 bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/80 text-cyber-text-primary  text-sm font-medium transition-colors disabled:opacity-50 disabled:cursor-not-allowed shadow-lg shadow-cyber-electric-fuchsia/20"
        >
          {isPending ? (
            <>
              <div className="w-4 h-4 relative">
                <div className="absolute inset-0 rounded-full border-2 border-cyber-text-primary/30 border-t-cyber-text-primary animate-spin" />
              </div>
              Switching...
            </>
          ) : (
            <>
              <span className="material-symbols-outlined text-lg">swap_horiz</span>
              Switch to Mainnet
            </>
          )}
        </button>
      </div>
    </div>
  )
}
