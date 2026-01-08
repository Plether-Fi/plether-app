import { useSwitchChain, useChainId } from 'wagmi'
import { mainnet, sepolia } from 'wagmi/chains'
import { Modal } from '../ui'

interface NetworkSwitcherProps {
  isOpen: boolean
  onClose: () => void
}

export function NetworkSwitcher({ isOpen, onClose }: NetworkSwitcherProps) {
  const chainId = useChainId()
  const { switchChain, isPending } = useSwitchChain()

  const networks = [
    { chain: mainnet, name: 'Ethereum Mainnet', icon: 'diamond' },
    { chain: sepolia, name: 'Sepolia Testnet', icon: 'science' },
  ]

  const handleSwitch = async (targetChainId: typeof mainnet.id | typeof sepolia.id) => {
    await switchChain({ chainId: targetChainId })
    onClose()
  }

  return (
    <Modal isOpen={isOpen} onClose={onClose} title="Select Network" size="sm">
      <div className="space-y-2">
        {networks.map(({ chain, name, icon }) => {
          const isActive = chainId === chain.id
          return (
            <button
              key={chain.id}
              onClick={() => handleSwitch(chain.id)}
              disabled={isPending}
              className={`
                w-full flex items-center gap-3 px-4 py-3  transition-all
                ${isActive
                  ? 'bg-cyber-neon-green/20 border border-cyber-neon-green/50 shadow-sm shadow-cyber-neon-green/20'
                  : 'bg-cyber-surface-light border border-cyber-border-glow/30 hover:border-cyber-bright-blue/50 hover:bg-cyber-surface-light/80'
                }
                disabled:opacity-50 disabled:cursor-not-allowed
              `}
            >
              <div className={`w-10 h-10  flex items-center justify-center ${isActive ? 'bg-cyber-neon-green/20' : 'bg-cyber-surface-dark'}`}>
                <span className={`material-symbols-outlined text-xl ${isActive ? 'text-cyber-neon-green' : 'text-cyber-text-secondary'}`}>
                  {icon}
                </span>
              </div>
              <div className="text-left flex-1">
                <p className={`font-medium ${isActive ? 'text-cyber-neon-green' : 'text-cyber-text-primary'}`}>{name}</p>
                <p className="text-sm text-cyber-text-secondary">Chain ID: {chain.id}</p>
              </div>
              {isActive && (
                <span className="material-symbols-outlined text-cyber-neon-green">check_circle</span>
              )}
            </button>
          )
        })}
      </div>
    </Modal>
  )
}

export function WrongNetworkBanner() {
  const chainId = useChainId()
  const { switchChain, isPending } = useSwitchChain()

  const isWrongNetwork = chainId !== mainnet.id && chainId !== sepolia.id

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
          onClick={() => switchChain({ chainId: mainnet.id })}
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
