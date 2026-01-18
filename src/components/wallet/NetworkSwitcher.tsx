import { useSwitchChain, useChainId } from 'wagmi'
import { mainnet, sepolia } from 'wagmi/chains'
import { anvil } from '../../config/wagmi'
import { Modal } from '../ui'

const SUPPORTED_CHAIN_IDS = [mainnet.id, sepolia.id, anvil.id] as const

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
    { chain: anvil, name: 'Anvil (Local)', icon: 'terminal' },
  ]

  const handleSwitch = (targetChainId: typeof SUPPORTED_CHAIN_IDS[number]) => {
    switchChain({ chainId: targetChainId })
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
              onClick={() => { handleSwitch(chain.id) }}
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
