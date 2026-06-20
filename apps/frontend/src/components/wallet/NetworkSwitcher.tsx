import { useState } from 'react'
import { useAppKitNetwork } from '@reown/appkit/react'
import { useChainId } from 'wagmi'
import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'
import { anvil, appKitNetworksByChainId } from '../../config/wagmi'
import { Modal } from '../ui'

interface NetworkSwitcherProps {
  isOpen: boolean
  onClose: () => void
}

export function NetworkSwitcher({ isOpen, onClose }: NetworkSwitcherProps) {
  const chainId = useChainId()
  const { switchNetwork } = useAppKitNetwork()
  const [isPending, setIsPending] = useState(false)

  type SupportedChainId = typeof mainnet.id | typeof sepolia.id | typeof arbitrumSepolia.id | typeof anvil.id

  const networks = [
    { chain: mainnet, name: 'Ethereum Mainnet', icon: 'diamond' },
    { chain: sepolia, name: 'Sepolia Testnet', icon: 'science' },
    { chain: arbitrumSepolia, name: 'Arbitrum Sepolia', icon: 'hub' },
    { chain: anvil, name: 'Anvil (Local)', icon: 'terminal' },
  ] as const

  const handleSwitch = async (targetChainId: SupportedChainId) => {
    const targetNetwork = appKitNetworksByChainId[targetChainId]

    setIsPending(true)
    try {
      await switchNetwork(targetNetwork)
      onClose()
    } finally {
      setIsPending(false)
    }
  }

  return (
    <Modal isOpen={isOpen} onClose={onClose} title="Select Network" size="sm">
      <div className="space-y-2">
        {networks.map(({ chain, name, icon }) => {
          const isActive = chainId === chain.id
          return (
            <button
              key={chain.id}
              onClick={() => { void handleSwitch(chain.id) }}
              disabled={isPending}
              className={`
                w-full flex items-center gap-3 px-4 py-3 transition-colors hover:underline hover:underline-offset-4
                ${isActive
                  ? 'bg-cyber-neon-green/20 border border-cyber-neon-green/50'
                  : 'bg-cyber-surface-light border border-cyber-border-glow/30 hover:border-[#FFAB96]/50 hover:bg-[#3B212D]'
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
