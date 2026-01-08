import { useAccount, useDisconnect, useChainId } from 'wagmi'
import { useWeb3Modal } from '@web3modal/wagmi/react'
import { mainnet, sepolia } from 'wagmi/chains'
import { formatAddress } from '../../utils/formatters'

export function ConnectButton() {
  const { address, isConnected } = useAccount()
  const { disconnect } = useDisconnect()
  const { open } = useWeb3Modal()
  const chainId = useChainId()

  const getNetworkName = () => {
    switch (chainId) {
      case mainnet.id:
        return 'Mainnet'
      case sepolia.id:
        return 'Sepolia'
      default:
        return 'Unknown'
    }
  }

  const isWrongNetwork = chainId !== mainnet.id && chainId !== sepolia.id

  if (!isConnected) {
    return (
      <button
        onClick={() => open()}
        className="flex items-center gap-2 bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/80 text-cyber-text-primary  px-4 py-2 transition-colors border border-transparent shadow-lg shadow-cyber-electric-fuchsia/20 font-medium text-sm"
      >
        <span className="material-symbols-outlined text-lg">account_balance_wallet</span>
        Connect Wallet
      </button>
    )
  }

  return (
    <div className="flex items-center gap-2">
      {/* Network badge */}
      <span className={`
        px-2 py-0.5 text-xs font-medium border
        ${isWrongNetwork
          ? 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia border-cyber-electric-fuchsia/30'
          : chainId === sepolia.id
            ? 'bg-cyber-warning-bg text-cyber-warning-text border-cyber-warning-text/30'
            : 'bg-cyber-surface-light text-cyber-text-secondary border-cyber-border-glow/30'
        }
      `}>
        {isWrongNetwork ? 'Wrong Network' : getNetworkName()}
      </span>

      {/* Account button */}
      <button
        onClick={() => open({ view: 'Account' })}
        className="flex items-center gap-2 bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/80 text-cyber-text-primary  px-4 py-2 transition-colors border border-transparent shadow-lg shadow-cyber-electric-fuchsia/20 group"
      >
        <div className="w-2 h-2 rounded-full bg-cyber-neon-green shadow-md shadow-cyber-neon-green/50" />
        <span className="font-medium text-xs sm:text-sm">
          {formatAddress(address || '')}
        </span>
      </button>

      {/* Disconnect button */}
      <button
        onClick={() => disconnect()}
        className="p-2 text-cyber-text-secondary hover:text-cyber-bright-blue transition-colors"
        title="Disconnect"
      >
        <span className="material-symbols-outlined text-xl">logout</span>
      </button>
    </div>
  )
}
