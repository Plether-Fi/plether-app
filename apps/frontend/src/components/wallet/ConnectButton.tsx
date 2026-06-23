import { useAccount, useDisconnect, useChainId } from 'wagmi'
import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'
import { anvil } from '../../config/wagmi'
import { formatAddress } from '../../utils/formatters'
import { useAppKit } from '@reown/appkit/react'
import { useSwitchToArbitrumSepolia } from '../../hooks'

const SUPPORTED_CHAIN_IDS: number[] = [mainnet.id, sepolia.id, arbitrumSepolia.id, anvil.id as number]
const WALLET_BUTTON_CLASS =
  'flex items-center gap-2 border border-[#FF572D] bg-[#FF572D] px-4 py-2 text-[#FFF5F9] transition-colors enabled:hover:border-[#FFF5F9] enabled:hover:bg-[#FFF5F9] enabled:hover:text-[#250917] enabled:hover:underline enabled:hover:underline-offset-4'
const SWITCH_NETWORK_BUTTON_CLASS =
  'flex cursor-pointer items-center gap-2 border border-[#FFAB96] bg-[#FFAB96] px-3 py-2 text-xs font-semibold text-[#250917] transition-colors enabled:hover:border-[#FFAB96] enabled:hover:bg-[#250917] enabled:hover:text-[#FFAB96] enabled:hover:underline enabled:hover:underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50'

export function ConnectButton() {
  const { address, isConnected } = useAccount()
  const { disconnect } = useDisconnect()
  const {
    switchToArbitrumSepolia,
    isSwitching: isSwitchingNetwork,
    switchError,
    clearSwitchError,
  } = useSwitchToArbitrumSepolia()
  const { open } = useAppKit()
  const chainId = useChainId()

  const getNetworkName = () => {
    switch (chainId) {
      case mainnet.id:
        return 'Mainnet'
      case sepolia.id:
        return 'Sepolia'
      case arbitrumSepolia.id:
        return 'Arbitrum Sepolia'
      case anvil.id:
        return 'Anvil - dev'
      default:
        return 'Unknown'
    }
  }

  const isWrongNetwork = !SUPPORTED_CHAIN_IDS.includes(chainId)
  const isArbitrumSepolia = chainId === arbitrumSepolia.id

  if (!isConnected) {
    return (
      <button
        onClick={() => {
          clearSwitchError()
          void open()
        }}
        className={`${WALLET_BUTTON_CLASS} text-sm font-medium`}
      >
        <span className="material-symbols-outlined text-lg">account_balance_wallet</span>
        Connect Wallet
      </button>
    )
  }

  return (
    <div className="flex flex-col items-end gap-1">
      <div className="flex items-center gap-4">
        {/* Network badge */}
        <span className={`
          px-2 py-0.5 text-xs font-medium border
          ${isWrongNetwork
            ? 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia border-cyber-electric-fuchsia/30'
            : chainId === sepolia.id || chainId === arbitrumSepolia.id
              ? 'bg-cyber-warning-bg text-cyber-warning-text border-cyber-warning-text/30'
              : chainId === mainnet.id
                ? 'bg-cyber-neon-green/20 text-cyber-neon-green border-cyber-neon-green/30'
                : 'bg-cyber-surface-light text-cyber-text-secondary border-cyber-border-glow/30'
          }
        `}>
          {isWrongNetwork ? 'Wrong Network' : getNetworkName()}
        </span>

        {!isArbitrumSepolia ? (
          <button
            type="button"
            onClick={() => { void switchToArbitrumSepolia() }}
            disabled={isSwitchingNetwork}
            className={SWITCH_NETWORK_BUTTON_CLASS}
            title="Switch wallet network to Arbitrum Sepolia"
          >
            {isSwitchingNetwork ? (
              <>
                <span className="relative h-4 w-4">
                  <span className="absolute inset-0 rounded-full border-2 border-[#250917]/30 border-t-[#250917] animate-spin" />
                </span>
                Switching...
              </>
            ) : (
              <>
                <span className="material-symbols-outlined text-base">swap_horiz</span>
                Switch Network
              </>
            )}
          </button>
        ) : null}

        {/* Account button */}
        <button
          onClick={() => { void open({ view: 'Account' }) }}
          title="Open wallet account"
          className={`group ${WALLET_BUTTON_CLASS}`}
        >
          <div className="w-2 h-2 rounded-full bg-cyber-neon-green" />
          <span className="font-medium text-xs sm:text-sm">
            {formatAddress(address ?? '')}
          </span>
        </button>

        {/* Disconnect button */}
        <button
          onClick={() => { disconnect(); }}
          className="p-2 text-cyber-text-secondary transition-colors hover:text-cyber-text-primary"
          title="Disconnect"
        >
          <span className="material-symbols-outlined text-xl">logout</span>
        </button>
      </div>
      {switchError && !isArbitrumSepolia ? (
        <p className="max-w-md text-right text-xs leading-4 text-[#FFAB96]">
          {switchError}
        </p>
      ) : null}
    </div>
  )
}
