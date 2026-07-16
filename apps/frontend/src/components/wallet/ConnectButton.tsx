import { useAccount, useDisconnect, useChainId } from 'wagmi'
import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'
import { useLocation } from 'react-router-dom'
import { anvil, syncAppKitModalStyleOverrides } from '../../config/wagmi'
import { formatAddress } from '../../utils/formatters'
import { useAppKit } from '@reown/appkit/react'
import { useSwitchToArbitrumSepolia } from '../../hooks'
import { usePerpsIdentity } from '../../perps-aa'

const SUPPORTED_CHAIN_IDS: number[] = [mainnet.id, sepolia.id, arbitrumSepolia.id, anvil.id as number]
const WALLET_BUTTON_CLASS =
  'flex items-center gap-2 whitespace-nowrap border border-[#FF572D] bg-[#FF572D] px-4 py-2 text-[#FFF5F9] transition-colors enabled:hover:border-[#FFF5F9] enabled:hover:bg-[#FFF5F9] enabled:hover:text-[#250917] enabled:hover:underline enabled:hover:underline-offset-4'
const SWITCH_NETWORK_BUTTON_CLASS =
  'flex cursor-pointer items-center gap-2 whitespace-nowrap border border-[#FFAB96] bg-[#FFAB96] px-3 py-2 text-xs font-semibold text-[#250917] transition-colors enabled:hover:border-[#FFAB96] enabled:hover:bg-[#250917] enabled:hover:text-[#FFAB96] enabled:hover:underline enabled:hover:underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50'

export function ConnectButton() {
  const { address, isConnected } = useAccount()
  const perpsIdentity = usePerpsIdentity()
  const { disconnect } = useDisconnect()
  const {
    switchToArbitrumSepolia,
    isSwitching: isSwitchingNetwork,
    switchError,
    clearSwitchError,
  } = useSwitchToArbitrumSepolia()
  const { open } = useAppKit()
  const chainId = useChainId()
  const location = useLocation()

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
  const isPerpsRoute = location.pathname === '/'
  const shouldShowPerpsNetworkSwitch = isPerpsRoute && !isArbitrumSepolia

  if (!isConnected) {
    return (
      <button
        onClick={() => {
          clearSwitchError()
          syncAppKitModalStyleOverrides()
          void open()
          syncAppKitModalStyleOverrides()
        }}
        className={`${WALLET_BUTTON_CLASS} text-sm font-medium`}
      >
        <span className="material-symbols-outlined text-lg">account_balance_wallet</span>
        Connect Wallet
      </button>
    )
  }

  return (
    <div className="flex min-w-0 flex-col items-end gap-1">
      <div className="flex min-w-0 items-center gap-4">
        {/* Network badge */}
        <span className={`
          max-w-36 truncate whitespace-nowrap border px-2 py-0.5 text-xs font-medium
          ${isWrongNetwork
            ? 'bg-brand-orange/20 text-brand-orange border-brand-orange/30'
            : chainId === sepolia.id || chainId === arbitrumSepolia.id
              ? 'bg-warning-bg text-warning border-warning/30'
              : chainId === mainnet.id
                ? 'bg-positive/20 text-positive border-positive/30'
                : 'bg-surface-muted text-content-secondary border-brand-border/30'
          }
        `}>
          {isWrongNetwork ? 'Wrong Network' : getNetworkName()}
        </span>

        {shouldShowPerpsNetworkSwitch ? (
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
          onClick={() => {
            syncAppKitModalStyleOverrides()
            void open({ view: 'Account' })
            syncAppKitModalStyleOverrides()
          }}
          title="Open wallet account"
          className={`group ${WALLET_BUTTON_CLASS}`}
        >
          <div className="w-2 h-2 rounded-full bg-positive" />
          <span className="whitespace-nowrap text-xs font-medium sm:text-sm">
            {formatAddress(address ?? '')}
          </span>
        </button>

        {/* Disconnect button */}
        <button
          onClick={() => { disconnect(); }}
          className="p-2 text-content-secondary transition-colors hover:text-content-primary"
          title="Disconnect"
        >
          <span className="material-symbols-outlined text-xl">logout</span>
        </button>
      </div>
      {switchError && shouldShowPerpsNetworkSwitch ? (
        <p className="max-w-md text-right text-xs leading-4 text-[#FFAB96]">
          {switchError}
        </p>
      ) : null}
      {isPerpsRoute && perpsIdentity.isAaManifestConfigured ? (
        <p className="max-w-md text-right text-xs leading-4 text-content-secondary">
          Owner Wallet {formatAddress(perpsIdentity.ownerAddress ?? '')}
          {' · '}
          {perpsIdentity.accountAddress
            ? `Trading Account ${formatAddress(perpsIdentity.accountAddress)}`
            : perpsIdentity.error?.message ?? 'Trading Account continuity check required'}
        </p>
      ) : null}
    </div>
  )
}
