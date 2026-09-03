import { lazy, Suspense } from 'react'
import { useAccount, useDisconnect, useChainId } from 'wagmi'
import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'
import { useLocation } from 'react-router-dom'
import { anvil, openAppKit } from '../../config/wagmi'
import { formatAddress } from '../../utils/formatters'
import { useSwitchToArbitrumSepolia } from '../../hooks'

const SponsoredOperationHistoryButton = lazy(() => import('../SponsoredOperationActivity').then((module) => ({ default: module.SponsoredOperationHistoryButton })))

const SUPPORTED_CHAIN_IDS: number[] = [mainnet.id, sepolia.id, arbitrumSepolia.id, anvil.id as number]
const WALLET_BUTTON_CLASS =
  'group inline-flex min-h-11 shrink-0 items-center justify-center gap-1.5 whitespace-nowrap border border-[#FF572D] bg-[#FF572D] px-2 py-2 text-[#FFF5F9] transition-colors enabled:hover:border-[#FFF5F9] enabled:hover:bg-[#FFF5F9] enabled:hover:text-[#250917] sm:gap-2 sm:px-4'
const SWITCH_NETWORK_BUTTON_CLASS =
  'group inline-flex h-11 w-11 shrink-0 cursor-pointer items-center justify-center gap-2 whitespace-nowrap border border-[#FFAB96] bg-[#FFAB96] px-2 py-2 text-xs font-semibold text-[#250917] transition-colors enabled:hover:border-[#FFAB96] enabled:hover:bg-[#250917] enabled:hover:text-[#FFAB96] disabled:cursor-not-allowed disabled:opacity-50 xl:w-auto xl:px-3'

export function ConnectButton() {
  const { address, isConnected } = useAccount()
  const { disconnect } = useDisconnect()
  const {
    switchToArbitrumSepolia,
    isSwitching: isSwitchingNetwork,
    switchError,
    clearSwitchError,
  } = useSwitchToArbitrumSepolia()
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
  const isArbitrumSepoliaRoute = isPerpsRoute
    || location.pathname === '/vaults'
    || location.pathname.startsWith('/vaults/')
  const shouldShowPerpsNetworkSwitch = isArbitrumSepoliaRoute && !isArbitrumSepolia

  if (!isConnected) {
    return (
      <button
        type="button"
        aria-label="Connect Wallet"
        onClick={() => {
          clearSwitchError()
          void openAppKit()
        }}
        className={`${WALLET_BUTTON_CLASS} text-xs font-medium sm:text-sm`}
      >
        <span className="material-symbols-outlined text-lg">account_balance_wallet</span>
        <span className="hidden group-hover:underline group-hover:underline-offset-4 min-[340px]:inline">
          Connect Wallet
        </span>
      </button>
    )
  }

  return (
    <div className="flex min-w-0 max-w-full flex-col items-end gap-1">
      <div className="flex min-w-0 max-w-full items-center gap-1.5 sm:gap-2 lg:gap-4">
        {/* Network badge */}
        <span className={`
          hidden max-w-36 truncate whitespace-nowrap border px-2 py-0.5 text-xs font-medium xl:inline-block
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
            aria-label="Switch wallet network to Arbitrum Sepolia"
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
                <span className="hidden group-hover:underline group-hover:underline-offset-4 xl:inline">
                  Switching...
                </span>
              </>
            ) : (
              <>
                <span className="material-symbols-outlined text-base">swap_horiz</span>
                <span className="hidden group-hover:underline group-hover:underline-offset-4 xl:inline">
                  Switch Network
                </span>
              </>
            )}
          </button>
        ) : null}

        {isPerpsRoute ? (
          <Suspense fallback={null}>
            <SponsoredOperationHistoryButton />
          </Suspense>
        ) : null}

        {/* Account button */}
        <button
          onClick={() => {
            void openAppKit({ view: 'Account' })
          }}
          title="Open wallet account"
          aria-label={`Open wallet account ${formatAddress(address ?? '')}`}
          className={`group h-11 w-11 !px-0 sm:w-auto sm:!px-3 ${WALLET_BUTTON_CLASS}`}
        >
          <div className="w-2 h-2 rounded-full bg-positive" />
          <span className="hidden whitespace-nowrap text-xs font-medium group-hover:underline group-hover:underline-offset-4 sm:inline sm:text-sm">
            {formatAddress(address ?? '')}
          </span>
        </button>

        {/* Disconnect button */}
        <button
          onClick={() => { disconnect(); }}
          className="hidden h-11 w-11 items-center justify-center text-content-secondary transition-colors hover:text-content-primary sm:inline-flex"
          title="Disconnect"
          aria-label="Disconnect wallet"
        >
          <span className="material-symbols-outlined text-xl">logout</span>
        </button>
      </div>
      {switchError && shouldShowPerpsNetworkSwitch ? (
        <p className="max-w-md text-right text-xs leading-4 text-[#FFAB96]">
          {switchError}
        </p>
      ) : null}
    </div>
  )
}
