import { useCallback, useState } from 'react'
import { useSwitchChain } from 'wagmi'
import { arbitrumSepolia } from 'wagmi/chains'
import { openAppKit, switchAppKitToArbitrumSepolia } from '../config/wagmi'

function getErrorText(error: unknown): string {
  if (!error) return ''
  if (typeof error === 'string') return error
  if (error instanceof Error) return `${error.name} ${error.message}`.trim()

  if (typeof error === 'object') {
    const errorRecord = error as Record<string, unknown>
    return [
      errorRecord.name,
      errorRecord.shortMessage,
      errorRecord.message,
      errorRecord.details,
    ]
      .filter((item): item is string => typeof item === 'string')
      .join(' ')
  }

  return ''
}

function getSwitchHelpMessage(error: unknown): string {
  const errorText = getErrorText(error).toLowerCase()

  if (errorText.includes('wallet_addethereumchain') || errorText.includes('missing or invalid')) {
    return 'Your wallet did not add Arbitrum Sepolia automatically. Open the wallet network selector and choose Arbitrum Sepolia, or add it manually.'
  }

  if (errorText.includes('user rejected') || errorText.includes('rejected the request')) {
    return 'Network switch was rejected in the wallet. Confirm the request, or choose Arbitrum Sepolia manually.'
  }

  return 'Could not switch automatically. Choose Arbitrum Sepolia in your wallet or the network selector.'
}

export function useSwitchToArbitrumSepolia() {
  const { switchChainAsync, isPending } = useSwitchChain()
  const [isOpeningFallback, setIsOpeningFallback] = useState(false)
  const [switchError, setSwitchError] = useState<string | null>(null)

  const switchToArbitrumSepolia = useCallback(async () => {
    let lastError: unknown

    setSwitchError(null)
    setIsOpeningFallback(true)

    try {
      await switchChainAsync({ chainId: arbitrumSepolia.id })
      return true
    } catch (switchChainError) {
      lastError = switchChainError
      console.warn('Failed to switch wallet network through Wagmi. Trying AppKit.', switchChainError)
    }

    try {
      await switchAppKitToArbitrumSepolia()
      return true
    } catch (appKitError) {
      lastError = appKitError
      console.warn('Failed to switch wallet network through AppKit. Opening network selector.', appKitError)
    }

    try {
      await openAppKit({ view: 'Networks' })
    } catch (networkSelectorError) {
      lastError = networkSelectorError
      console.warn('Failed to open AppKit network selector.', networkSelectorError)
    } finally {
      setSwitchError(getSwitchHelpMessage(lastError))
      setIsOpeningFallback(false)
    }
    return false
  }, [switchChainAsync])

  const clearSwitchError = useCallback(() => {
    setSwitchError(null)
  }, [])

  return {
    switchToArbitrumSepolia,
    isSwitching: isPending || isOpeningFallback,
    switchError,
    clearSwitchError,
  }
}
