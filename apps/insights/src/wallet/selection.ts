import { isAddress } from 'viem'

interface WalletAddress {
  address: string
}

export type WalletSelection = string | null | undefined

export function selectedWallet<T extends WalletAddress>(wallets: T[], selectedAddress: WalletSelection): T | undefined {
  if (selectedAddress === null) return undefined
  if (selectedAddress !== undefined) {
    return wallets.find((wallet) => wallet.address.toLowerCase() === selectedAddress.toLowerCase())
  }
  return wallets.length === 1 ? wallets[0] : undefined
}

export function readSelectedWallet(storageKey: string): WalletSelection {
  // Keep the selected owner alongside Privy's persistent authentication.
  // Read an existing tab choice once when upgrading, or if local storage fails.
  for (const getStorage of [() => window.localStorage, () => window.sessionStorage]) {
    try {
      const value = getStorage().getItem(storageKey)
      const selection = value === 'disconnected' ? null : value && isAddress(value) ? value.toLowerCase() : undefined
      if (selection !== undefined) {
        saveSelectedWallet(storageKey, selection)
        return selection
      }
    } catch { /* Try tab storage when persistent storage is blocked. */ }
  }
  return undefined
}

export function saveSelectedWallet(storageKey: string, address: string | null): void {
  for (const getStorage of [() => window.localStorage, () => window.sessionStorage]) {
    try {
      getStorage().setItem(storageKey, address?.toLowerCase() ?? 'disconnected')
    } catch { /* Selection still works when browser storage is unavailable. */ }
  }
}
