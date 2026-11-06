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
  try {
    const value = window.sessionStorage.getItem(storageKey)
    return value === 'disconnected' ? null : value ?? undefined
  } catch {
    return undefined
  }
}

export function saveSelectedWallet(storageKey: string, address: string | null): void {
  try {
    window.sessionStorage.setItem(storageKey, address?.toLowerCase() ?? 'disconnected')
  } catch {
    // Selection still works when browser storage is unavailable.
  }
}
