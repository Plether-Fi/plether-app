interface WalletAddress {
  address: string
}

export function selectedWallet<T extends WalletAddress>(wallets: T[], selectedAddress: string | null): T | undefined {
  if (selectedAddress) {
    return wallets.find((wallet) => wallet.address.toLowerCase() === selectedAddress.toLowerCase())
  }
  return wallets.length === 1 ? wallets[0] : undefined
}

export function readSelectedWallet(storageKey: string): string | null {
  try {
    return window.sessionStorage.getItem(storageKey)
  } catch {
    return null
  }
}

export function saveSelectedWallet(storageKey: string, address: string): void {
  try {
    window.sessionStorage.setItem(storageKey, address.toLowerCase())
  } catch {
    // Selection still works when browser storage is unavailable.
  }
}
