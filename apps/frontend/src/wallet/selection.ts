import { isAddress } from 'viem'

export type WalletSelection = string | null | undefined

export function readWalletSelection(storage: Pick<Storage, 'getItem'>, key: string): WalletSelection {
  try {
    const stored = storage.getItem(key)
    if (stored === 'disconnected') return null
    return stored && isAddress(stored) ? stored.toLowerCase() : undefined
  } catch {
    return undefined
  }
}

export function writeWalletSelection(storage: Pick<Storage, 'setItem'>, key: string, address: string | null): void {
  try {
    storage.setItem(key, address?.toLowerCase() ?? 'disconnected')
  } catch {
    // Browser storage may be unavailable. The selection still lives in memory.
  }
}

// Privy's session survives a discarded mobile tab. Remember the user's wallet
// choice for the same lifetime, including an explicit disconnected choice.
export function loadWalletSelection(key: string): WalletSelection {
  for (const getStorage of [() => window.localStorage, () => window.sessionStorage]) {
    try {
      const selection = readWalletSelection(getStorage(), key)
      if (selection !== undefined) {
        saveWalletSelection(key, selection)
        return selection
      }
    } catch { /* Try tab storage when persistent storage is blocked. */ }
  }
  return undefined
}

export function saveWalletSelection(key: string, address: string | null): void {
  for (const getStorage of [() => window.localStorage, () => window.sessionStorage]) {
    try { writeWalletSelection(getStorage(), key, address) } catch { /* Storage can be blocked independently. */ }
  }
}

export function selectedWallet<T extends { address: string }>(wallets: T[], selection: WalletSelection): T | undefined {
  if (selection === null) return undefined
  if (selection !== undefined) {
    return wallets.find(wallet => wallet.address.toLowerCase() === selection.toLowerCase())
  }
  // A restored session containing multiple wallets needs an explicit choice.
  return wallets.length === 1 ? wallets[0] : undefined
}
