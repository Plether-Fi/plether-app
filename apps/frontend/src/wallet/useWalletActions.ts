import { createContext, use } from 'react'

export interface WalletOption {
  address: string
  name: string
  embedded: boolean
}

export interface WalletActions {
  configured: boolean
  ready: boolean
  signerReady: boolean
  authenticated: boolean
  modalOpen: boolean
  wallets: WalletOption[]
  error: string | null
  connect: () => void
  openAccount: () => Promise<void>
  selectWallet: (address: string) => void
  disconnect: () => Promise<void>
  exportWallet: (address: string) => Promise<void>
  setRecovery: () => Promise<void>
  clearError: () => void
}

const unavailable = async () => { /* Connection is disabled without a Privy app. */ }

export const WalletActionsContext = createContext<WalletActions>({
  configured: false,
  ready: false,
  signerReady: false,
  authenticated: false,
  modalOpen: false,
  wallets: [],
  error: null,
  connect: () => { /* Connection is disabled without a Privy app. */ },
  selectWallet: () => { /* Connection is disabled without a Privy app. */ },
  openAccount: unavailable,
  disconnect: unavailable,
  exportWallet: unavailable,
  setRecovery: unavailable,
  clearError: () => { /* No error outside the configured provider. */ },
})

export function useWalletActions() {
  return use(WalletActionsContext)
}
