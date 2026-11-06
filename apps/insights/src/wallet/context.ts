import { createContext } from 'react'

export interface RegistrationWalletChoice {
  address: string
  label: string
}

export interface RegistrationWalletActions {
  configured: boolean
  ready: boolean
  isSelecting: boolean
  connect: () => void
  selectWallet: (address: string) => void
  wallets: RegistrationWalletChoice[]
  error: string | null
}

export const RegistrationWalletContext = createContext<RegistrationWalletActions>({
  configured: false,
  ready: false,
  isSelecting: false,
  connect: () => { /* Connection is disabled until a configured provider is ready. */ },
  selectWallet: () => { /* No wallets are available without a configured provider. */ },
  wallets: [],
  error: null,
})
