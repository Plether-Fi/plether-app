import { useContext } from 'react'
import { RegistrationWalletContext } from './context'

export function useRegistrationWallet() {
  return useContext(RegistrationWalletContext)
}
