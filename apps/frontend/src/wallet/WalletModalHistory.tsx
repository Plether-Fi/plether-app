import type { ReactNode } from 'react'
import { useModalBackButton } from '../hooks/useModalBackButton'

export function WalletModalHistory({ onClose, children }: { onClose: () => void; children: ReactNode }) {
  useModalBackButton(true, onClose, 'privy-wallet-dialog')
  return children
}
