import { useState } from 'react'
import { Modal } from '../components/ui/Modal'
import { Button } from '../components/ui/Button'

export function WalletConnectionError({ message, onRetry }: { message: string; onRetry: () => void }) {
  const [dismissed, setDismissed] = useState(false)
  return <Modal isOpen={!dismissed} onClose={() => { setDismissed(true) }}
    title="Wallet connection unavailable" size="sm"
    footer={<Button onClick={onRetry} className="w-full">Retry connection</Button>}>
    <p role="alert" className="text-sm leading-relaxed text-content-secondary">{message}</p>
    <p className="mt-3 text-xs leading-relaxed text-content-secondary">
      You can close this message and keep browsing. Retry reloads the app and keeps your saved wallet and login data.
    </p>
  </Modal>
}
