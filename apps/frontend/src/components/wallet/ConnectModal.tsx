import { useState, useEffect, useCallback } from 'react'
import { useConnect, useConnectors } from 'wagmi'
import QRCode from 'qrcode'
import { Modal } from '../ui/Modal'
import { Spinner } from '../ui/Spinner'
import { useWalletModalStore } from '../../stores/walletModalStore'
import { config } from '../../config/wagmi'

type View = 'list' | 'walletconnect'

export function ConnectModal() {
  const { isOpen, close } = useWalletModalStore()
  const connectors = useConnectors()
  const [view, setView] = useState<View>('list')
  const [qrDataUrl, setQrDataUrl] = useState<string | null>(null)

  const resetModal = useCallback(() => {
    setView('list')
    setQrDataUrl(null)
  }, [])

  const { connect, isPending, error, reset } = useConnect({
    mutation: {
      onSuccess: () => {
        useWalletModalStore.getState().close()
        resetModal()
      },
    },
  })

  const handleClose = useCallback(() => {
    close()
    resetModal()
    reset()
  }, [close, resetModal, reset])

  const injectedConnector = connectors.find((c) => c.id === 'injected')
  const wcConnector = connectors.find((c) => c.id === 'walletConnect')

  useEffect(() => {
    if (!wcConnector || view !== 'walletconnect') return

    const handleMessage = (message: { type: string; data?: unknown }) => {
      if (message.type === 'display_uri' && typeof message.data === 'string') {
        void QRCode.toDataURL(message.data, {
          width: 280,
          margin: 2,
          color: { dark: '#000000', light: '#ffffff' },
        }).then(setQrDataUrl)
      }
    }

    wcConnector.emitter.on('message', handleMessage)
    return () => { wcConnector.emitter.off('message', handleMessage) }
  }, [wcConnector, view])

  const connectInjected = () => {
    if (!injectedConnector) return
    connect({ connector: injectedConnector })
  }

  const connectWalletConnect = () => {
    if (!wcConnector) return
    setView('walletconnect')
    setQrDataUrl(null)
    connect({ connector: wcConnector })
  }

  const hasInjected = !!injectedConnector && !!config.connectors.find((c) => c.id === 'injected')

  return (
    <Modal isOpen={isOpen} onClose={handleClose} title="Connect Wallet" size="sm">
      {view === 'list' && (
        <div className="flex flex-col gap-3">
          {hasInjected && (
            <button
              onClick={connectInjected}
              disabled={isPending}
              className="flex items-center gap-3 w-full p-3 bg-cyber-surface-light hover:bg-cyber-surface-light/80 border border-cyber-border-glow/30 hover:border-cyber-border-glow/60 transition-colors disabled:opacity-50"
            >
              {isPending ? (
                <Spinner size="sm" />
              ) : (
                <span className="material-symbols-outlined text-xl text-cyber-bright-blue">language</span>
              )}
              <span className="text-cyber-text-primary font-medium">Browser Wallet</span>
            </button>
          )}

          {wcConnector && (
            <button
              onClick={connectWalletConnect}
              disabled={isPending}
              className="flex items-center gap-3 w-full p-3 bg-cyber-surface-light hover:bg-cyber-surface-light/80 border border-cyber-border-glow/30 hover:border-cyber-border-glow/60 transition-colors disabled:opacity-50"
            >
              <span className="material-symbols-outlined text-xl text-cyber-bright-blue">qr_code_2</span>
              <span className="text-cyber-text-primary font-medium">WalletConnect</span>
            </button>
          )}

          {error && (
            <p className="text-sm text-cyber-electric-fuchsia mt-1">{error.message}</p>
          )}
        </div>
      )}

      {view === 'walletconnect' && (
        <div className="flex flex-col items-center gap-4">
          <button
            onClick={() => { setView('list'); setQrDataUrl(null); reset() }}
            className="self-start flex items-center gap-1 text-sm text-cyber-text-secondary hover:text-cyber-bright-blue transition-colors"
          >
            <span className="material-symbols-outlined text-base">arrow_back</span>
            Back
          </button>

          {qrDataUrl ? (
            <img src={qrDataUrl} alt="WalletConnect QR Code" className="rounded-lg" width={280} height={280} />
          ) : (
            <div className="flex items-center justify-center w-[280px] h-[280px]">
              <Spinner size="lg" />
            </div>
          )}

          <p className="text-sm text-cyber-text-secondary text-center">
            Scan with your mobile wallet
          </p>

          {error && (
            <div className="flex flex-col items-center gap-2">
              <p className="text-sm text-cyber-electric-fuchsia">{error.message}</p>
              <button
                onClick={connectWalletConnect}
                className="text-sm text-cyber-bright-blue hover:underline"
              >
                Retry
              </button>
            </div>
          )}
        </div>
      )}
    </Modal>
  )
}
