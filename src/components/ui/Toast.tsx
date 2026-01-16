import { useEffect, useState } from 'react'

export type ToastType = 'success' | 'error' | 'info'

interface ToastProps {
  id: string
  type: ToastType
  title: string
  message?: string
  txHash?: string
  duration?: number
  onClose: (id: string) => void
}

export function Toast({ id, type, title, message, txHash, duration = 5000, onClose }: ToastProps) {
  const [isLeaving, setIsLeaving] = useState(false)

  useEffect(() => {
    const timer = setTimeout(() => {
      setIsLeaving(true)
      setTimeout(() => { onClose(id); }, 300)
    }, duration)

    return () => { clearTimeout(timer); }
  }, [id, duration, onClose])

  const handleClose = () => {
    setIsLeaving(true)
    setTimeout(() => { onClose(id); }, 300)
  }

  const bgColor = {
    success: 'bg-cyber-neon-green/20 border-cyber-neon-green',
    error: 'bg-red-500/20 border-red-500',
    info: 'bg-cyber-bright-blue/20 border-cyber-bright-blue',
  }[type]

  const iconColor = {
    success: 'text-cyber-neon-green',
    error: 'text-red-500',
    info: 'text-cyber-bright-blue',
  }[type]

  const icon = {
    success: 'check_circle',
    error: 'error',
    info: 'info',
  }[type]

  return (
    <div
      className={`${bgColor} border rounded-lg p-4 shadow-lg backdrop-blur-sm transition-all duration-300 ${
        isLeaving ? 'opacity-0 translate-x-full' : 'opacity-100 translate-x-0'
      }`}
    >
      <div className="flex items-start gap-3">
        <span className={`material-symbols-outlined ${iconColor}`}>{icon}</span>
        <div className="flex-1 min-w-0">
          <p className="text-cyber-text-primary font-medium">{title}</p>
          {message && <p className="text-cyber-text-secondary text-sm mt-1">{message}</p>}
          {txHash && (
            <a
              href={`https://sepolia.etherscan.io/tx/${txHash}`}
              target="_blank"
              rel="noopener noreferrer"
              className="text-cyber-bright-blue text-sm mt-1 hover:underline inline-flex items-center gap-1"
            >
              View on Etherscan
              <span className="material-symbols-outlined text-sm">open_in_new</span>
            </a>
          )}
        </div>
        <button
          onClick={handleClose}
          className="text-cyber-text-secondary hover:text-cyber-text-primary"
        >
          <span className="material-symbols-outlined text-lg">close</span>
        </button>
      </div>
    </div>
  )
}

interface ToastItem {
  id: string
  type: ToastType
  title: string
  message?: string
  txHash?: string
}

interface ToastContainerProps {
  toasts: ToastItem[]
  onClose: (id: string) => void
}

export function ToastContainer({ toasts, onClose }: ToastContainerProps) {
  return (
    <div className="fixed bottom-4 right-4 z-50 flex flex-col gap-2 max-w-sm w-full">
      {toasts.map((toast) => (
        <Toast key={toast.id} {...toast} onClose={onClose} />
      ))}
    </div>
  )
}
