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
    success: 'bg-positive/20 border-positive',
    error: 'bg-red-500/20 border-red-500',
    info: 'bg-brand-peach/20 border-brand-peach',
  }[type]

  const iconColor = {
    success: 'text-positive',
    error: 'text-red-500',
    info: 'text-brand-peach',
  }[type]

  const icon = {
    success: 'check_circle',
    error: 'error',
    info: 'info',
  }[type]

  return (
    <div
      className={`${bgColor} border rounded-lg p-4 transition-all duration-300 ${
        isLeaving ? 'opacity-0 translate-x-full' : 'opacity-100 translate-x-0'
      }`}
    >
      <div className="flex items-start gap-3">
        <span className={`material-symbols-outlined ${iconColor}`}>{icon}</span>
        <div className="flex-1 min-w-0">
          <p className="text-content-primary font-medium">{title}</p>
          {message && <p className="text-content-secondary text-sm mt-1">{message}</p>}
          {txHash && (
            <a
              href={`https://sepolia.etherscan.io/tx/${txHash}`}
              target="_blank"
              rel="noopener noreferrer"
              className="text-brand-peach text-sm mt-1 hover:underline inline-flex items-center gap-1"
            >
              View on Etherscan
              <span className="material-symbols-outlined text-sm">open_in_new</span>
            </a>
          )}
        </div>
        <button
          onClick={handleClose}
          className="text-content-secondary hover:text-content-primary"
        >
          <span className="material-symbols-outlined text-lg">close</span>
        </button>
      </div>
    </div>
  )
}
