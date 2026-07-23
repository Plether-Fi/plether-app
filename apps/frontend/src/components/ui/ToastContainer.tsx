import { Toast, type ToastType } from './Toast'

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
    <div className="fixed inset-x-4 bottom-[calc(5rem+env(safe-area-inset-bottom))] z-[80] flex w-auto flex-col gap-2 sm:left-auto sm:w-full sm:max-w-sm lg:bottom-4">
      {toasts.map((toast) => (
        <Toast key={toast.id} {...toast} onClose={onClose} />
      ))}
    </div>
  )
}
