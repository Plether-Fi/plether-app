import type { ReactNode } from 'react'

type AlertVariant = 'info' | 'warning' | 'success' | 'error'

const variantStyles: Record<AlertVariant, { container: string; icon: string }> = {
  info: {
    container: 'bg-brand-peach/10 border-brand-peach/30 text-brand-peach',
    icon: 'info',
  },
  warning: {
    container: 'bg-brand-peach/10 border-brand-peach/40 text-brand-peach',
    icon: 'warning',
  },
  success: {
    container: 'bg-positive/10 border-positive/30 text-positive',
    icon: 'check_circle',
  },
  error: {
    container: 'bg-brand-orange/10 border-brand-orange/30 text-brand-orange',
    icon: 'error',
  },
}

export interface AlertProps {
  variant?: AlertVariant
  title?: string
  children: ReactNode
  icon?: string
  className?: string
}

export function Alert({ variant = 'info', title, children, icon, className = '' }: AlertProps) {
  const styles = variantStyles[variant]
  const iconName = icon ?? styles.icon

  return (
    <div className={`border p-4 flex items-start gap-3 ${styles.container} ${className}`}>
      <span className="material-symbols-outlined mt-0.5">{iconName}</span>
      <div>
        {title && <h3 className="font-medium text-sm">{title}</h3>}
        <div className={`text-sm ${title ? 'mt-1 opacity-80' : ''}`}>{children}</div>
      </div>
    </div>
  )
}
