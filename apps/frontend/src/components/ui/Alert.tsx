import type { ReactNode } from 'react'

type AlertVariant = 'info' | 'warning' | 'success' | 'error'

const variantStyles: Record<AlertVariant, { container: string; icon: string }> = {
  info: {
    container: 'bg-cyber-bright-blue/10 border-cyber-bright-blue/30 text-cyber-bright-blue',
    icon: 'info',
  },
  warning: {
    container: 'bg-cyber-warning-bg border-cyber-warning-text/30 text-cyber-warning-text',
    icon: 'warning',
  },
  success: {
    container: 'bg-cyber-neon-green/10 border-cyber-neon-green/30 text-cyber-neon-green',
    icon: 'check_circle',
  },
  error: {
    container: 'bg-cyber-electric-fuchsia/10 border-cyber-electric-fuchsia/30 text-cyber-electric-fuchsia',
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
