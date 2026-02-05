import { type ReactNode } from 'react'

type BadgeVariant = 'default' | 'success' | 'warning' | 'danger' | 'info'

interface BadgeProps {
  children: ReactNode
  variant?: BadgeVariant
  size?: 'sm' | 'md'
}

const variantStyles: Record<BadgeVariant, string> = {
  default: 'bg-cyber-surface-light text-cyber-text-secondary border border-cyber-border-glow/30',
  success: 'bg-cyber-neon-green/20 text-cyber-neon-green border border-cyber-neon-green/30 shadow-sm shadow-cyber-neon-green/10',
  warning: 'bg-cyber-warning-bg text-cyber-warning-text border border-cyber-warning-text/30 shadow-sm shadow-cyber-warning-text/10',
  danger: 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia border border-cyber-electric-fuchsia/30 shadow-sm shadow-cyber-electric-fuchsia/10',
  info: 'bg-cyber-bright-blue/20 text-cyber-bright-blue border border-cyber-bright-blue/30 shadow-sm shadow-cyber-bright-blue/10',
}

const sizeStyles = {
  sm: 'px-2 py-0.5 text-xs',
  md: 'px-2.5 py-1 text-sm',
}

export function Badge({ children, variant = 'default', size = 'sm' }: BadgeProps) {
  return (
    <span
      className={`
        inline-flex items-center font-medium rounded-full
        ${variantStyles[variant]}
        ${sizeStyles[size]}
      `}
    >
      {children}
    </span>
  )
}
