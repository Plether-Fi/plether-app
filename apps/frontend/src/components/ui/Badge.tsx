import { type ReactNode } from 'react'

type BadgeVariant = 'default' | 'success' | 'warning' | 'danger' | 'info'

interface BadgeProps {
  children: ReactNode
  variant?: BadgeVariant
  size?: 'sm' | 'md'
}

const variantStyles: Record<BadgeVariant, string> = {
  default: 'bg-surface-muted text-content-secondary border border-brand-border/30',
  success: 'bg-positive/20 text-positive border border-positive/30',
  warning: 'bg-warning-bg text-warning border border-warning/30',
  danger: 'bg-brand-orange/20 text-brand-orange border border-brand-orange/30',
  info: 'bg-brand-peach/20 text-brand-peach border border-brand-peach/30',
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
