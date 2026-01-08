import { type ReactNode } from 'react'

interface CardProps {
  children: ReactNode
  className?: string
  padding?: 'none' | 'sm' | 'md' | 'lg'
}

const paddingStyles = {
  none: '',
  sm: 'p-3',
  md: 'p-4',
  lg: 'p-6',
}

export function Card({ children, className = '', padding = 'md' }: CardProps) {
  return (
    <div
      className={`
        bg-cyber-surface-dark  border border-cyber-border-glow/30
        shadow-lg shadow-cyber-border-glow/10
        ${paddingStyles[padding]}
        ${className}
      `}
    >
      {children}
    </div>
  )
}

interface CardHeaderProps {
  title: string
  subtitle?: string
  action?: ReactNode
}

export function CardHeader({ title, subtitle, action }: CardHeaderProps) {
  return (
    <div className="flex items-center justify-between mb-4">
      <div>
        <h3 className="text-lg font-semibold text-cyber-text-primary">{title}</h3>
        {subtitle && <p className="text-sm text-cyber-text-secondary">{subtitle}</p>}
      </div>
      {action && <div>{action}</div>}
    </div>
  )
}
