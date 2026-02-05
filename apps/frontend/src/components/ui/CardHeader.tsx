import { type ReactNode } from 'react'

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
