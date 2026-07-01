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
        <h3 className="text-lg font-semibold text-content-primary">{title}</h3>
        {subtitle && <p className="text-sm text-content-secondary">{subtitle}</p>}
      </div>
      {action && <div>{action}</div>}
    </div>
  )
}
