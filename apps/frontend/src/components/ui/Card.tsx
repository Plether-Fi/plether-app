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
