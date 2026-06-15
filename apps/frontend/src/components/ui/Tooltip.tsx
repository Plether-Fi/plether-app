import { type ReactNode } from 'react'

interface TooltipProps {
  content: ReactNode
  children: ReactNode
  position?: 'top' | 'bottom' | 'left' | 'right'
}

const positionStyles = {
  top: 'bottom-full left-1/2 -translate-x-1/2 mb-2',
  bottom: 'top-full left-1/2 -translate-x-1/2 mt-2',
  left: 'right-full top-1/2 -translate-y-1/2 mr-2',
  right: 'left-full top-1/2 -translate-y-1/2 ml-2',
}

export function Tooltip({ content, children, position = 'top' }: TooltipProps) {
  return (
    <div className="group/tooltip relative inline-flex">
      {children}
      <div
        className={`
          pointer-events-none absolute z-[1000] hidden whitespace-nowrap border border-cyber-border-glow/50
          bg-cyber-surface-dark px-3 py-2 text-sm text-cyber-text-primary shadow-lg shadow-cyber-border-glow/20
          group-hover/tooltip:block group-focus-within/tooltip:block
          ${positionStyles[position]}
        `}
      >
        {content}
      </div>
    </div>
  )
}
