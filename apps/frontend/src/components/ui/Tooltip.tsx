import { type ReactNode } from 'react'

interface TooltipProps {
  content: ReactNode
  children: ReactNode
  position?: 'top' | 'bottom' | 'bottom-end' | 'left' | 'right'
  className?: string
}

const positionStyles = {
  top: 'bottom-full left-1/2 -translate-x-1/2 mb-2',
  bottom: 'top-full left-1/2 -translate-x-1/2 mt-2',
  'bottom-end': 'top-full right-0 mt-2',
  left: 'right-full top-1/2 -translate-y-1/2 mr-2',
  right: 'left-full top-1/2 -translate-y-1/2 ml-2',
}

export function Tooltip({ content, children, position = 'top', className = '' }: TooltipProps) {
  const whitespaceClass = className.includes('whitespace-') ? '' : 'whitespace-nowrap'

  return (
    <div className="group/tooltip relative inline-flex">
      {children}
      <div
        className={`
          pointer-events-none absolute z-[1000] box-border hidden min-w-0 break-words border border-cyber-border-glow/50 ${whitespaceClass}
          bg-cyber-bg px-3 py-2 text-sm normal-case text-cyber-text-primary
          group-hover/tooltip:block group-focus-within/tooltip:block
          ${positionStyles[position]} ${className}
        `}
      >
        {content}
      </div>
    </div>
  )
}
