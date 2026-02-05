import { type ReactNode, useState } from 'react'

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
  const [isVisible, setIsVisible] = useState(false)

  return (
    <div
      className="relative inline-flex"
      onMouseEnter={() => { setIsVisible(true); }}
      onMouseLeave={() => { setIsVisible(false); }}
    >
      {children}
      {isVisible && (
        <div
          className={`
            absolute z-50 px-3 py-2 text-sm text-cyber-text-primary bg-cyber-surface-dark
            border border-cyber-border-glow/50  shadow-lg shadow-cyber-border-glow/20 whitespace-nowrap
            ${positionStyles[position]}
          `}
        >
          {content}
        </div>
      )}
    </div>
  )
}
