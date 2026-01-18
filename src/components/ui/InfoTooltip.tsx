import { type ReactNode } from 'react'
import { Tooltip } from './Tooltip'

interface InfoTooltipProps {
  content: ReactNode
}

export function InfoTooltip({ content }: InfoTooltipProps) {
  return (
    <Tooltip content={content}>
      <span className="inline-flex items-center justify-center w-4 h-4 text-cyber-text-secondary hover:text-cyber-bright-blue cursor-help transition-colors">
        <span className="material-symbols-outlined text-sm">help</span>
      </span>
    </Tooltip>
  )
}
