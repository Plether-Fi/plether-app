import { type ReactNode } from 'react'
import { Tooltip } from './Tooltip'

interface InfoTooltipProps {
  content: ReactNode
  ariaLabel?: string
}

export function InfoTooltip({ content, ariaLabel = 'More information' }: InfoTooltipProps) {
  return (
    <Tooltip content={content} className="max-w-80 whitespace-normal">
      <button
        type="button"
        className="text-content-secondary transition-colors hover:text-[#FFAB96]"
        aria-label={ariaLabel}
      >
        (i)
      </button>
    </Tooltip>
  )
}
