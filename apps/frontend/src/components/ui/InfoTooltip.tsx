import { type ReactNode } from 'react'
import { Tooltip } from './Tooltip'

interface InfoTooltipProps {
  content: ReactNode
  ariaLabel?: string
}

export function InfoTooltip({ content, ariaLabel = 'More information' }: InfoTooltipProps) {
  return (
    <Tooltip content={content} className="max-w-80 whitespace-normal">
      <span
        className="inline-flex h-3.5 w-3.5 shrink-0 cursor-help items-center justify-center rounded-full border border-current text-[9px] font-semibold leading-none text-content-secondary/80 transition-colors hover:text-[#FFAB96]"
        aria-label={ariaLabel}
        tabIndex={0}
      >
        i
      </span>
    </Tooltip>
  )
}
