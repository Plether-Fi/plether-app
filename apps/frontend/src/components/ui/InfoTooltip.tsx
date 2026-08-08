import { type ReactNode } from 'react'
import { Tooltip, type TooltipDocsLink } from './Tooltip'

interface InfoTooltipProps {
  content: ReactNode
  ariaLabel?: string
  docsLink?: TooltipDocsLink
}

export const INFO_TOOLTIP_PANEL_CLASS_NAME =
  'w-[320px] max-w-[calc(100vw-2rem)] whitespace-normal p-3 text-left leading-5'

export function InfoTooltip({ content, ariaLabel = 'More information', docsLink }: InfoTooltipProps) {
  return (
    <Tooltip
      content={content}
      className={INFO_TOOLTIP_PANEL_CLASS_NAME}
      docsLink={docsLink}
    >
      <span
        className="relative inline-flex h-3.5 w-3.5 shrink-0 cursor-help items-center justify-center rounded-full border border-current text-[9px] font-semibold leading-none text-content-secondary/80 transition-colors after:absolute after:-inset-4 after:content-[''] hover:text-[#FFAB96]"
        aria-label={ariaLabel}
        tabIndex={0}
      >
        i
      </span>
    </Tooltip>
  )
}
