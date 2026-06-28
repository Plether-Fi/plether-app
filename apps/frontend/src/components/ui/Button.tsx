import { type ButtonHTMLAttributes, type MouseEvent, type ReactNode } from 'react'
import { trackPerpsButtonClicked } from '../../analytics/perps'
import type { AnalyticsProperties } from '../../analytics/client'

type ButtonVariant = 'primary' | 'secondary' | 'danger' | 'ghost'
type ButtonSize = 'sm' | 'md' | 'lg'

interface ButtonProps extends ButtonHTMLAttributes<HTMLButtonElement> {
  variant?: ButtonVariant
  size?: ButtonSize
  isLoading?: boolean
  analyticsId?: string
  analyticsSurface?: string
  analyticsProperties?: AnalyticsProperties
  children: ReactNode
}

const variantStyles: Record<ButtonVariant, string> = {
  primary:
    'border border-positive bg-positive text-app-bg enabled:hover:border-[#00CC77] enabled:hover:bg-[#00CC77] enabled:hover:underline enabled:hover:underline-offset-4',
  secondary:
    'border border-brand-border/30 bg-surface-muted text-content-primary enabled:hover:border-[#FFAB96] enabled:hover:bg-[#3B212D] enabled:hover:underline enabled:hover:underline-offset-4',
  danger:
    'border border-brand-orange bg-brand-orange text-content-primary enabled:hover:border-[#FF572D] enabled:hover:bg-[#FF572D] enabled:hover:underline enabled:hover:underline-offset-4',
  ghost:
    'border border-transparent bg-transparent text-content-secondary enabled:hover:bg-[#3B212D] enabled:hover:text-content-primary enabled:hover:underline enabled:hover:underline-offset-4',
}

const sizeStyles: Record<ButtonSize, string> = {
  sm: 'px-3 py-1.5 text-sm',
  md: 'px-4 py-2 text-base',
  lg: 'px-6 py-3 text-lg',
}

export function Button({
  variant = 'primary',
  size = 'md',
  isLoading = false,
  disabled,
  children,
  className = '',
  onClick,
  analyticsId,
  analyticsSurface = 'perps',
  analyticsProperties,
  ...props
}: ButtonProps) {
  function handleClick(event: MouseEvent<HTMLButtonElement>) {
    if (analyticsId) {
      trackPerpsButtonClicked(analyticsId, {
        surface: analyticsSurface,
        ...analyticsProperties,
      })
    }
    onClick?.(event)
  }

  return (
    <button
      disabled={disabled === true || isLoading}
      onClick={handleClick}
      className={`
        inline-flex items-center justify-center gap-2 font-medium cursor-pointer
        transition-all duration-200
        disabled:opacity-50 disabled:cursor-not-allowed
        ${variantStyles[variant]}
        ${sizeStyles[size]}
        ${className}
      `}
      {...props}
    >
      {isLoading && (
        <div className="w-4 h-4 relative">
          <div className="absolute inset-0 rounded-full border-2 border-current/30 border-t-current animate-spin" />
        </div>
      )}
      {children}
    </button>
  )
}
