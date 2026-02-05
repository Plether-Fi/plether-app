import { type ButtonHTMLAttributes, type ReactNode } from 'react'

type ButtonVariant = 'primary' | 'secondary' | 'danger' | 'ghost'
type ButtonSize = 'sm' | 'md' | 'lg'

interface ButtonProps extends ButtonHTMLAttributes<HTMLButtonElement> {
  variant?: ButtonVariant
  size?: ButtonSize
  isLoading?: boolean
  children: ReactNode
}

const variantStyles: Record<ButtonVariant, string> = {
  primary: 'bg-cyber-neon-green hover:bg-cyber-neon-green/80 text-cyber-bg shadow-lg shadow-cyber-neon-green/30',
  secondary: 'bg-cyber-surface-light hover:bg-cyber-surface-light/80 text-cyber-text-primary border border-cyber-border-glow/30 hover:border-cyber-bright-blue/50',
  danger: 'bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/80 text-cyber-text-primary shadow-lg shadow-cyber-electric-fuchsia/30',
  ghost: 'bg-transparent hover:bg-cyber-surface-light text-cyber-text-secondary hover:text-cyber-bright-blue',
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
  ...props
}: ButtonProps) {
  return (
    <button
      disabled={disabled === true || isLoading}
      className={`
        inline-flex items-center justify-center gap-2  font-medium
        transition-all duration-200
        disabled:opacity-50 disabled:cursor-not-allowed disabled:shadow-none
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
