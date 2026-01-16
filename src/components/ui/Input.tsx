import { type InputHTMLAttributes, type ReactNode } from 'react'

interface InputProps extends InputHTMLAttributes<HTMLInputElement> {
  label?: string
  error?: string
  rightElement?: ReactNode
}

export const Input = ({ ref, label, error, rightElement, className = '', ...props }: InputProps & { ref?: React.RefObject<HTMLInputElement | null> }) => {
    return (
      <div className="w-full">
        {label && (
          <label className="block text-sm font-medium text-cyber-text-secondary mb-1.5">
            {label}
          </label>
        )}
        <div className="relative">
          <input
            ref={ref}
            className={`
              w-full px-4 py-3 bg-cyber-surface-light border  text-cyber-text-primary
              placeholder-cyber-text-secondary/50 focus:outline-none focus:ring-1
              focus:ring-cyber-bright-blue focus:border-cyber-bright-blue
              transition-all shadow-sm shadow-cyber-border-glow/10
              ${error ? 'border-cyber-electric-fuchsia' : 'border-cyber-border-glow/30'}
              ${rightElement ? 'pr-20' : ''}
              ${className}
            `}
            {...props}
          />
          {rightElement && (
            <div className="absolute right-3 top-1/2 -translate-y-1/2">
              {rightElement}
            </div>
          )}
        </div>
        {error && <p className="mt-1 text-sm text-cyber-electric-fuchsia">{error}</p>}
      </div>
    )
  }

Input.displayName = 'Input'
