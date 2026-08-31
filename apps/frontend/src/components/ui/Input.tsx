import { type InputHTMLAttributes, type ReactNode, useId } from 'react'

interface InputProps extends InputHTMLAttributes<HTMLInputElement> {
  label?: string
  error?: string
  rightElement?: ReactNode
}

export const Input = ({ ref, label, error, rightElement, className = '', ...props }: InputProps & { ref?: React.RefObject<HTMLInputElement | null> }) => {
  const generatedId = useId()
  const inputId = props.id ?? generatedId

  return (
    <div className="w-full">
      {label && (
        <label htmlFor={inputId} className="block text-sm font-medium text-content-secondary mb-1.5">
          {label}
        </label>
      )}
      <div className="relative">
        <input
          id={inputId}
          ref={ref}
          className={`
              w-full px-4 py-3 bg-app-bg border  text-content-primary
              placeholder-content-secondary/50 focus:outline-none
              focus:border-[#FFAB96]
              transition-all
              ${error ? 'border-brand-orange' : 'border-brand-border/30'}
              ${rightElement ? 'pr-20' : ''}
              ${className}
            `}
          {...props}
        />
        {rightElement && (
          <div className="absolute right-3 top-1/2 flex -translate-y-1/2 items-center">
            {rightElement}
          </div>
        )}
      </div>
      {error && <p className="mt-1 text-sm text-brand-orange">{error}</p>}
    </div>
  )
}

Input.displayName = 'Input'
