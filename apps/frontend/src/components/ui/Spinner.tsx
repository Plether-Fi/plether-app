interface SpinnerProps {
  size?: 'sm' | 'md' | 'lg'
  variant?: 'default' | 'confirming'
  className?: string
}

const sizeStyles = {
  sm: 'h-4 w-4',
  md: 'h-6 w-6',
  lg: 'h-8 w-8',
}

const variantStyles = {
  default: 'border-cyber-bright-blue/30 border-t-cyber-bright-blue',
  confirming: 'border-cyber-neon-green/30 border-t-cyber-neon-green',
}

export function Spinner({ size = 'md', variant = 'default', className = '' }: SpinnerProps) {
  return (
    <div className={`${sizeStyles[size]} ${className} relative`}>
      <div className={`absolute inset-0 rounded-full border-2 ${variantStyles[variant]} animate-spin`} />
    </div>
  )
}
