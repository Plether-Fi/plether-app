interface SkeletonProps {
  className?: string
  variant?: 'text' | 'rectangular' | 'circular'
  width?: string | number
  height?: string | number
}

export function Skeleton({
  className = '',
  variant = 'text',
  width,
  height,
}: SkeletonProps) {
  const baseStyles = 'animate-pulse bg-cyber-surface-light'

  const variantStyles = {
    text: 'h-4',
    rectangular: '',
    circular: 'rounded-full',
  }

  const style: React.CSSProperties = {}
  if (width) style.width = typeof width === 'number' ? `${String(width)}px` : width
  if (height) style.height = typeof height === 'number' ? `${String(height)}px` : height

  return (
    <div
      className={`${baseStyles} ${variantStyles[variant]} ${className}`}
      style={style}
    />
  )
}
