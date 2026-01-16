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

export function SkeletonCard() {
  return (
    <div className="bg-cyber-surface-dark  border border-cyber-border-glow/30 p-4 shadow-lg shadow-cyber-border-glow/10">
      <Skeleton className="w-1/3 mb-4" height={20} />
      <Skeleton className="w-full mb-2" height={16} />
      <Skeleton className="w-2/3" height={16} />
    </div>
  )
}
