import { Skeleton } from './Skeleton'

export function SkeletonCard() {
  return (
    <div className="bg-cyber-surface-dark  border border-cyber-border-glow/30 p-4 shadow-lg shadow-cyber-border-glow/10">
      <Skeleton className="w-1/3 mb-4" height={20} />
      <Skeleton className="w-full mb-2" height={16} />
      <Skeleton className="w-2/3" height={16} />
    </div>
  )
}
