import { Skeleton } from './Skeleton'

export function SkeletonCard() {
  return (
    <div className="bg-surface-panel  border border-brand-border/30 p-4">
      <Skeleton className="w-1/3 mb-4" height={20} />
      <Skeleton className="w-full mb-2" height={16} />
      <Skeleton className="w-2/3" height={16} />
    </div>
  )
}
